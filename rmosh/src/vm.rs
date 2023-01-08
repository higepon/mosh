use std::{
    collections::HashMap,
    fmt::Display,
    ptr::{null, null_mut},
};

use crate::{
    compiler,
    equal::Equal,
    fasl::Fasl,
    gc::{Gc, GcRef},
    objects::{Closure, Object, Pair, Symbol, Vox},
    op::OpOld,
    procs::{self, default_free_vars},
};

const STACK_SIZE: usize = 256;
const MAX_NUM_VALUES: usize = 256;

#[macro_export]
macro_rules! branch_number_op {
    ($op:tt, $self:ident, $pc:ident, $skip_offset:ident) => {
        {
            match ($self.pop(), $self.ac) {
                (Object::Number(lhs), Object::Number(rhs)) => {
                    let op_result = lhs $op rhs;
                    $self.set_return_value(Object::make_bool(op_result));
                    if op_result {
                        // go to then.
                    } else {
                        // Branch and jump to else.
                        $pc = $self.jump($pc, $skip_offset - 1);
                    }
                }
                obj => {
                    panic!("{}: numbers requierd but got {:?}",  stringify!($op), obj);
                }
            }
        }
    };
}

#[macro_export]
macro_rules! number_op {
    ($op:tt, $self:ident) => {
        {
            match ($self.pop(), $self.ac) {
                (Object::Number(l), Object::Number(r)) => {
                    $self.set_return_value(Object::make_bool(l $op r))
                }
                obj => {
                    panic!("{}: numbers required but got {:?}",  stringify!($op), obj);
                }
            }
        }
    };
}

pub struct VmOld {
    pub gc: Box<Gc>,
    // The stack.
    stack: [Object; STACK_SIZE],
    // accumulator register.
    pub ac: Object,
    // display closure register.
    dc: Object,
    // expected register to retain expected value for tests.
    pub expected: Object,
    // stack pointer.
    sp: *mut Object,
    // frame pointer.
    fp: *mut Object,
    // global variables.
    globals: HashMap<GcRef<Symbol>, Object>,
    // We keep the lib_ops here so that the lib_ops live longer than every call of run.
    // If we kept lib_ops as local variable, it can/will be immediately freed after run(lib_ops).
    pub lib_ops: Vec<OpOld>,
    // Return values.
    values: [Object; MAX_NUM_VALUES],
    num_values: usize,
    pub rtds: HashMap<Object, Object>,
    pub should_load_compiler: bool,
    // Note when we add new vars here, please make sure we take care of them in mark_roots.
    // Otherwise they can cause memory leak or double free.
}

impl VmOld {
    pub fn new() -> Self {
        Self {
            gc: Box::new(Gc::new()),
            stack: [Object::Unspecified; STACK_SIZE],
            ac: Object::Unspecified,
            dc: Object::Unspecified,
            expected: Object::Unspecified,
            sp: null_mut(),
            fp: null_mut(),
            globals: HashMap::new(),
            lib_ops: vec![],
            num_values: 0,
            values: [Object::Unspecified; MAX_NUM_VALUES],
            rtds: HashMap::new(),
            should_load_compiler: false,
        }
    }

    pub fn intern(&mut self, s: &str) -> GcRef<Symbol> {
        self.gc.intern(s)
    }

    pub fn values(&mut self, values: &[Object]) -> Object {
        let n = values.len();
        self.num_values = n;
        if 0 == n {
            return Object::Unspecified;
        }
        for i in 1..n as usize {
            if i >= MAX_NUM_VALUES {
                panic!("values: too many values");
            }
            self.values[i - 1] = values[i];
        }
        // this is set to ac later.
        return values[0];
    }

    fn initialize_free_vars(&mut self, ops: *const OpOld, ops_len: usize) {
        let free_vars = default_free_vars(&mut self.gc);
        let mut display = self
            .gc
            .alloc(Closure::new(ops, ops_len, 0, false, free_vars));
        display.prev = self.dc;
        self.dc = Object::Closure(display);
    }

    // GC functions.
    fn alloc<T: Display + 'static>(&mut self, object: T) -> GcRef<T> {
        self.mark_and_sweep();
        self.gc.alloc(object)
    }

    pub fn mark_and_sweep(&mut self) {
        if self.gc.should_gc() {
            #[cfg(feature = "debug_log_gc")]
            println!("-- gc begin");

            self.mark_roots();
            self.gc.collect_garbage();

            #[cfg(feature = "debug_log_gc")]
            println!("-- gc end");
        }
    }

    fn mark_roots(&mut self) {
        // Base library ops.
        for &op in &self.lib_ops {
            self.gc.mark_op(op);
        }

        // Stack.
        for &obj in &self.stack[0..self.stack_len()] {
            self.gc.mark_object(obj);
        }

        // Values.
        for &obj in &self.values[0..self.num_values] {
            self.gc.mark_object(obj);
        }

        // Symbols.
        let symbols = self
            .gc
            .symbols
            .values()
            .cloned()
            .collect::<Vec<GcRef<Symbol>>>();
        for symbol in symbols {
            self.gc.mark_object(Object::Symbol(symbol));
        }

        // Global variables.
        for &obj in self.globals.values() {
            self.gc.mark_object(obj);
        }

        // RTDs.
        for (k, v) in self.rtds.iter() {
            self.gc.mark_object(*k);
            self.gc.mark_object(*v);
        }

        // Registers.
        self.gc.mark_object(self.ac);
        self.gc.mark_object(self.dc);
        self.gc.mark_object(self.expected);
    }

    pub fn run(&mut self, ops: *const OpOld, ops_len: usize) -> Object {
        // Create display closure and make free variables accessible.
        self.initialize_free_vars(ops, ops_len);

        // Load the base library.
        let lib_ops = if self.should_load_compiler {
            self.register_compiler()
        } else {
            self.register_baselib()
        };
        self.run_ops(lib_ops);

        // Run the program.
        let ret = self.run_ops(ops);

        // Clean up so that GC can sweep them.
        self.reset_roots();
        ret
    }

    fn run_ops(&mut self, ops: *const OpOld) -> Object {
        self.sp = self.stack.as_mut_ptr();
        self.fp = self.sp;

        let mut pc: *const OpOld = ops;
        loop {
            let op = unsafe { *pc };
            match op {
                OpOld::List(n) => {
                    let mut list = Object::Nil;
                    for i in 0..n {
                        list = self.gc.cons(self.index(self.sp, i as isize), list);
                    }
                    self.set_return_value(list);
                    self.sp = self.dec(self.sp, n as isize);
                }
                OpOld::ReferLocalBranchNotLt(n, skip_offset) => {
                    self.refer_local_op(n);
                    branch_number_op!(<, self, pc, skip_offset);
                }
                OpOld::SimpleStructRef => match (self.pop(), self.ac) {
                    (Object::SimpleStruct(s), Object::Number(idx)) => {
                        self.set_return_value(s.data[idx as usize]);
                    }
                    (obj1, obj2) => {
                        panic!(
                            "simple-struct-ref: simple-struct and idx required but got {} {}",
                            obj1, obj2
                        );
                    }
                },
                OpOld::Vector(n) => {
                    let mut v = vec![Object::Unspecified; n];
                    let mut arg = self.ac;
                    if n > 0 {
                        let mut i = n - 1;
                        loop {
                            if i == 0 {
                                break;
                            }
                            v[i] = arg;
                            arg = self.pop();
                            i -= 1;
                        }
                        v[0] = arg;
                    }
                    let vec = self.gc.new_vector(&v);
                    self.set_return_value(vec);
                }
                OpOld::BranchNotEqual(_) => todo!(),
                OpOld::Cddr => match self.ac {
                    Object::Pair(pair) => match pair.cdr {
                        Object::Pair(pair) => {
                            self.set_return_value(pair.cdr);
                        }
                        obj => {
                            self.arg_err("cddr", "pair", obj);
                        }
                    },
                    obj => {
                        self.arg_err("cddr", "pair", obj);
                    }
                },
                OpOld::NotTest(jump_offset) => {
                    self.ac = if self.ac.is_false() {
                        Object::True
                    } else {
                        Object::False
                    };
                    if self.ac.is_false() {
                        pc = self.jump(pc, jump_offset - 1);
                    }
                }
                OpOld::NumberAddPush => {
                    self.number_add_op();
                    self.push_op();
                }
                OpOld::ReferGlobalPush(symbol) => {
                    self.refer_global_op(symbol);
                    self.push_op();
                }
                OpOld::BranchNotEq(skip_offset) => {
                    let pred = self.pop().eq(&self.ac);
                    self.set_return_value(Object::make_bool(pred));
                    if !pred {
                        // Branch and jump to else.
                        pc = self.jump(pc, skip_offset - 1);
                    }
                }
                OpOld::PushConstant(c) => {
                    self.push_op();
                    self.constant_op(c);
                }
                OpOld::NumberSubPush => {
                    self.number_sub_op();
                    self.push_op();
                }
                OpOld::ReferLocalPushConstantBranchNotLe(_, _, _) => {
                    panic!("not implemented");
                }
                OpOld::ReferLocalPushConstantBranchNotGe(n, c, skip_offset) => {
                    self.refer_local_op(n);
                    self.push_op();
                    self.constant_op(c);
                    branch_number_op!(>=, self, pc, skip_offset);
                }
                OpOld::MakeContinuation(_) => {
                    panic!("not implemented");
                }
                OpOld::Shiftj(depth, diff, display_count) => {
                    // SHIFT for embedded jump which appears in named let optimization.
                    //   Two things happens.
                    //   1. SHIFT the stack (same as SHIFT operation)
                    //   2. Restore fp and c registers.
                    //      This is necessary for jump which is across let or closure boundary.
                    //      new-fp => new-sp - arg-length
                    //
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    self.fp = self.dec(self.sp, depth);
                    let mut i = display_count;
                    loop {
                        if i <= 0 {
                            break;
                        }
                        self.dc = self.dc.to_closure().prev;
                        i -= 1;
                    }
                }
                OpOld::ReferFreeCall(n, argc) => {
                    self.refer_free_op(n);
                    self.call_op(&mut pc, argc);
                }
                OpOld::PushEnter(n) => {
                    self.push_op();
                    self.enter_op(n);
                }
                OpOld::LocalTailCall(depth, diff) => {
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);                    
                    let closure = self.ac.to_closure();                    
                    let argc = depth;
                    self.dc = self.ac;
                    pc = closure.ops_old;
                    self.fp = self.dec(self.sp, argc);
                }
                OpOld::LocalCall(argc) => {
                    // Locall is lighter than Call
                    // We can omit checking closure type and arguments length.
                    match self.ac {
                        Object::Closure(c) => {
                            self.dc = self.ac;
                            // todo
                            //self.cl = self.ac;
                            pc = c.ops_old;
                            self.fp = self.dec(self.sp, argc);
                        }
                        obj => {
                            panic!("LocalCall: Bug {}", obj)
                        }
                    }
                }
                OpOld::Cdar => {
                    panic!("not implemented");
                }
                OpOld::Caar => {
                    panic!("not implemented");
                }
                OpOld::ReferLocalPushConstant(n, c) => {
                    self.refer_local_op(n);
                    self.push_op();
                    self.constant_op(c);
                }
                OpOld::ReferLocalPush(n) => {
                    self.refer_local_op(n);
                    self.push_op();
                }
                OpOld::ReferLocalCall(n, argc) => {
                    self.refer_local_op(n);
                    self.call_op(&mut pc, argc);
                }
                OpOld::ReferLocalBranchNotNull(n, skip_offset) => {
                    self.refer_local_op(n);
                    self.branch_not_null_op(&mut pc, skip_offset);
                }
                OpOld::ReferGlobalCall(symbol, argc) => {
                    self.refer_global_op(symbol);
                    self.call_op(&mut pc, argc);
                }
                OpOld::ReferFreePush(n) => {
                    self.refer_free_op(n);
                    self.push_op();
                }
                OpOld::CarPush => {
                    self.car_op();
                    self.push_op();
                }
                OpOld::ConstantPush(c) => {
                    self.constant_op(c);
                    self.push_op();
                }
                OpOld::CdrPush => {
                    self.cdr_op();
                    self.push_op();
                }
                OpOld::PushFrame(skip_offset) => {
                    self.push_op();
                    self.frame_op(pc, skip_offset);
                }
                OpOld::MakeVector => match self.pop() {
                    Object::Number(size) => {
                        let v = vec![self.ac; size as usize];
                        let v = self.gc.new_vector(&v);
                        self.set_return_value(v);
                    }
                    obj => {
                        self.arg_err("make-vector", "numbers", obj);
                    }
                },
                OpOld::VectorP => match self.ac {
                    Object::Vector(_) => {
                        self.set_return_value(Object::True);
                    }
                    _ => {
                        self.set_return_value(Object::False);
                    }
                },
                OpOld::VectorLength => match self.ac {
                    Object::Vector(v) => {
                        self.set_return_value(Object::Number(v.len() as isize));
                    }
                    obj => {
                        self.arg_err("vector-length", "vector", obj);
                    }
                },
                OpOld::VectorRef => match (self.pop(), self.ac) {
                    (Object::Vector(v), Object::Number(idx)) => {
                        let idx = idx as usize;
                        if idx < v.data.len() {
                            self.set_return_value(v.data[idx]);
                        } else {
                            self.arg_err("vector-ref", "valid idx to vector", self.ac);
                        }
                    }
                    (a, b) => {
                        panic!(
                            "vecto-ref: vector and number required but got {:?} {:?}",
                            a, b
                        );
                    }
                },
                OpOld::VectorSet => {
                    let n = self.pop();
                    let obj = self.pop();
                    match (obj, n) {
                        (Object::Vector(mut v), Object::Number(idx)) => {
                            let idx = idx as usize;
                            if idx < v.data.len() {
                                v.data[idx] = self.ac;
                            } else {
                                self.arg_err("vector-set", "valid idx to vector", obj);
                            }
                        }
                        (a, b) => {
                            panic!(
                                "vecto-set: vector and number required but got {:?} {:?}",
                                a, b
                            );
                        }
                    }
                }
                OpOld::Append2 => {
                    let head = self.pop();
                    if Pair::is_list(head) {
                        let p = self.gc.append2(head, self.ac);
                        self.set_return_value(p);
                    } else {
                        self.arg_err("append", "pair", head);
                    }
                }
                OpOld::Receive(num_req_args, num_opt_args) => {
                    if self.num_values < num_req_args {
                        panic!(
                            "receive: received fewer valeus than expected {} {}",
                            num_req_args, self.num_values
                        );
                    } else if num_opt_args == 0 && self.num_values > num_req_args {
                        panic!(
                            "receive: received more values than expected {} {}",
                            num_req_args, self.num_values
                        );
                    }
                    // (receive (a b c) ...)
                    if num_opt_args == 0 {
                        if num_req_args > 0 {
                            self.push(self.ac);
                        }
                        for i in 0..num_req_args - 1 {
                            self.push(self.values[i]);
                        }
                    // (receive a ...)
                    } else if num_req_args == 0 {
                        let mut ret = if self.num_values == 0 {
                            Object::Nil
                        } else {
                            self.gc.list1(self.ac)
                        };
                        for i in 0..self.num_values - 1 {
                            ret = Pair::append_destructive(ret, self.gc.list1(self.values[i]));
                        }
                        self.push(ret);
                    // (receive (a b . c) ...)
                    } else {
                        let mut ret = Object::Nil;
                        self.push(self.ac);
                        for i in 0..self.num_values - 1 {
                            if i < num_req_args - 1 {
                                self.push(self.values[i]);
                            } else {
                                ret = Pair::append_destructive(ret, self.gc.list1(self.values[i]));
                            }
                        }
                        self.push(ret);
                    }
                }
                OpOld::SetCar => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.car = self.ac;
                        self.set_return_value(Object::Unspecified);
                    }
                    obj => {
                        self.arg_err("set-car!", "pair", obj);
                    }
                },
                OpOld::SetCdr => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.cdr = self.ac;
                        self.set_return_value(Object::Unspecified);
                    }
                    obj => {
                        self.arg_err("set-cdr!", "pair", obj);
                    }
                },
                OpOld::Not => {
                    self.set_return_value(Object::make_bool(self.ac.is_false()));
                }
                OpOld::PairP => self.set_return_value(Object::make_bool(self.ac.is_pair())),
                OpOld::NullP => self.set_return_value(Object::make_bool(self.ac.is_nil())),
                OpOld::SymbolP => self.set_return_value(Object::make_bool(self.ac.is_symbol())),
                OpOld::BranchNotNumberEqual(skip_offset) => {
                    branch_number_op!(==, self, pc, skip_offset);
                }
                OpOld::BranchNotGe(skip_offset) => {
                    branch_number_op!(>=, self, pc, skip_offset);
                }
                OpOld::BranchNotGt(skip_offset) => {
                    branch_number_op!(>, self, pc, skip_offset);
                }
                OpOld::BranchNotLe(skip_offset) => {
                    branch_number_op!(<=, self, pc, skip_offset);
                }
                OpOld::BranchNotLt(skip_offset) => {
                    branch_number_op!(<, self, pc, skip_offset);
                }
                OpOld::BranchNotNull(skip_offset) => {
                    self.branch_not_null_op(&mut pc, skip_offset);
                }
                OpOld::BranchNotEqv(skip_offset) => {
                    if self.pop().eqv(&self.ac) {
                        self.set_return_value(Object::True);
                    } else {
                        pc = self.jump(pc, skip_offset - 1);
                        self.set_return_value(Object::False);
                    }
                }
                OpOld::Eq => {
                    let is_eq = self.pop().eq(&self.ac);
                    self.set_return_value(Object::make_bool(is_eq));
                }
                OpOld::Equal => {
                    let e = Equal::new();
                    let val = self.pop();
                    let ret = e.is_equal(&mut self.gc, &val, &self.ac);
                    self.set_return_value(Object::make_bool(ret));
                }
                OpOld::Eqv => {
                    let ret = self.pop().eqv(&self.ac);
                    self.set_return_value(Object::make_bool(ret));
                }
                OpOld::NumberEqual => {
                    number_op!(==, self);
                }
                OpOld::NumberGe => {
                    number_op!(>=, self);
                }
                OpOld::NumberGt => {
                    number_op!(>, self);
                }
                OpOld::NumberLe => {
                    number_op!(<=, self);
                }
                OpOld::NumberLt => {
                    number_op!(<, self);
                }
                OpOld::Car => {
                    self.car_op();
                }
                OpOld::Cdr => {
                    self.cdr_op();
                }
                OpOld::Cadr => match self.ac {
                    Object::Pair(pair) => match pair.cdr {
                        Object::Pair(pair) => {
                            self.set_return_value(pair.car);
                        }
                        obj => {
                            self.arg_err("cadr", "pair", obj);
                        }
                    },
                    obj => {
                        self.arg_err("cadr", "pair", obj);
                    }
                },
                OpOld::Indirect => match self.ac {
                    Object::Vox(vox) => {
                        self.set_return_value(vox.value);
                    }
                    obj => {
                        self.arg_err("indirect", "vox", obj);
                    }
                },
                OpOld::AssignLocal(n) => match self.refer_local(n) {
                    Object::Vox(mut vox) => vox.value = self.ac,
                    _ => {
                        panic!("assign_local: vox not found")
                    }
                },
                OpOld::Box(n) => {
                    let vox = self.alloc(Vox::new(self.index(self.sp, n)));
                    self.index_set(self.sp, n, Object::Vox(vox));
                }
                OpOld::Constant(c) => {
                    self.constant_op(c);
                }
                OpOld::Push => {
                    self.push_op();
                }
                OpOld::Cons => {
                    let car = self.pop();
                    let cdr = self.ac;
                    let pair = self.gc.cons(car, cdr);
                    self.set_return_value(pair);
                }
                OpOld::NumberAdd => self.number_add_op(),
                OpOld::NumberSub => self.number_sub_op(),
                OpOld::NumberMul => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.set_return_value(Object::Number(a * b));
                    }
                    (a, b) => {
                        panic!("+: numbers required but got {:?} {:?}", a, b);
                    }
                },
                OpOld::NumberDiv => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.set_return_value(Object::Number(a / b));
                    }
                    (a, b) => {
                        panic!("/: numbers required but got {:?} {:?}", a, b);
                    }
                },
                OpOld::DefineGlobal(symbol) => {
                    self.define_global_op(symbol);
                }
                OpOld::AssignGlobal(symbol) => {
                    // Same as define global op.
                    self.define_global_op(symbol);
                }
                OpOld::ReferGlobal(symbol) => {
                    self.refer_global_op(symbol);
                }
                OpOld::Values(n) => {
                    //  values stack layout
                    //    (value 'a 'b 'c 'd)
                    //    ==>
                    //    =====
                    //      a
                    //    =====
                    //      b
                    //    =====
                    //      c    [ac_] = d
                    //    =====
                    //  values are stored in [valuez vector] and [a-reg] like following.
                    //  #(b c d)
                    //  [ac_] = a

                    if n > MAX_NUM_VALUES + 1 {
                        panic!("values too many values {}", n);
                    } else {
                        self.num_values = n;

                        for i in (1..n).rev() {
                            self.values[i - 1] = self.ac;
                            self.ac = self.index(self.sp, (n - i - 1) as isize);
                        }

                        if n > 1 {
                            self.sp = self.dec(self.sp, self.num_values as isize - 1);
                        } else {
                            // there's no need to push
                        }
                    }
                    if n == 0 {
                        self.ac = Object::Unspecified;
                    }
                }
                OpOld::Enter(n) => {
                    self.enter_op(n);
                }
                OpOld::LetFrame(_) => {
                    // TODO: expand stack.
                    self.push(self.dc);
                    self.push(Object::ObjectPointer(self.fp));
                }
                OpOld::ReferLocal(n) => {
                    self.refer_local_op(n);
                }
                OpOld::Leave(n) => {
                    let sp = self.dec(self.sp, n);

                    match self.index(sp, 0) {
                        Object::ObjectPointer(fp) => {
                            self.fp = fp;
                        }
                        obj => {
                            panic!("leave: fp expected but got {:?}", obj);
                        }
                    }
                    self.dc = self.index(sp, 1);
                    self.sp = self.dec(sp, 2);
                }
                OpOld::Display(num_free_vars) => {
                    let mut free_vars = vec![];
                    let start = self.dec(self.sp, 1);
                    for i in 0..num_free_vars {
                        let var = unsafe { *start.offset(-i) };
                        free_vars.push(var);
                    }
                    let mut display = self.alloc(Closure::new([].as_ptr(), 0, 0, false, free_vars));
                    display.prev = self.dc;

                    let display = Object::Closure(display);
                    self.dc = display;
                    self.sp = self.dec(self.sp, num_free_vars);
                }
                OpOld::ReferFree(n) => self.refer_free_op(n),
                OpOld::AssignFree(n) => {
                    let closure = self.dc.to_closure();
                    match closure.refer_free(n) {
                        Object::Vox(mut vox) => {
                            vox.value = self.ac;
                        }
                        _ => {
                            panic!("assign_free: vox not found")
                        }
                    }
                }
                OpOld::Test(jump_offset) => {
                    if self.ac.is_false() {
                        pc = self.jump(pc, jump_offset - 1);
                    }
                }
                OpOld::LocalJmp(jump_offset) => {
                    pc = self.jump(pc, jump_offset - 1);
                }
                OpOld::Closure {
                    size,
                    arg_len,
                    is_optional_arg,
                    num_free_vars,
                } => {
                    let mut free_vars = vec![];
                    let start = self.dec(self.sp, 1);
                    for i in 0..num_free_vars {
                        let var = unsafe { *start.offset(-i) };
                        free_vars.push(var);
                    }
                    let c = self.alloc(Closure::new(
                        pc,
                        size - 1,
                        arg_len,
                        is_optional_arg,
                        free_vars,
                    ));
                    self.set_return_value(Object::Closure(c));
                    self.sp = self.dec(self.sp, num_free_vars);
                    pc = self.jump(pc, size as isize - 1);
                }
                OpOld::TailCall(depth, diff) => {
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    let argc = depth;
                    self.call_op(&mut pc, argc);
                }
                OpOld::Call(argc) => {
                    self.call_op(&mut pc, argc);
                }
                OpOld::Return(n) => {
                    self.return_n(n, &mut pc);
                }
                OpOld::Frame(skip_offset) => {
                    self.frame_op(pc, skip_offset);
                }
                OpOld::Halt => {
                    break;
                }
                OpOld::ReadChar => match self.ac {
                    Object::InputPort(mut port) => match port.read_char() {
                        Some(c) => {
                            self.set_return_value(Object::Char(c));
                        }
                        None => {
                            self.set_return_value(Object::Eof);
                        }
                    },
                    obj => {
                        self.arg_err("read-char", "text-input-port", obj);
                    }
                },
                OpOld::Undef => self.set_return_value(Object::Unspecified),
                OpOld::Nop => {}
            }
            self.print_vm(op);
            pc = self.jump(pc, 1);
        }
        self.ac
    }

    pub fn set_symbol_value(&mut self, symbol: GcRef<Symbol>, value: Object) {
        self.globals.insert(symbol, value);
    }

    fn define_global_op(&mut self, symbol: GcRef<Symbol>) {
        self.globals.insert(symbol, self.ac);
    }

    #[inline(always)]
    fn number_sub_op(&mut self) {
        match (self.pop(), self.ac) {
            (Object::Number(a), Object::Number(b)) => {
                self.set_return_value(Object::Number(a - b));
            }
            (a, b) => {
                panic!("-: numbers required but got {:?} {:?}", a, b);
            }
        }
    }

    #[inline(always)]
    fn number_add_op(&mut self) {
        match (self.pop(), self.ac) {
            (Object::Number(a), Object::Number(b)) => {
                self.set_return_value(Object::Number(a + b));
            }
            (a, b) => {
                panic!("+: numbers required but got {:?} {:?}", a, b);
            }
        }
    }

    #[inline(always)]
    fn enter_op(&mut self, n: isize) {
        self.fp = self.dec(self.sp, n);
    }

    #[inline(always)]
    fn constant_op(&mut self, c: Object) {
        self.set_return_value(c);
    }
    #[inline(always)]
    fn branch_not_null_op(&mut self, pc: &mut *const OpOld, skip_offset: isize) {
        if self.ac.is_nil() {
            self.set_return_value(Object::False);
        } else {
            self.set_return_value(Object::True);
            *pc = self.jump(*pc, skip_offset - 1);
        }
    }

    #[inline(always)]
    fn frame_op(&mut self, pc: *const OpOld, skip_offset: isize) {
        // Call frame in stack.
        // ======================
        //          pc*
        // ======================
        //          dc
        // ======================
        //          cl
        // ======================
        //          fp
        // ======== sp ==========
        //
        // where pc* = pc + skip_offset -1
        let next_pc = self.jump(pc, skip_offset - 1);
        self.push(Object::OpPointer(next_pc));
        self.push(self.dc);
        // TODO: This should be cl register.
        self.push(self.dc);
        self.push(Object::ObjectPointer(self.fp));
    }

    #[inline(always)]
    fn refer_free_op(&mut self, n: usize) {
        let val = self.dc.to_closure().refer_free(n);
        self.set_return_value(val);
    }

    #[inline(always)]
    fn push_op(&mut self) {
        self.push(self.ac);
    }

    #[inline(always)]
    fn car_op(&mut self) {
        match self.ac {
            Object::Pair(pair) => {
                self.set_return_value(pair.car);
            }
            obj => {
                self.arg_err("car", "pair", obj);
            }
        }
    }

    #[inline(always)]
    fn refer_local_op(&mut self, n: isize) {
        let obj = self.refer_local(n);
        self.set_return_value(obj);
    }

    #[inline(always)]
    fn refer_global_op(&mut self, symbol: GcRef<Symbol>) {
        match self.globals.get(&symbol) {
            Some(&value) => {
                self.set_return_value(value);
            }
            None => {
                panic!("identifier {} not found", symbol.string);
            }
        }
    }

    #[inline(always)]
    fn cdr_op(&mut self) {
        match self.ac {
            Object::Pair(pair) => {
                self.set_return_value(pair.cdr);
            }
            obj => {
                self.arg_err("cdr", "pair", obj);
            }
        }
    }

    #[inline(always)]
    fn call_op(&mut self, pc: &mut *const OpOld, argc: isize) {
        match self.ac {
            Object::Closure(closure) => {
                self.dc = self.ac;
                // TODO:
                // self.cl = self.ac;
                *pc = closure.ops_old;
                if closure.is_optional_arg {
                    let extra_len = argc - closure.argc;
                    if -1 == extra_len {
                        let sp = self.unshift_args(self.sp, 1);
                        self.index_set(sp, 0, Object::Nil);
                        self.sp = sp;
                        self.fp = self.dec(self.sp, closure.argc);
                    } else if extra_len >= 0 {
                        let args = self.stack_to_pair(extra_len + 1);
                        self.index_set(self.sp, extra_len, args);
                        let sp = self.dec(self.sp, extra_len);
                        self.fp = self.dec(sp, closure.argc);
                        self.sp = sp;
                    } else {
                        panic!(
                            "call: wrong number of arguments {} required bug got {}",
                            closure.argc, argc
                        );
                    }
                } else if argc == closure.argc {
                    self.fp = self.dec(self.sp, argc);
                } else {
                    panic!(
                        "call: wrong number of arguments {} required bug got {}",
                        closure.argc, argc
                    );
                }
            }
            Object::Procedure(procedure) => {
                let start = unsafe { self.sp.offset_from(self.stack.as_ptr()) } - argc;
                let start: usize = start as usize;
                let uargc: usize = argc as usize;
                let args = &self.stack[start..start + uargc];

                // copying args here because we can't borrow.
                let args = &args.to_owned()[..];

                // We convert apply call to Op::Call.
                if procedure.func as usize == procs::apply as usize {
                    if argc == 1 {
                        panic!("apply: need two or more arguments but only 1 argument");
                    }
                    self.sp = self.dec(self.sp, argc);
                    self.ac = args[0];
                    // (apply proc arg1 arg2 ... args-as-list)
                    // We push arguments here. The last argument is flatten list.
                    for i in 1..argc {
                        if i == argc - 1 {
                            let mut last_pair = args[i as usize];
                            if !last_pair.is_list() {
                                panic!(
                                    "apply: last arguments shoulbe proper list but got {}",
                                    last_pair
                                );
                            }
                            let mut j: isize = 0;
                            loop {
                                if last_pair.is_nil() {
                                    let new_argc = argc - 2 + j;
                                    println!("Warning recursive self.call()");
                                    self.call_op(pc, new_argc);
                                    break;
                                } else {
                                    match last_pair {
                                        Object::Pair(pair) => {
                                            self.push(pair.car);
                                            last_pair = pair.cdr;
                                        }
                                        _ => {
                                            panic!("never reached");
                                        }
                                    }
                                }
                                j = j + 1;
                            }
                        } else {
                            self.push(args[i as usize]);
                        }
                    }
                } else {
                    // TODO: Take care of cl.
                    // self.cl = self.ac
                    self.ac = (procedure.func)(self, args);
                    self.return_n(argc, pc);
                }
            }
            _ => {
                panic!("can't call {:?}", self.ac);
            }
        }
    }

    fn reset_roots(&mut self) {
        // Clean up display closure so that Objects in ops can be freed.
        let mut closure = self.dc.to_closure();
        closure.ops_old = null();
        closure.ops_len = 0;
    }
    // Note we keep self.ac here, so that it can live after it returned by run().

    pub fn register_compiler(&mut self) -> *const OpOld {
        let mut fasl = Fasl {
            bytes: compiler::BIN_COMPILER,
        };
        let mut ops = vec![];
        loop {
            match fasl.read_op(&mut self.gc) {
                Ok(op) => {
                    ops.push(op);
                }
                Err(_) => {
                    break;
                }
            }
        }
        self.lib_ops = ops;
        self.lib_ops.as_ptr()
    }

    pub fn register_baselib(&mut self) -> *const OpOld {
        let sym0 = self.gc.symbol_intern("for-all");
        let str0 = self.gc.new_string("expected same length proper lists");
        let str1 = self
            .gc
            .new_string("traversal reached to non-pair element ~s");
        let str2 = self
            .gc
            .new_string("expected chain of pairs, but got ~r, as argument 2");

        self.lib_ops = vec![
            OpOld::Closure {
                size: 15,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            OpOld::ReferLocalBranchNotNull(1, 3),
            OpOld::ReferLocal(1),
            OpOld::Return(2),
            OpOld::Frame(4),
            OpOld::ReferLocal(1),
            OpOld::CarPush,
            OpOld::ReferLocalCall(0, 1),
            OpOld::PushFrame(5),
            OpOld::ReferLocalPush(0),
            OpOld::ReferLocal(1),
            OpOld::CdrPush,
            OpOld::ReferGlobalCall(self.gc.intern("map1"), 2),
            OpOld::Cons,
            OpOld::Return(2),
            OpOld::DefineGlobal(self.gc.intern("map1")),
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::ReferFreePush(197),
            OpOld::ReferFreePush(152),
            OpOld::ReferFreePush(2),
            OpOld::Closure {
                size: 39,
                arg_len: 3,
                is_optional_arg: true,
                num_free_vars: 3,
            },
            OpOld::ReferLocalBranchNotNull(2, 6),
            OpOld::ReferLocalPush(0),
            OpOld::ReferLocalPush(1),
            OpOld::ReferGlobal(self.gc.intern("for-all-1")),
            OpOld::TailCall(2, 3),
            OpOld::Return(3),
            OpOld::LetFrame(11),
            OpOld::ReferLocalPush(0),
            OpOld::ReferLocalPush(1),
            OpOld::ReferLocalPush(2),
            OpOld::ReferFreePush(0),
            OpOld::ReferFreePush(2),
            OpOld::ReferFreePush(1),
            OpOld::Display(6),
            OpOld::Frame(5),
            OpOld::ReferFreePush(1),
            OpOld::ReferLocalPush(1),
            OpOld::ReferLocalPush(2),
            OpOld::ReferFreeCall(0, 3),
            OpOld::PushEnter(1),
            OpOld::ReferLocal(0),
            OpOld::Test(6),
            OpOld::ReferFreePush(5),
            OpOld::ReferLocalPush(0),
            OpOld::ReferGlobal(self.gc.intern("for-all-n-quick")),
            OpOld::TailCall(2, 6),
            OpOld::LocalJmp(10),
            OpOld::ConstantPush(sym0),
            OpOld::ConstantPush(str0),
            OpOld::Frame(4),
            OpOld::ReferFreePush(4),
            OpOld::ReferFreePush(3),
            OpOld::ReferFreeCall(2, 2),
            OpOld::Push,
            OpOld::ReferGlobal(self.gc.intern("assertion-violation")),
            OpOld::TailCall(3, 6),
            OpOld::Leave(1),
            OpOld::Return(3),
            OpOld::DefineGlobal(self.gc.intern("for-all")),
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::ReferFreePush(48),
            OpOld::ReferFreePush(89),
            OpOld::Closure {
                size: 68,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 2,
            },
            OpOld::ReferLocalBranchNotNull(1, 3),
            OpOld::Constant(Object::True),
            OpOld::Return(2),
            OpOld::ReferLocal(1),
            OpOld::PairP,
            OpOld::Test(49),
            OpOld::LetFrame(14),
            OpOld::ReferLocalPush(0),
            OpOld::ReferLocalPush(1),
            OpOld::ReferFreePush(1),
            OpOld::ReferFreePush(0),
            OpOld::Display(4),
            OpOld::ReferLocal(1),
            OpOld::CarPush,
            OpOld::ReferLocal(1),
            OpOld::CdrPush,
            OpOld::Enter(2),
            OpOld::ReferLocalBranchNotNull(1, 5),
            OpOld::ReferLocalPush(0),
            OpOld::ReferFree(3),
            OpOld::TailCall(1, 6),
            OpOld::LocalJmp(31),
            OpOld::ReferLocal(1),
            OpOld::PairP,
            OpOld::Test(12),
            OpOld::Frame(3),
            OpOld::ReferLocalPush(0),
            OpOld::ReferFreeCall(3, 1),
            OpOld::Test(24),
            OpOld::ReferLocal(1),
            OpOld::CarPush,
            OpOld::ReferLocal(1),
            OpOld::CdrPush,
            OpOld::Shiftj(2, 2, 0),
            OpOld::LocalJmp(-17),
            OpOld::LocalJmp(17),
            OpOld::Frame(3),
            OpOld::ReferLocalPush(0),
            OpOld::ReferFreeCall(3, 1),
            OpOld::Test(13),
            OpOld::ConstantPush(sym0),
            OpOld::Frame(4),
            OpOld::ConstantPush(str1),
            OpOld::ReferLocalPush(1),
            OpOld::ReferFreeCall(1, 2),
            OpOld::PushFrame(4),
            OpOld::ReferFreePush(3),
            OpOld::ReferFreePush(2),
            OpOld::ReferFreeCall(0, 2),
            OpOld::Push,
            OpOld::ReferGlobal(self.gc.intern("assertion-violation")),
            OpOld::TailCall(3, 6),
            OpOld::Leave(2),
            OpOld::Return(2),
            OpOld::ConstantPush(sym0),
            OpOld::Frame(4),
            OpOld::ConstantPush(str2),
            OpOld::ReferLocalPush(1),
            OpOld::ReferFreeCall(1, 2),
            OpOld::PushFrame(4),
            OpOld::ReferLocalPush(0),
            OpOld::ReferLocalPush(1),
            OpOld::ReferFreeCall(0, 2),
            OpOld::Push,
            OpOld::ReferGlobal(self.gc.intern("assertion-violation")),
            OpOld::TailCall(3, 2),
            OpOld::Return(2),
            OpOld::DefineGlobal(self.gc.intern("for-all-1")),
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::ReferFreePush(152),
            OpOld::Closure {
                size: 32,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            OpOld::ReferLocalBranchNotNull(1, 2),
            OpOld::Return(2),
            OpOld::LetFrame(8),
            OpOld::ReferLocalPush(0),
            OpOld::ReferFreePush(0),
            OpOld::ReferLocalPush(1),
            OpOld::Display(3),
            OpOld::ReferLocal(1),
            OpOld::CarPush,
            OpOld::ReferLocal(1),
            OpOld::CdrPush,
            OpOld::Enter(2),
            OpOld::ReferLocalBranchNotNull(1, 6),
            OpOld::ReferFreePush(2),
            OpOld::ReferLocalPush(0),
            OpOld::ReferFree(1),
            OpOld::TailCall(2, 6),
            OpOld::LocalJmp(12),
            OpOld::Frame(4),
            OpOld::ReferFreePush(2),
            OpOld::ReferLocalPush(0),
            OpOld::ReferFreeCall(1, 2),
            OpOld::Test(7),
            OpOld::ReferLocal(1),
            OpOld::CarPush,
            OpOld::ReferLocal(1),
            OpOld::CdrPush,
            OpOld::Shiftj(2, 2, 0),
            OpOld::LocalJmp(-16),
            OpOld::Leave(2),
            OpOld::Return(2),
            OpOld::DefineGlobal(self.gc.intern("for-all-n-quick")),
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Nop,
            OpOld::Halt,
        ];

        self.lib_ops.as_ptr()
    }

    // Helpers.
    fn pop(&mut self) -> Object {
        unsafe {
            self.sp = self.dec(self.sp, 1);
            *self.sp
        }
    }

    fn push(&mut self, value: Object) {
        unsafe {
            *self.sp = value;
            self.sp = self.inc(self.sp, 1);
        }
    }

    fn index(&self, sp: *mut Object, n: isize) -> Object {
        unsafe { *self.dec(sp, n + 1) }
    }

    fn index_set(&mut self, sp: *mut Object, n: isize, obj: Object) {
        unsafe { *self.dec(sp, n + 1) = obj }
    }

    fn stack_len(&self) -> usize {
        unsafe { self.sp.offset_from(self.stack.as_ptr()) as usize }
    }

    #[cfg(feature = "debug_log_vm")]
    fn print_vm(&mut self, op: OpOld) {
        println!("-----------------------------------------");
        println!("{} executed", op);
        println!("  ac={}", self.ac);
        println!("  dc={}", self.dc);
        println!("-----------------------------------------");
        let fp_idx = unsafe { self.fp.offset_from(self.stack.as_ptr()) };
        for i in 0..self.stack_len() {
            println!(
                "  {}{}",
                self.stack[i],
                if fp_idx == i.try_into().unwrap() {
                    "  <== fp"
                } else {
                    ""
                }
            );
        }
        println!("-----------------------------------------<== sp")
    }
    #[cfg(not(feature = "debug_log_vm"))]
    fn print_vm(&mut self, _: OpOld) {}

    #[inline(always)]
    fn set_return_value(&mut self, obj: Object) {
        self.ac = obj;
        self.num_values = 1;
    }

    #[inline(always)]
    fn refer_local(&mut self, n: isize) -> Object {
        unsafe { *self.fp.offset(n) }
    }

    #[inline(always)]
    fn jump(&self, pc: *const OpOld, offset: isize) -> *const OpOld {
        unsafe { pc.offset(offset) }
    }

    #[inline(always)]
    fn inc(&self, pointer: *mut Object, offset: isize) -> *mut Object {
        unsafe { pointer.offset(offset) }
    }

    #[inline(always)]
    fn dec(&self, pointer: *mut Object, offset: isize) -> *mut Object {
        unsafe { pointer.offset(-offset) }
    }

    fn arg_err(&self, who: &str, expected: &str, actual: Object) {
        panic!("{}: requires {} but got {}", who, expected, actual);
    }

    fn return_n(&mut self, n: isize, pc: &mut *const OpOld) {
        #[cfg(feature = "debug_log_vm")]
        println!("  return {}", n);
        let sp = self.dec(self.sp, n);
        match self.index(sp, 0) {
            Object::ObjectPointer(fp) => {
                self.fp = fp;
            }
            obj => {
                panic!("not fp pointer but {}", obj)
            }
        }
        // TODO: Take care of cl register.
        // self.cl = index(sp, 1);
        self.dc = self.index(sp, 2);
        match self.index(sp, 3) {
            Object::OpPointer(next_pc) => {
                *pc = next_pc;
            }
            _ => {
                panic!("not a pc");
            }
        }
        self.sp = self.dec(sp, 4);
    }

    fn shift_args_to_bottom(&mut self, sp: *mut Object, depth: isize, diff: isize) -> *mut Object {
        let mut i = depth - 1;
        while i >= 0 {
            self.index_set(sp, i + diff, self.index(self.sp, i));
            i = i - 1;
        }
        self.dec(sp, diff)
    }

    fn unshift_args(&mut self, sp: *mut Object, diff: isize) -> *mut Object {
        for i in 0..diff {
            self.index_set(self.inc(sp, diff - i), 0, self.index(sp, 1));
        }
        self.inc(sp, diff)
    }

    fn stack_to_pair(&mut self, n: isize) -> Object {
        let mut args = Object::Nil;
        for i in 0..n {
            args = self.gc.cons(self.index(self.sp, i), args);
        }
        args
    }

    pub fn set_rtd(&mut self, key: Object, rtd: Object) {
        self.rtds.insert(key, rtd);
    }

    pub fn lookup_rtd(&self, key: Object) -> Object {
        match self.rtds.get(&key) {
            Some(&value) => value,
            _ => Object::False,
        }
    }
}

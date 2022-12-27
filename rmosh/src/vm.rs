use std::{
    collections::HashMap,
    fmt::Display,
    ptr::{null, null_mut},
};

use crate::{
    gc::{Gc, GcRef},
    objects::{Closure, Equal, Object, Pair, Symbol, Vox},
    op::Op,
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

pub struct Vm {
    pub gc: Box<Gc>,
    // The stack.
    stack: [Object; STACK_SIZE],
    // accumulator register.
    pub ac: Object,
    // display closure register.
    dc: Object,
    // stack pointer.
    sp: *mut Object,
    // frame pointer.
    fp: *mut Object,
    // global variables.
    globals: HashMap<GcRef<Symbol>, Object>,
    // We keep the lib_ops here so that the lib_ops live longer than every call of run.
    // If we kept lib_ops as local variable, it can/will be immediately freed after run(lib_ops).
    lib_ops: Vec<Op>,
    // Return values.
    values: [Object; MAX_NUM_VALUES],
    num_values: usize,
    // Note when we add new vars here, please make sure we take care of them in mark_roots.
    // Otherwise they can cause memory leak or double free.
}

impl Vm {
    pub fn new() -> Self {
        Self {
            gc: Box::new(Gc::new()),
            stack: [Object::Unspecified; STACK_SIZE],
            ac: Object::Unspecified,
            dc: Object::Unspecified,
            sp: null_mut(),
            fp: null_mut(),
            globals: HashMap::new(),
            lib_ops: vec![],
            num_values: 0,
            values: [Object::Unspecified; MAX_NUM_VALUES],
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

    fn initialize_free_vars(&mut self, ops: *const Op, ops_len: usize) {
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

        // Registers.
        self.gc.mark_object(self.ac);
        self.gc.mark_object(self.dc);
    }

    pub fn run(&mut self, ops: *const Op, ops_len: usize) -> Object {
        // Create display closure and make free variables accessible.
        self.initialize_free_vars(ops, ops_len);

        // Load the base library.
        let lib_ops = self.register_baselib();
        self.run_ops(lib_ops);

        // Run the program.
        let ret = self.run_ops(ops);

        // Clean up so that GC can sweep them.
        self.reset_roots();
        ret
    }

    fn run_ops(&mut self, ops: *const Op) -> Object {
        self.sp = self.stack.as_mut_ptr();
        self.fp = self.sp;

        let mut pc: *const Op = ops;
        loop {
            let op = unsafe { *pc };
            match op {
                Op::MakeVector => match self.pop() {
                    Object::Number(size) => {
                        let v = vec![self.ac; size as usize];
                        let v = self.gc.new_vector(&v);
                        self.set_return_value(v);
                    }
                    obj => {
                        self.arg_err("make-vector", "numbers", obj);
                    }
                },
                Op::VectorP => match self.ac {
                    Object::Vector(_) => {
                        self.set_return_value(Object::True);
                    }
                    _ => {
                        self.set_return_value(Object::False);
                    }
                },
                Op::VectorLength => match self.ac {
                    Object::Vector(v) => {
                        self.set_return_value(Object::Number(v.len() as isize));
                    }
                    obj => {
                        self.arg_err("vector-length", "vector", obj);
                    }
                },
                Op::VectorRef => match (self.pop(), self.ac) {
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
                Op::VectorSet => {
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
                Op::Append2 => {
                    let head = self.pop();
                    if Pair::is_list(head) {
                        let p = self.gc.append2(head, self.ac);
                        self.set_return_value(p);
                    } else {
                        self.arg_err("append", "pair", head);
                    }
                }
                Op::Receive(num_req_args, num_opt_args) => {
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
                Op::SetCar => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.car = self.ac;
                        self.set_return_value(Object::Unspecified);
                    }
                    obj => {
                        self.arg_err("set-car!", "pair", obj);
                    }
                },
                Op::SetCdr => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.cdr = self.ac;
                        self.set_return_value(Object::Unspecified);
                    }
                    obj => {
                        self.arg_err("set-cdr!", "pair", obj);
                    }
                },
                Op::Not => {
                    self.set_return_value(Object::make_bool(self.ac.is_false()));
                }
                Op::PairP => self.set_return_value(Object::make_bool(self.ac.is_pair())),
                Op::NullP => self.set_return_value(Object::make_bool(self.ac.is_nil())),
                Op::SymbolP => self.set_return_value(Object::make_bool(self.ac.is_symbol())),
                Op::BranchNotNumberEqual(skip_offset) => {
                    branch_number_op!(==, self, pc, skip_offset);
                }
                Op::BranchNotGe(skip_offset) => {
                    branch_number_op!(>=, self, pc, skip_offset);
                }
                Op::BranchNotGt(skip_offset) => {
                    branch_number_op!(>, self, pc, skip_offset);
                }
                Op::BranchNotLe(skip_offset) => {
                    branch_number_op!(<=, self, pc, skip_offset);
                }
                Op::BranchNotLt(skip_offset) => {
                    branch_number_op!(<, self, pc, skip_offset);
                }
                Op::BranchNotNull(skip_offset) => {
                    if self.ac.is_nil() {
                        self.set_return_value(Object::False);
                    } else {
                        self.set_return_value(Object::True);
                        pc = self.jump(pc, skip_offset - 1);
                    }
                }
                Op::BranchNotEqv(skip_offset) => {
                    if self.pop().eqv(&self.ac) {
                        self.set_return_value(Object::True);
                    } else {
                        pc = self.jump(pc, skip_offset - 1);
                        self.set_return_value(Object::False);
                    }
                }
                Op::Eq => {
                    let is_eq = self.pop().eq(&self.ac);
                    self.set_return_value(Object::make_bool(is_eq));
                }
                Op::Equal => {
                    let e = Equal::new();
                    let val = self.pop();
                    let ret = e.is_equal(&mut self.gc, &val, &self.ac);
                    self.set_return_value(Object::make_bool(ret));
                }
                Op::Eqv => {
                    let ret = self.pop().eqv(&self.ac);
                    self.set_return_value(Object::make_bool(ret));
                }
                Op::NumberEqual => {
                    number_op!(==, self);
                }
                Op::NumberGe => {
                    number_op!(>=, self);
                }
                Op::NumberGt => {
                    number_op!(>, self);
                }
                Op::NumberLe => {
                    number_op!(<=, self);
                }
                Op::NumberLt => {
                    number_op!(<, self);
                }
                Op::Car => match self.ac {
                    Object::Pair(pair) => {
                        self.set_return_value(pair.car);
                    }
                    obj => {
                        self.arg_err("car", "pair", obj);
                    }
                },
                Op::Cdr => match self.ac {
                    Object::Pair(pair) => {
                        self.set_return_value(pair.cdr);
                    }
                    obj => {
                        self.arg_err("cdr", "pair", obj);
                    }
                },
                Op::Cadr => match self.ac {
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
                Op::Indirect => match self.ac {
                    Object::Vox(vox) => {
                        self.set_return_value(vox.value);
                    }
                    obj => {
                        self.arg_err("indirect", "vox", obj);
                    }
                },
                Op::AssignLocal(n) => match self.refer_local(n) {
                    Object::Vox(mut vox) => vox.value = self.ac,
                    _ => {
                        panic!("assign_local: vox not found")
                    }
                },
                Op::Box(n) => {
                    let vox = self.alloc(Vox::new(self.index(self.sp, n)));
                    self.index_set(self.sp, n, Object::Vox(vox));
                }
                Op::Constant(c) => {
                    self.set_return_value(c);
                }
                Op::Push => {
                    self.push(self.ac);
                }
                Op::Cons => {
                    let car = self.pop();
                    let cdr = self.ac;
                    let pair = self.gc.cons(car, cdr);
                    self.set_return_value(pair);
                }
                Op::NumberAdd => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.set_return_value(Object::Number(a + b));
                    }
                    (a, b) => {
                        panic!("+: numbers required but got {:?} {:?}", a, b);
                    }
                },
                Op::NumberSub => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.set_return_value(Object::Number(a - b));
                    }
                    (a, b) => {
                        panic!("-: numbers required but got {:?} {:?}", a, b);
                    }
                },
                Op::NumberMul => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.set_return_value(Object::Number(a * b));
                    }
                    (a, b) => {
                        panic!("+: numbers required but got {:?} {:?}", a, b);
                    }
                },
                Op::NumberDiv => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.set_return_value(Object::Number(a / b));
                    }
                    (a, b) => {
                        panic!("/: numbers required but got {:?} {:?}", a, b);
                    }
                },
                Op::DefineGlobal(symbol) => {
                    self.globals.insert(symbol, self.ac);
                }
                Op::AssignGlobal(symbol) => {
                    self.globals.insert(symbol, self.ac);
                }
                Op::ReferGlobal(symbol) => match self.globals.get(&symbol) {
                    Some(&value) => {
                        self.set_return_value(value);
                    }
                    None => {
                        panic!("identifier {:?} not found", symbol);
                    }
                },
                Op::Values(n) => {
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
                Op::Enter(n) => {
                    self.fp = self.dec(self.sp, n);
                }
                Op::LetFrame(_) => {
                    // TODO: expand stack.
                    self.push(self.dc);
                    self.push(Object::StackPointer(self.fp));
                }
                Op::ReferLocal(n) => {
                    let obj = self.refer_local(n);
                    self.set_return_value(obj);
                }
                Op::Leave(n) => {
                    let sp = self.dec(self.sp, n);

                    match self.index(sp, 0) {
                        Object::StackPointer(fp) => {
                            self.fp = fp;
                        }
                        obj => {
                            panic!("leave: fp expected but got {:?}", obj);
                        }
                    }
                    self.dc = self.index(sp, 1);
                    self.sp = self.dec(sp, 2);
                }
                Op::Display(num_free_vars) => {
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
                Op::ReferFree(n) => match self.dc {
                    Object::Closure(mut closure) => {
                        self.set_return_value(closure.refer_free(n));
                    }
                    _ => {
                        panic!("refer_free: display closure required but got {:?}", self.dc);
                    }
                },
                Op::AssignFree(n) => match self.dc {
                    Object::Closure(mut closure) => match closure.refer_free(n) {
                        Object::Vox(mut vox) => {
                            vox.value = self.ac;
                        }
                        _ => {
                            panic!("assign_free: vox not found")
                        }
                    },
                    _ => {
                        panic!(
                            "assign_free: display closure required but got {:?}",
                            self.dc
                        );
                    }
                },
                Op::Test(jump_offset) => {
                    if self.ac.is_false() {
                        pc = self.jump(pc, jump_offset - 1);
                    }
                }
                Op::LocalJmp(jump_offset) => {
                    pc = self.jump(pc, jump_offset - 1);
                }
                Op::Closure {
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
                    pc = self.jump(pc, size - 1);
                }
                Op::TailCall(depth, diff) => {
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    let argc = depth;
                    self.call(&mut pc, argc);
                }
                Op::Call(argc) => {
                    self.call(&mut pc, argc);
                }
                Op::Return(n) => {
                    self.return_n(n, &mut pc);
                }
                Op::Frame(skip_offset) => {
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
                    self.push(Object::StackPointer(self.fp));
                }
                Op::Halt => {
                    break;
                }
                Op::ReadChar => match self.ac {
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
                Op::Undef => self.set_return_value(Object::Unspecified),
                Op::Nop => {}
            }
            self.print_vm(op);
            pc = self.jump(pc, 1);
        }
        self.ac
    }

    #[inline(always)]
    fn call(&mut self, pc: &mut *const Op, argc: isize) {
        match self.ac {
            Object::Closure(closure) => {
                self.dc = self.ac;
                // TODO:
                // self.cl = self.ac;
                *pc = closure.ops;
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
                                    self.call(pc, new_argc);
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
        match self.dc {
            Object::Closure(mut c) => {
                c.ops = null();
                c.ops_len = 0;
            }
            _ => {}
        }
        // Note we keep self.ac here, so that it can live after it returned by run().
    }

    fn register_baselib(&mut self) -> *const Op {
        self.lib_ops = vec![
            Op::Closure {
                size: 22,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(1),
            Op::BranchNotNull(3),
            Op::ReferLocal(1),
            Op::Return(2),
            Op::Frame(6),
            Op::ReferLocal(1),
            Op::Car,
            Op::Push,
            Op::ReferLocal(0),
            Op::Call(1),
            Op::Push,
            Op::Frame(8),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::Cdr,
            Op::Push,
            Op::ReferGlobal(self.gc.intern("map1")),
            Op::Call(2),
            Op::Cons,
            Op::Return(2),
            Op::DefineGlobal(self.gc.intern("map1")),
            Op::Halt,
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
    fn print_vm(&mut self, op: Op) {
        println!("-----------------------------------------");
        println!("{:?} executed", op);
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
    fn print_vm(&mut self, _: Op) {}

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
    fn jump(&self, pc: *const Op, offset: usize) -> *const Op {
        unsafe { pc.offset(offset as isize) }
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

    fn return_n(&mut self, n: isize, pc: &mut *const Op) {
        #[cfg(feature = "debug_log_vm")]
        println!("  return {}", n);
        let sp = self.dec(self.sp, n);
        match self.index(sp, 0) {
            Object::StackPointer(fp) => {
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
}

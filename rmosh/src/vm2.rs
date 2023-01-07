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
    objects::{Closure, Object, Op2, Pair, Symbol, Vox},
    op::OpOld,
    procs::{self, default_free_vars},
};

const STACK_SIZE: usize = 256;
const MAX_NUM_VALUES: usize = 256;

pub struct Vm {
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
    pub lib_ops: Vec<Object>,
    // Return values.
    values: [Object; MAX_NUM_VALUES],
    num_values: usize,
    pub rtds: HashMap<Object, Object>,
    pub should_load_compiler: bool,
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

    fn initialize_free_vars(&mut self, ops: *const Object, ops_len: usize) {
        let free_vars = default_free_vars(&mut self.gc);
        let mut display = self
            .gc
            .alloc(Closure::new2(ops, ops_len, 0, false, free_vars));
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
            self.gc.mark_object(op);
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

    pub fn run(&mut self, ops: *const Object, ops_len: usize) -> Object {
        // Create display closure and make free variables accessible.
        self.initialize_free_vars(ops, ops_len);

        // Load the base library.
        /*
                let lib_ops = if self.should_load_compiler {
                    self.register_compiler()
                } else {
                    self.register_baselib()
                };
                self.run_ops(lib_ops);
        */
        // Run the program.
        let ret = self.run_ops(ops);

        // Clean up so that GC can sweep them.
        self.reset_roots();
        ret
    }

    fn run_ops(&mut self, ops: *const Object) -> Object {
        self.sp = self.stack.as_mut_ptr();
        self.fp = self.sp;

        let mut pc: *const Object = ops;
        let op: Op2 = unsafe { *pc }.to_instruction();
        match op {
            Op2::CompileError => todo!(),
            Op2::BranchNotLe => todo!(),
            Op2::BranchNotGe => todo!(),
            Op2::BranchNotLt => todo!(),
            Op2::BranchNotGt => todo!(),
            Op2::BranchNotNull => todo!(),
            Op2::BranchNotNumberEqual => todo!(),
            Op2::BranchNotEq => todo!(),
            Op2::BranchNotEqv => todo!(),
            Op2::BranchNotEqual => todo!(),
            Op2::Append2 => todo!(),
            Op2::Call => todo!(),
            Op2::Apply => todo!(),
            Op2::Push => todo!(),
            Op2::AssignFree => todo!(),
            Op2::AssignGlobal => todo!(),
            Op2::AssignLocal => todo!(),
            Op2::Box => todo!(),
            Op2::Caar => todo!(),
            Op2::Cadr => todo!(),
            Op2::Car => todo!(),
            Op2::Cdar => todo!(),
            Op2::Cddr => todo!(),
            Op2::Cdr => todo!(),
            Op2::Closure => todo!(),
            Op2::Cons => todo!(),
            Op2::Constant => todo!(),
            Op2::DefineGlobal => todo!(),
            Op2::Display => todo!(),
            Op2::Enter => todo!(),
            Op2::Eq => todo!(),
            Op2::Eqv => todo!(),
            Op2::Equal => todo!(),
            Op2::Frame => todo!(),
            Op2::Indirect => todo!(),
            Op2::Leave => todo!(),
            Op2::LetFrame => todo!(),
            Op2::List => todo!(),
            Op2::LocalJmp => todo!(),
            Op2::MakeContinuation => todo!(),
            Op2::MakeVector => todo!(),
            Op2::Nop => todo!(),
            Op2::Not => todo!(),
            Op2::NullP => todo!(),
            Op2::NumberAdd => todo!(),
            Op2::NumberEqual => todo!(),
            Op2::NumberGe => todo!(),
            Op2::NumberGt => todo!(),
            Op2::NumberLe => todo!(),
            Op2::NumberLt => todo!(),
            Op2::NumberMul => todo!(),
            Op2::NumberDiv => todo!(),
            Op2::NumberSub => todo!(),
            Op2::PairP => todo!(),
            Op2::Read => todo!(),
            Op2::ReadChar => todo!(),
            Op2::Reduce => todo!(),
            Op2::ReferFree => todo!(),
            Op2::ReferGlobal => todo!(),
            Op2::ReferLocal => todo!(),
            Op2::RestoreContinuation => todo!(),
            Op2::Return => todo!(),
            Op2::SetCar => todo!(),
            Op2::SetCdr => todo!(),
            Op2::Shift => todo!(),
            Op2::SymbolP => todo!(),
            Op2::Test => todo!(),
            Op2::Values => todo!(),
            Op2::Receive => todo!(),
            Op2::UnfixedJump => todo!(),
            Op2::Stop => todo!(),
            Op2::Shiftj => todo!(),
            Op2::Undef => todo!(),
            Op2::VectorLength => todo!(),
            Op2::VectorP => todo!(),
            Op2::VectorRef => todo!(),
            Op2::VectorSet => todo!(),
            Op2::PushEnter => todo!(),
            Op2::Halt => todo!(),
            Op2::ConstantPush => todo!(),
            Op2::NumberSubPush => todo!(),
            Op2::NumberAddPush => todo!(),
            Op2::PushConstant => todo!(),
            Op2::PushFrame => todo!(),
            Op2::CarPush => todo!(),
            Op2::CdrPush => todo!(),
            Op2::ShiftCall => todo!(),
            Op2::NotTest => todo!(),
            Op2::ReferGlobalCall => todo!(),
            Op2::ReferFreePush => todo!(),
            Op2::ReferLocalPush => todo!(),
            Op2::ReferLocalPushConstant => todo!(),
            Op2::ReferLocalPushConstantBranchNotLe => todo!(),
            Op2::ReferLocalPushConstantBranchNotGe => todo!(),
            Op2::ReferLocalPushConstantBranchNotNumberEqual => todo!(),
            Op2::ReferLocalBranchNotNull => todo!(),
            Op2::ReferLocalBranchNotLt => todo!(),
            Op2::ReferFreeCall => todo!(),
            Op2::ReferGlobalPush => todo!(),
            Op2::ReferLocalCall => todo!(),
            Op2::LocalCall => todo!(),
            Op2::Vector => todo!(),
            Op2::SimpleStructRef => todo!(),
            Op2::DynamicWinders => todo!(),
            Op2::TailCall => todo!(),
            Op2::LocalTailCall => todo!(),
        }
        //self.print_vm(op);
        //pc = self.jump(pc, 1);

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
        self.push(Object::StackPointer(self.fp));
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
                    panic!("procedure invocation");
                   // self.ac = (procedure.func)(self, args);
                   // self.return_n(argc, pc);
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
/*
    pub fn register_compiler(&mut self) -> *const Op {
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

    pub fn register_baselib(&mut self) -> *const Op {
        let sym0 = self.gc.symbol_intern("for-all");
        let str0 = self.gc.new_string("expected same length proper lists");
        let str1 = self
            .gc
            .new_string("traversal reached to non-pair element ~s");
        let str2 = self
            .gc
            .new_string("expected chain of pairs, but got ~r, as argument 2");

        self.lib_ops = vec![
            Op::Closure {
                size: 15,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocalBranchNotNull(1, 3),
            Op::ReferLocal(1),
            Op::Return(2),
            Op::Frame(4),
            Op::ReferLocal(1),
            Op::CarPush,
            Op::ReferLocalCall(0, 1),
            Op::PushFrame(5),
            Op::ReferLocalPush(0),
            Op::ReferLocal(1),
            Op::CdrPush,
            Op::ReferGlobalCall(self.gc.intern("map1"), 2),
            Op::Cons,
            Op::Return(2),
            Op::DefineGlobal(self.gc.intern("map1")),
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::ReferFreePush(197),
            Op::ReferFreePush(152),
            Op::ReferFreePush(2),
            Op::Closure {
                size: 39,
                arg_len: 3,
                is_optional_arg: true,
                num_free_vars: 3,
            },
            Op::ReferLocalBranchNotNull(2, 6),
            Op::ReferLocalPush(0),
            Op::ReferLocalPush(1),
            Op::ReferGlobal(self.gc.intern("for-all-1")),
            Op::TailCall(2, 3),
            Op::Return(3),
            Op::LetFrame(11),
            Op::ReferLocalPush(0),
            Op::ReferLocalPush(1),
            Op::ReferLocalPush(2),
            Op::ReferFreePush(0),
            Op::ReferFreePush(2),
            Op::ReferFreePush(1),
            Op::Display(6),
            Op::Frame(5),
            Op::ReferFreePush(1),
            Op::ReferLocalPush(1),
            Op::ReferLocalPush(2),
            Op::ReferFreeCall(0, 3),
            Op::PushEnter(1),
            Op::ReferLocal(0),
            Op::Test(6),
            Op::ReferFreePush(5),
            Op::ReferLocalPush(0),
            Op::ReferGlobal(self.gc.intern("for-all-n-quick")),
            Op::TailCall(2, 6),
            Op::LocalJmp(10),
            Op::ConstantPush(sym0),
            Op::ConstantPush(str0),
            Op::Frame(4),
            Op::ReferFreePush(4),
            Op::ReferFreePush(3),
            Op::ReferFreeCall(2, 2),
            Op::Push,
            Op::ReferGlobal(self.gc.intern("assertion-violation")),
            Op::TailCall(3, 6),
            Op::Leave(1),
            Op::Return(3),
            Op::DefineGlobal(self.gc.intern("for-all")),
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::ReferFreePush(48),
            Op::ReferFreePush(89),
            Op::Closure {
                size: 68,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 2,
            },
            Op::ReferLocalBranchNotNull(1, 3),
            Op::Constant(Object::True),
            Op::Return(2),
            Op::ReferLocal(1),
            Op::PairP,
            Op::Test(49),
            Op::LetFrame(14),
            Op::ReferLocalPush(0),
            Op::ReferLocalPush(1),
            Op::ReferFreePush(1),
            Op::ReferFreePush(0),
            Op::Display(4),
            Op::ReferLocal(1),
            Op::CarPush,
            Op::ReferLocal(1),
            Op::CdrPush,
            Op::Enter(2),
            Op::ReferLocalBranchNotNull(1, 5),
            Op::ReferLocalPush(0),
            Op::ReferFree(3),
            Op::TailCall(1, 6),
            Op::LocalJmp(31),
            Op::ReferLocal(1),
            Op::PairP,
            Op::Test(12),
            Op::Frame(3),
            Op::ReferLocalPush(0),
            Op::ReferFreeCall(3, 1),
            Op::Test(24),
            Op::ReferLocal(1),
            Op::CarPush,
            Op::ReferLocal(1),
            Op::CdrPush,
            Op::Shiftj(2, 2, 0),
            Op::LocalJmp(-17),
            Op::LocalJmp(17),
            Op::Frame(3),
            Op::ReferLocalPush(0),
            Op::ReferFreeCall(3, 1),
            Op::Test(13),
            Op::ConstantPush(sym0),
            Op::Frame(4),
            Op::ConstantPush(str1),
            Op::ReferLocalPush(1),
            Op::ReferFreeCall(1, 2),
            Op::PushFrame(4),
            Op::ReferFreePush(3),
            Op::ReferFreePush(2),
            Op::ReferFreeCall(0, 2),
            Op::Push,
            Op::ReferGlobal(self.gc.intern("assertion-violation")),
            Op::TailCall(3, 6),
            Op::Leave(2),
            Op::Return(2),
            Op::ConstantPush(sym0),
            Op::Frame(4),
            Op::ConstantPush(str2),
            Op::ReferLocalPush(1),
            Op::ReferFreeCall(1, 2),
            Op::PushFrame(4),
            Op::ReferLocalPush(0),
            Op::ReferLocalPush(1),
            Op::ReferFreeCall(0, 2),
            Op::Push,
            Op::ReferGlobal(self.gc.intern("assertion-violation")),
            Op::TailCall(3, 2),
            Op::Return(2),
            Op::DefineGlobal(self.gc.intern("for-all-1")),
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::ReferFreePush(152),
            Op::Closure {
                size: 32,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferLocalBranchNotNull(1, 2),
            Op::Return(2),
            Op::LetFrame(8),
            Op::ReferLocalPush(0),
            Op::ReferFreePush(0),
            Op::ReferLocalPush(1),
            Op::Display(3),
            Op::ReferLocal(1),
            Op::CarPush,
            Op::ReferLocal(1),
            Op::CdrPush,
            Op::Enter(2),
            Op::ReferLocalBranchNotNull(1, 6),
            Op::ReferFreePush(2),
            Op::ReferLocalPush(0),
            Op::ReferFree(1),
            Op::TailCall(2, 6),
            Op::LocalJmp(12),
            Op::Frame(4),
            Op::ReferFreePush(2),
            Op::ReferLocalPush(0),
            Op::ReferFreeCall(1, 2),
            Op::Test(7),
            Op::ReferLocal(1),
            Op::CarPush,
            Op::ReferLocal(1),
            Op::CdrPush,
            Op::Shiftj(2, 2, 0),
            Op::LocalJmp(-16),
            Op::Leave(2),
            Op::Return(2),
            Op::DefineGlobal(self.gc.intern("for-all-n-quick")),
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Halt,
        ];

        self.lib_ops.as_ptr()
    }
*/
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

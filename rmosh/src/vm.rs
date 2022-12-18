use std::{collections::HashMap, fmt::Display, ptr::null_mut};

use crate::{
    gc::{Gc, GcRef},
    objects::{Closure, Object, Pair, Procedure, Symbol, Vox},
    op::Op,
    procs,
};

const STACK_SIZE: usize = 256;

pub struct Vm {
    pub gc: Box<Gc>,
    ac: Object, // accumulator register.
    dc: Object, // display closure register.
    stack: [Object; STACK_SIZE],
    sp: *mut Object,
    fp: *mut Object,
    globals: HashMap<GcRef<Symbol>, Object>,
    ops: Vec<Op>, // Keep running ops so that they are not garbage collected.
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ac: Object::Undef,
            dc: Object::Undef,
            gc: Box::new(Gc::new()),
            stack: [Object::Undef; STACK_SIZE],
            sp: null_mut(),
            fp: null_mut(),
            globals: HashMap::new(),
            ops: vec![],
        }
    }

    fn initialize_free_vars(&mut self) {
        let free_vars = vec![
            Object::Procedure(self.gc.alloc(Procedure::new(procs::numberp))),
            Object::Procedure(self.gc.alloc(Procedure::new(procs::write))),
        ];
        let mut display = self.gc.alloc(Closure::new(0, 0, false, free_vars));
        display.prev = self.dc;
        self.dc = Object::Closure(display);
    }

    /// GC functions.
    fn alloc<T: Display + 'static>(&mut self, object: T) -> GcRef<T> {
        self.mark_and_sweep();
        self.gc.alloc(object)
    }

    fn mark_and_sweep(&mut self) {
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
        for &value in &self.stack[0..self.stack_len()] {
            self.gc.mark_object(value);
        }

        self.gc.mark_object(self.ac);
        self.gc.mark_object(self.dc);

        for &value in &self.ops {
            match value {
                Op::BranchNotGe(_) => (),
                Op::BranchNotNumberEqual(_) => (),                
                Op::Closure { .. } => (),
                Op::Constant(v) => {
                    self.gc.mark_object(v);
                }
                Op::DefineGlobal(symbol) => {
                    self.gc.mark_heap_object(symbol);
                }
                Op::ReferGlobal(symbol) => {
                    self.gc.mark_heap_object(symbol);
                }
                Op::Display(_) => (),
                Op::ReferFree(_) => (),
                Op::LetFrame(_) => (),
                Op::Box(_) => (),
                Op::Enter(_) => (),
                Op::Halt => (),
                Op::Car => (),
                Op::Cdr => (),
                Op::Cadr => (),
                Op::NumberEqual => (),
                Op::AssignFree(_) => (),
                Op::AssignLocal(_) => (),
                Op::Indirect => (),
                Op::Nop => (),
                Op::Undef => (),
                Op::ReferLocal(_) => (),
                Op::Leave(_) => (),
                Op::Push => (),
                Op::NumberAdd => (),
                Op::AddPair => (),
                Op::Cons => (),
                Op::LocalJmp(_) => (),
                Op::TailCall(_, _) => (),
                Op::Test(_) => (),
                Op::Call(_) => (),
                Op::Return(_) => (),
                Op::Frame(_) => (),
            }
        }
    }

    pub fn intern(&mut self, s: &str) -> GcRef<Symbol> {
        self.gc.intern(s.to_owned())
    }

    fn pop(&mut self) -> Object {
        unsafe {
            self.sp = self.sp.offset(-1);
            *self.sp
        }
    }

    fn push(&mut self, value: Object) {
        unsafe {
            *self.sp = value;
            self.sp = self.sp.offset(1);
        }
    }

    fn index(&self, sp: *mut Object, n: isize) -> Object {
        unsafe { *sp.offset(-n - 1) }
    }

    fn index_set(&mut self, sp: *mut Object, n: isize, obj: Object) {
        unsafe { *sp.offset(-n - 1) = obj }
    }
            
    fn stack_len(&self) -> usize {
        unsafe { self.sp.offset_from(self.stack.as_ptr()) as usize }
    }

    #[cfg(feature = "debug_log_vm")]
    fn print_vm(&mut self, op: Op) {
        println!("-----------------------------------------");
        println!("{:?} executed ac={:?}", op, self.ac);
        println!("-----------------------------------------");
        let fp_idx = unsafe { self.fp.offset_from(self.stack.as_ptr()) };
        for i in 0..self.stack_len() {
            println!(
                "  {:?}{}",
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

    pub fn run(&mut self, ops: Vec<Op>) -> Object {
        // todo move to the initializer
        self.ops = ops;
        self.sp = self.stack.as_mut_ptr();
        self.fp = self.sp;
        let len = self.ops.len();
        self.initialize_free_vars();
        let mut pc = 0;
        while pc < len {
            let op = self.ops[pc];
            match op {
                Op::BranchNotNumberEqual(skip_size) => {
                    match (self.pop(), self.ac) {
                        (Object::Number(lhs), Object::Number(rhs)) => {
                            self.ac = Object::make_bool(lhs == rhs);
                            if self.ac.is_false() {
                                pc = pc + skip_size - 1;
                            } else {
                                // go to next pc
                            }
                        }
                        _ => {
                            panic!("number expected")
                        }
                    }                    
                }
                Op::BranchNotGe(skip_size) => {
                    match (self.pop(), self.ac) {
                        (Object::Number(lhs), Object::Number(rhs)) => {
                            self.ac = Object::make_bool(lhs >= rhs);
                            if self.ac.is_false() {
                                pc = pc + skip_size - 1;
                            } else {
                                // go to next pc
                            }
                        }
                        _ => {
                            panic!("number expected")
                        }
                    }
                }
                Op::NumberEqual => {
                    let lhs = self.pop();
                    let rhs = self.ac;
                    match (lhs, rhs) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.ac = if l == r { Object::True } else { Object::False }
                        }
                        _ => {
                            panic!("= requres number")
                        }
                    }
                }
                Op::Car => match self.ac {
                    Object::Pair(pair) => {
                        self.ac = pair.first;
                    }
                    _ => {
                        panic!("car pair required")
                    }
                },
                Op::Cdr => match self.ac {
                    Object::Pair(pair) => {
                        self.ac = pair.second;
                    }
                    _ => {
                        panic!("cdr pair required")
                    }
                },
                Op::Cadr => match self.ac {
                    Object::Pair(pair) => match pair.second {
                        Object::Pair(pair) => {
                            self.ac = pair.first;
                        }
                        _ => {
                            panic!("cadr pair required")
                        }
                    },
                    _ => {
                        panic!("car pair required")
                    }
                },
                Op::Indirect => match self.ac {
                    Object::Vox(vox) => {
                        self.ac = vox.value;
                    }
                    _ => {
                        panic!("indirect vox not found")
                    }
                },
                Op::AssignLocal(n) => match self.refer_local(n) {
                    Object::Vox(mut vox) => vox.value = self.ac,
                    _ => {
                        panic!("assign local vox not found")
                    }
                },
                Op::Box(n) => {
                    let vox = self.alloc(Vox::new(self.index(self.sp, n)));
                    self.index_set(self.sp, n, Object::Vox(vox));
                }
                Op::Constant(c) => {
                    self.ac = c;
                }
                Op::Push => {
                    self.push(self.ac);
                }
                Op::Cons => {
                    let first = self.pop();
                    let second = self.ac;
                    let pair = self.alloc(Pair::new(first, second));
                    self.ac = Object::Pair(pair);
                }
                Op::NumberAdd => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.ac = Object::Number(a + b);
                    }
                    (a, b) => {
                        panic!("add: numbers required but got {:?} {:?}", a, b);
                    }
                },
                Op::AddPair => {
                    if let Object::Pair(pair) = self.ac {
                        match (pair.first, pair.second) {
                            (Object::Number(lhs), Object::Number(rhs)) => {
                                self.ac = Object::Number(lhs + rhs);
                            }
                            _ => {
                                panic!(
                                    "add pair: numbers require but got {:?} and {:?}",
                                    pair.first, pair.second
                                );
                            }
                        }
                    } else {
                        panic!("pair required but got {:?}", self.ac);
                    }
                }
                Op::DefineGlobal(symbol) => {
                    self.globals.insert(symbol, self.ac);
                    self.ac = Object::Undef;
                }
                Op::ReferGlobal(symbol) => match self.globals.get(&symbol) {
                    Some(&value) => {
                        self.ac = value;
                    }
                    None => {
                        panic!("identifier {:?} not found", symbol);
                    }
                },
                Op::Enter(n) => unsafe {
                    self.fp = self.sp.offset(-n);
                },
                Op::LetFrame(_) => {
                    // todo: expand stack here.
                    self.push(self.dc);
                    self.push(Object::VMStackPointer(self.fp));
                }
                Op::ReferLocal(n) => {
                    self.ac = self.refer_local(n);
                }
                Op::Leave(n) => unsafe {
                    let sp = self.sp.offset(-n);

                    match self.index(sp, 0) {
                        Object::VMStackPointer(fp) => {
                            self.fp = fp;
                        }
                        x => {
                            panic!("fp expected but got {:?}", x);
                        }
                    }
                    self.dc = self.index(sp, 1);
                    self.sp = sp.offset(-2);
                },
                Op::Display(num_free_vars) => {
                    let mut free_vars = vec![];
                    let start = unsafe { self.sp.offset(-1) };
                    for i in 0..num_free_vars {
                        let var = unsafe { *start.offset(-i) };
                        free_vars.push(var);
                    }
                    let mut display = self.alloc(Closure::new(0, 0, false, free_vars));
                    display.prev = self.dc;

                    let display = Object::Closure(display);
                    self.dc = display;
                    self.sp = unsafe { self.sp.offset(-num_free_vars) };
                }
                Op::ReferFree(n) => match self.dc {
                    Object::Closure(mut closure) => {
                        self.ac = closure.refer_free(n);
                    }
                    _ => {
                        panic!("refer_free: display closure expected but got {:?}", self.dc);
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
                            "assign_free: display closure expected but got {:?}",
                            self.dc
                        );
                    }
                },
                Op::Test(skip_size) => {
                    if self.ac.is_false() {
                        pc = pc + skip_size - 1;
                    }
                }
                Op::LocalJmp(jump_size) => {
                    pc = pc + jump_size - 1;
                }
                Op::Closure {
                    size,
                    arg_len,
                    is_optional_arg,
                    num_free_vars,
                } => {
                    let mut free_vars = vec![];
                    let start = unsafe { self.sp.offset(-1) };
                    for i in 0..num_free_vars {
                        let var = unsafe { *start.offset(-i) };
                        free_vars.push(var);
                    }
                    self.ac = Object::Closure(self.alloc(Closure::new(
                        pc,
                        arg_len,
                        is_optional_arg,
                        free_vars,
                    )));

                    self.sp = unsafe { self.sp.offset(-num_free_vars) };
                    pc += size - 1;
                }
                Op::TailCall(depth, diff) => {
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    let argc = depth;
                    // todo dedup logic with Op::Call
                    match self.ac {
                        Object::Closure(closure) => {
                            self.dc = self.ac;
                            // self.cl = self.ac;
                            pc = closure.pc;
                            if closure.is_optional_arg {
                                panic!("not supported yet");
                            } else if argc == closure.argc {
                                self.fp = unsafe { self.sp.offset(-argc) };
                            } else {
                                panic!("wrong arguments");
                            }
                        }
                        Object::Procedure(procedure) => {
                            // self.cl = self.ac
                            let offset = unsafe { self.fp.offset_from(self.stack.as_ptr()) };
                            let offset: usize =
                                usize::try_from(offset).expect("offset can't be usize");
                            let argc: usize = usize::try_from(argc).expect("argc can't be usize");
                            let args = &self.stack[offset..offset + argc];
                            self.ac = (procedure.func)(args);

                            self.return_n(1, &mut pc);
                        }
                        _ => {
                            panic!("can't call {:?}", self.ac);
                        }
                    }
                }
                Op::Call(argc) => {
                    match self.ac {
                        Object::Closure(closure) => {
                            self.dc = self.ac;
                            // self.cl = self.ac;
                            pc = closure.pc;
                            if closure.is_optional_arg {
                                panic!("not supported yet");
                            } else if argc == closure.argc {
                                self.fp = unsafe { self.sp.offset(-argc) };
                            } else {
                                panic!("wrong arguments");
                            }
                        }
                        Object::Procedure(procedure) => {
                            // self.cl = self.ac
                            let offset = unsafe { self.sp.offset_from(self.stack.as_ptr()) } - 1;
                            let offset: usize =
                                usize::try_from(offset).expect("offset can't be usize");
                            let argc: usize = usize::try_from(argc).expect("argc can't be usize");
                            let args = &self.stack[offset..offset + argc];
                            self.ac = (procedure.func)(args);

                            self.return_n(1, &mut pc);
                        }
                        _ => {
                            panic!("can't call {:?}", self.ac);
                        }
                    }
                }
                Op::Return(n) => {
                    self.return_n(n, &mut pc);
                }
                Op::Frame(skip_size) => {
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
                    // where pc* = pc + skip_size -1
                    let next_pc =
                        isize::try_from(pc + skip_size - 1).expect("can't convert to isize");
                    self.push(Object::Number(next_pc));
                    self.push(self.dc);
                    self.push(self.dc); // todo this should be cl.
                    self.push(Object::VMStackPointer(self.fp));
                }
                Op::Halt => return self.ac,
                Op::Undef => self.ac = Object::Undef,
                Op::Nop => {}
            }
            self.print_vm(op);
            pc += 1;
        }
        self.ac
    }

    fn refer_local(&mut self, n: isize) -> Object {
        unsafe { *self.fp.offset(n) }
    }

    fn return_n(&mut self, n: isize, pc: &mut usize) {
        let sp = unsafe { self.sp.offset(-n) };
        match self.index(sp, 0) {
            Object::VMStackPointer(fp) => {
                self.fp = fp;
            }
            _ => {
                panic!("not fp pointer")
            }
        }
        // todo We don't have cl yet.
        // self.cl = index(sp, 1);
        self.dc = self.index(sp, 2);
        match self.index(sp, 3) {
            Object::Number(next_pc) => {
                *pc = usize::try_from(next_pc).expect("pc it not a number");
            }
            _ => {
                panic!("not a pc");
            }
        }
        self.sp = unsafe { sp.offset(-4) }
    }

    fn shift_args_to_bottom(&mut self, sp: *mut Object, depth: isize, diff: isize) -> *mut Object {
        let mut i = depth - 1;
        while i >= 0 {
            self.index_set(sp, i + diff, self.index(self.sp, i));
            i = i - 1;
        }
        unsafe { sp.offset(-diff) }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    static SIZE_OF_PAIR: usize = std::mem::size_of::<Pair>();
    static SIZE_OF_CLOSURE: usize = std::mem::size_of::<Closure>();
    static SIZE_OF_PROCEDURE: usize = std::mem::size_of::<Procedure>();
    // Base closure + procedure as free variable
    static SIZE_OF_MIN_VM: usize = SIZE_OF_CLOSURE + SIZE_OF_PROCEDURE + SIZE_OF_PROCEDURE;

    fn test_ops_with_size(vm: &mut Vm, ops: Vec<Op>, expected: Object, expected_heap_diff: usize) {
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(ops);
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(
            after_size - before_size,
            SIZE_OF_MIN_VM + expected_heap_diff
        );
        assert_eq!(ret, expected);
    }

    // Custom hand written tests.
    #[test]
    fn test_symbol_intern() {
        let mut gc = Gc::new();

        let symbol = gc.intern("foo".to_owned());
        let symbol2 = gc.intern("foo".to_owned());
        assert_eq!(symbol.pointer, symbol2.pointer);
    }

    #[test]
    fn test_vm_call_proc() {
        let mut vm = Vm::new();
        // ((lambda (a) (+ a a))
        let ops: Vec<Op> = vec![
            Op::Frame(8),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferFree(1),
            Op::Call(1),
        ];
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(ops);
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
        match ret {
            Object::Undef => {}
            _ => panic!("ac was {:?}", ret),
        }
    }

    #[test]
    fn test_vm_alloc_many_pairs() {
        let mut vm = Vm::new();
        let mut ops = vec![];

        for _ in 0..100 {
            ops.push(Op::Constant(Object::Number(99)));
            ops.push(Op::Push);
            ops.push(Op::Constant(Object::Number(101)));
            ops.push(Op::Cons);
        }
        let before_size = vm.gc.bytes_allocated();
        vm.run(ops);
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM + SIZE_OF_PAIR);
    }

    #[test]
    fn test_vm_define() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(9)),
            Op::DefineGlobal(vm.gc.intern("a".to_owned())),
            Op::ReferGlobal(vm.gc.intern("a".to_owned())),
        ];
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(ops);
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 9);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_run_add_pair() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(99)),
            Op::Push,
            Op::Constant(Object::Number(101)),
            Op::Cons,
            Op::AddPair,
        ];
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(ops);
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 200);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    // All ops in the following tests are generated in data/.

    #[test]
    fn test_call0() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_call1() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(10),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Closure {
                size: 6,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
    }

    #[test]
    fn test_call2() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(12),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Closure {
                size: 6,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::NumberAdd,
            Op::Return(2),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_if0() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Test(3),
            Op::Constant(Object::Number(2)),
            Op::LocalJmp(2),
            Op::Constant(Object::Number(3)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
    }

    #[test]
    fn test_if1() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::Number(2)),
            Op::LocalJmp(2),
            Op::Constant(Object::Number(3)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_let0() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(0), 0);
    }

    #[test]
    fn test_let1() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Enter(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::NumberAdd,
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_define0() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::DefineGlobal(vm.gc.intern("a".to_owned())),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Undef, 0);
    }

    #[test]
    fn test_nested_let0() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_nested_let1() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(5),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(4),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(3),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(1),
            Op::Push,
            Op::ReferFree(0),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(6), 0);
    }

    #[test]
    fn test_and() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::True), Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    #[test]
    fn test_if2() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::False),
            Op::LocalJmp(2),
            Op::Constant(Object::True),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    #[test]
    fn test_test0() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::True), Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    #[test]
    fn test_test2() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::True), Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    #[test]
    fn test_test3() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test4() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test5() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::False),
            Op::LocalJmp(2),
            Op::Constant(Object::True),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    #[test]
    fn test_test6() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(7),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test7() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(6)),
            Op::Push,
            Op::Closure {
                size: 7,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Test(3),
            Op::Constant(Object::Number(7)),
            Op::Return(1),
            Op::Constant(Object::Number(5)),
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
    }

    #[test]
    fn test_test8() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test9() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(7),
            Op::Constant(Object::Number(101)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
    }

    #[test]
    fn test_test10() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(9),
            Op::Frame(7),
            Op::Closure {
                size: 5,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(102)),
            Op::Return(0),
            Op::Return(0),
            Op::Call(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
    }

    #[test]
    fn test_test11() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(101)),
            Op::Push,
            Op::Frame(7),
            Op::Closure {
                size: 5,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(102)),
            Op::Return(1),
            Op::Return(0),
            Op::Call(0),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
    }

    #[test]
    fn test_test12() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(103)),
            Op::Push,
            Op::Frame(7),
            Op::Closure {
                size: 5,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Return(0),
            Op::Call(0),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(103), 0);
    }

    #[test]
    fn test_test13() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Frame(11),
            Op::Constant(Object::Number(10)),
            Op::Push,
            Op::Closure {
                size: 7,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferFree(0),
            Op::Return(0),
            Op::Return(1),
            Op::Call(1),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
    }

    #[test]
    fn test_test14() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Closure {
                size: 7,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Box(0),
            Op::Constant(Object::Number(12)),
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
    }

    #[test]
    fn test_test15() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(14),
            Op::Constant(Object::Nil),
            Op::Push,
            Op::Closure {
                size: 10,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Box(0),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 4,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::Constant(Object::Number(101)),
            Op::AssignFree(0),
            Op::Return(0),
            Op::TailCall(0, 1),
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
    }

    #[test]
    fn test_test16() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(24),
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Push,
            Op::Closure {
                size: 18,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 6,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferFree(0),
            Op::TailCall(1, 1),
            Op::Return(1),
            Op::Push,
            Op::Closure {
                size: 6,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::ReferLocal(0),
            Op::TailCall(1, 1),
            Op::Return(1),
            Op::TailCall(1, 1),
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
    }

    #[test]
    fn test_test17() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(7),
            Op::Closure {
                size: 5,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Constant(Object::Number(4)),
            Op::Constant(Object::Number(5)),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
    }

    #[test]
    fn test_test18() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    #[test]
    fn test_test19() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(Object::Symbol(vm.gc.intern("a".to_owned()))),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    #[test]
    fn test_test20() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(Object::Symbol(vm.gc.intern("a".to_owned()))),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    #[test]
    fn test_test21() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test22() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberAdd,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
    }

    #[test]
    fn test_test23() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberAdd,
            Op::Push,
            Op::Constant(Object::Number(10)),
            Op::NumberAdd,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(17), 0);
    }

    #[test]
    fn test_test24() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test25() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(10)),
            Op::Push,
            Op::Constant(Object::Number(-5)),
            Op::NumberAdd,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
    }

    #[test]
    fn test_test26() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(10)),
            Op::Push,
            Op::Constant(Object::Number(-5)),
            Op::NumberAdd,
            Op::Push,
            Op::Constant(Object::Number(-2)),
            Op::NumberAdd,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test27_modified() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Symbol(vm.gc.intern("a".to_owned()))),
            Op::Push,
            Op::Constant(Object::Symbol(vm.gc.intern("b".to_owned()))),
            Op::Cons,
            Op::Halt,
        ];
        let a = Object::Symbol(vm.gc.intern("a".to_owned()));
        let b = Object::Symbol(vm.gc.intern("b".to_owned()));
        let pair = vm.gc.alloc(Pair::new(a, b));

        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(ops);
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
        match ret {
            Object::Pair(pair2) => {
                assert_eq!(pair.first, pair2.first);
                assert_eq!(pair.second, pair2.second);
            }
            _ => {
                panic!("not a pair");
            }
        }
    }

    #[test]
    fn test_test28() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Cons,
            Op::Car,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
    }

    #[test]
    fn test_test29() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Cons,
            Op::Cdr,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test30() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Nil),
            Op::Cons,
            Op::Cons,
            Op::Cadr,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test31() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberEqual,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    #[test]
    fn test_test32() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::NumberEqual,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    #[test]
    fn test_test33() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test34() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(2),
            Op::ReferLocal(1),
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    #[test]
    fn test_test35() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(2),
            Op::ReferLocal(0),
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test36() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(2),
            Op::ReferLocal(0),
            Op::ReferLocal(1),
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    #[test]
    fn test_test37() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test38() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test39() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(0),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test40() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
    }

    #[test]
    fn test_test41() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(4),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(3),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(2),
            Op::Constant(Object::Number(5)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(1),
            Op::Push,
            Op::ReferFree(0),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
    }

    #[test]
    fn test_test42() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(2),
            Op::LetFrame(3),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::Push,
            Op::Display(2),
            Op::Constant(Object::Number(5)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(1),
            Op::Push,
            Op::ReferFree(0),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
    }

    #[test]
    fn test_test43() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(6),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(2),
            Op::LetFrame(3),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::Push,
            Op::Display(2),
            Op::Constant(Object::Number(5)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(1),
            Op::Push,
            Op::ReferFree(0),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(13), 0);
    }

    #[test]
    fn test_test44() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test45() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
    }

    #[test]
    fn test_test46() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::ReferFree(0),
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test47() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(2),
            Op::ReferLocal(0),
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
    }

    #[test]
    fn test_test48() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferFree(0),
            Op::Return(0),
            Op::Push,
            Op::Enter(1),
            Op::Frame(3),
            Op::ReferLocal(0),
            Op::Call(0),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test49() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferFree(0),
            Op::Return(0),
            Op::Push,
            Op::Enter(1),
            Op::Frame(3),
            Op::ReferLocal(0),
            Op::Call(0),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test50() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Enter(3),
            Op::ReferLocal(2),
            Op::Leave(3),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
    }

    #[test]
    fn test_test51() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(5),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(4),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(3),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferFree(0),
            Op::Push,
            Op::Display(3),
            Op::ReferFree(0),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferFree(1),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Leave(1),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test52() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test53() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::NumberAdd,
            Op::Leave(2),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
    }

    #[test]
    fn test_test54() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test55() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferFree(0),
            Op::Return(0),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::AssignFree(1),
            Op::Leave(1),
            Op::Leave(1),
            Op::Frame(4),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Call(0),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    #[test]
    fn test_test56() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(2),
            Op::LetFrame(1),
            Op::ReferLocal(1),
            Op::Push,
            Op::Display(1),
            Op::ReferLocal(1),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferFree(0),
            Op::Return(0),
            Op::Push,
            Op::Enter(1),
            Op::Frame(3),
            Op::ReferLocal(0),
            Op::Call(0),
            Op::Leave(1),
            Op::Leave(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    #[test]
    fn test_test57() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(2),
            Op::Frame(7),
            Op::ReferLocal(1),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferFree(0),
            Op::Return(0),
            Op::Call(0),
            Op::Leave(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    #[test]
    fn test_test58() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Box(0),
            Op::Enter(2),
            Op::LetFrame(1),
            Op::ReferLocal(1),
            Op::Push,
            Op::Display(1),
            Op::ReferLocal(1),
            Op::Push,
            Op::Closure {
                size: 6,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::Constant(Object::Number(3)),
            Op::AssignFree(0),
            Op::ReferFree(0),
            Op::Indirect,
            Op::Return(0),
            Op::Push,
            Op::Enter(1),
            Op::Frame(3),
            Op::ReferLocal(0),
            Op::Call(0),
            Op::Leave(1),
            Op::Leave(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test59() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Box(0),
            Op::Enter(2),
            Op::LetFrame(1),
            Op::ReferLocal(1),
            Op::Push,
            Op::Display(1),
            Op::ReferLocal(1),
            Op::Push,
            Op::Closure {
                size: 6,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::Constant(Object::Number(3)),
            Op::AssignFree(0),
            Op::ReferFree(0),
            Op::Indirect,
            Op::Return(0),
            Op::Push,
            Op::Enter(1),
            Op::Frame(3),
            Op::ReferLocal(0),
            Op::Call(0),
            Op::Leave(1),
            Op::Leave(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    #[test]
    fn test_test60() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(100)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::LetFrame(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferFree(0),
            Op::Return(0),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Push,
            Op::Enter(1),
            Op::Frame(3),
            Op::ReferLocal(0),
            Op::Call(0),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(100), 0);
    }


    // (letrec ((a 1) (b (lambda () a))) (b)) => 1
    #[test]
    fn test_test61() {
        let mut vm = Vm::new();        
        let ops = vec![
            Op::LetFrame(0),
            Op::Undef,
            Op::Push,
            Op::Undef,
            Op::Push,
            Op::Box(1),
            Op::Box(0),
            Op::Enter(2),
            Op::Constant(Object::Number(1)),
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {size: 4, arg_len: 0, is_optional_arg: false, num_free_vars: 1},
            Op::ReferFree(0),
            Op::Indirect,
            Op::Return(0),
            Op::AssignLocal(1),
            Op::Frame(4),
            Op::ReferLocal(1),
            Op::Indirect,
            Op::Call(0),
            Op::Leave(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    // (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0)) => 10
    #[test]
    fn test_test62() {
        let mut vm = Vm::new();        
        let ops = vec![
            Op::LetFrame(1),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {size: 16, arg_len: 1, is_optional_arg: false, num_free_vars: 1},
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(10)),
            Op::BranchNotNumberEqual(3),
            Op::ReferLocal(0),
            Op::Return(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Push,
            Op::ReferFree(0),
            Op::Indirect,
            Op::TailCall(1, 1),
            Op::Return(1),
            Op::AssignLocal(0),
            Op::Frame(6),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Call(1),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
    }

    // (let ((a '())) (let ((G68 (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (set! a G68) (a 0))) => 1000
    #[test]
    fn test_test63() {
        let mut vm = Vm::new();        
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Nil),
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {size: 16, arg_len: 1, is_optional_arg: false, num_free_vars: 1},
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(1000)),
            Op::BranchNotGe(3),
            Op::ReferLocal(0),
            Op::Return(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Push,
            Op::ReferFree(0),
            Op::Indirect,
            Op::TailCall(1, 1),
            Op::Return(1),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::AssignFree(0),
            Op::Frame(6),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::ReferFree(0),
            Op::Indirect,
            Op::Call(1),
            Op::Leave(1),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
    }


    // (letrec ((a (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (a 0)) => 1000
    #[test]
    fn test_test64() {
        let mut vm = Vm::new();        
        let ops = vec![
            Op::LetFrame(1),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {size: 16, arg_len: 1, is_optional_arg: false, num_free_vars: 1},
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(1000)),
            Op::BranchNotGe(3),
            Op::ReferLocal(0),
            Op::Return(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
            Op::Push,
            Op::ReferFree(0),
            Op::Indirect,
            Op::TailCall(1, 1),
            Op::Return(1),
            Op::AssignLocal(0),
            Op::Frame(6),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Call(1),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
    }


    // ((lambda (a) (set! a 1000) a) '()) => 1000
    #[test]
    fn test_test65() {
        let mut vm = Vm::new();        
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Nil),
            Op::Push,
            Op::Closure {size: 7, arg_len: 1, is_optional_arg: false, num_free_vars: 0},
            Op::Box(0),
            Op::Constant(Object::Number(1000)),
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
    }

}

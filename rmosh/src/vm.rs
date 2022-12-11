use std::{collections::HashMap, fmt::Display, ptr::null_mut};

use crate::{
    gc::{Gc, GcHeader, GcRef},
    objects::{Closure, Object, Pair, Procedure, Symbol},
    op::Op,
    procs::scm_write,
};

const STACK_SIZE: usize = 256;

pub struct Vm {
    pub gc: Box<Gc>,
    // ToDo: Do they need to be pub?
    ac: Object,
    dc: Object, // display closure
    stack: [Object; STACK_SIZE],
    sp: *mut Object,
    fp: *mut Object,
    globals: HashMap<GcRef<Symbol>, Object>,
    ops: Vec<Op>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ac: Object::Undef,
            dc: Object::Undef,
            gc: Box::new(Gc::new()),
            stack: [Object::Number(0); STACK_SIZE],
            sp: null_mut(),
            fp: null_mut(),
            globals: HashMap::new(),
            ops: vec![],
        }
    }

    fn initialize_free_vars(&mut self) {
        let mut free_vars = vec![];
        let proc = self.gc.alloc(Procedure::new(scm_write));
        let proc = Object::Procedure(proc);
        free_vars.push(proc);
        let mut display = self.gc.alloc(Closure::new(0, 0, false, 0, free_vars));
        display.prev = self.dc;
        let display = Object::Closure(display);
        self.dc = display;
    }

    fn alloc<T: Display + 'static>(&mut self, object: T) -> GcRef<T> {
        self.mark_and_sweep();
        self.gc.alloc(object)
    }

    fn mark_and_sweep(&mut self) {
        println!("-- gc begin");

        self.mark_roots();
        self.gc.collect_garbage();

        println!("-- gc end");
    }

    fn stack_len(&self) -> usize {
        unsafe { self.sp.offset_from(self.stack.as_ptr()) as usize }
    }

    fn mark_roots(&mut self) {
        for &value in &self.stack[0..self.stack_len()] {
            self.gc.mark_value(value);
        }

        self.gc.mark_value(self.ac);
        self.gc.mark_value(self.dc);

        for &value in &self.ops {
            match value {
                Op::Closure { .. } => (),
                Op::Constant(v) => {
                    self.gc.mark_value(v);
                }
                Op::DefineGlobal(symbol) => {
                    self.gc.mark_object(symbol);
                }
                Op::ReferGlobal(symbol) => {
                    self.gc.mark_object(symbol);
                }
                Op::Display(_) => (),
                Op::ReferFree(_) => (),
                Op::LetFrame(_) => (),
                Op::Enter(_) => (),
                Op::ReferLocal(_) => (),
                Op::Leave(_) => (),
                Op::Push => (),
                Op::Add => (),
                Op::AddPair => (),
                Op::Cons => (),
                Op::LocalJump(_) => (),
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

    fn index(&mut self, sp: *mut Object, n: isize) -> Object {
        unsafe { *sp.offset(-n - 1) }
    }

    fn print_stack(&mut self) {
        println!("-----------------------------------------");
        for &value in &self.stack[0..self.stack_len()] {
            println!("{:?}", value);
        }
        println!("-----------------------------------------<== sp")
    }

    pub fn run(&mut self, ops: Vec<Op>) -> Object {
        // todo move to the initializer
        self.ops = ops; // gc roots
        self.sp = self.stack.as_mut_ptr();
        self.fp = self.sp;
        let len = self.ops.len();
        self.initialize_free_vars();
        let mut pc = 0;
        while pc < len {
            let op = self.ops[pc];
            match op {
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
                    println!("pair alloc(adr:{:?})", &pair.header as *const GcHeader);
                    self.ac = Object::Pair(pair);
                }
                Op::Add => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        println!("a={} ac={}", a, b);
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
                Op::ReferLocal(n) => unsafe {
                    self.ac = *self.fp.offset(n);
                },
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
                    let mut display = self.alloc(Closure::new(0, 0, false, 0, free_vars));
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
                Op::Test(skip_size) => {
                    if self.ac.is_false() {
                        pc = pc + skip_size - 1;
                    }
                }
                Op::LocalJump(jump_size) => {
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
                        size,
                        free_vars,
                    )));

                    self.sp = unsafe { self.sp.offset(-num_free_vars) };
                    pc += size;
                }
                Op::Call(arg_len) => {
                    match self.ac {
                        Object::Closure(closure) => {
                            self.dc = self.ac;
                            // self.cl = self.ac;
                            pc = closure.pc;
                            if closure.is_optional_arg {
                                panic!("not supported yet");
                            } else if arg_len == closure.arg_len {
                                self.fp = unsafe { self.sp.offset(-arg_len) };
                            } else {
                                panic!("wrong arguments");
                            }
                        }
                        Object::Procedure(procedure) => {
                            // self.cl = self.ac
                            assert_eq!(1, arg_len);
                            let arg = unsafe { *self.fp.offset(arg_len) };
                            //self.fp = unsafe { self.sp.offset(-arg_len) };
                            self.ac = (procedure.func)(arg);

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
            }
            println!("after op={:?} ac={:?}", op, self.ac);
            self.print_stack();
            pc += 1;
        }
        self.ac
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
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_vm_call_proc() {
        let mut vm = Vm::new();
        // ((lambda (a) (+ a a))
        let ops: Vec<Op> = vec![
            Op::Frame(8),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Undef => {}
            _ => panic!("ac was {:?}", ret),
        }
    }

    #[test]
    fn test_vm_call() {
        let mut vm = Vm::new();
        // ((lambda (a) (+ a a))
        let ops: Vec<Op> = vec![
            Op::Frame(21),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Closure {
                size: 5,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Add,
            Op::Return(1),
            Op::Call(1),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 2);
            }
            _ => panic!("ac was {:?}", ret),
        }
    }

    #[test]
    fn test_vm_call2() {
        let mut vm = Vm::new();
        // ((lambda (a b) (+ a b) 1 2)
        let ops: Vec<Op> = vec![
            Op::Frame(24),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Closure {
                size: 5,
                arg_len: 2,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::Add,
            Op::Return(2),
            Op::Call(2),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 3);
            }
            _ => panic!("ac was {:?}", ret),
        }
    }

    #[test]
    fn test_if() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Test(3),
            Op::Constant(Object::Number(2)),
            Op::LocalJump(2),
            Op::Constant(Object::Number(3)),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 2);
            }
            _ => panic!("{:?}", "todo"),
        }
    }
    #[test]
    fn test_if2() {
        let mut vm = Vm::new();
        // (if #f 2 3)
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::Number(2)),
            Op::LocalJump(2),
            Op::Constant(Object::Number(3)),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 3);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_let() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 3);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_define() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(9)),
            Op::DefineGlobal(vm.gc.intern("a".to_owned())),
            Op::ReferGlobal(vm.gc.intern("a".to_owned())),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 9);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_run_add() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(99)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Add,
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 100);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_run_nested_let() {
        let mut vm = Vm::new();
        // (let ([a 2]) (let ([b 1]) (+ a b)))
        let ops: Vec<Op> = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Add,
            Op::Leave(1),
            Op::Leave(1),
        ];
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 3);
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
        let ret = vm.run(ops);
        match ret {
            Object::Number(a) => {
                assert_eq!(a, 200);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_symbol_intern() {
        let mut gc = Gc::new();
        let symbol = gc.intern("foo".to_owned());
        let symbol2 = gc.intern("foo".to_owned());
        assert_eq!(symbol.pointer, symbol2.pointer);
    }
}

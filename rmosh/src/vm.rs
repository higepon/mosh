use std::{collections::HashMap, fmt::Display, ptr::{null_mut, null}};

use crate::{
    gc::{Gc, GcRef},
    objects::{Closure, Object, Pair, Symbol, Vox},
    op::Op,
    procs::{self, default_free_vars},
};

const STACK_SIZE: usize = 256;

#[macro_export]
macro_rules! branch_number_op {
    ($op:tt, $self:ident, $pc:ident, $skip_offset:ident) => {
        {
            match ($self.pop(), $self.ac) {
                (Object::Number(lhs), Object::Number(rhs)) => {
                    let op_result = lhs $op rhs;
                    $self.ac = Object::make_bool(op_result);
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
                    $self.ac = Object::make_bool(l $op r)
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
    ac: Object,                      
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
        }
    }

    pub fn intern(&mut self, s: &str) -> GcRef<Symbol> {
        self.gc.intern(s)
    }    

    fn initialize_free_vars(&mut self, ops: *const Op, ops_len: usize) {
        let free_vars = default_free_vars(&mut self.gc);
        let mut display = self.gc.alloc(Closure::new(ops, ops_len, 0, false, free_vars));
        display.prev = self.dc;
        self.dc = Object::Closure(display);
    }

    // GC functions.
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
        // Stack.
        for &obj in &self.stack[0..self.stack_len()] {
            self.gc.mark_object(obj);
        }

        // Symbols.
        let symbols = self.gc.symbols.values().cloned().collect::<Vec<GcRef<Symbol>>>();
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

    pub fn run(&mut self, ops: *const Op, ops_len:usize) -> Object {
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

        let mut pc:  *const Op = ops;
        loop {
            let op = unsafe { *pc };
            match op {
                Op::MakeVector => match self.pop() {
                    Object::Number(size) => {
                        let v = vec![self.ac; size as usize];
                        self.ac = self.gc.new_vector(&v);
                    }
                    obj => {
                        self.arg_err("make-vector", "numbers", obj);
                    }
                },
                Op::VectorLength => match self.ac {
                    Object::Vector(v) => {
                        self.ac = Object::Number(v.len() as isize);
                    }
                    obj => {
                        self.arg_err("vector-length", "vector", obj);
                    }
                },
                Op::Append2 => {
                    let head = self.pop();
                    if Pair::is_list(head) {
                        self.ac = self.gc.append2(head, self.ac);
                    } else {
                        self.arg_err("append", "pair", head);                        
                    }
                }
                Op::SetCar => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.car = self.ac;
                        self.ac = Object::Unspecified;
                    }
                    obj => {
                        self.arg_err("set-car!", "pair", obj);                        
                    }
                },
                Op::SetCdr => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.cdr = self.ac;
                        self.ac = Object::Unspecified;
                    }
                    obj => {
                        self.arg_err("set-cdr!", "pair", obj);                     
                    }
                },
                Op::Not => {
                    self.ac = Object::make_bool(self.ac.is_false());
                }
                Op::PairP => self.ac = Object::make_bool(self.ac.is_pair()),
                Op::NullP => self.ac = Object::make_bool(self.ac.is_nil()),
                Op::SymbolP => self.ac = Object::make_bool(self.ac.is_symbol()),
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
                        self.ac = Object::False;                        
                    } else {
                        self.ac = Object::True;
                        pc = self.jump(pc, skip_offset - 1);
                    }
                }
                Op::Eq => {
                    let is_eq = self.pop().eq(&self.ac);
                    self.ac = Object::make_bool(is_eq);
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
                        self.ac = pair.car;
                    }
                    obj => {
                        self.arg_err("car", "pair", obj);                     
                    }
                },
                Op::Cdr => match self.ac {
                    Object::Pair(pair) => {
                        self.ac = pair.cdr;
                    }
                    obj => {
                        self.arg_err("cdr", "pair", obj);                                             
                    }
                },
                Op::Cadr => match self.ac {
                    Object::Pair(pair) => match pair.cdr {
                        Object::Pair(pair) => {
                            self.ac = pair.car;
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
                        self.ac = vox.value;
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
                    self.ac = c;
                }
                Op::Push => {
                    self.push(self.ac);
                }
                Op::Cons => {
                    let car = self.pop();
                    let cdr = self.ac;
                    self.ac = self.gc.cons(car, cdr);
                }
                Op::NumberAdd => match (self.pop(), self.ac) {
                    (Object::Number(a), Object::Number(b)) => {
                        self.ac = Object::Number(a + b);
                    }
                    (a, b) => {
                        panic!("+: numbers required but got {:?} {:?}", a, b);
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
                        self.ac = value;
                    }
                    None => {
                        panic!("identifier {:?} not found", symbol);
                    }
                },
                Op::Enter(n) => {
                    self.fp = self.dec(self.sp, n);
                },
                Op::LetFrame(_) => {
                    // TODO: expand stack.
                    self.push(self.dc);
                    self.push(Object::StackPointer(self.fp));
                }
                Op::ReferLocal(n) => {
                    self.ac = self.refer_local(n);
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
                },
                Op::Display(num_free_vars) => {
                    let mut free_vars = vec![];
                    let start = self.dec(self.sp, 1);
                    for i in 0..num_free_vars {
                        let var = unsafe { *start.offset(-i) };
                        free_vars.push(var);
                    }
                    let mut display = self.alloc(Closure::new(&[] as  *const Op, 0, 0, false, free_vars));
                    display.prev = self.dc;

                    let display = Object::Closure(display);
                    self.dc = display;
                    self.sp =  self.dec(self.sp, num_free_vars);
                }
                Op::ReferFree(n) => match self.dc {
                    Object::Closure(mut closure) => {
                        self.ac = closure.refer_free(n);
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
                    self.ac = Object::Closure(self.alloc(Closure::new(
                       pc,
                       size - 1,
                       arg_len,
                       is_optional_arg,
                       free_vars,
                    )));
                    self.sp =  self.dec(self.sp, num_free_vars);
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
                Op::Halt => { break; }
                Op::Undef => self.ac = Object::Unspecified,
                Op::Nop => {}
            }
            self.print_vm(op);
            pc = self.jump(pc, 1);
        }
        self.ac
    }

    #[inline(always)]
    fn call(&mut self, pc: &mut *const Op, argc: isize) {
        match self.ac   {
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
                        self.fp =  self.dec(self.sp, closure.argc);
                    } else if extra_len >= 0 {
                        let args = self.stack_to_pair(extra_len + 1);
                        self.index_set(self.sp, extra_len, args);
                        let sp = self.dec(self.sp, extra_len);
                        self.fp = self.dec(sp, closure.argc);
                        self.sp = sp;
                    } else {
                        panic!("call: wrong number of arguments {} required bug got {}", closure.argc, argc);
                    }
                } else if argc == closure.argc {
                    self.fp = self.dec(self.sp,argc);
                } else {
                    panic!("call: wrong number of arguments {} required bug got {}", closure.argc, argc);
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
                        panic!("apply: need two or more arguments");
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
                                    "apply last arguments shoulbe proper list but got {}",
                                    last_pair
                                );
                            }
                            let mut j: isize = 0;
                            loop {
                                if last_pair.is_nil() {
                                    let new_argc = argc - 2 + j;
                                    println!("**** Call call from call ****");
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
        unsafe {*self.dec(sp, n + 1)}
    }

    fn index_set(&mut self, sp: *mut Object, n: isize, obj: Object) {
        unsafe { *self.dec(sp, n+1) = obj }
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
    fn refer_local(&mut self, n: isize) -> Object {
        unsafe { *self.fp.offset(n) }
    }

    #[inline(always)]
    fn jump(&self, pc: *const Op, offset: usize) -> *const Op{
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

    fn return_n(&mut self, n: isize, pc: &mut  *const Op) {
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
        // todo We don't have cl yet.
        // self.cl = index(sp, 1);
        self.dc = self.index(sp, 2);
        match self.index(sp, 3) {
            Object::OpPointer(next_pc) => {
                *pc = next_pc;
               // *pc = usize::try_from(next_pc).expect("pc it not a number");
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
            self.index_set(self.inc(sp, diff -i), 0, self.index(sp, 1));
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

    //fn precompiled_lib(&mut self) -> &[Op] {

//        &ops
    //}
}

#[cfg(test)]
pub mod tests {

    use crate::objects::{Procedure, SString};

    use super::*;

    #[test]
    fn test_vm_define() {
        let mut vm = Vm::new();
        let ops = [
            Op::Constant(Object::Number(9)),
            Op::DefineGlobal(vm.gc.intern("a")),
            Op::ReferGlobal(vm.gc.intern("a")),
            Op::Halt,
        ];
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(&ops as  *const Op, ops.len());
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

    pub static SIZE_OF_PAIR: usize = std::mem::size_of::<Pair>(); // 56
    pub static SIZE_OF_CLOSURE: usize = std::mem::size_of::<Closure>(); // 88
    pub static SIZE_OF_PROCEDURE: usize = std::mem::size_of::<Procedure>(); // 56
    pub static SIZE_OF_SYMBOL: usize = std::mem::size_of::<Symbol>(); // 48
    pub static SIZE_OF_STRING: usize = std::mem::size_of::<SString>(); // 48
    // Base closure + procedure as free variable
    static SIZE_OF_MIN_VM: usize =
        SIZE_OF_CLOSURE /* base display closure */
        + (SIZE_OF_PROCEDURE * 623) /* free variables */ 
        + SIZE_OF_CLOSURE + SIZE_OF_SYMBOL; /* baselib name and closure of map1 */

    fn test_ops_with_size(vm: &mut Vm, ops: Vec<Op>, expected: Object, expected_heap_diff: usize) {
        //let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(&ops[0] as *const Op, ops.len());
        // Remove reference to ret.
        vm.ac = Object::Unspecified;
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(
            after_size,
            SIZE_OF_MIN_VM + expected_heap_diff
        );
        assert_eq!(ret, expected);
    }

    fn test_ops_with_size_as_str(
        vm: &mut Vm,
        ops: Vec<Op>,
        expected: &str,
        expected_heap_diff: usize,
    ) {
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(&ops[0] as *const Op, ops.len());
        // Remove reference to ret.
        // todo this shouuld be done in run.
        vm.ac = Object::Unspecified;
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        println!("before size={} after_size={}", before_size, after_size);
        assert_eq!(ret.to_string(), expected);
    //pub static SIZE_OF_SYMBOL: usize = std::mem::size_of::<Symbol>(); // 48
        println!("SIZE_OF_CLOSURE={} SIZE_OF_SYMBOL={} SIZE_OF_PAIR={} SIZE_OF_MIN_VM={}", SIZE_OF_CLOSURE, SIZE_OF_SYMBOL, SIZE_OF_PAIR, SIZE_OF_MIN_VM);
        assert_eq!(
            after_size,
            SIZE_OF_MIN_VM + expected_heap_diff
        );
    }

    // Custom hand written tests.
    #[test]
    fn test_symbol_intern() {
        let mut gc = Gc::new();

        let symbol = gc.intern("foo");
        let symbol2 = gc.intern("foo");
        assert_eq!(symbol.pointer, symbol2.pointer);
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
        ops.push(Op::Halt);        
        let before_size = vm.gc.bytes_allocated();
        vm.run(&ops[..][0] as *const Op, ops.len());
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM + SIZE_OF_PAIR);
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
        test_ops_with_size(&mut vm, ops,  Object::Number(3), 0);
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
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL);
    }

    #[test]
    fn test_test20() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL);
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
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::Push,
            Op::Constant(Object::Symbol(vm.gc.intern("b"))),
            Op::Cons,
            Op::Halt,
        ];
        let a = Object::Symbol(vm.gc.intern("a"));
        let b = Object::Symbol(vm.gc.intern("b"));
        let pair = vm.gc.alloc(Pair::new(a, b));

        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(&ops[0], ops.len());
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
        match ret {
            Object::Pair(pair2) => {
                assert_eq!(pair.car, pair2.car);
                assert_eq!(pair.cdr, pair2.cdr);
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
            Op::Closure {
                size: 4,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 1,
            },
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
            Op::Closure {
                size: 16,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
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
            Op::Closure {
                size: 16,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
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
            Op::Closure {
                size: 16,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
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
            Op::Closure {
                size: 7,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
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

    // ((lambda (a) (set! a (lambda (i) (if (= i 20) i (a (+ i 1))))) (a 0)) '()) => 20
    #[test]
    fn test_test66() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(31),
            Op::Constant(Object::Nil),
            Op::Push,
            Op::Closure {
                size: 27,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Box(0),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 16,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(20)),
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
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::ReferLocal(0),
            Op::Indirect,
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
        test_ops_with_size(&mut vm, ops, Object::Number(20), 0);
    }

    // (define a 3) => 3
    #[test]
    fn test_test68() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::DefineGlobal(vm.gc.intern("a")),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), SIZE_OF_SYMBOL);
    }
    // (= 3 4) => #f
    #[test]
    fn test_test70() {
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

    // (= 3 3 3) => #t
    #[test]
    fn test_test71() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::BranchNotNumberEqual(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberEqual,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (= 3 4 5) => #f
    #[test]
    fn test_test72() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::BranchNotNumberEqual(5),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(5)),
            Op::NumberEqual,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (((lambda (a) (lambda () a)) 101)) => 101
    #[test]
    fn test_test73() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Frame(11),
            Op::Constant(Object::Number(101)),
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
        test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
    }

    // (((lambda (a) (lambda (b) (+ a b))) 101) 1) => 102
    #[test]
    fn test_test74() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(18),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Frame(14),
            Op::Constant(Object::Number(101)),
            Op::Push,
            Op::Closure {
                size: 10,
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
            Op::ReferFree(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Return(1),
            Op::Return(1),
            Op::Call(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
    }

    // (null? '()) => #t
    #[test]
    fn test_test75() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Nil), Op::NullP, Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (null? 3) => #f
    #[test]
    fn test_test76() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Number(3)), Op::NullP, Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (cons 1 2) => (1 . 2)
    #[test]
    fn test_test77_modified() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Cons,
            Op::Halt,
        ];
        let pair = vm.gc.alloc(Pair::new(Object::Number(1), Object::Number(2)));
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(&ops[0] as *const Op, ops.len());
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
        match ret {
            Object::Pair(pair2) => {
                assert_eq!(pair.car, pair2.car);
                assert_eq!(pair.cdr, pair2.cdr);
            }
            _ => {
                panic!("not a pair");
            }
        }
    }

    // (cons 1 (cons 2 '())) => (1 2)
    #[test]
    fn test_test78() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Nil),
            Op::Cons,
            Op::Cons,
            Op::Halt,
        ];
        // (2 . nil)
        let pair1 = Object::Pair(vm.gc.alloc(Pair::new(Object::Number(2), Object::Nil)));
        // (1 2)
        let pair2 = vm.gc.alloc(Pair::new(Object::Number(1), pair1));
        let before_size = vm.gc.bytes_allocated();
        let ret = vm.run(&ops[0], ops.len());
        vm.mark_and_sweep();
        let after_size = vm.gc.bytes_allocated();
        assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
        match ret {
            Object::Pair(pair3) => {
                assert_eq!(pair2.car, pair3.car);
            }
            _ => {
                panic!("not a pair");
            }
        }
    }

    // (begin 1 2 3) => 3
    #[test]
    fn test_test79() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Constant(Object::Number(2)),
            Op::Constant(Object::Number(3)),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda () (set! a 4) a)) => 4
    #[test]
    fn test_test80() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(7),
            Op::Closure {
                size: 5,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(4)),
            Op::AssignGlobal(vm.gc.intern("a")),
            Op::ReferGlobal(vm.gc.intern("a")),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), SIZE_OF_SYMBOL);
    }

    // ((lambda () ((lambda () 3)))) => 3
    #[test]
    fn test_test81() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(8),
            Op::Closure {
                size: 6,
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
            Op::Constant(Object::Number(3)),
            Op::Return(0),
            Op::TailCall(0, 0),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda () ((lambda (x) x) 3))) => 3
    #[test]
    fn test_test82() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(10),
            Op::Closure {
                size: 8,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::TailCall(1, 0),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda (y) ((lambda (x) x) 3)) 4) => 3
    #[test]
    fn test_test83() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(12),
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 8,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::TailCall(1, 1),
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda () (let1 a 1 ((lambda () 3))))) => 3
    #[test]
    fn test_test84() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Closure {
                size: 11,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::LetFrame(1),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Return(0),
            Op::TailCall(0, 3),
            Op::Leave(1),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda () (let1 b 2 (let1 a 1 ((lambda () 3)))))) => 3
    #[test]
    fn test_test85() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(18),
            Op::Closure {
                size: 16,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::LetFrame(2),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(1),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Return(0),
            Op::TailCall(0, 6),
            Op::Leave(1),
            Op::Leave(1),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda () (if 3 ((lambda () 3))))) => 3
    #[test]
    fn test_test86() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(12),
            Op::Closure {
                size: 10,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Test(6),
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Return(0),
            Op::TailCall(0, 0),
            Op::Return(0),
            Op::Undef,
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda () (if ((lambda () 3)) 4 5))) => 4
    #[test]
    fn test_test87() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Closure {
                size: 11,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
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
            Op::Test(3),
            Op::Constant(Object::Number(4)),
            Op::Return(0),
            Op::Constant(Object::Number(5)),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
    }

    // (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b)))))) => 10
    #[test]
    fn test_test88() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 41,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(10)),
            Op::BranchNotNumberEqual(3),
            Op::ReferLocal(0),
            Op::Return(1),
            Op::LetFrame(5),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferFree(0),
            Op::Push,
            Op::Display(2),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(4),
            Op::ReferFree(1),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferFree(0),
            Op::Push,
            Op::Display(3),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(2),
            Op::Push,
            Op::ReferFree(1),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Push,
            Op::ReferFree(0),
            Op::Indirect,
            Op::TailCall(1, 7),
            Op::Leave(1),
            Op::Leave(1),
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

    // (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b)))))) => 10
    #[test]
    fn test_test89() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 41,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(10)),
            Op::BranchNotNumberEqual(3),
            Op::ReferLocal(0),
            Op::Return(1),
            Op::LetFrame(5),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferFree(0),
            Op::Push,
            Op::Display(2),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(4),
            Op::ReferFree(1),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferFree(0),
            Op::Push,
            Op::Display(3),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Enter(1),
            Op::ReferFree(2),
            Op::Push,
            Op::ReferFree(1),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(0),
            Op::NumberAdd,
            Op::Push,
            Op::ReferFree(0),
            Op::Indirect,
            Op::TailCall(1, 7),
            Op::Leave(1),
            Op::Leave(1),
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

    // ((lambda () (define d (lambda (x y z) (+ x y z))) (d 1 2 3))) => 6
    #[test]
    fn test_test90() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(29),
            Op::Closure {
                size: 27,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::LetFrame(3),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::Closure {
                size: 9,
                arg_len: 3,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(2),
            Op::NumberAdd,
            Op::Return(3),
            Op::AssignLocal(0),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferLocal(0),
            Op::Indirect,
            Op::TailCall(3, 3),
            Op::Leave(1),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(6), 0);
    }

    // ((lambda () (define b (lambda () 3)) (b))) => 3
    #[test]
    fn test_test91() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(17),
            Op::Closure {
                size: 15,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::LetFrame(0),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::Closure {
                size: 3,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::Constant(Object::Number(3)),
            Op::Return(0),
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::TailCall(0, 3),
            Op::Leave(1),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // ((lambda a a) 1 2 3) => (1 2 3)
    #[test]
    fn test_test92() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
    }

    // ((lambda (a . b) b) 1 2 3) => (2 3)
    #[test]
    fn test_test93() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 2,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(1),
            Op::Return(2),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(2 3)", 0);
    }

    // ((lambda (a . b) b) 1 2 3 4) => (2 3 4)
    #[test]
    fn test_test94() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 2,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(1),
            Op::Return(2),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(2 3 4)", 0);
    }

    // ((lambda (a b . c) c) 1 2 3 4) => (3 4)
    #[test]
    fn test_test95() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 3,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(2),
            Op::Return(3),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(3 4)", 0);
    }

    // ((lambda (a b c . d) d) 1 2 3 4) => (4)
    #[test]
    fn test_test96() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 4,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(3),
            Op::Return(4),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(4)", 0);
    }

    // ((lambda (a b c . d) d) 1 2 3) => ()
    #[test]
    fn test_test97() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 4,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(3),
            Op::Return(4),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Nil, 0);
    }

    // ((lambda a a)) => ()
    #[test]
    fn test_test98() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Nil, 0);
    }

    // ((lambda a a) 1) => (1)
    #[test]
    fn test_test99() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(7),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1)", 0);
    }

    // (when #t 1 2 34) => 34
    #[test]
    fn test_test100() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::True),
            Op::Test(5),
            Op::Constant(Object::Number(1)),
            Op::Constant(Object::Number(2)),
            Op::Constant(Object::Number(34)),
            Op::LocalJmp(2),
            Op::Undef,
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(34), 0);
    }

    // (not 3) => #f
    #[test]
    fn test_test101() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Number(3)), Op::Not, Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (unless #f 1 2 48) => 48
    #[test]
    fn test_test102() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Undef,
            Op::LocalJmp(4),
            Op::Constant(Object::Number(1)),
            Op::Constant(Object::Number(2)),
            Op::Constant(Object::Number(48)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(48), 0);
    }

    // (and 3 4 5) => 5
    #[test]
    fn test_test103() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Test(4),
            Op::Constant(Object::Number(4)),
            Op::Test(2),
            Op::Constant(Object::Number(5)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
    }

    // (let1 a 0 (and (set! a (+ a 1))) a) => 1
    #[test]
    fn test_test104() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(0)),
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
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    // (let1 a 0 (or (set! a (+ a 1))) a) => 1
    #[test]
    fn test_test105() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(0)),
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
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    // (and 3 #f 5) => #f
    #[test]
    fn test_test106() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Test(4),
            Op::Constant(Object::False),
            Op::Test(2),
            Op::Constant(Object::Number(5)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (or 3 4 5) => 3
    #[test]
    fn test_test107() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Test(2),
            Op::LocalJmp(5),
            Op::Constant(Object::Number(4)),
            Op::Test(2),
            Op::LocalJmp(2),
            Op::Constant(Object::Number(5)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // (or #f #f #f) => #f
    #[test]
    fn test_test108() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(2),
            Op::LocalJmp(3),
            Op::Constant(Object::False),
            Op::Test(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (> 4 3) => #t
    #[test]
    fn test_test109() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberGt,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (> 4 3 2) => #t
    #[test]
    fn test_test110() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::BranchNotGt(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::NumberGt,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (> 4 3 1 2) => #f
    #[test]
    fn test_test111() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::BranchNotGt(9),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::BranchNotGt(5),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::NumberGt,
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (>= 3 3 3) => #t
    #[test]
    fn test_test112() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::BranchNotGe(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberGe,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (>= 4 3 3) => #t
    #[test]
    fn test_test113() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::BranchNotGe(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberGe,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (>= 4 3) => #t
    #[test]
    fn test_test114() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberGe,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (< 1 2) => #t
    #[test]
    fn test_test115() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::NumberLt,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (< 1 2 3) => #t
    #[test]
    fn test_test116() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::BranchNotLt(5),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberLt,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (< 1 5 3) => #f
    #[test]
    fn test_test117() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(5)),
            Op::BranchNotLt(5),
            Op::Constant(Object::Number(5)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberLt,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (<= 1 2) => #t
    #[test]
    fn test_test118() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::NumberLe,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (<= 1 2 3) => #t
    #[test]
    fn test_test119() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::BranchNotLe(5),
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberLe,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (<= 1 3 3) => #t
    #[test]
    fn test_test120() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::BranchNotLe(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberLe,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (<= 1 5 3) => #f
    #[test]
    fn test_test121() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(5)),
            Op::BranchNotLe(5),
            Op::Constant(Object::Number(5)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::NumberLe,
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (eq? #t #t) => #t
    #[test]
    fn test_test122() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::True),
            Op::Push,
            Op::Constant(Object::True),
            Op::Eq,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (eq? #t #f) => #f
    #[test]
    fn test_test123() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::True),
            Op::Push,
            Op::Constant(Object::False),
            Op::Eq,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (eq? 'a 'a) => #t
    #[test]
    fn test_test124() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::Push,
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::Eq,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, SIZE_OF_SYMBOL);
    }

    // (eq? 'a 'b) => #f
    #[test]
    fn test_test125() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::Push,
            Op::Constant(Object::Symbol(vm.gc.intern("b"))),
            Op::Eq,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL*2);
    }

    // (pair? (cons 1 2)) => #t
    #[test]
    fn test_test126() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Cons,
            Op::PairP,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, 0);
    }

    // (pair? 3) => #f
    #[test]
    fn test_test127() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Number(3)), Op::PairP, Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (symbol? 'a) => #t
    #[test]
    fn test_test128() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::SymbolP,
            Op::Halt,
        ];
        test_ops_with_size(&mut vm, ops, Object::True, SIZE_OF_SYMBOL);
    }

    // (symbol? 3) => #f
    #[test]
    fn test_test129() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Number(3)), Op::SymbolP, Op::Halt];
        test_ops_with_size(&mut vm, ops, Object::False, 0);
    }

    // (cond (#f 1) (#t 3)) => 3
    #[test]
    fn test_test130() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::Number(1)),
            Op::LocalJmp(6),
            Op::Constant(Object::True),
            Op::Test(3),
            Op::Constant(Object::Number(3)),
            Op::LocalJmp(2),
            Op::Undef,
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // (cond (#f 1) (#f 2) (else 3)) => 3
    #[test]
    fn test_test131() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::Number(1)),
            Op::LocalJmp(6),
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::Number(2)),
            Op::LocalJmp(2),
            Op::Constant(Object::Number(3)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // (cond (#t 3) (#f 2) (else 1)) => 3
    #[test]
    fn test_test132() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::True),
            Op::Test(3),
            Op::Constant(Object::Number(3)),
            Op::LocalJmp(6),
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::Number(2)),
            Op::LocalJmp(2),
            Op::Constant(Object::Number(1)),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // (cond ((cons 1 2) => car) (#f 2) (else 3)) => 1
    #[test]
    fn test_test133() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::ReferFree(3),
            Op::Push,
            Op::Display(1),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Cons,
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Test(7),
            Op::Frame(11),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
            Op::LocalJmp(6),
            Op::Constant(Object::False),
            Op::Test(3),
            Op::Constant(Object::Number(2)),
            Op::LocalJmp(2),
            Op::Constant(Object::Number(3)),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
    }

    // (let ((a 0)) `(,a 4 5)) => (0 4 5)
    #[test]
    fn test_test134() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(0 4 5)", 0);
    }

    // (let ((a '(1 2 3))) `(,a 4 5)) => ((1 2 3) 4 5)
    #[test]
    fn test_test135() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "((1 2 3) 4 5)", 0);
    }

    // (let ((a '(1 2 3))) `(,@a 4 5)) => (1 2 3 4 5)
    #[test]
    fn test_test136() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
            Op::Append2,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4 5)", 0);
    }

    // (let ((name 'a)) `(list ,name ',name)) => (list a 'a)
    #[test]
    fn test_test137() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(6),
            Op::Constant(Object::Symbol(vm.gc.intern("a"))),
            Op::Push,
            Op::Enter(1),
            Op::Constant(Object::Symbol(vm.gc.intern("list"))),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Symbol(vm.gc.intern("quote"))),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Nil),
            Op::Cons,
            Op::Cons,
            Op::Push,
            Op::Constant(Object::Nil),
            Op::Cons,
            Op::Cons,
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(list a 'a)", SIZE_OF_SYMBOL * 3);
    }

    // `(list ,(+ 1 2) 4) => (list 3 4)
    #[test]
    fn test_test138() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Symbol(vm.gc.intern("list"))),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::NumberAdd,
            Op::Push,
            Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
            Op::Cons,
            Op::Cons,
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(list 3 4)", SIZE_OF_SYMBOL);
    }

    // (let ((a '(1 2 3))) `(1 . ,a)) => (1 1 2 3)
    #[test]
    fn test_test139() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::ReferLocal(0),
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 1 2 3)", 0);
    }

    // (let ((a '(1 2 3))) `,a) => (1 2 3)
    #[test]
    fn test_test140() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
    }

    // (let ((a '(1 2 3))) `(,@a)) => (1 2 3)
    #[test]
    fn test_test141() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
    }

    // (let ((a '(1 2 3))) `(0 ,@a)) => (0 1 2 3)
    #[test]
    fn test_test142() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::ReferLocal(0),
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(0 1 2 3)", 0);
    }

    // (let ((a '(1 2 3))) `(0 ,a 4)) => (0 (1 2 3) 4)
    #[test]
    fn test_test143() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
            Op::Cons,
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(0 (1 2 3) 4)", 0);
    }

    // (let ((a '(1 2 3))) `(,@a 4)) => (1 2 3 4)
    #[test]
    fn test_test144() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
            Op::Append2,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", 0);
    }

    // (let ((a '(1 2 3))) `((,@a) 4)) => ((1 2 3) 4)
    #[test]
    fn test_test145() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "((1 2 3) 4)", 0);
    }

    // (let ((a '(1 2 3))) `((,a) 4)) => (((1 2 3)) 4)
    #[test]
    fn test_test146() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(
                vm.gc
                    .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
            ),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Nil),
            Op::Cons,
            Op::Push,
            Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
            Op::Cons,
            Op::Leave(1),
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(((1 2 3)) 4)", 0);
    }

    // `b => b
    #[test]
    fn test_test147_modified() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Symbol(vm.gc.intern("b"))), Op::Halt];
        let obj = vm.gc.symbol_intern("b");
        test_ops_with_size(&mut vm, ops, obj, SIZE_OF_SYMBOL);
    }

    // (list 1 2 3) => (1 2 3)
    #[test]
    fn test_test148() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(9),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferFree(89),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
    }

    // (aif (+ 1 2) it #f) => 3
    #[test]
    fn test_test149() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(2),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::NumberAdd,
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Test(2),
            Op::ReferLocal(0),
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // (string-length abc) => 3
    #[test]
    fn test_test150() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(vm.gc.new_string("abc")),
            Op::Push,
            Op::ReferFree(19),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // (string-length あいう) => 3
    #[test]
    fn test_test151() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(vm.gc.new_string("あいう")),
            Op::Push,
            Op::ReferFree(19),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
    }

    // (string->symbol abc) => abc
    #[test]
    fn test_test152_modified() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(vm.gc.new_string("abc")),
            Op::Push,
            Op::ReferFree(20),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        let s = vm.gc.symbol_intern("abc");
        test_ops_with_size(&mut vm, ops, s, SIZE_OF_SYMBOL);
    }

    // (number->string 123) => 123
    #[test]
    fn test_test153() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(Object::Number(123)),
            Op::Push,
            Op::ReferFree(25),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "\"123\"", 0);
    }

    // (begin (define (proc1 . a) a) (proc1 1 2 3 4)) => (1 2 3 4)
    #[test]
    fn test_test154_modified() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::DefineGlobal(vm.gc.intern("proc1")),
            Op::Frame(11),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::ReferGlobal(vm.gc.intern("proc1")),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        // This register a closure globally and increase size.        
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", SIZE_OF_CLOSURE + SIZE_OF_SYMBOL);
    }

    // ((lambda (a . b) b) 1 2 3) => (2 3)
    #[test]
    fn test_test155() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(11),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 2,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(1),
            Op::Return(2),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(2 3)", 0);
    }

    // ((lambda (a . b) a) 1 2 3 4 5) => 1
    #[test]
    fn test_test156() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(15),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(5)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 2,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(2),
            Op::Call(5),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(1);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // ((lambda (a . b) b) 1 2 3 4 5) => (2 3 4 5)
    #[test]
    fn test_test157() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(15),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Constant(Object::Number(5)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 2,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(1),
            Op::Return(2),
            Op::Call(5),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(2 3 4 5)", 0);
    }

    // ((lambda (a b c d . e) e) 1 2 3 4) => ()
    #[test]
    fn test_test158() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 5,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(4),
            Op::Return(5),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Nil;
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // ((lambda (a b c d . e) a) 1 2 3 4) => 1
    #[test]
    fn test_test159() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 5,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(5),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(1);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // ((lambda (a b c d . e) b) 1 2 3 4) => 2
    #[test]
    fn test_test160() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 5,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(1),
            Op::Return(5),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(2);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // ((lambda (a b c d . e) c) 1 2 3 4) => 3
    #[test]
    fn test_test161() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Number(4)),
            Op::Push,
            Op::Closure {
                size: 3,
                arg_len: 5,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(2),
            Op::Return(5),
            Op::Call(4),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(3);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (append '(1 2) '(3 4)) => (1 2 3 4)
    #[test]
    fn test_test163() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
            Op::Push,
            Op::Constant(vm.gc.list2(Object::Number(3), Object::Number(4))),
            Op::Append2,
            Op::Halt,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", 0);
    }

    // (append) => ()
    #[test]
    fn test_test164() {
        let mut vm = Vm::new();
        let ops = vec![Op::Constant(Object::Nil), Op::Halt];
        let expected = Object::Nil;
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (begin (define x 3) x) => 3
    #[test]
    fn test_test165_modified() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::DefineGlobal(vm.gc.intern("x")),
            Op::ReferGlobal(vm.gc.intern("x")),
            Op::Halt,
        ];
        let expected = Object::Number(3);
        test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
    }

    // (begin (define (hoge . a) a) (hoge 1 2 3)) => (1 2 3)
    #[test]
    fn test_test166() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::DefineGlobal(vm.gc.intern("hoge")),
            Op::Frame(9),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferGlobal(vm.gc.intern("hoge")),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        // This register a closure globally and increase size.
        test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", SIZE_OF_CLOSURE+SIZE_OF_SYMBOL);
    }

    // (begin (define (hige a . b) b) (hige 1 2 3)) => (2 3)
    #[test]
    fn test_test167() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Closure {
                size: 3,
                arg_len: 2,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(1),
            Op::Return(2),
            Op::DefineGlobal(vm.gc.intern("hige")),
            Op::Frame(9),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferGlobal(vm.gc.intern("hige")),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        // This register a closure globally and increase size.        
        test_ops_with_size_as_str(&mut vm, ops, "(2 3)", SIZE_OF_CLOSURE+SIZE_OF_SYMBOL);
    }

    // (apply (lambda a a) '(3 2)) => (3 2)
    #[test]
    fn test_test168() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(9),
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Push,
            Op::Constant(vm.gc.list2(Object::Number(3), Object::Number(2))),
            Op::Push,
            Op::ReferFree(152),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(3 2)", 0);
    }

    // (let ((a 3)) 3 2 1) => 1
    #[test]
    fn test_test170() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::Constant(Object::Number(3)),
            Op::Constant(Object::Number(2)),
            Op::Constant(Object::Number(1)),
            Op::Leave(1),
            Op::Halt,
        ];
        let expected = Object::Number(1);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (make-string 3) =>
    #[test]
    fn test_test171() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::ReferFree(17),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "\"   \"", 0);
    }

    // (make-string 3 #\c) => "ccc"
    #[test]
    fn test_test172() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(7),
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Char('c')),
            Op::Push,
            Op::ReferFree(17),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "\"ccc\"", 0);
    }

    // (apply car '((3))) => 3
    #[test]
    fn test_test173_modified() {
        let mut vm = Vm::new();
        let list = vm.gc.list1(Object::Number(3));
        let ops = vec![
            Op::Frame(7),
            Op::ReferFree(3),
            Op::Push,
            Op::Constant(vm.gc.list1(list)),
            Op::Push,
            Op::ReferFree(152),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
        ];
        let expected = Object::Number(3);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (apply (lambda (a) a) '(3)) => 3
    #[test]
    fn test_test174() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(9),
            Op::Closure {
                size: 3,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Return(1),
            Op::Push,
            Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
            Op::Push,
            Op::ReferFree(152),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(3);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (apply (lambda (a b) (+ a b)) '(5 2)) => 7
    #[test]
    fn test_test175() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(12),
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
            Op::Push,
            Op::Constant(vm.gc.list2(Object::Number(5), Object::Number(2))),
            Op::Push,
            Op::ReferFree(152),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(7);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (apply (lambda (a b c) (+ a b c)) '(5 2 1)) => 8
    #[test]
    fn test_test176() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(15),
            Op::Closure {
                size: 9,
                arg_len: 3,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::NumberAdd,
            Op::Push,
            Op::ReferLocal(2),
            Op::NumberAdd,
            Op::Return(3),
            Op::Push,
            Op::Constant(
                vm.gc
                    .list3(Object::Number(5), Object::Number(2), Object::Number(1)),
            ),
            Op::Push,
            Op::ReferFree(152),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(8);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (apply (lambda (a) (car a)) '((3))) => 3
    #[test]
    fn test_test177_modified() {
        let mut vm = Vm::new();
        let list = vm.gc.list1(Object::Number(3));
        let ops = vec![
            Op::Frame(10),
            Op::Closure {
                size: 4,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Car,
            Op::Return(1),
            Op::Push,
            Op::Constant(vm.gc.list1(list)),
            Op::Push,
            Op::ReferFree(152),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(3);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (apply (lambda (a . b) (+ a (car b))) '(1 2)) => 3
    #[test]
    fn test_test178() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(13),
            Op::Closure {
                size: 7,
                arg_len: 2,
                is_optional_arg: true,
                num_free_vars: 0,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(1),
            Op::Car,
            Op::NumberAdd,
            Op::Return(2),
            Op::Push,
            Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
            Op::Push,
            Op::ReferFree(152),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(3);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (string-append "12" "345" "6") => "123456"
    #[test]
    fn test_test179() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(9),
            Op::Constant(vm.gc.new_string("12")),
            Op::Push,
            Op::Constant(vm.gc.new_string("345")),
            Op::Push,
            Op::Constant(vm.gc.new_string("6")),
            Op::Push,
            Op::ReferFree(22),
            Op::Call(3),
            Op::Halt,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "\"123456\"", 0);
    }

    // (string? "hige") => #t
    #[test]
    fn test_test181() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(vm.gc.new_string("hige")),
            Op::Push,
            Op::ReferFree(31),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        let expected = Object::True;
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p)) => (1 . 3)
    #[test]
    fn test_test184() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(22),
            Op::Closure {
                size: 20,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::LetFrame(2),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Cons,
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::SetCdr,
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Leave(1),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(1 . 3)", 0);
    }

    // ((lambda () (define q (cons 1 2)) (set-car! q 3) q)) => (3 . 2)
    #[test]
    fn test_test185() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(22),
            Op::Closure {
                size: 20,
                arg_len: 0,
                is_optional_arg: false,
                num_free_vars: 0,
            },
            Op::LetFrame(2),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::Constant(Object::Number(1)),
            Op::Push,
            Op::Constant(Object::Number(2)),
            Op::Cons,
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Push,
            Op::Constant(Object::Number(3)),
            Op::SetCar,
            Op::ReferLocal(0),
            Op::Indirect,
            Op::Leave(1),
            Op::Return(0),
            Op::Call(0),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(3 . 2)", 0);
    }

    // (begin #f #t) => #t
    #[test]
    fn test_test186() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::False),
            Op::Constant(Object::True),
            Op::Halt,
        ];
        let expected = Object::True;
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (vector-length (make-vector 3)) => 3
    #[test]
    fn test_test187() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Object::Number(3)),
            Op::Push,
            Op::Constant(Object::Nil),
            Op::MakeVector,
            Op::VectorLength,
            Op::Halt,
        ];
        let expected = Object::Number(3);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (let loop ((i 0)) (if (= i 100) (+ i 1) (loop (+ i 1)))) => 101
    #[test]
    fn test_test188() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(1),
            Op::Undef,
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::Closure {
                size: 19,
                arg_len: 1,
                is_optional_arg: false,
                num_free_vars: 1,
            },
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(100)),
            Op::BranchNotNumberEqual(6),
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(Object::Number(1)),
            Op::NumberAdd,
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
        let expected = Object::Number(101);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (let ((a 0)) (cond (#t (set! a (+ a 1)) (set! a (+ a 1)) a))) => 2
    #[test]
    fn test_test189() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::Constant(Object::Number(0)),
            Op::Push,
            Op::Box(0),
            Op::Enter(1),
            Op::Constant(Object::True),
            Op::Test(16),
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
            Op::AssignLocal(0),
            Op::ReferLocal(0),
            Op::Indirect,
            Op::LocalJmp(2),
            Op::Undef,
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::Number(2);
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (char? #\あ) => #t
    #[test]
    fn test_test190() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(Object::Char('あ')),
            Op::Push,
            Op::ReferFree(53),
            Op::Call(1),
            Op::Halt,
            Op::Nop,
        ];
        let expected = Object::True;
        test_ops_with_size(&mut vm, ops, expected, 0);
    }

    // (eq? (list 'a) (list 'a)) => #f
    #[test]
    fn test_test191() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Frame(5),
            Op::Constant(vm.gc.symbol_intern("a")),
            Op::Push,
            Op::ReferFree(89),
            Op::Call(1),
            Op::Push,
            Op::Frame(5),
            Op::Constant(vm.gc.symbol_intern("a")),
            Op::Push,
            Op::ReferFree(89),
            Op::Call(1),
            Op::Eq,
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        let expected = Object::False;
        test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
    }

    // (let ((x (list 'a))) (eq? x x)) => #t
    #[test]
    fn test_test192() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::LetFrame(3),
            Op::ReferFree(89),
            Op::Push,
            Op::Display(1),
            Op::Frame(5),
            Op::Constant(vm.gc.symbol_intern("a")),
            Op::Push,
            Op::ReferFree(0),
            Op::Call(1),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Push,
            Op::ReferLocal(0),
            Op::Eq,
            Op::Leave(1),
            Op::Halt,
            Op::Nop,
        ];
        let expected = Object::True;
        test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
    }



    // (map1 (lambda (x) 2) '(1)) => (2)
    #[test]
    fn test_test193_modified0() {
        let mut vm = Vm::new();        
        let ops = vec![
            Op::Frame(9),
            // size was originally 3
            Op::Closure {size: 3, arg_len: 1, is_optional_arg: false, num_free_vars: 0},
            Op::Constant(Object::Number(2)),
            Op::Return(1),
            Op::Push,
            Op::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
            Op::Push,
            Op::ReferGlobal(vm.gc.intern("map1")),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];

        test_ops_with_size_as_str(&mut vm, ops, "(2)", 0);


    }



    // (map1 (lambda (s) (string-append s "123")) '("ABC" "DEF")) => ("ABC123" "DEF123")
    #[test]
    fn test_test193_modified() {  

        let mut vm = Vm::new();        
        let abc = vm.gc.new_string("ABC");
        let def = vm.gc.new_string("DEF");
        let ops = vec![
            Op::Frame(16),
            Op::ReferFree(22),
            Op::Push,
            Op::Closure {size: 8, arg_len: 1, is_optional_arg: false, num_free_vars: 1},
            Op::ReferLocal(0),
            Op::Push,
            Op::Constant(vm.gc.new_string("123")),
            Op::Push,
            Op::ReferFree(0),
            Op::TailCall(2, 1),
            Op::Return(1),
            Op::Push,
            Op::Constant(vm.gc.list2(abc, def)),
            Op::Push,
            Op::ReferGlobal(vm.gc.intern("map1")),
            Op::Call(2),
            Op::Halt,
            Op::Nop,
            Op::Nop,
        ];
        test_ops_with_size_as_str(&mut vm, ops, "(\"ABC123\" \"DEF123\")", 0);

    }
    
}

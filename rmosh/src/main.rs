// GC implementation based on Loxido written by Manuel Cerón.
// See https://github.com/ceronman/loxido.
use std::alloc;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ptr::{null_mut, NonNull};
use std::{ops::Deref, ops::DerefMut, sync::atomic::AtomicUsize, usize};

struct GlobalAllocator {
    bytes_allocated: AtomicUsize,
}

impl GlobalAllocator {
    fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
            .load(std::sync::atomic::Ordering::Relaxed)
    }
}

unsafe impl alloc::GlobalAlloc for GlobalAllocator {
    unsafe fn alloc(&self, layout: alloc::Layout) -> *mut u8 {
        self.bytes_allocated
            .fetch_add(layout.size(), std::sync::atomic::Ordering::Relaxed);
        mimalloc::MiMalloc.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: alloc::Layout) {
        mimalloc::MiMalloc.dealloc(ptr, layout);
        self.bytes_allocated
            .fetch_sub(layout.size(), std::sync::atomic::Ordering::Relaxed);
    }
}

#[global_allocator]
static GLOBAL: GlobalAllocator = GlobalAllocator {
    bytes_allocated: AtomicUsize::new(0),
};

#[derive(Debug)]
pub struct Pair {
    pub header: GcHeader,
    pub first: Value,
    pub second: Value,
}

impl Pair {
    pub fn new(first: Value, second: Value) -> Self {
        Pair {
            header: GcHeader::new(ObjectType::Pair),
            first: first,
            second: second,
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.first, self.second)
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub header: GcHeader,
    pub string: String,
}

impl Symbol {
    pub fn new(s: String) -> Self {
        Symbol {
            header: GcHeader::new(ObjectType::Symbol),
            string: s,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.string)
    }
}

#[derive(Debug)]
pub struct Closure {
    pub header: GcHeader,
    pub pc: usize,
    size: usize,
    arg_len: isize,
    is_optional_arg: bool,
    pub free_vars: Vec<Value>,
    pub prev: Value,
}

impl Closure {
    pub fn new(
        pc: usize,
        size: usize,
        arg_len: isize,
        is_optional_arg: bool,
        free_vars: Vec<Value>,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            pc: pc,
            size: size,
            arg_len: arg_len,
            is_optional_arg: is_optional_arg,
            free_vars: free_vars,
            prev: Value::Undef,
        }
    }

    pub fn refer_free(&mut self, n: usize) -> Value {
        self.free_vars[n]
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

#[derive(Debug)]
pub struct GcRef<T> {
    pointer: NonNull<T>,
}

impl<T> Display for GcRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GcRef<T>")
    }
}

impl<T> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.pointer == other.pointer
    }
}
impl<T> Eq for GcRef<T> {}

impl<T> Hash for GcRef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pointer.hash(state);
    }
}

impl<T> Copy for GcRef<T> {}

impl<T> Clone for GcRef<T> {
    fn clone(&self) -> GcRef<T> {
        *self
    }
}

impl<T> Deref for GcRef<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.pointer.as_ref() }
    }
}

impl<T> DerefMut for GcRef<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.pointer.as_mut() }
    }
}

pub struct Gc {
    next_gc: usize,
    first: Option<NonNull<GcHeader>>,
    grey_stack: Vec<NonNull<GcHeader>>,
    symbols: HashMap<String, GcRef<Symbol>>,
}

impl Gc {
    const HEAP_GROW_FACTOR: usize = 2;

    pub fn new() -> Self {
        Gc {
            next_gc: 1024 * 1024,
            first: None,
            grey_stack: Vec::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn alloc<T: Display + 'static>(&mut self, object: T) -> GcRef<T> {
        unsafe {
            let boxed = Box::new(object);
            let pointer = NonNull::new_unchecked(Box::into_raw(boxed));
            let mut header: NonNull<GcHeader> = mem::transmute(pointer.as_ref());
            header.as_mut().next = self.first.take();
            self.first = Some(header);

            GcRef { pointer }
        }
    }

    pub fn intern(&mut self, s: String) -> GcRef<Symbol> {
        match self.symbols.get(s.as_str()) {
            Some(&symbol) => symbol,
            None => {
                let symbol = self.alloc(Symbol::new(s.to_owned()));
                self.symbols.insert(s, symbol);
                symbol
            }
        }
    }

    // Top level mark.
    // This mark root objects only and push them to grey_stack.
    pub fn mark_value(&mut self, value: Value) {
        match value {
            Value::Number(_) => {}
            Value::VMStackPointer(_) => {}
            Value::False => {}
            Value::Undef => {}
            Value::Closure(closure) => {
                self.mark_object(closure);
            }
            Value::Symbol(symbol) => {
                self.mark_object(symbol);
            }
            Value::Pair(pair) => {
                self.mark_object(pair);
            }
        }
    }

    pub fn mark_object<T: 'static>(&mut self, mut reference: GcRef<T>) {
        unsafe {
            let mut header: NonNull<GcHeader> = mem::transmute(reference.pointer.as_mut());
            header.as_mut().marked = true;
            self.grey_stack.push(header);
        }
    }

    fn trace_references(&mut self) {
        while let Some(pointer) = self.grey_stack.pop() {
            self.trace_pointer(pointer);
        }
    }

    fn trace_value(&mut self, value: Value) {
        match value {
            Value::Number(_) => {}
            Value::False => {}
            Value::Undef => {}
            Value::VMStackPointer(_) => {}
            Value::Closure(closure) => {
                for var in &closure.free_vars {
                    self.trace_value(*var);
                }
                self.trace_value(closure.prev);
            }
            Value::Symbol(pair) => {
                self.trace_object(pair);
            }
            Value::Pair(pair) => {
                self.trace_object(pair);
            }
        }
    }

    pub fn trace_object<T: 'static>(&mut self, mut reference: GcRef<T>) {
        unsafe {
            let header: NonNull<GcHeader> = mem::transmute(reference.pointer.as_mut());
            self.trace_pointer(header);
        }
    }

    fn trace_pointer(&mut self, pointer: NonNull<GcHeader>) {
        let object_type = unsafe { &pointer.as_ref().obj_type };
        #[cfg(feature = "debug_log_gc")]
        println!("blacken(adr:{:?})", pointer);

        match object_type {
            ObjectType::Symbol => {}
            ObjectType::Closure => {
                panic!("TODO");
            }
            ObjectType::Pair => {
                let pair: &Pair = unsafe { mem::transmute(pointer.as_ref()) };
                self.mark_value(pair.first);
                self.mark_value(pair.second);
                self.trace_value(pair.first);
                self.trace_value(pair.second);
            }
        }
    }

    pub fn collect_garbage(&mut self) {
        self.trace_references();
        self.sweep();
        self.next_gc = GLOBAL.bytes_allocated() * Gc::HEAP_GROW_FACTOR;
    }
    fn sweep(&mut self) {
        let mut previous: Option<NonNull<GcHeader>> = None;
        let mut current: Option<NonNull<GcHeader>> = self.first;
        while let Some(mut object) = current {
            unsafe {
                let object_ptr = object.as_mut();
                current = object_ptr.next;
                if object_ptr.marked {
                    object_ptr.marked = false;
                    previous = Some(object);
                } else {
                    if let Some(mut previous) = previous {
                        previous.as_mut().next = object_ptr.next
                    } else {
                        self.first = object_ptr.next
                    }

                    println!("free(adr:{:?})", object_ptr as *mut GcHeader);
                    drop(Box::from_raw(object_ptr))
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ObjectType {
    Pair,
    Symbol,
    Closure,
}

#[repr(C)]
#[derive(Debug)]
pub struct GcHeader {
    marked: bool,
    next: Option<NonNull<GcHeader>>,
    obj_type: ObjectType,
}

impl GcHeader {
    pub fn new(obj_type: ObjectType) -> Self {
        Self {
            marked: false,
            next: None,
            obj_type,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Constant(Value),
    Push,
    Add,
    AddPair,
    Cons,
    DefineGlobal(GcRef<Symbol>),
    ReferGlobal(GcRef<Symbol>),
    LetFrame(isize),
    Enter(isize),
    ReferLocal(isize),
    Leave(isize),
    ReferFree(usize),
    Display(isize),
    Test(usize),
    LocalJump(usize),
    Closure {
        size: usize,
        arg_len: isize,
        is_optional_arg: bool,
        num_free_vars: isize,
    },
    Call(isize),
    Return(isize),
    Frame(usize),
}

#[derive(Copy, Clone, Debug)]
pub enum Value {
    Number(isize),
    Pair(GcRef<Pair>),
    Symbol(GcRef<Symbol>),
    VMStackPointer(*mut Value),
    Closure(GcRef<Closure>),
    False,
    Undef,
}

impl Value {
    pub fn is_false(&self) -> bool {
        match self {
            Value::False => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => {
                write!(f, "{}", n)
            }
            Value::Closure(closure) => {
                write!(f, "{}", closure)
            }
            Value::Pair(pair) => {
                write!(f, "{}", pair)
            }
            Value::Symbol(symbol) => {
                write!(f, "{}", symbol)
            }
            Value::False => {
                write!(f, "false")
            }
            Value::VMStackPointer(_) => {
                write!(f, "<stack pointer>")
            }
            Value::Undef => {
                write!(f, "<undefined>")
            }
        }
    }
}

const STACK_SIZE: usize = 256;

pub struct Vm {
    pub gc: Box<Gc>,
    // ToDo: Do they need to be pub?
    ac: Value,
    dc: Value, // display closure
    stack: [Value; STACK_SIZE],
    sp: *mut Value,
    fp: *mut Value,
    globals: HashMap<GcRef<Symbol>, Value>,
    ops: Vec<Op>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ac: Value::Undef,
            dc: Value::Undef,
            gc: Box::new(Gc::new()),
            stack: [Value::Number(0); STACK_SIZE],
            sp: null_mut(),
            fp: null_mut(),
            globals: HashMap::new(),
            ops: vec![],
        }
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

    fn pop(&mut self) -> Value {
        unsafe {
            self.sp = self.sp.offset(-1);
            *self.sp
        }
    }

    fn push(&mut self, value: Value) {
        unsafe {
            *self.sp = value;
            self.sp = self.sp.offset(1);
        }
    }

    fn index(&mut self, sp: *mut Value, n: isize) -> Value {
        unsafe { *sp.offset(-n - 1) }
    }

    fn print_stack(&mut self) {
        println!("-----------------------------------------");
        for &value in &self.stack[0..self.stack_len()] {
            println!("{:?}", value);
        }
        println!("-----------------------------------------<== sp")
    }

    pub fn run(&mut self, ops: Vec<Op>) -> Value {
        self.ops = ops; // gc roots
        self.sp = self.stack.as_mut_ptr();
        self.fp = self.sp;
        let len = self.ops.len();
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
                    self.ac = Value::Pair(pair);
                }
                Op::Add => match (self.pop(), self.ac) {
                    (Value::Number(a), Value::Number(b)) => {
                        println!("a={} ac={}", a, b);
                        self.ac = Value::Number(a + b);
                    }
                    (a, b) => {
                        panic!("add: numbers required but got {:?} {:?}", a, b);
                    }
                },
                Op::AddPair => {
                    if let Value::Pair(pair) = self.ac {
                        match (pair.first, pair.second) {
                            (Value::Number(lhs), Value::Number(rhs)) => {
                                self.ac = Value::Number(lhs + rhs);
                            }
                            _ => {
                                panic!("add pair: numbers require but got {:?} and {:?}", pair.first, pair.second);
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
                    self.push(Value::VMStackPointer(self.fp));
                }
                Op::ReferLocal(n) => unsafe {
                    self.ac = *self.fp.offset(n);
                },
                Op::Leave(n) => unsafe {
                    let sp = self.sp.offset(-n);

                    match self.index(sp, 0) {
                        Value::VMStackPointer(fp) => {
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
                    let mut display = self.alloc(Closure::new(0, 0, 0, false, free_vars));
                    display.prev = self.dc;

                    let display = Value::Closure(display);
                    self.dc = display;
                    self.sp = unsafe { self.sp.offset(-num_free_vars) };
                }
                Op::ReferFree(n) => match self.dc {
                    Value::Closure(mut closure) => {
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
                    self.ac = Value::Closure(self.alloc(Closure::new(
                        pc,
                        size,
                        arg_len,
                        is_optional_arg,
                        free_vars,
                    )));

                    self.sp = unsafe { self.sp.offset(-num_free_vars) };
                    pc += size;
                }
                Op::Call(arg_len) => {
                    let closure = match self.ac {
                        Value::Closure(c) => c,
                        _ => panic!("Can't call {:?}", self.ac),
                    };
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
                Op::Return(n) => {
                    let sp = unsafe { self.sp.offset(-n) };
                    match self.index(sp, 0) {
                        Value::VMStackPointer(fp) => {
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
                        Value::Number(next_pc) => {
                            pc = usize::try_from(next_pc).expect("pc it not a number");
                        }
                        _ => {
                            panic!("not a pc");
                        }
                    }
                    self.sp = unsafe { sp.offset(-4) }
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
                    self.push(Value::Number(next_pc));
                    self.push(self.dc);
                    self.push(self.dc); // todo this should be cl.
                    self.push(Value::VMStackPointer(self.fp));
                }
            }
            println!("after op={:?} ac={:?}", op, self.ac);
            self.print_stack();
            pc += 1;
        }
        self.ac
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_vm_call() {
        let mut vm = Vm::new();
        // ((lambda (a) (+ a a))
        let ops: Vec<Op> = vec![
            Op::Frame(21),
            Op::Constant(Value::Number(1)),
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
            Value::Number(a) => {
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
            Op::Constant(Value::Number(1)),
            Op::Push,
            Op::Constant(Value::Number(2)),
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
            Value::Number(a) => {
                assert_eq!(a, 3);
            }
            _ => panic!("ac was {:?}", ret),
        }
    }

    #[test]
    fn test_if() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Value::Number(1)),
            Op::Test(3),
            Op::Constant(Value::Number(2)),
            Op::LocalJump(2),
            Op::Constant(Value::Number(3)),
        ];
        let ret = vm.run(ops);
        match ret {
            Value::Number(a) => {
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
            Op::Constant(Value::False),
            Op::Test(3),
            Op::Constant(Value::Number(2)),
            Op::LocalJump(2),
            Op::Constant(Value::Number(3)),
        ];
        let ret = vm.run(ops);
        match ret {
            Value::Number(a) => {
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
            Op::Constant(Value::Number(3)),
            Op::Push,
            Op::Enter(1),
            Op::ReferLocal(0),
            Op::Leave(1),
        ];
        let ret = vm.run(ops);
        match ret {
            Value::Number(a) => {
                assert_eq!(a, 3);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_define() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Value::Number(9)),
            Op::DefineGlobal(vm.gc.intern("a".to_owned())),
            Op::ReferGlobal(vm.gc.intern("a".to_owned())),
        ];
        let ret = vm.run(ops);
        match ret {
            Value::Number(a) => {
                assert_eq!(a, 9);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_run_add() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Value::Number(99)),
            Op::Push,
            Op::Constant(Value::Number(1)),
            Op::Add,
        ];
        let ret = vm.run(ops);
        match ret {
            Value::Number(a) => {
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
            Op::Constant(Value::Number(2)),
            Op::Push,
            Op::Enter(1),
            Op::LetFrame(2),
            Op::ReferLocal(0),
            Op::Push,
            Op::Display(1),
            Op::Constant(Value::Number(1)),
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
            Value::Number(a) => {
                assert_eq!(a, 3);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_run_add_pair() {
        let mut vm = Vm::new();
        let ops = vec![
            Op::Constant(Value::Number(99)),
            Op::Push,
            Op::Constant(Value::Number(101)),
            Op::Cons,
            Op::AddPair,
        ];
        let ret = vm.run(ops);
        match ret {
            Value::Number(a) => {
                assert_eq!(a, 200);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_symbol() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("define".to_owned()));
        let symbol = Value::Symbol(symbol);
        match symbol {
            Value::Symbol(s) => {
                assert_eq!(s.string, "define");
            }
            _ => {
                panic!("{:?}", "todo");
            }
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
fn main() {}

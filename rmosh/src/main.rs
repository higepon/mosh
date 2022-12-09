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
    pub free_vars: Vec<Value>,
    pub prev: Value,
}

impl Closure {
    pub fn new(free_vars: Vec<Value>) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            free_vars: free_vars,
            prev: Value::Number(0),
        }
    }

    pub fn refer_free(&mut self, n: usize) -> Value {
        self.free_vars[n]
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "closure")
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

//#[macro_use]
//extern crate enum_display_derive;

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
}

#[derive(Copy, Clone, Debug)]
pub enum Value {
    Number(isize),
    Pair(GcRef<Pair>),
    Symbol(GcRef<Symbol>),
    VMStackPointer(*mut Value),
    Closure(GcRef<Closure>),
    False, // todo
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
            Value::Closure(_) => {
                write!(f, "closure")
            }
            Value::Pair(_) => {
                write!(f, "pair")
            }
            Value::Symbol(_) => {
                write!(f, "symbol")
            }
            Value::False => {
                write!(f, "false")
            }
            Value::VMStackPointer(_) => {
                write!(f, "stack pointer")
            }
        }
    }
}

const STACK_SIZE: usize = 256;

pub struct Vm {
    // ToDo: Do they need to be pub?
    ac: Value,
    dc: Value, // display closure
    pub gc: Box<Gc>,
    pub stack: [Value; STACK_SIZE],
    sp: *mut Value,
    fp: *mut Value,
    globals: HashMap<GcRef<Symbol>, Value>,
    ops: Vec<Op>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ac: Value::Number(0),
            dc: Value::Number(0), // todo
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

    pub fn run(&mut self, ops: Vec<Op>) -> Value {
        self.ops = ops; // gc roots
                        // move!
        self.sp = self.stack.as_mut_ptr();
        self.fp = self.sp;
        let len = self.ops.len();
        let mut pc = 0;
        while pc < len {
            let op = self.ops[pc];
            //println!("op={:?}", op);
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
                    _ => {
                        panic!("{:?}", "todo");
                    }
                },
                Op::AddPair => match self.ac {
                    Value::Pair(p) => match (p.first, p.second) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.ac = Value::Number(lhs + rhs);
                        }
                        _ => {
                            panic!("{:?}", "todo");
                        }
                    },
                    _ => {
                        panic!("{:?}", "todo");
                    }
                },
                Op::DefineGlobal(symbol) => {
                    self.globals.insert(symbol, self.ac);
                }
                Op::ReferGlobal(symbol) => match self.globals.get(&symbol) {
                    Some(&value) => {
                        self.ac = value;
                    }
                    None => {
                        panic!("{:?}", "refer global error");
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
                        _ => {
                            panic!("{:?}", "fp not found");
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
                    let mut display = self.alloc(Closure::new(free_vars));
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
                        panic!("todo");
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
            }
            pc += 1;
        }
        self.ac
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

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

// GC implementation based on Loxido written by Manuel Cerón.
// See https://github.com/ceronman/loxido.
use std::alloc;
use std::fmt;
use std::fmt::Display;
use std::mem;
use std::ptr::NonNull;
use std::{ops::Deref, sync::atomic::AtomicUsize, usize};

// TODO:
// - Simple GC w/o tagged pointers.
//   - [*] Format and reorganize all
//   - [*] Have GC in VM
//   - [*] Define Value.
//   - [*] Run w/o caring garbage collection
//   - [*] Implement self alloc
//   - [*] Implement trace
//   - [*] Actually run garbage collection
//   - [*] Add debug for alloc and free.
//   - [ ] Figure out how to add ops to root.
//   - [ ] Test push push push and see if object is allocated.

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

pub struct Fixnum {
    pub header: GcHeader,
    pub value: isize,
}

impl Fixnum {
    pub fn new(value: isize) -> Self {
        Fixnum {
            header: GcHeader::new(ObjectType::Fixnum),
            value: value,
        }
    }
}

impl Display for Fixnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct Pair {
    pub header: GcHeader,
    pub first: GcRef<GcHeader>,
    pub second: GcRef<GcHeader>,
}

impl Pair {
    pub fn new(first: GcRef<GcHeader>, second: GcRef<GcHeader>) -> Self {
        Pair {
            header: GcHeader::new(ObjectType::Pair),
            first: first,
            second: second,
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pair")
    }
}

pub struct GcRef<T> {
    pointer: NonNull<T>,
}

impl<T> GcRef<T> {
    pub fn as_header(&self) -> GcRef<GcHeader> {
        let header: NonNull<GcHeader> = unsafe { mem::transmute(self.pointer.as_ref()) };
        GcRef { pointer: header }
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

pub struct Gc {
    next_gc: usize,
    first: Option<NonNull<GcHeader>>,
    grey_stack: Vec<NonNull<GcHeader>>,
}

impl Gc {
    const HEAP_GROW_FACTOR: usize = 2;

    pub fn new() -> Self {
        Gc {
            next_gc: 1024 * 1024,
            first: None,
            grey_stack: Vec::new(),
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

    pub fn mark_value(&mut self, value: Value) {
        match value {
            Value::Number(value) => self.mark_object(value),
            Value::Pair(value) => self.mark_object(value),
            _ => (),
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
            self.blacken_object(pointer);
        }
    }

    fn blacken_object(&mut self, pointer: NonNull<GcHeader>) {
        let object_type = unsafe { &pointer.as_ref().obj_type };
        #[cfg(feature = "debug_log_gc")]
        println!("blacken(adr:{:?})", pointer);

        match object_type {
            ObjectType::Fixnum => {}
            ObjectType::Pair => {
                let pair: &Pair = unsafe { mem::transmute(pointer.as_ref()) };
                self.blacken_object(pair.first.pointer);
                self.blacken_object(pair.second.pointer);
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
    Fixnum,
    Pair,
}

#[repr(C)]
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

#[derive(Copy, Clone)]
pub enum Op {
    Constant(Value),
    Push,
    Add,
    AddPair,
}

#[derive(Copy, Clone)]
pub enum Value {
    Number(GcRef<Fixnum>),
    Pair(GcRef<Pair>),
    None,
}

const STACK_SIZE: usize = 256;

pub struct Vm {
    pub ac: Value,
    pub gc: Gc,
    pub stack: [Value; STACK_SIZE],
    pub sp: usize,
    ops: Vec<Op>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ac: Value::None,
            gc: Gc::new(),
            stack: [Value::None; STACK_SIZE],
            sp: 0,
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

    fn mark_roots(&mut self) {
        for &value in &self.stack[0..self.sp] {
            self.gc.mark_value(value);
        }

        self.gc.mark_value(self.ac);
    }

    pub fn run_add(&mut self) -> Value {
        // Don't call self.alloc here. It can trigger mark&sweep.
        let fixnum = self.gc.alloc(Fixnum::new(99));
        println!("alloc(adr:{:?})", &fixnum.header as *const GcHeader);
        let ops = vec![
            Op::Constant(Value::Number(fixnum)),
            Op::Push,
            Op::Constant(Value::Number(self.gc.alloc(Fixnum::new(1)))),
            Op::Add,
        ];
        self.ops = ops;
        self.run()
    }

    pub fn run_add_pair(&mut self) -> Value {
        let mut vm = Vm::new();
        let x = vm.gc.alloc(Fixnum::new(99));
        let y = vm.gc.alloc(Fixnum::new(101));
        println!("alloc(adr:{:?})", &x.header as *const GcHeader);
        println!("alloc(adr:{:?})", &y.header as *const GcHeader);
        let pair = vm.gc.alloc(Pair::new(x.as_header(), y.as_header()));
        let ops = vec![Op::Constant(Value::Pair(pair)), Op::AddPair];
        self.ops = ops;
        self.run()
    }

    pub fn run(&mut self) -> Value {
        let len = self.ops.len();
        let mut idx = 0;
        while idx < len {
            let op = self.ops[idx];
            idx += 1;
            match op {
                Op::Constant(c) => {
                    self.ac = c;
                }
                Op::Push => {
                    assert!(self.sp < STACK_SIZE);
                    self.stack[self.sp] = self.ac;
                    self.sp += 1;
                }
                Op::Add => {
                    self.sp -= 1;
                    match (self.stack[self.sp], self.ac) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.ac = Value::Number(self.alloc(Fixnum::new(a.value + b.value)));
                        }
                        _ => {
                            panic!("{:?}", "todo");
                        }
                    }
                }
                Op::AddPair => match self.ac {
                    Value::Pair(p) => match (p.first.obj_type, p.second.obj_type) {
                        (ObjectType::Fixnum, ObjectType::Fixnum) => {
                            let lhs: &Fixnum = unsafe { mem::transmute(p.first.pointer.as_ref()) };
                            let rhs: &Fixnum = unsafe { mem::transmute(p.second.pointer.as_ref()) };
                            self.ac = Value::Number(self.alloc(Fixnum::new(lhs.value + rhs.value)));
                        }
                        _ => {
                            panic!("{:?}", "todo");
                        }
                    },
                    _ => {
                        panic!("{:?}", "todo");
                    }
                },
            }
        }
        self.ac
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_vm_run_add() {
        let mut vm = Vm::new();
        let ret = vm.run_add();
        match ret {
            Value::Number(a) => {
                assert_eq!(a.value, 100);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_vm_run_add_pair() {
        let mut vm = Vm::new();
        let ret = vm.run_add_pair();
        match ret {
            Value::Number(a) => {
                assert_eq!(a.value, 200);
            }
            _ => panic!("{:?}", "todo"),
        }
    }

    #[test]
    fn test_gc() {
        let mut gc = Gc::new();
        let x: GcRef<Fixnum> = gc.alloc(Fixnum::new(1234));
        let y: GcRef<Fixnum> = gc.alloc(Fixnum::new(1));
        let z = gc.alloc(Fixnum::new(x.value + y.value));
        assert_eq!(z.value, 1235);
    }
    #[test]
    fn test_gc_pair() {
        let mut gc = Gc::new();
        let x: GcRef<Fixnum> = gc.alloc(Fixnum::new(1234));
        let y: GcRef<Fixnum> = gc.alloc(Fixnum::new(1));
        let p = gc.alloc(Pair::new(x.as_header(), y.as_header()));
        assert_eq!(p.first.obj_type, ObjectType::Fixnum);
    }
}

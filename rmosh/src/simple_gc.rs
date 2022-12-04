// GC implementation based on loxido.
use std::fmt;
use std::fmt::Display;
use std::mem;
use std::ptr::NonNull;
use std::{
    ops::{Deref, DerefMut},
    sync::atomic::AtomicUsize,
    usize,
};

// TODO:
// - Simple GC w/o tagged pointers.
//   - [*] Format and reorganize all
//   - [*] Have GC in VM
//   - [*] Define Value.
//   - Run w/o caring garbage collection
//   - Implement self alloc
//   - Implement trace
//   - Actually run garbage collection
//   - Test push push push and see if object is allocated.

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

pub struct Pair2 {
    pub header: GcHeader,
    pub first: GcRef<GcHeader>,
    pub second: GcRef<GcHeader>,
}

impl Pair2 {
    pub fn new(first: GcRef<GcHeader>, second: GcRef<GcHeader>) -> Self {
        Pair2 {
            header: GcHeader::new(ObjectType::Pair),
            first: first,
            second: second,
        }
    }
}

impl Display for Pair2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pair")
    }
}

pub struct GcRef<T> {
    pointer: NonNull<T>,
}

impl<T> GcRef<T> {
    pub fn as_header(&self) -> GcRef<GcHeader> {
        //let ptr: NonNull<T> = self.pointer;
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
}

#[derive(Debug, PartialEq)]
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
    CONSTANT(Value),
    PUSH,
    ADD,
}

#[derive(Copy, Clone)]
pub enum Value {
    Number(GcRef<Fixnum>),
    Pair(GcRef<Pair2>),
}

pub struct Vm {
    pub ac: Value,
    pub gc: Gc,
}

impl<'a> Vm {
    pub fn run(&mut self, ops: &Vec<Op>) -> Value {
        const STACK_SIZE: usize = 256;
        let mut stack: [Value; STACK_SIZE] =
            [Value::Number(self.gc.alloc(Fixnum::new(1234))); STACK_SIZE];
        let mut sp: usize = 0;
        let len = ops.len();
        let mut idx = 0;
        while idx < len {
            let op = ops[idx];
            idx += 1;
            match op {
                Op::CONSTANT(c) => {
                    self.ac = c;
                }
                Op::PUSH => {
                    assert!(sp < STACK_SIZE);
                    stack[sp] = self.ac;
                    sp += 1;
                }
                Op::ADD => {
                    sp -= 1;
                    match stack[sp] {
                        Value::Number(a) => match self.ac {
                            Value::Number(b) => {
                                self.ac =
                                    Value::Number(self.gc.alloc(Fixnum::new(a.value + b.value)));
                            }
                            Value::Pair(_) => panic!("{:?}", "todo"),
                        },
                        Value::Pair(_) => panic!("{:?}", "todo"),
                    }
                }
            }
        }
        self.ac
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_vm_run() {
        let mut gc = Gc::new();
        let ops = vec![
            Op::CONSTANT(Value::Number(gc.alloc(Fixnum::new(99)))),
            Op::PUSH,
            Op::CONSTANT(Value::Number(gc.alloc(Fixnum::new(1)))),
            Op::ADD,
        ];
        let mut vm = Vm {
            ac: Value::Number(gc.alloc(Fixnum::new(0))),
            gc: gc,
        };
        let ret = vm.run(&ops);
        match ret {
            Value::Number(a) => {
                assert_eq!(a.value, 100);
            }
            Value::Pair(_) => panic!("{:?}", "todo"),
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
        let p = gc.alloc(Pair2::new(x.as_header(), y.as_header()));
        assert_eq!(p.first.obj_type, ObjectType::Fixnum);
    }
}

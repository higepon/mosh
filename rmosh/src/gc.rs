// GC implementation based on Loxido written by Manuel Cerón.
// See https://github.com/ceronman/loxido.

// TODO
// https://github.com/ceronman/loxido/issues/3
//

use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ptr::NonNull;
use std::{ops::Deref, ops::DerefMut, sync::atomic::AtomicUsize, usize};

use crate::alloc::GlobalAllocator;
use crate::objects::{Closure, Object, Pair, Symbol};

#[global_allocator]
static GLOBAL: GlobalAllocator = GlobalAllocator {
    bytes_allocated: AtomicUsize::new(0),
};

/// GcRef.
/// This holds raw pointer to an object.
#[derive(Debug)]
pub struct GcRef<T> {
    pub pointer: NonNull<T>,
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ObjectType {
    Pair,
    Procedure,
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

pub struct Gc {
    next_gc: usize,
    first: Option<NonNull<GcHeader>>,
    marked_objects: Vec<NonNull<GcHeader>>,
    symbols: HashMap<String, GcRef<Symbol>>,
}

impl Gc {
    const HEAP_GROW_FACTOR: usize = 2;

    pub fn new() -> Self {
        Gc {
            next_gc: 1024 * 1024,
            first: None,
            marked_objects: Vec::new(),
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

    // Mark Object as used and push it to marked_roots.
    // This should be called only for root Objects.
    pub fn mark_object(&mut self, obj: Object) {
        match obj {
            Object::Number(_) => {}
            Object::VMStackPointer(_) => {}
            Object::False => {}
            Object::Undef => {}
            Object::Procedure(_) => {}
            Object::Closure(closure) => {
                self.mark_heap_object(closure);
            }
            Object::Symbol(symbol) => {
                self.mark_heap_object(symbol);
            }
            Object::Pair(pair) => {
                self.mark_heap_object(pair);
            }
        }
    }

    // Mark heap allocated object as used and push it to marked_roots.
    // This should be called only for root objects.
    pub fn mark_heap_object<T: 'static>(&mut self, mut reference: GcRef<T>) {
        unsafe {
            let mut header: NonNull<GcHeader> = mem::transmute(reference.pointer.as_mut());
            //if header.as_mut().marked {
            //    return;
            //}
            header.as_mut().marked = true;

            self.marked_objects.push(header);
        }
    }

    // Collect garbage.
    // This traces all references used starting from marked_roots.
    pub fn collect_garbage(&mut self) {
        self.trace_references();
        self.sweep();
        self.next_gc = GLOBAL.bytes_allocated() * Gc::HEAP_GROW_FACTOR;
    }

    // Mark each object's fields.
    fn trace_references(&mut self) {
        while let Some(obj_header) = self.marked_objects.pop() {
            self.mark_object_fields(obj_header);
        }
    }

    fn mark_object_fields(&mut self, pointer: NonNull<GcHeader>) {
        let object_type = unsafe { &pointer.as_ref().obj_type };
        #[cfg(feature = "debug_log_gc")]
        println!("blacken(adr:{:?})", pointer);

        match object_type {
            ObjectType::Symbol => {}
            ObjectType::Procedure => {}
            ObjectType::Closure => {
                let closure: &Closure = unsafe { mem::transmute(pointer.as_ref()) };
                for i in 0..closure.free_vars.len() {
                    let obj = closure.free_vars[i];
                    self.mark_object(obj);
                }
            }
            ObjectType::Pair => {
                let pair: &Pair = unsafe { mem::transmute(pointer.as_ref()) };
                self.mark_object(pair.first);
                self.mark_object(pair.second);
            }
        }
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

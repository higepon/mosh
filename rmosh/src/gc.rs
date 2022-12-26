// GC implementation based on Loxido written by Manuel Cerón.
// See https://github.com/ceronman/loxido.

// TODO
// https://github.com/ceronman/loxido/issues/3
//
// Make test link work everytime.
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ptr::NonNull;
use std::{ops::Deref, ops::DerefMut, sync::atomic::AtomicUsize, usize};

use crate::alloc::GlobalAllocator;
use crate::objects::{Closure, Object, Pair, Procedure, SString, Symbol, Vector, Vox, EqHashTable};
use crate::op::Op;
use crate::vm::Vm;

#[global_allocator]
static GLOBAL: GlobalAllocator = GlobalAllocator {
    bytes_allocated: AtomicUsize::new(0),
};

// GcRef.
// This holds raw pointer to an object.
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
    Closure,
    EqHashTable,
    InputPort,
    Pair,
    Procedure,
    String,
    Symbol,
    Vector,
    Vox,
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

#[cfg(feature = "test_gc_size")]
pub fn short_type_name<T: std::any::Any>() -> &'static str {
    let full_name = std::any::type_name::<T>();
    full_name.split("::").last().unwrap()
}

pub struct Gc {
    next_gc: usize,
    first: Option<NonNull<GcHeader>>,
    marked_objects: Vec<NonNull<GcHeader>>,
    pub symbols: HashMap<String, GcRef<Symbol>>,
    current_alloc_size: usize,
}

impl Gc {
    const HEAP_GROW_FACTOR: usize = 2;

    pub fn new() -> Self {
        Gc {
            next_gc: 1024 * 1024,
            first: None,
            marked_objects: Vec::new(),
            symbols: HashMap::new(),
            current_alloc_size: 0,
        }
    }

    pub fn cons(&mut self, first: Object, second: Object) -> Object {
        let pair = self.alloc(Pair::new(first, second));
        Object::Pair(pair)
    }

    pub fn list1(&mut self, obj: Object) -> Object {
        self.cons(obj, Object::Nil)
    }

    pub fn list2(&mut self, first: Object, second: Object) -> Object {
        let second = self.cons(second, Object::Nil);
        self.cons(first, second)
    }

    pub fn list3(&mut self, first: Object, second: Object, third: Object) -> Object {
        let third = self.cons(third, Object::Nil);
        let second = self.cons(second, third);
        self.cons(first, second)
    }

    pub fn list4(
        &mut self,
        first: Object,
        second: Object,
        third: Object,
        fourth: Object,
    ) -> Object {
        let fourth = self.cons(fourth, Object::Nil);
        let third = self.cons(third, fourth);
        let second = self.cons(second, third);
        self.cons(first, second)
    }

    pub fn list5(
        &mut self,
        first: Object,
        second: Object,
        third: Object,
        fourth: Object,
        fifth: Object,        
    ) -> Object {
        let fifth = self.cons(fifth, Object::Nil);        
        let fourth = self.cons(fourth, fifth);        
        let third = self.cons(third, fourth);
        let second = self.cons(second, third);
        self.cons(first, second)
    }    


    pub fn list6(
        &mut self,
        first: Object,
        second: Object,
        third: Object,
        fourth: Object,
        fifth: Object,        
        sixth: Object,                
    ) -> Object {
        let sixth = self.cons(sixth, Object::Nil);                
        let fifth = self.cons(fifth, sixth);        
        let fourth = self.cons(fourth, fifth);        
        let third = self.cons(third, fourth);
        let second = self.cons(second, third);
        self.cons(first, second)
    }        

    pub fn symbol_intern(&mut self, s: &str) -> Object {
        let symbol = self.intern(s);
        Object::Symbol(symbol)
    }

    pub fn new_procedure(&mut self, func: fn(&mut Vm, &[Object]) -> Object, name: &str) -> Object {
        Object::Procedure(self.alloc(Procedure::new(func, name.to_string())))
    }

    pub fn new_string(&mut self, s: &str) -> Object {
        let s = self.alloc(SString::new(s));
        Object::String(s)
    }

    pub fn new_vector(&mut self, data: &Vec<Object>) -> Object {
        let v = self.alloc(Vector::new(data));
        Object::Vector(v)
    }

    pub fn new_eq_hash_table(&mut self) -> Object {
        let obj = self.alloc(EqHashTable::new());
        Object::EqHashTable(obj)
    }    

    // append o (list or obj) to l.
    // if l is not list return o.
    // allocate new cons sell.
    pub fn append2(&mut self, list: Object, obj: Object) -> Object {
        if !list.is_pair() {
            return obj;
        }
        let mut start = Object::Nil;
        let mut last = Object::Nil;
        let mut p = list;
        loop {
            match p {
                Object::Pair(pair) => {
                    if start.is_nil() {
                        start = self.cons(pair.car, Object::Nil);
                        last = start
                    } else {
                        match last {
                            Object::Pair(mut last_pair) => {
                                last_pair.cdr = self.cons(pair.car, Object::Nil);
                                last = last_pair.cdr;
                            }
                            _ => {
                                panic!("last is not pair");
                            }
                        }
                    }
                    p = pair.cdr;
                }
                _ => match last {
                    Object::Pair(mut pair) => {
                        pair.cdr = obj;
                        return start;
                    }
                    _ => {
                        panic!("last is not pair");
                    }
                },
            }
        }
    }

    #[cfg(not(feature = "test_gc_size"))]
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

    #[cfg(feature = "test_gc_size")]
    pub fn alloc<T: Display + 'static>(&mut self, object: T) -> GcRef<T> {
        unsafe {
            #[cfg(feature = "debug_log_gc")]
            let repr = format!("{}", object)
                .chars()
                .into_iter()
                .take(32)
                .collect::<String>();

            let alloc_size = std::mem::size_of_val(&object);
            self.current_alloc_size += alloc_size;

            let boxed = Box::new(object);
            let pointer = NonNull::new_unchecked(Box::into_raw(boxed));
            let mut header: NonNull<GcHeader> = mem::transmute(pointer.as_ref());
            header.as_mut().next = self.first.take();
            self.first = Some(header);
            #[cfg(feature = "debug_log_gc")]
            println!(
                "alloc(adr:{:?} type:{} repr:{}, alloc_size={}, allocated bytes:{} next:{})",
                header,
                short_type_name::<T>(),
                repr,
                alloc_size,
                GLOBAL.bytes_allocated(),
                self.next_gc,
            );

            GcRef { pointer }
        }
    }

    pub fn intern(&mut self, s: &str) -> GcRef<Symbol> {
        match self.symbols.get(s) {
            Some(&symbol) => symbol,
            None => {
                let symbol = self.alloc(Symbol::new(s.to_owned()));
                self.symbols.insert(s.to_string(), symbol);
                symbol
            }
        }
    }

    pub fn bytes_allocated(&self) -> usize {
        self.current_alloc_size
    }

    // Mark Object as used and push it to marked_objects.
    pub fn mark_object(&mut self, obj: Object) {
        match obj {
            Object::Char(_) => {}
            Object::Eof => {}
            Object::False => {}
            Object::InputPort(_) => {}
            Object::Nil => {}
            Object::Number(_) => {}
            Object::StackPointer(_) => {}
            Object::OpPointer(op) => {
                self.mark_op(unsafe { *op });
            }
            Object::True => {}
            Object::Unspecified => {}
            Object::Vox(vox) => {
                self.mark_heap_object(vox);
            }
            Object::EqHashTable(hash_table) => {
                self.mark_heap_object(hash_table);
            }            
            Object::Procedure(procedure) => {
                self.mark_heap_object(procedure);
            }
            Object::Closure(closure) => {
                self.mark_heap_object(closure);
            }
            Object::String(string) => {
                self.mark_heap_object(string);
            }
            Object::Symbol(symbol) => {
                self.mark_heap_object(symbol);
            }
            Object::Pair(pair) => {
                self.mark_heap_object(pair);
            }
            Object::Vector(vector) => {
                self.mark_heap_object(vector);
            }
        }
    }

    // Mark heap allocated object as used and push it to marked_objects.
    pub fn mark_heap_object<T: 'static>(&mut self, mut reference: GcRef<T>) {
        unsafe {
            let mut header: NonNull<GcHeader> = mem::transmute(reference.pointer.as_mut());
            if header.as_mut().marked {
                return;
            }
            header.as_mut().marked = true;

            self.marked_objects.push(header);

            #[cfg(feature = "debug_log_gc")]
            if header.as_ref().obj_type != ObjectType::Procedure {
                println!("mark(adr:{:?}, type:{:?}", header, header.as_ref().obj_type,);
            }
        }
    }

    // Collect garbage.
    // This traces all references starting from marked_objects.
    pub fn collect_garbage(&mut self) {
        #[cfg(feature = "debug_log_gc")]
        let before: isize = GLOBAL.bytes_allocated() as isize;

        self.trace_references();
        self.sweep();
        self.next_gc = GLOBAL.bytes_allocated() * Gc::HEAP_GROW_FACTOR;

        #[cfg(feature = "debug_log_gc")]
        println!(
            "collected(bytes:{} before:{} after:{} next:{})",
            before - GLOBAL.bytes_allocated() as isize,
            before,
            GLOBAL.bytes_allocated(),
            self.next_gc
        );
    }

    // Mark each object's fields.
    fn trace_references(&mut self) {
        while let Some(obj_header) = self.marked_objects.pop() {
            self.mark_object_fields(obj_header);
        }
    }

    // Some Op contains GC-ed objects.
    pub fn mark_op(&mut self, op: Op) {
        match op {
            Op::AssignGlobal(symbol) => {
                self.mark_heap_object(symbol);
            }
            Op::Constant(v) => {
                self.mark_object(v);
            }
            Op::DefineGlobal(symbol) => {
                self.mark_heap_object(symbol);
            }
            Op::ReferGlobal(symbol) => {
                self.mark_heap_object(symbol);
            }
            Op::Append2 => {}
            Op::AssignFree(_) => (),
            Op::AssignLocal(_) => (),
            Op::Box(_) => (),
            Op::BranchNotEqv(_) => (),
            Op::BranchNotGe(_) => (),
            Op::BranchNotGt(_) => (),
            Op::BranchNotLe(_) => (),
            Op::BranchNotLt(_) => (),
            Op::BranchNotNull(_) => (),
            Op::BranchNotNumberEqual(_) => (),
            Op::Cadr => (),
            Op::Call(_) => (),
            Op::Car => (),
            Op::Cdr => (),
            Op::Closure { .. } => (),
            Op::Cons => (),
            Op::Display(_) => (),
            Op::Enter(_) => (),
            Op::Eq => (),
            Op::Equal => (),            
            Op::Eqv => (),                      
            Op::Frame(_) => (),
            Op::Halt => (),
            Op::Indirect => (),
            Op::Leave(_) => (),
            Op::LetFrame(_) => (),
            Op::LocalJmp(_) => (),
            Op::MakeVector => (),
            Op::Nop => (),
            Op::Not => (),
            Op::NullP => (),
            Op::NumberAdd => (),
            Op::NumberDiv => (),
            Op::NumberMul => (),
            Op::NumberSub => (),            
            Op::NumberEqual => (),
            Op::NumberGe => (),
            Op::NumberGt => (),
            Op::NumberLe => (),
            Op::NumberLt => (),
            Op::PairP => (),
            Op::Push => (),
            Op::ReadChar => (),
            Op::Receive(_, _) => (),            
            Op::ReferFree(_) => (),
            Op::ReferLocal(_) => (),
            Op::Return(_) => (),
            Op::SetCar => (),
            Op::SetCdr => (),
            Op::SymbolP => (),
            Op::TailCall(_, _) => (),
            Op::Test(_) => (),
            Op::Undef => (),
            Op::Values(_) => (),
            Op::VectorLength => (),
            Op::VectorP => (),
            Op::VectorRef => (),
            Op::VectorSet => (),
        }
    }

    fn mark_object_fields(&mut self, pointer: NonNull<GcHeader>) {
        let object_type = unsafe { &pointer.as_ref().obj_type };
        match object_type {
            ObjectType::Closure => {
                let closure: &Closure = unsafe { mem::transmute(pointer.as_ref()) };
                for i in 0..closure.free_vars.len() {
                    let obj = closure.free_vars[i];
                    self.mark_object(obj);
                }
                for i in 0..closure.ops_len {
                    let op = unsafe { *closure.ops.offset(i as isize) };
                    self.mark_op(op);
                }

                if !closure.prev.is_unspecified() {
                    self.mark_object(closure.prev);
                }
            }
            ObjectType::Vox => {
                let vox: &Vox = unsafe { mem::transmute(pointer.as_ref()) };
                self.mark_object(vox.value);
            }
            ObjectType::Pair => {
                let pair: &Pair = unsafe { mem::transmute(pointer.as_ref()) };
                self.mark_object(pair.car);
                self.mark_object(pair.cdr);
            }
            ObjectType::Vector => {
                let vector: &Vector = unsafe { mem::transmute(pointer.as_ref()) };
                for i in 0..vector.data.len() {
                    self.mark_object(vector.data[i]);
                }
            }
            ObjectType::EqHashTable => {
                let hash_table: &EqHashTable = unsafe { mem::transmute(pointer.as_ref()) };

                for &obj in hash_table.hash_map.values() {
                    self.mark_object(obj);
                }
                for &obj in hash_table.hash_map.keys() {
                    self.mark_object(obj);
                }
            }            
            ObjectType::InputPort => {}
            ObjectType::String => {}
            ObjectType::Symbol => {}
            ObjectType::Procedure => {}
        }
    }

    #[cfg(feature = "test_gc_size")]
    pub fn should_gc(&self) -> bool {
        true
    }

    #[cfg(not(feature = "test_gc_size"))]
    pub fn should_gc(&self) -> bool {
        GLOBAL.bytes_allocated() > self.next_gc
    }

    #[cfg(feature = "test_gc_size")]
    fn free(&mut self, object_ptr: &mut GcHeader) {
        use crate::objects::InputPort;

        let object_type = object_ptr.obj_type;

        let hige: &GcHeader = object_ptr;

        let free_size = match object_type {
            ObjectType::Symbol => 0,
            ObjectType::Procedure => {
                panic!("procedure should not be freed");
            }
            ObjectType::String => {
                let sstring: &SString = unsafe { mem::transmute(hige) };
                std::mem::size_of_val(sstring)
            }
            ObjectType::Closure => {
                let closure: &Closure = unsafe { mem::transmute(hige) };
                std::mem::size_of_val(closure)
            }
            ObjectType::InputPort => {
                let port: &InputPort = unsafe { mem::transmute(hige) };
                std::mem::size_of_val(port)
            }
            ObjectType::Vox => {
                let vox: &Vox = unsafe { mem::transmute(hige) };
                std::mem::size_of_val(vox)
            }
            ObjectType::Pair => {
                let pair: &Pair = unsafe { mem::transmute(hige) };
                std::mem::size_of_val(pair)
            }
            ObjectType::Vector => {
                let v: &Vector = unsafe { mem::transmute(hige) };
                std::mem::size_of_val(v)
            }
            ObjectType::EqHashTable => {
                let hash_table: &EqHashTable = unsafe { mem::transmute(hige) };
                std::mem::size_of_val(hash_table)
            }            
        };
        #[cfg(feature = "debug_log_gc")]
        println!(
            "free(adr:{:?}) type={:?} size={} ******* ",
            object_ptr as *mut GcHeader, object_type, free_size
        );

        self.current_alloc_size -= free_size;
        unsafe { drop(Box::from_raw(object_ptr)) }
    }

    #[cfg(not(feature = "test_gc_size"))]
    fn free(&self, object_ptr: &mut GcHeader) {
        panic!("not tested");
        unsafe { drop(Box::from_raw(object_ptr)) }
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
                    self.free(object_ptr);
                }
            }
        }
    }
}

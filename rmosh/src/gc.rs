// GC implementation based on Loxido written by Manuel Cerón.
// See https://github.com/ceronman/loxido.

// TODO
// https://github.com/ceronman/loxido/issues/3
//
use crate::error::SchemeError;
use crate::numbers::{Bignum, Compnum, Ratnum};
use crate::objects::{
    Bytevector, Closure, Continuation, ContinuationStack, EqHashtable, EqvHashtable,
    GenericHashtable, Object, Pair, Procedure, SString, SimpleStruct, Symbol, Vector, Vox,
};
use crate::regexp::{RegMatch, Regexp};
use crate::socket::Socket;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ptr::NonNull;
use std::{ops::Deref, ops::DerefMut, usize};

use crate::ports::{
    BinaryFileInputOutputPort, BinaryFileInputPort, BinaryFileOutputPort,
    BinarySocketInputOutputPort, BytevectorInputPort, BytevectorOutputPort,
    CustomBinaryInputOutputPort, CustomBinaryInputPort, CustomBinaryOutputPort,
    CustomTextInputOutputPort, CustomTextInputPort, CustomTextOutputPort, FileInputPort,
    FileOutputPort, Latin1Codec, StdErrorPort, StdInputPort, StdOutputPort, StringInputPort,
    StringOutputPort, TranscodedInputOutputPort, TranscodedInputPort, TranscodedOutputPort,
    Transcoder, UTF16Codec, UTF8Codec,
};
use crate::vm::Vm;

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

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum ObjectType {
    Bignum,
    BinaryFileInputPort,
    BinaryFileInputOutputPort,
    BinaryFileOutputPort,
    BinarySocketInputOutputPort,
    Bytevector,
    BytevectorInputPort,
    BytevectorOutputPort,
    Closure,
    Compnum,
    Continuation,
    ContinuationStack,
    CustomTextInputPort,
    CustomTextInputOutputPort,
    CustomTextOutputPort,
    CustomBinaryInputPort,
    CustomBinaryInputOutputPort,
    CustomBinaryOutputPort,
    EqHashtable,
    EqvHashtable,
    FileInputPort,
    FileOutputPort,
    Latin1Codec,
    UTF8Codec,
    UTF16Codec,
    GenericHashtable,
    Pair,
    Procedure,
    Ratnum,
    Regexp,
    RegMatch,
    SimpleStruct,
    Socket,
    StdErrorPort,
    StdInputPort,
    StdOutputPort,
    SString,
    StringInputPort,
    StringOutputPort,
    Symbol,
    Transcoder,
    TranscodedInputPort,
    TranscodedInputOutputPort,
    TranscodedOutputPort,
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

pub trait Trace {
    fn trace(&self, gc: &mut Gc);
}

pub struct Gc {
    next_gc: usize,
    first: Option<NonNull<GcHeader>>,
    marked_objects: Vec<NonNull<GcHeader>>,
    pub symbols: HashMap<String, GcRef<Symbol>>,
    current_alloc_size: usize,
    pub show_stats: bool,
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}

impl Gc {
    pub fn new() -> Self {
        Gc {
            next_gc: 1024 * 1024,
            first: None,
            marked_objects: Vec::new(),
            symbols: HashMap::new(),
            current_alloc_size: 0,
            show_stats: false,
        }
    }

    pub fn cons(&mut self, first: Object, second: Object) -> Object {
        let pair = self.alloc(Pair::new(first, second));
        Object::Pair(pair)
    }

    pub fn cons_src(&mut self, first: Object, second: Object, src: Object) -> Object {
        let mut pair = self.alloc(Pair::new(first, second));
        pair.src = src;
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

    pub fn dot_pair(&mut self, objects: &[Object], last: Object) -> Object {
        let mut ret = last;
        for obj in objects.iter().rev() {
            ret = self.cons(*obj, ret);
        }
        ret
    }

    pub fn dot_pair_src(&mut self, objects: &[Object], last: Object, src: Object) -> Object {
        let mut ret = last;
        for obj in objects.iter().rev() {
            ret = self.cons_src(*obj, ret, src);
        }
        ret
    }

    pub fn listn(&mut self, objects: &[Object]) -> Object {
        let mut ret = Object::Nil;
        for obj in objects.iter().rev() {
            ret = self.cons(*obj, ret);
        }
        ret
    }

    pub fn listn_src(&mut self, objects: &[Object], src: Object) -> Object {
        let mut ret = Object::Nil;
        for obj in objects.iter().rev() {
            ret = self.cons_src(*obj, ret, src);
        }
        ret
    }

    pub fn symbol_intern(&mut self, s: &str) -> Object {
        let symbol = self.intern(s);
        Object::Symbol(symbol)
    }

    pub fn new_compnum(&mut self, real: Object, imag: Object) -> Object {
        if imag.is_exact_zero() {
            real
        } else {
            Object::Compnum(self.alloc(Compnum::new(real, imag)))
        }
    }

    pub fn new_procedure(
        &mut self,
        func: fn(&mut Vm, &mut [Object]) -> Result<Object, SchemeError>,
        name: &str,
    ) -> Object {
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

    pub fn new_bytevector_u8(&mut self, values: &Vec<u8>) -> Object {
        let bv = self.alloc(Bytevector::new(values));
        Object::Bytevector(bv)
    }

    pub fn new_bytevector(&mut self, objects: &Vec<Object>) -> Result<Object, SchemeError> {
        let mut u8_vec: Vec<u8> = vec![];
        for obj in objects {
            if let Object::Fixnum(n) = obj {
                if *n >= 0 && *n <= 255 {
                    u8_vec.push(*n as u8);
                } else {
                    return Err(SchemeError::assertion_violation(
                        "bytevector",
                        &format!("malformed bytevector: u8 value required but got {}", *n),
                        &[],
                    ));
                }
            } else {
                return Err(SchemeError::assertion_violation(
                    "bytevector",
                    "malformed bytevector: u8 value required",
                    &[*obj],
                ));
            }
        }
        let bv = self.alloc(Bytevector::new(&u8_vec));
        Ok(Object::Bytevector(bv))
    }

    pub fn new_eq_hashtable(&mut self) -> Object {
        let obj = self.alloc(EqHashtable::new());
        Object::EqHashtable(obj)
    }

    pub fn new_eqv_hashtable(&mut self) -> Object {
        let obj = self.alloc(EqvHashtable::new());
        Object::EqvHashtable(obj)
    }

    pub fn new_generic_hashtable(&mut self, hash_func: Object, eq_func: Object) -> Object {
        let obj = self.alloc(GenericHashtable::new(hash_func, eq_func));
        Object::GenericHashtable(obj)
    }

    // append o (list or obj) to l.
    // if l is not list return o.
    // allocate new cons sell.
    pub fn append2(&mut self, list: Object, obj: Object) -> Result<Object, SchemeError> {
        if !list.is_pair() {
            return Ok(obj);
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
                                return Err(SchemeError::assertion_violation(
                                    "append",
                                    "last is not pair",
                                    &[last],
                                ));
                            }
                        }
                    }
                    p = pair.cdr;
                }
                _ => match last {
                    Object::Pair(mut pair) => {
                        pair.cdr = obj;
                        return Ok(start);
                    }
                    _ => {
                        return Err(SchemeError::assertion_violation(
                            "append",
                            "last is not pair",
                            &[last],
                        ));
                    }
                },
            }
        }
    }

    #[cfg(not(feature = "test_gc_size"))]
    pub fn alloc<T: Display + 'static>(&mut self, object: T) -> GcRef<T> {
        unsafe {
            let alloc_size = std::mem::size_of_val(&object);
            self.current_alloc_size += alloc_size;
            let boxed = Box::new(object);
            let mut pointer = NonNull::new_unchecked(Box::into_raw(boxed));
            let mut header: NonNull<GcHeader> = mem::transmute(pointer.as_mut());
            header.as_mut().next = self.first.take();
            self.first = Some(header);

            GcRef { pointer }
        }
    }

    #[cfg(feature = "test_gc_size")]
    pub fn alloc<T: Display + 'static>(&mut self, object: T) -> GcRef<T> {
        unsafe {
            #[cfg(feature = "debug_log_gc")]
            let repr = format!("{}", object).chars().take(32).collect::<String>();

            let alloc_size = std::mem::size_of_val(&object);
            self.current_alloc_size += alloc_size;

            let boxed = Box::new(object);
            let mut pointer = NonNull::new_unchecked(Box::into_raw(boxed));
            let mut header: NonNull<GcHeader> = mem::transmute(pointer.as_mut());
            header.as_mut().next = self.first.take();
            self.first = Some(header);

            #[cfg(feature = "debug_log_gc")]
            println!(
                "alloc(adr:{:?} type:{} repr:{}, alloc_size={}, allocated bytes:{} next:{})",
                header,
                short_type_name::<T>(),
                repr,
                alloc_size,
                self.current_alloc_size,
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
            Object::Nil => {}
            Object::Flonum(_) => {}
            Object::Fixnum(_) => {}
            Object::Instruction(_) => {}
            Object::ObjectPointer(_) => {}
            Object::ProgramCounter(_) => {}
            Object::True => {}
            Object::Unspecified => {}
            Object::DefinedShared(_) => todo!(),
            Object::Continuation(c) => {
                self.mark_heap_object(c);
            }
            Object::ContinuationStack(c) => {
                self.mark_heap_object(c);
            }
            Object::CustomBinaryInputPort(c) => {
                self.mark_heap_object(c);
            }
            Object::CustomBinaryInputOutputPort(c) => {
                self.mark_heap_object(c);
            }
            Object::CustomBinaryOutputPort(c) => {
                self.mark_heap_object(c);
            }
            Object::CustomTextInputPort(c) => {
                self.mark_heap_object(c);
            }
            Object::CustomTextInputOutputPort(c) => {
                self.mark_heap_object(c);
            }
            Object::CustomTextOutputPort(c) => {
                self.mark_heap_object(c);
            }
            Object::FileInputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::BinaryFileInputOutputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::BinaryFileOutputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::BytevectorInputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::BytevectorOutputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::BinaryFileInputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::BinarySocketInputOutputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::FileOutputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::Socket(socket) => {
                self.mark_heap_object(socket);
            }
            Object::StringInputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::StringOutputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::StdInputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::StdOutputPort(port) => {
                self.mark_heap_object(port);
            }
            Object::StdErrorPort(port) => {
                self.mark_heap_object(port);
            }
            Object::Latin1Codec(c) => {
                self.mark_heap_object(c);
            }
            Object::UTF8Codec(c) => {
                self.mark_heap_object(c);
            }
            Object::UTF16Codec(c) => {
                self.mark_heap_object(c);
            }
            Object::Transcoder(t) => {
                self.mark_heap_object(t);
            }
            Object::TranscodedOutputPort(t) => {
                self.mark_heap_object(t);
            }
            Object::TranscodedInputPort(t) => {
                self.mark_heap_object(t);
            }
            Object::TranscodedInputOutputPort(t) => {
                self.mark_heap_object(t);
            }
            Object::Compnum(c) => {
                self.mark_heap_object(c);
            }
            Object::Bignum(b) => {
                self.mark_heap_object(b);
            }
            Object::Ratnum(r) => {
                self.mark_heap_object(r);
            }
            Object::Regexp(r) => {
                self.mark_heap_object(r);
            }
            Object::RegMatch(r) => {
                self.mark_heap_object(r);
            }
            Object::Vox(vox) => {
                self.mark_heap_object(vox);
            }
            Object::EqHashtable(hashtable) => {
                self.mark_heap_object(hashtable);
            }
            Object::EqvHashtable(hashtable) => {
                self.mark_heap_object(hashtable);
            }
            Object::GenericHashtable(hashtable) => {
                self.mark_heap_object(hashtable);
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
            Object::Bytevector(bytevector) => {
                self.mark_heap_object(bytevector);
            }
            Object::Vector(vector) => {
                self.mark_heap_object(vector);
            }
            Object::SimpleStruct(s) => {
                self.mark_heap_object(s);
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

            /*     #[cfg(feature = "debug_log_gc")]
                if header.as_ref().obj_type != ObjectType::Procedure {
                    println!("mark(adr:{:?}, type:{:?}", header, header.as_ref().obj_type,);
                }
            }*/
        }
    }

    // Collect garbage.
    // This traces all references starting from marked_objects.
    pub fn collect_garbage(&mut self) {
        #[cfg(feature = "debug_log_gc")]
        let before: isize = self.current_alloc_size as isize;
        self.trace_references();
        self.sweep();

        self.next_gc = self.current_alloc_size + 100 * 1024 * 1024;

        #[cfg(feature = "debug_log_gc")]
        println!(
            "collected(bytes:{} before:{} after:{} next:{})",
            before - self.current_alloc_size as isize,
            before,
            self.current_alloc_size,
            self.next_gc
        );
    }

    // Mark each object's fields.
    fn trace_references(&mut self) {
        while let Some(obj_header) = self.marked_objects.pop() {
            self.mark_object_fields(obj_header);
        }
    }

    fn mark_object_fields(&mut self, pointer: NonNull<GcHeader>) {
        let object_type = unsafe { &pointer.as_ref().obj_type };
        match object_type {
            ObjectType::Socket => {
                let socket: &Socket = unsafe { mem::transmute(pointer.as_ref()) };
                socket.trace(self);
            }
            ObjectType::Closure => {
                let closure: &Closure = unsafe { mem::transmute(pointer.as_ref()) };
                closure.trace(self);
            }
            ObjectType::Regexp => {
                let x: &Regexp = unsafe { mem::transmute(pointer.as_ref()) };
                x.trace(self);
            }
            ObjectType::RegMatch => {
                let x: &RegMatch = unsafe { mem::transmute(pointer.as_ref()) };
                x.trace(self);
            }
            ObjectType::Vox => {
                let vox: &Vox = unsafe { mem::transmute(pointer.as_ref()) };
                vox.trace(self);
            }
            ObjectType::Pair => {
                let pair: &Pair = unsafe { mem::transmute(pointer.as_ref()) };
                pair.trace(self);
            }
            ObjectType::CustomBinaryInputPort => {
                let port: &CustomBinaryInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::CustomBinaryInputOutputPort => {
                let port: &CustomBinaryInputOutputPort =
                    unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::CustomBinaryOutputPort => {
                let port: &CustomBinaryOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::CustomTextOutputPort => {
                let port: &CustomTextOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::CustomTextInputPort => {
                let port: &CustomTextInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::CustomTextInputOutputPort => {
                let port: &CustomTextInputOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::Vector => {
                let vector: &Vector = unsafe { mem::transmute(pointer.as_ref()) };
                vector.trace(self);
            }
            ObjectType::SimpleStruct => {
                let s: &SimpleStruct = unsafe { mem::transmute(pointer.as_ref()) };
                s.trace(self);
            }
            ObjectType::EqHashtable => {
                let hashtable: &EqHashtable = unsafe { mem::transmute(pointer.as_ref()) };
                hashtable.trace(self);
            }
            ObjectType::EqvHashtable => {
                let hashtable: &EqvHashtable = unsafe { mem::transmute(pointer.as_ref()) };
                hashtable.trace(self);
            }
            ObjectType::GenericHashtable => {
                let hashtable: &GenericHashtable = unsafe { mem::transmute(pointer.as_ref()) };
                hashtable.trace(self);
            }
            ObjectType::FileInputPort => {
                let port: &FileInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::TranscodedInputPort => {
                let port: &TranscodedInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }

            ObjectType::TranscodedInputOutputPort => {
                let port: &TranscodedInputOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }

            ObjectType::Continuation => {
                let c: &Continuation = unsafe { mem::transmute(pointer.as_ref()) };
                c.trace(self);
            }
            ObjectType::ContinuationStack => {
                let c: &ContinuationStack = unsafe { mem::transmute(pointer.as_ref()) };
                c.trace(self);
            }
            ObjectType::StringInputPort => {
                let s: &StringInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                s.trace(self);
            }
            ObjectType::TranscodedOutputPort => {
                let t: &TranscodedOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                t.trace(self);
            }
            ObjectType::Transcoder => {
                let t: &Transcoder = unsafe { mem::transmute(pointer.as_ref()) };
                t.trace(self);
            }
            ObjectType::Compnum => {
                let c: &Compnum = unsafe { mem::transmute(pointer.as_ref()) };
                c.trace(self);
            }
            ObjectType::BytevectorInputPort => {
                let port: &BytevectorInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::BytevectorOutputPort => {
                let port: &BytevectorOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::BinaryFileInputPort => {
                let port: &BinaryFileInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::BinarySocketInputOutputPort => {
                let port: &BinarySocketInputOutputPort =
                    unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::BinaryFileInputOutputPort => {
                let port: &BinaryFileInputOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::BinaryFileOutputPort => {
                let port: &BinaryFileOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::FileOutputPort => {
                let port: &FileOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::StdOutputPort => {
                let port: &StdOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::StdInputPort => {
                let port: &StdInputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::StdErrorPort => {
                let port: &StdErrorPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::StringOutputPort => {
                let port: &StringOutputPort = unsafe { mem::transmute(pointer.as_ref()) };
                port.trace(self);
            }
            ObjectType::SString => {
                let obj: &SString = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::Symbol => {
                let obj: &Symbol = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::Procedure => {
                let obj: &Procedure = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::Ratnum => {
                let obj: &Ratnum = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::Latin1Codec => {
                let obj: &Latin1Codec = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::UTF8Codec => {
                let obj: &UTF8Codec = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::UTF16Codec => {
                let obj: &UTF16Codec = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::Bignum => {
                let obj: &Bignum = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
            ObjectType::Bytevector => {
                let obj: &Bytevector = unsafe { mem::transmute(pointer.as_ref()) };
                obj.trace(self);
            }
        }
    }

    #[cfg(feature = "test_gc_size")]
    pub fn should_gc(&self) -> bool {
        true
    }

    #[cfg(not(feature = "test_gc_size"))]
    pub fn should_gc(&self) -> bool {
        self.current_alloc_size > self.next_gc
    }

    #[cfg(feature = "test_gc_size")]
    fn free(&mut self, object_ptr: &mut GcHeader) {
        let object_type = object_ptr.obj_type;

        let header: &GcHeader = object_ptr;

        let free_size = match object_type {
            ObjectType::Symbol => 0,
            ObjectType::Procedure => 0,
            ObjectType::CustomBinaryInputPort => {
                let x: &CustomBinaryInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::Regexp => {
                let x: &Regexp = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::RegMatch => {
                let x: &RegMatch = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::CustomBinaryInputOutputPort => {
                let x: &CustomBinaryInputOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::CustomBinaryOutputPort => {
                let x: &CustomBinaryOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::CustomTextInputPort => {
                let x: &CustomTextInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::CustomTextInputOutputPort => {
                let x: &CustomTextInputOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::CustomTextOutputPort => {
                let x: &CustomTextOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::SString => {
                let sstring: &SString = unsafe { mem::transmute(header) };
                std::mem::size_of_val(sstring)
            }
            ObjectType::Bignum => {
                let n: &Bignum = unsafe { mem::transmute(header) };
                std::mem::size_of_val(n)
            }
            ObjectType::BinarySocketInputOutputPort => {
                let n: &BinarySocketInputOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(n)
            }
            ObjectType::Socket => {
                let n: &Socket = unsafe { mem::transmute(header) };
                std::mem::size_of_val(n)
            }
            ObjectType::Ratnum => {
                let n: &Ratnum = unsafe { mem::transmute(header) };
                std::mem::size_of_val(n)
            }
            ObjectType::Compnum => {
                let n: &Compnum = unsafe { mem::transmute(header) };
                std::mem::size_of_val(n)
            }
            ObjectType::Continuation => {
                let n: &Continuation = unsafe { mem::transmute(header) };
                std::mem::size_of_val(n)
            }
            ObjectType::ContinuationStack => {
                let n: &ContinuationStack = unsafe { mem::transmute(header) };
                std::mem::size_of_val(n)
            }
            ObjectType::Closure => {
                let closure: &Closure = unsafe { mem::transmute(header) };
                std::mem::size_of_val(closure)
            }
            ObjectType::FileOutputPort => {
                let port: &FileOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::FileInputPort => {
                let port: &FileInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::BinaryFileInputPort => {
                let port: &BinaryFileInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::BytevectorInputPort => {
                let port: &BytevectorInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::BytevectorOutputPort => {
                let port: &BytevectorOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::BinaryFileOutputPort => {
                let port: &BinaryFileOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::StringInputPort => {
                let port: &StringInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::StringOutputPort => {
                let port: &StringOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::StdOutputPort => {
                let port: &StdOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::StdInputPort => {
                let port: &StdInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::StdErrorPort => {
                let port: &StdErrorPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(port)
            }
            ObjectType::Vox => {
                let vox: &Vox = unsafe { mem::transmute(header) };
                std::mem::size_of_val(vox)
            }
            ObjectType::Pair => {
                let pair: &Pair = unsafe { mem::transmute(header) };
                std::mem::size_of_val(pair)
            }
            ObjectType::SimpleStruct => {
                let s: &SimpleStruct = unsafe { mem::transmute(header) };
                std::mem::size_of_val(s)
            }
            ObjectType::Bytevector => {
                let v: &Bytevector = unsafe { mem::transmute(header) };
                std::mem::size_of_val(v)
            }
            ObjectType::Vector => {
                let v: &Vector = unsafe { mem::transmute(header) };
                std::mem::size_of_val(v)
            }
            ObjectType::EqHashtable => {
                let hashtable: &EqHashtable = unsafe { mem::transmute(header) };
                std::mem::size_of_val(hashtable)
            }
            ObjectType::BinaryFileInputOutputPort => {
                let x: &BinaryFileInputOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::EqvHashtable => {
                let x: &EqvHashtable = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::Latin1Codec => {
                let x: &Latin1Codec = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::UTF8Codec => {
                let x: &UTF8Codec = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::UTF16Codec => {
                let x: &UTF16Codec = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::GenericHashtable => {
                let x: &GenericHashtable = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::Transcoder => {
                let x: &Transcoder = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::TranscodedInputPort => {
                let x: &TranscodedInputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::TranscodedInputOutputPort => {
                let x: &TranscodedInputOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
            }
            ObjectType::TranscodedOutputPort => {
                let x: &TranscodedOutputPort = unsafe { mem::transmute(header) };
                std::mem::size_of_val(x)
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
    #[inline(always)]
    fn free(&self, object_ptr: *mut GcHeader) {
        match unsafe { (*object_ptr).obj_type } {
            ObjectType::Socket => {
                let x: *mut Socket = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::BinarySocketInputOutputPort => {
                let x: *mut BinarySocketInputOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Symbol => {
                let x: *mut Symbol = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Regexp => {
                let x: *mut Regexp = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::RegMatch => {
                let x: *mut RegMatch = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Bignum => {
                let x: *mut Bignum = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::BinaryFileInputPort => {
                let x: *mut BinaryFileInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::BinaryFileInputOutputPort => {
                let x: *mut BinaryFileInputOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::BinaryFileOutputPort => {
                let x: *mut BinaryFileOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Bytevector => {
                let x: *mut Bytevector = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::BytevectorInputPort => {
                let x: *mut BytevectorInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::BytevectorOutputPort => {
                let x: *mut BytevectorOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Closure => {
                let x: *mut Closure = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Compnum => {
                let x: *mut Compnum = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Continuation => {
                let x: *mut Continuation = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::ContinuationStack => {
                let x: *mut ContinuationStack = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::CustomTextInputPort => {
                let x: *mut CustomTextInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::CustomTextInputOutputPort => {
                let x: *mut CustomTextInputOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::CustomTextOutputPort => {
                let x: *mut CustomTextOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::CustomBinaryInputPort => {
                let x: *mut CustomBinaryInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::CustomBinaryInputOutputPort => {
                let x: *mut CustomBinaryInputOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::CustomBinaryOutputPort => {
                let x: *mut CustomBinaryOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::EqHashtable => {
                let x: *mut EqHashtable = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::EqvHashtable => {
                let x: *mut EqvHashtable = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::FileInputPort => {
                let x: *mut FileInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::FileOutputPort => {
                let x: *mut FileOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Latin1Codec => {
                let x: *mut Latin1Codec = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::UTF8Codec => {
                let x: *mut UTF8Codec = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::UTF16Codec => {
                let x: *mut UTF16Codec = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::GenericHashtable => {
                let x: *mut GenericHashtable = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Pair => {
                let x: *mut Pair = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Procedure => {
                let x: *mut Procedure = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Ratnum => {
                let x: *mut Ratnum = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::SimpleStruct => {
                let x: *mut SimpleStruct = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::StdErrorPort => {
                let x: *mut StdErrorPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::StdInputPort => {
                let x: *mut StdInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::StdOutputPort => {
                let x: *mut StdOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::SString => {
                let x: *mut SString = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::StringInputPort => {
                let x: *mut StringInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::StringOutputPort => {
                let x: *mut StringOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Transcoder => {
                let x: *mut Transcoder = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::TranscodedInputPort => {
                let x: *mut TranscodedInputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::TranscodedInputOutputPort => {
                let x: *mut TranscodedInputOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::TranscodedOutputPort => {
                let x: *mut TranscodedOutputPort = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Vector => {
                let x: *mut Vector = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
            ObjectType::Vox => {
                let x: *mut Vox = unsafe { mem::transmute(object_ptr) };
                unsafe {
                    drop(Box::from_raw(x));
                }
            }
        }
    }

    fn sweep(&mut self) {
        let mut total = 0;
        let mut stayed = 0;
        let mut _freed = 0;
        let mut previous: Option<NonNull<GcHeader>> = None;
        let mut current: Option<NonNull<GcHeader>> = self.first;
        let mut counter_map: HashMap<ObjectType, usize> = HashMap::new();
        let mut freed_map: HashMap<ObjectType, usize> = HashMap::new();
        while let Some(mut object) = current {
            total += 1;
            unsafe {
                let object_ptr = object.as_mut();
                current = object_ptr.next;
                if object_ptr.marked {
                    if self.show_stats {
                        stayed += 1;
                        *counter_map.entry(object_ptr.obj_type).or_insert(0) += 1;
                    }
                    object_ptr.marked = false;
                    previous = Some(object);
                } else {
                    if self.show_stats {
                        *freed_map.entry(object_ptr.obj_type).or_insert(0) += 1;
                    }
                    if let Some(mut previous) = previous {
                        previous.as_mut().next = object_ptr.next
                    } else {
                        self.first = object_ptr.next
                    }
                    self.free(object_ptr);
                }
            }
        }

        if self.show_stats {
            let mem = proc_status::mem_usage().unwrap();
            eprintln!(
                "[GC] Mem usage: current={:.1}mb, peak={:.1}mb",
                mem.current as f64 / 1024.0 / 1024.0,
                mem.peak as f64 / 1024.0 / 1024.0
            );

            // Collect the key-value pairs into a Vec
            let mut counter_vec: Vec<(&ObjectType, &usize)> = counter_map.iter().collect();

            // Sort the Vec by value in descending order
            counter_vec.sort_by(|a, b| match b.1.cmp(a.1) {
                Ordering::Less => Ordering::Less,
                Ordering::Greater => Ordering::Greater,
                Ordering::Equal => Ordering::Equal,
            });

            // Iterate through the sorted Vec and print the key-value pairs
            println!("Living Objects");
            for (key, value) in counter_vec {
                eprintln!("  {:?}: {}", key, value);
            }

            // Collect the key-value pairs into a Vec
            let mut freed_vec: Vec<(&ObjectType, &usize)> = freed_map.iter().collect();

            // Sort the Vec by value in descending order
            freed_vec.sort_by(|a, b| match b.1.cmp(a.1) {
                Ordering::Less => Ordering::Less,
                Ordering::Greater => Ordering::Greater,
                Ordering::Equal => Ordering::Equal,
            });

            println!("Freed Objects");
            // Iterate through the sorted Vec and print the key-value pairs
            for (key, value) in freed_vec {
                eprintln!("  {:?}: {}", key, value);
            }

            eprintln!(
                "  {}/{}={:.1}%",
                stayed,
                total,
                (stayed as f64) / (total as f64) * 100.0
            );
        }
    }
}

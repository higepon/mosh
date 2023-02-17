use crate::error;
use crate::gc::{Gc, GcRef};
use crate::gc::{GcHeader, ObjectType};
use crate::numbers::{self, Bignum, Compnum, Flonum, Ratnum};
use crate::op::Op;
use crate::ports::{
    BinaryFileInputPort, BinaryFileOutputPort, BytevectorInputPort, FileInputPort, FileOutputPort,
    StdErrorPort, StdOutputPort, StringInputPort, StringOutputPort, TextOutputPort,
};
use crate::vm::Vm;

use std::cmp::min;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

/// Wrapper of heap allocated or simple stack objects.
#[derive(Copy, Clone, PartialEq, Hash)]
pub enum Object {
    Bignum(GcRef<Bignum>),
    BinaryFileInputPort(GcRef<BinaryFileInputPort>),
    BinaryFileOutputPort(GcRef<BinaryFileOutputPort>),
    Bytevector(GcRef<Bytevector>),
    BytevectorInputPort(GcRef<BytevectorInputPort>),
    Char(char),
    Closure(GcRef<Closure>),
    Continuation(GcRef<Continuation>),
    ContinuationStack(GcRef<ContinuationStack>),
    Compnum(GcRef<Compnum>),
    Eof,
    EqHashtable(GcRef<EqHashtable>),
    False,
    FileInputPort(GcRef<FileInputPort>),
    FileOutputPort(GcRef<FileOutputPort>),
    Fixnum(isize),
    Flonum(Flonum),
    Instruction(Op),
    Nil,
    ObjectPointer(*mut Object),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    ProgramCounter(*const Object),
    Ratnum(GcRef<Ratnum>),
    Regexp(GcRef<Regexp>),
    SimpleStruct(GcRef<SimpleStruct>),
    StdErrorPort(GcRef<StdErrorPort>),
    StdOutputPort(GcRef<StdOutputPort>),
    String(GcRef<SString>),
    StringInputPort(GcRef<StringInputPort>),
    StringOutputPort(GcRef<StringOutputPort>),
    Symbol(GcRef<Symbol>),
    True,
    Unspecified,
    Vector(GcRef<Vector>),
    Vox(GcRef<Vox>),
}

impl Object {
    pub fn to_string(&self) -> String {
        let mut port = StringOutputPort::new();
        port.display(*self).ok();
        port.string()
    }

    pub fn to_short_string(&self) -> String {
        let s = self.to_string();
        s[..min(s.len(), 40)].to_string()
    }

    pub fn is_false(&self) -> bool {
        match self {
            Object::False => true,
            _ => false,
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            Object::True => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        Pair::is_list(*self)
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Object::Pair(_) => true,
            _ => false,
        }
    }

    pub fn is_bytevector(&self) -> bool {
        match self {
            Object::Bytevector(_) => true,
            _ => false,
        }
    }

    pub fn is_vox(&self) -> bool {
        match self {
            Object::Vox(_) => true,
            _ => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            Object::True => true,
            Object::False => true,
            _ => false,
        }
    }
    pub fn is_bignum(&self) -> bool {
        match self {
            Object::Bignum(_) => true,
            _ => false,
        }
    }

    pub fn is_fixnum(&self) -> bool {
        match self {
            Object::Fixnum(_) => true,
            _ => false,
        }
    }
    pub fn is_flonum(&self) -> bool {
        match self {
            Object::Flonum(_) => true,
            _ => false,
        }
    }
    pub fn is_ratnum(&self) -> bool {
        match self {
            Object::Ratnum(_) => true,
            _ => false,
        }
    }
    pub fn is_compnum(&self) -> bool {
        match self {
            Object::Compnum(_) => true,
            _ => false,
        }
    }
    pub fn is_regexp(&self) -> bool {
        match self {
            Object::Regexp(_) => true,
            _ => false,
        }
    }
    pub fn is_nil(&self) -> bool {
        match self {
            Object::Nil => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Object::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Object::String(_) => true,
            _ => false,
        }
    }

    pub fn is_input_port(self) -> bool {
        match self {
            Object::BinaryFileInputPort(_)
            | Object::FileInputPort(_)
            | Object::StringInputPort(_) => true,
            _ => false,
        }
    }

    pub fn is_unspecified(&self) -> bool {
        match self {
            Object::Unspecified => true,
            _ => false,
        }
    }

    pub fn make_bool(pred: bool) -> Self {
        if pred {
            Object::True
        } else {
            Object::False
        }
    }

    pub fn eq(&self, other: &Self) -> bool {
        self == other
    }

    pub fn to_bool(self) -> bool {
        match self {
            Object::True => true,
            Object::False => false,
            _ => {
                panic!("Not a bool object")
            }
        }
    }
    pub fn to_isize(self) -> isize {
        if let Self::Fixnum(n) = self {
            n
        } else {
            panic!("Not a Object::Fixnum but {}", self)
        }
    }
    pub fn to_eq_hashtable(self) -> GcRef<EqHashtable> {
        if let Self::EqHashtable(e) = self {
            e
        } else {
            panic!("Not a Object::EqHashtable")
        }
    }
    pub fn to_flonum(self) -> Flonum {
        if let Self::Flonum(fl) = self {
            fl
        } else {
            panic!("Not a Object::Flonum")
        }
    }
    pub fn to_bignum(self) -> GcRef<Bignum> {
        if let Self::Bignum(b) = self {
            b
        } else {
            panic!("Not a Object::Bignum")
        }
    }
    pub fn to_compnum(self) -> GcRef<Compnum> {
        if let Self::Compnum(c) = self {
            c
        } else {
            panic!("Not a Object::Compnum")
        }
    }
    pub fn to_simple_struct(self) -> GcRef<SimpleStruct> {
        if let Self::SimpleStruct(s) = self {
            s
        } else {
            panic!("Not a Object::SimpleStruct")
        }
    }
    pub fn to_instruction(self) -> Op {
        if let Self::Instruction(p) = self {
            p
        } else {
            panic!("Not a Object::Instruction {}", self)
        }
    }
    pub fn to_pair(self) -> GcRef<Pair> {
        if let Self::Pair(p) = self {
            p
        } else {
            panic!("Not a Object::Pair")
        }
    }
    pub fn to_continuation_stack(self) -> GcRef<ContinuationStack> {
        if let Self::ContinuationStack(c) = self {
            c
        } else {
            panic!("Not a Object::Pair")
        }
    }
    pub fn car_unchecked(self) -> Object {
        self.to_pair().car
    }
    pub fn cdr_unchecked(self) -> Object {
        self.to_pair().cdr
    }
    pub fn to_symbol(self) -> GcRef<Symbol> {
        if let Self::Symbol(s) = self {
            s
        } else {
            panic!("Not a Object::Symbol {}", self)
        }
    }

    pub fn to_vector(self) -> GcRef<Vector> {
        if let Self::Vector(v) = self {
            v
        } else {
            panic!("Not a Object::Vector")
        }
    }

    pub fn to_bytevector(self) -> GcRef<Bytevector> {
        if let Self::Bytevector(v) = self {
            v
        } else {
            panic!("Not a Object::Bytevector")
        }
    }

    pub fn to_vox(self) -> GcRef<Vox> {
        if let Self::Vox(v) = self {
            v
        } else {
            panic!("Not a Object::Vox")
        }
    }

    pub fn to_closure(self) -> GcRef<Closure> {
        if let Self::Closure(c) = self {
            c
        } else {
            panic!("Not a Object::Closure but {}", self)
        }
    }

    pub fn to_procedure(self) -> GcRef<Procedure> {
        if let Self::Procedure(p) = self {
            p
        } else {
            panic!("Not a Object::Procedure but {}", self)
        }
    }

    pub fn is_port(self) -> bool {
        match self {
            Object::BinaryFileInputPort(_)
            | Object::BinaryFileOutputPort(_)
            | Object::FileInputPort(_)
            | Object::FileOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::StdOutputPort(_)
            | Object::StringInputPort(_)
            | Object::StringOutputPort(_) => true,
            _ => false,
        }
    }

    pub fn is_output_port(self) -> bool {
        match self {
            Object::BinaryFileOutputPort(_)
            | Object::FileOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::StdOutputPort(_)
            | Object::StringOutputPort(_) => true,
            _ => false,
        }
    }

    pub fn is_textual_port(self) -> bool {
        match self {
            Object::FileInputPort(_)
            | Object::FileOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::StdOutputPort(_)
            | Object::StringInputPort(_)
            | Object::StringOutputPort(_) => true,
            _ => false,
        }
    }

    pub fn eqv(&self, other: &Self) -> bool {
        if self.is_number() {
            if other.is_number() {
                if self.is_flonum() && other.is_flonum() {
                    self.to_flonum().eqv(&other.to_flonum())
                } else {
                    let is_exact1 = self.is_exact();
                    let is_exact2 = other.is_exact();
                    if (is_exact1 && !is_exact2) || (!is_exact1 && is_exact2) {
                        false
                    } else {
                        numbers::eqv(*self, *other)
                    }
                }
            } else {
                false
            }
        } else {
            self.eq(&other)
        }
    }

    pub fn neg(&self, gc: &mut Box<Gc>) -> Self {
        match self {
            Object::Fixnum(n) => Object::Fixnum(n * -1),
            Object::Flonum(f) => Object::Flonum(Flonum::new(f.value() * -1.0)),
            Object::Ratnum(r) => Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(-r.ratio))),
            _ => todo!(),
        }
    }

    pub fn obj_type(&self) -> String {
        match self {
            Object::Bignum(_) => todo!(),
            Object::BinaryFileInputPort(_) => todo!(),
            Object::BinaryFileOutputPort(_) => todo!(),
            Object::Bytevector(_) => todo!(),
            Object::BytevectorInputPort(_) => todo!(),
            Object::Char(_) => todo!(),
            Object::Closure(_) => todo!(),
            Object::Continuation(_) => todo!(),
            Object::ContinuationStack(_) => todo!(),
            Object::Compnum(_) => todo!(),
            Object::Eof => todo!(),
            Object::EqHashtable(_) => todo!(),
            Object::False => todo!(),
            Object::FileInputPort(_) => todo!(),
            Object::FileOutputPort(_) => todo!(),
            Object::Fixnum(_) => todo!(),
            Object::Flonum(_) => todo!(),
            Object::Instruction(_) => todo!(),
            Object::Nil => todo!(),
            Object::ObjectPointer(_) => todo!(),
            Object::Pair(_) => todo!(),
            Object::Procedure(_) => todo!(),
            Object::ProgramCounter(_) => todo!(),
            Object::Ratnum(_) => todo!(),
            Object::Regexp(_) => todo!(),
            Object::SimpleStruct(_) => todo!(),
            Object::StdErrorPort(_) => todo!(),
            Object::StdOutputPort(_) => todo!(),
            Object::String(_) => todo!(),
            Object::StringInputPort(_) => todo!(),
            Object::StringOutputPort(_) => todo!(),
            Object::Symbol(_) => todo!(),
            Object::True => todo!(),
            Object::Unspecified => todo!(),
            Object::Vector(_) => todo!(),
            Object::Vox(_) => todo!(),
        }
    }
}

// For HashMap<Object, Object>
impl Eq for Object {}

// This is for debug
impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::StdOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::StdErrorPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::Bignum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::BytevectorInputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Continuation(c) => {
                write!(f, "{}", unsafe { c.pointer.as_ref() })
            }
            Object::ContinuationStack(c) => {
                write!(f, "{}", unsafe { c.pointer.as_ref() })
            }
            Object::Ratnum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Compnum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::StringInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::StringOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::FileInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::BinaryFileOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::BinaryFileInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::FileOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::Regexp(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::Char(c) => {
                write!(f, "{}", c)
            }
            Object::Flonum(n) => {
                write!(f, "{}", n)
            }
            Object::Fixnum(n) => {
                write!(f, "{}", n)
            }
            Object::Instruction(op) => {
                write!(f, "#<instruction {}>", op)
            }
            Object::Vox(obj) => {
                write!(f, "#<vox {}>", obj.value)
            }
            Object::Closure(closure) => {
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::Pair(pair) => {
                write!(f, "{}", unsafe { pair.pointer.as_ref() })
            }
            Object::String(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
            Object::Symbol(symbol) => {
                write!(f, "{}", unsafe { symbol.pointer.as_ref() })
            }
            Object::Eof => {
                write!(f, "#<eof>")
            }
            Object::True => {
                write!(f, "#t")
            }
            Object::False => {
                write!(f, "#f")
            }
            Object::ObjectPointer(v) => {
                write!(f, "#<stack pointer {:?}>", v)
            }
            Object::ProgramCounter(v) => {
                write!(f, "#<program counter {:?}>", v)
            }
            Object::Unspecified => {
                write!(f, "#<unspecified>")
            }
            Object::Nil => {
                write!(f, "()")
            }
            Object::Procedure(proc) => {
                write!(f, "#<procedure {}>", proc.name)
            }
            Object::Vector(vector) => {
                write!(f, "{}", unsafe { vector.pointer.as_ref() })
            }
            Object::Bytevector(bytevector) => {
                write!(f, "{}", unsafe { bytevector.pointer.as_ref() })
            }
            Object::SimpleStruct(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
        }
    }
}

// This is for (display ...)
impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::StdOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::StdErrorPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::FileInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::BinaryFileOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::BinaryFileInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::BytevectorInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::FileOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::StringInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::StringOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::Char(c) => {
                write!(f, "{}", c)
            }
            Object::Flonum(n) => {
                write!(f, "{}", n)
            }
            Object::Bignum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Continuation(c) => {
                write!(f, "{}", unsafe { c.pointer.as_ref() })
            }
            Object::ContinuationStack(c) => {
                write!(f, "{}", unsafe { c.pointer.as_ref() })
            }
            Object::Ratnum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Compnum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Regexp(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::Fixnum(n) => {
                write!(f, "{}", n)
            }
            Object::Instruction(op) => {
                write!(f, "#<instruction {}>", op)
            }
            Object::Vox(obj) => {
                write!(f, "#<vox {}>", obj.value)
            }
            Object::Closure(closure) => {
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::Pair(pair) => {
                write!(f, "{}", unsafe { pair.pointer.as_ref() })
            }
            Object::String(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
            Object::Symbol(symbol) => {
                write!(f, "{}", unsafe { symbol.pointer.as_ref() })
            }
            Object::Eof => {
                write!(f, "#<eof>")
            }
            Object::True => {
                write!(f, "#t")
            }
            Object::False => {
                write!(f, "#f")
            }
            Object::ObjectPointer(v) => {
                write!(f, "#<stack pointer {:?}>", v)
            }
            Object::ProgramCounter(v) => {
                write!(f, "#<program counter {:?}>", v)
            }
            Object::Unspecified => {
                write!(f, "#<unspecified>")
            }
            Object::Nil => {
                write!(f, "()")
            }
            Object::Procedure(proc) => {
                write!(f, "#<procedure {}>", proc.name)
            }
            Object::Vector(vector) => {
                write!(f, "{}", unsafe { vector.pointer.as_ref() })
            }
            Object::Bytevector(bytevector) => {
                write!(f, "{}", unsafe { bytevector.pointer.as_ref() })
            }
            Object::SimpleStruct(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
        }
    }
}

/// Vector
#[derive(Debug)]
pub struct Vector {
    pub header: GcHeader,
    pub data: Vec<Object>,
}

impl Vector {
    pub fn new(data: &Vec<Object>) -> Self {
        Vector {
            header: GcHeader::new(ObjectType::Vector),
            data: data.to_owned(),
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn fill(&mut self, obj: Object) {
        self.data.fill(obj);
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#[")?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

/// ByteVector
#[derive(Debug)]
pub struct Bytevector {
    pub header: GcHeader,
    pub data: Vec<u8>,
}

impl Bytevector {
    pub fn new(data: &Vec<u8>) -> Self {
        Bytevector {
            header: GcHeader::new(ObjectType::ByteVector),
            data: data.to_owned(),
        }
    }

    pub fn from_list(list: Object) -> Option<Self> {
        let mut v: Vec<u8> = vec![];
        let mut obj = list;
        loop {
            match obj {
                Object::Pair(p) => match p.car {
                    Object::Fixnum(fx) if fx >= 0 && fx <= 255 => {
                        v.push(fx as u8);
                        obj = p.cdr;
                    }
                    _ => {
                        return None;
                    }
                },
                Object::Nil => {
                    return Some(Self::new(&v));
                }
                _ => return None,
            }
        }
    }

    pub fn ref_u8(&self, i: usize) -> Option<&u8> {
        self.data.get(i)
    }

    pub fn ref_u8_unchecked(&self, i: usize) -> u8 {
        self.data[i]
    }

    pub fn set_u8_unchecked(&mut self, i: usize, v: u8) {
        self.data[i] = v;
    }

    pub fn copy(&self) -> Self {
        Self::new(&self.data)
    }

    pub fn equal(&self, other: &Bytevector) -> bool {
        self.data == other.data
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl Display for Bytevector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#vu8(")?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

/// SimpleStruct
#[derive(Debug)]
pub struct SimpleStruct {
    pub header: GcHeader,
    pub name: Object,
    pub data: Vec<Object>,
    len: usize,
}

impl SimpleStruct {
    pub fn new(name: Object, len: usize) -> Self {
        SimpleStruct {
            header: GcHeader::new(ObjectType::SimpleStruct),
            name: name,
            data: vec![Object::Unspecified; len],
            len: len,
        }
    }

    pub fn initialize(&mut self, args: Object) {
        let mut args = args;
        for i in 0..self.len {
            if args.is_nil() {
                self.data[i] = Object::Unspecified;
            } else {
                let p = args.to_pair();
                self.data[i] = p.car;
                args = p.cdr;
            }
        }
    }

    pub fn field(&self, index: usize) -> Object {
        self.data[index]
    }

    pub fn set(&mut self, index: usize, obj: Object) {
        self.data[index] = obj;
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl Display for SimpleStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#< simple-struct {} ", self.name)?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ">")
    }
}

/// Cons cell
#[derive(Debug)]
pub struct Pair {
    pub header: GcHeader,
    pub car: Object,
    pub cdr: Object,
    pub src: Object,
}

impl Pair {
    pub fn new(first: Object, second: Object) -> Self {
        Pair {
            header: GcHeader::new(ObjectType::Pair),
            car: first,
            cdr: second,
            src: Object::False,
        }
    }

    // Caller should check if obj is_list().
    pub fn list_len(obj: Object) -> usize {
        let mut obj = obj;
        let mut len = 0;
        loop {
            if obj.is_nil() {
                break;
            }
            obj = obj.cdr_unchecked();
            len += 1;
        }
        len
    }

    pub fn is_list(obj: Object) -> bool {
        let mut obj = obj;
        let mut seen = obj;
        loop {
            if obj.is_nil() {
                return true;
            }
            match obj {
                Object::Pair(pair) => {
                    obj = pair.cdr;
                    if obj.is_nil() {
                        return true;
                    }

                    match obj {
                        Object::Pair(pair) => {
                            obj = pair.cdr;
                            match seen {
                                Object::Pair(pair) => {
                                    seen = pair.cdr;
                                    if obj == seen {
                                        // Circular
                                        return false;
                                    }
                                }
                                _ => {
                                    panic!("seen not a pair")
                                }
                            }
                        }
                        _ => {
                            // Dot pair
                            return false;
                        }
                    }
                }
                _ => {
                    // Dot pair.
                    return false;
                }
            }
        }
    }

    fn print_abbreviated(&self, f: &mut fmt::Formatter<'_>) -> bool {
        match self.cdr {
            Object::Pair(cdr) => {
                if !cdr.cdr.is_nil() {
                    return false;
                }
                let car = self.car;
                match car {
                    Object::Symbol(symbol) => {
                        if symbol.string.eq("quote") {
                            match write!(f, "'") {
                                Ok(_) => {
                                    return true;
                                }
                                Err(_) => {
                                    return false;
                                }
                            }
                        }
                        return false;
                    }
                    _ => {
                        return false;
                    }
                }
            }
            _ => {
                return false;
            }
        }
    }

    fn last_pair(p: Object) -> Object {
        let mut o = p;
        loop {
            match o {
                Object::Pair(pair) => {
                    let kdr = pair.cdr;
                    if kdr.is_nil() {
                        return o;
                    } else {
                        o = kdr;
                    }
                }
                _ => {
                    panic!("last_pair: pair requied but got {}", o);
                }
            }
        }
    }

    // append!
    pub fn append_destructive(l1: Object, l2: Object) -> Object {
        if l1.is_nil() {
            return l2;
        }
        let last = Self::last_pair(l1);
        match last {
            Object::Pair(mut pair) => {
                pair.cdr = l2;
                return l1;
            }
            _ => {
                panic!("never reached");
            }
        }
    }

    pub fn reverse(gc: &mut Box<Gc>, lst: Object) -> Object {
        let mut ret = Object::Nil;
        let mut p = lst;
        loop {
            if !p.is_pair() {
                break;
            }
            ret = gc.cons(p.car_unchecked(), ret);
            p = p.cdr_unchecked();
        }
        return ret;
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let abbreviated = self.print_abbreviated(f);
        let mut e = self.cdr;
        if abbreviated {
            match e {
                Object::Pair(pair) => {
                    let car_str = pair.car.to_string();
                    write!(f, "{}", car_str)?;
                    e = pair.cdr;
                }
                _ => panic!("should not reach"),
            }
        } else {
            let car_str = self.car.to_string();
            write!(f, "({}", car_str)?;
        }

        loop {
            match e {
                Object::Pair(pair) => {
                    write!(f, " ")?;
                    write!(f, "{}", pair.car)?;
                    e = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    write!(f, " . ")?;
                    write!(f, "{}", e)?;
                    break;
                }
            }
        }
        if !abbreviated {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl PartialEq for Pair {
    fn eq(&self, other: &Self) -> bool {
        (self.car == other.car) && (self.cdr == other.cdr)
    }
}

/// Vox
#[derive(Debug)]
pub struct Vox {
    pub header: GcHeader,
    pub value: Object,
}

impl Vox {
    pub fn new(value: Object) -> Self {
        Vox {
            header: GcHeader::new(ObjectType::Vox),
            value: value,
        }
    }
}

impl Display for Vox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Vox({})", self.value)
    }
}

/// SString (Sceheme String)
pub struct SString {
    pub header: GcHeader,
    pub string: String,
}

impl SString {
    pub fn new(string: &str) -> Self {
        SString {
            header: GcHeader::new(ObjectType::String),
            string: string.to_string(),
        }
    }
}

impl Display for SString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl Debug for SString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.string)
    }
}

impl PartialEq for SString {
    fn eq(&self, other: &Self) -> bool {
        self.string.eq(&other.string)
    }
}

impl Deref for SString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl DerefMut for SString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.string
    }
}

/// Symbol
#[derive(Debug)]
pub struct Symbol {
    pub header: GcHeader,
    pub string: String,
}

impl Symbol {
    pub fn new(string: String) -> Self {
        Symbol {
            header: GcHeader::new(ObjectType::Symbol),
            string: string,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

/// Procedures written in Rust.
pub struct Procedure {
    pub header: GcHeader,
    pub func: fn(&mut Vm, &mut [Object]) -> error::Result<Object>,
    pub name: String,
}

impl Procedure {
    pub fn new(func: fn(&mut Vm, &mut [Object]) -> error::Result<Object>, name: String) -> Self {
        Procedure {
            header: GcHeader::new(ObjectType::Procedure),
            func: func,
            name: name,
        }
    }
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<procedure>")
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<procedure>")
    }
}

/// Regexp.
pub struct Regexp {
    pub header: GcHeader,
}

impl Regexp {}

impl fmt::Debug for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("#<regexp>")
    }
}

impl Display for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<regexp>")
    }
}

/// Continuation.
pub struct Continuation {
    pub header: GcHeader,
    pub shift_size: isize,
    pub stack: Object,
    pub winders: Object,
}

impl Continuation {
    pub fn new(shift_size: isize, stack: Object, winders: Object) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Continuation),
            shift_size: shift_size,
            stack: stack,
            winders: winders,
        }
    }
}

impl fmt::Debug for Continuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("#<continuation>")
    }
}

impl Display for Continuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<continuation>")
    }
}

/// Continuation.
pub struct ContinuationStack {
    pub header: GcHeader,
    pub data: Vec<Object>,
}

impl ContinuationStack {
    pub fn new(source: &[Object]) -> Self {
        let mut c = Self {
            header: GcHeader::new(ObjectType::ContinuationStack),
            data: vec![],
        };
        c.data.extend(source);
        c
    }

    pub fn restore(&self, dest: &mut Vec<Object>) -> usize {
        for (index, element) in self.data.iter().enumerate() {
            dest[index] = *element;
        }
        self.data.len()
    }
}

impl fmt::Debug for ContinuationStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("#<continuation>")
    }
}

impl Display for ContinuationStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<continuation>")
    }
}

/// Closure
#[derive(Debug)]
pub struct Closure {
    pub header: GcHeader,
    pub ops: *const Object,
    pub ops_len: usize,
    pub argc: isize,
    pub is_optional_arg: bool,
    pub free_vars: Vec<Object>,
    pub prev: Object,
    pub src: Object,
}

impl Closure {
    pub fn new(
        ops: *const Object,
        ops_len: usize,
        argc: isize,
        is_optional_arg: bool,
        free_vars: Vec<Object>,
        src: Object,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            ops,
            ops_len: ops_len,
            argc: argc,
            is_optional_arg: is_optional_arg,
            free_vars: free_vars,
            prev: Object::Unspecified,
            src: src,
        }
    }

    pub fn refer_free(&self, n: usize) -> Object {
        self.free_vars[n]
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

/// EqHashtable
#[derive(Debug)]
pub struct EqHashtable {
    pub header: GcHeader,
    pub hash_map: HashMap<Object, Object>,
    pub is_mutable: bool,
}

impl EqHashtable {
    pub fn new() -> Self {
        EqHashtable {
            header: GcHeader::new(ObjectType::EqHashtable),
            hash_map: HashMap::new(),
            is_mutable: true,
        }
    }

    pub fn get(&self, key: Object, default: Object) -> Object {
        match self.hash_map.get(&key) {
            Some(value) => *value,
            _ => default,
        }
    }

    pub fn copy(&self) -> Self {
        let mut h = Self::new();
        h.is_mutable = self.is_mutable;
        h.hash_map.clone_from(&self.hash_map);
        h
    }

    pub fn contains(&self, key: Object) -> bool {
        match self.hash_map.get(&key) {
            Some(_) => true,
            _ => false,
        }
    }

    pub fn set(&mut self, key: Object, value: Object) {
        match self.hash_map.insert(key, value) {
            _ => (),
        }
    }

    pub fn size(&self) -> usize {
        self.hash_map.len()
    }

    pub fn delte(&mut self, key: Object) {
        match self.hash_map.remove(&key) {
            _ => (),
        }
    }

    pub fn is_mutable(&self) -> bool {
        return self.is_mutable;
    }

    pub fn clear(&mut self) {
        self.hash_map.clear()
    }
}

impl Display for EqHashtable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<eq-hashtable>")
    }
}

/// Tests.
#[cfg(test)]
pub mod tests {

    use std::{mem, ptr::NonNull};

    use super::*;
    use crate::gc::Gc;
    use regex::Regex;

    // Helpers.
    fn procedure1(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
        assert_eq!(args.len(), 1);
        Ok(args[0])
    }

    /*
    #[test]
    fn test_input_port() {
        match InputPort::open("file_not_exists") {
            Ok(_) => {}
            Err(e) => {
                println!("port error {:?}", e);
            }
        }
    }
    */

    #[test]
    fn test_symbol() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("define".to_owned()));
        let symbol = Object::Symbol(symbol);
        match symbol {
            Object::Symbol(s) => {
                assert_eq!(s.string, "define");
            }
            _ => {
                panic!("not a symbo");
            }
        }
    }

    #[test]
    fn test_procedure() {
        let mut vm = Vm::new();
        let p = vm.gc.alloc(Procedure::new(procedure1, "proc1".to_owned()));
        let mut stack = [Object::Fixnum(1), Object::Fixnum(2)];
        match (p.func)(&mut vm, &mut stack[0..1]) {
            Ok(Object::Fixnum(1)) => {}
            _ => {
                panic!("Wrong return value");
            }
        }
    }

    #[test]
    fn test_simple_to_string() {
        assert_eq!("101", Object::Fixnum(101).to_string());
        assert_eq!("#t", Object::True.to_string());
        assert_eq!("#f", Object::False.to_string());
        assert_eq!("()", Object::Nil.to_string());
        assert_eq!("#<unspecified>", Object::Unspecified.to_string());
    }

    #[test]
    fn test_symbol_to_string() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("hello".to_owned()));
        let symbol = Object::Symbol(symbol);
        println!("hoge symbol{:?}", symbol);
        assert_eq!("hello", symbol.to_string());
    }

    #[test]
    fn test_closure_to_string() {
        let mut gc = Gc::new();
        let closure = gc.alloc(Closure::new(
            [].as_ptr(),
            0,
            0,
            false,
            vec![],
            Object::False,
        ));
        let closure = Object::Closure(closure);

        let re = Regex::new(r"^#<closure\s[^>]+>$").unwrap();
        assert!(re.is_match(&closure.to_string()));
    }

    #[test]
    fn test_procedure_to_string() {
        let mut gc = Gc::new();
        let proc = gc.alloc(Procedure::new(procedure1, "number?".to_owned()));
        let proc = Object::Procedure(proc);
        assert_eq!("#<procedure number?>", proc.to_string());
    }

    #[test]
    fn test_stack_pointer_to_string() {
        let obj = Object::Fixnum(10);
        let pointer: *mut Object = &obj as *const Object as *mut Object;
        let stack_pointer = Object::ObjectPointer(pointer);
        let re = Regex::new(r"^#<stack pointer\s[^>]+>$").unwrap();
        assert!(re.is_match(&stack_pointer.to_string()));
    }

    #[test]
    fn test_vox_to_string() {
        let mut gc = Gc::new();
        let vox = gc.alloc(Vox::new(Object::Fixnum(101)));
        let vox = Object::Vox(vox);
        assert_eq!("#<vox 101>", vox.to_string());

        let symbol = gc.alloc(Symbol::new("my-symbol".to_owned()));
        let symbol = Object::Symbol(symbol);
        let vox = gc.alloc(Vox::new(symbol));
        let vox = Object::Vox(vox);
        assert_eq!("#<vox my-symbol>", vox.to_string());
    }

    #[test]
    fn test_dot_pair_to_string() {
        let mut gc = Gc::new();
        let pair = gc.cons(Object::Fixnum(1), Object::Fixnum(2));
        assert_eq!("(1 . 2)", pair.to_string());
    }

    #[test]
    fn test_simple_pair_to_string() {
        let mut gc = Gc::new();
        let pair1 = gc.cons(Object::Fixnum(2), Object::Nil);
        let pair2 = gc.cons(Object::Fixnum(1), pair1);
        assert_eq!("(1 2)", pair2.to_string());
    }

    #[test]
    fn test_pair_to_string() {
        let mut gc = Gc::new();
        let pair1 = gc.cons(Object::Fixnum(3), Object::Nil);
        let pair2 = gc.cons(Object::Fixnum(2), pair1);
        let pair3 = gc.cons(Object::Fixnum(1), pair2);
        assert_eq!("(1 2 3)", pair3.to_string());
    }

    #[test]
    fn test_quote_to_string() {
        let mut gc = Gc::new();
        let sym_quote = gc.symbol_intern("quote");
        let sym_a = gc.symbol_intern("a");
        let pair = gc.list2(sym_quote, sym_a);
        assert_eq!("'a", pair.to_string());
    }

    #[test]
    fn test_quote_list_to_string() {
        let mut gc = Gc::new();
        let sym_quote = gc.symbol_intern("quote");
        let sym_a = gc.symbol_intern("a");
        let sym_b = gc.symbol_intern("b");
        let list = gc.list2(sym_a, sym_b);
        let pair = gc.list2(sym_quote, list);
        assert_eq!("'(a b)", pair.to_string());
    }

    #[test]
    fn test_sstring_eq() {
        let a = SString::new("abc");
        let b = SString::new("abc");
        assert_eq!(a, b);
    }

    #[test]
    fn test_sstring_deref() {
        let mut a = SString::new("abc");
        *a = "def".to_owned();
        assert_eq!("def".to_string(), a.string);
    }

    #[test]
    fn test_vector_to_string() {
        let mut gc = Gc::new();
        let data = vec![Object::Fixnum(1), Object::Fixnum(2)];
        let v = gc.new_vector(&data);
        assert_eq!("#(1 2)", v.to_string());
    }

    trait InputPort {
        fn read(&self);
    }

    struct StringInputPort {}

    impl Drop for StringInputPort {
        fn drop(&mut self) {
            println!("StringInputPort dropped");
        }
    }

    impl InputPort for StringInputPort {
        fn read(&self) {
            println!("StringInputPort::read called");
        }
    }

    fn do_something(input_port: &mut dyn InputPort) {
        // Do something with input_port.
        // We don't care if it is StringInputPort or not.
        input_port.read();

        // Now we are free the object as InputPort.
        let pointer = input_port as *mut dyn InputPort;
        unsafe { drop(Box::from_raw(pointer)) }
    }

    #[test]
    fn test_string_input_port() {
        let boxed = Box::new(StringInputPort {});
        // The GC manages the pointer going forward.
        let pointer = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        let mut string_input_port: NonNull<StringInputPort> =
            unsafe { mem::transmute(pointer.as_ref()) };

        // Call read() as StringInputPort
        unsafe { string_input_port.as_ref().read() };

        // Use the object as InputPort.
        unsafe { do_something(string_input_port.as_mut()) };
    }

    #[test]
    fn test_ref_ref() {
        {
            let mut a: isize = 3;

            let a_ref: &isize = &a;
            assert_eq!(3, *a_ref);

            let a_ref: &mut isize = &mut a;
            *a_ref = 4;
            assert_eq!(4, *a_ref);
        }

        {
            let a: isize = 3;
            let b: isize = 4;

            let a_ref: &isize = &a;
            assert_eq!(3, *a_ref);

            let b_ref: &isize = &b;
            assert_eq!(4, *b_ref);

            let a_ref_ref: &&isize = &&a;
            assert_eq!(3, **a_ref_ref);

            let ref_ref: &mut &isize = &mut &a;
            assert_eq!(3, **ref_ref);
            *ref_ref = &b;
            assert_eq!(4, **ref_ref);
        }
    }
}

use crate::error::ErrorType;
use crate::gc::{Gc, GcRef, Trace};
use crate::gc::{GcHeader, ObjectType};
use crate::numbers::{self, Bignum, Compnum, Flonum, Ratnum};
use crate::op::Op;
use crate::ports::{
    BinaryFileInputOutputPort, BinaryFileInputPort, BinaryFileOutputPort, BytevectorInputPort,
    BytevectorOutputPort, CustomBinaryInputOutputPort, CustomBinaryInputPort,
    CustomBinaryOutputPort, CustomTextInputOutputPort, CustomTextInputPort, CustomTextOutputPort,
    FileInputPort, FileOutputPort, Latin1Codec, StdErrorPort, StdInputPort, StdOutputPort,
    StringInputPort, StringOutputPort, TextOutputPort, TranscodedInputOutputPort,
    TranscodedInputPort, TranscodedOutputPort, Transcoder, UTF16Codec, UTF8Codec,
};
use crate::vm::Vm;
use crate::{bug, error};

use std::cmp::min;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::ops::{Deref, DerefMut};

/// Wrapper of heap allocated or simple stack objects.
#[derive(Copy, Clone, PartialEq, Hash)]
pub enum Object {
    Bignum(GcRef<Bignum>),
    BinaryFileInputOutputPort(GcRef<BinaryFileInputOutputPort>),
    BinaryFileInputPort(GcRef<BinaryFileInputPort>),
    BinaryFileOutputPort(GcRef<BinaryFileOutputPort>),
    Bytevector(GcRef<Bytevector>),
    BytevectorInputPort(GcRef<BytevectorInputPort>),
    BytevectorOutputPort(GcRef<BytevectorOutputPort>),
    Char(char),
    Closure(GcRef<Closure>),
    Compnum(GcRef<Compnum>),
    Continuation(GcRef<Continuation>),
    ContinuationStack(GcRef<ContinuationStack>),
    CustomTextInputPort(GcRef<CustomTextInputPort>),
    CustomTextInputOutputPort(GcRef<CustomTextInputOutputPort>),
    CustomTextOutputPort(GcRef<CustomTextOutputPort>),
    CustomBinaryInputPort(GcRef<CustomBinaryInputPort>),
    CustomBinaryInputOutputPort(GcRef<CustomBinaryInputOutputPort>),
    CustomBinaryOutputPort(GcRef<CustomBinaryOutputPort>),
    DefinedShared(u32),
    Eof,
    EqHashtable(GcRef<EqHashtable>),
    EqvHashtable(GcRef<EqvHashtable>),
    False,
    FileInputPort(GcRef<FileInputPort>),
    FileOutputPort(GcRef<FileOutputPort>),
    Fixnum(isize),
    Flonum(Flonum),
    GenericHashtable(GcRef<GenericHashtable>),
    Instruction(Op),
    Latin1Codec(GcRef<Latin1Codec>),
    Nil,
    ObjectPointer(*mut Object),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    ProgramCounter(*const Object),
    Ratnum(GcRef<Ratnum>),
    Regexp(GcRef<Regexp>),
    SimpleStruct(GcRef<SimpleStruct>),
    StdErrorPort(GcRef<StdErrorPort>),
    StdInputPort(GcRef<StdInputPort>),
    StdOutputPort(GcRef<StdOutputPort>),
    String(GcRef<SString>),
    StringInputPort(GcRef<StringInputPort>),
    StringOutputPort(GcRef<StringOutputPort>),
    Symbol(GcRef<Symbol>),
    TranscodedInputPort(GcRef<TranscodedInputPort>),
    TranscodedOutputPort(GcRef<TranscodedOutputPort>),
    TranscodedInputOutputPort(GcRef<TranscodedInputOutputPort>),
    Transcoder(GcRef<Transcoder>),
    True,
    Unspecified,
    UTF16Codec(GcRef<UTF16Codec>),
    UTF8Codec(GcRef<UTF8Codec>),
    Vector(GcRef<Vector>),
    Vox(GcRef<Vox>),
}

impl Object {
    pub fn to_string(&self) -> String {
        const SHARED_AWARE: bool = false;
        let mut port = StringOutputPort::new();
        port.display(*self, SHARED_AWARE).ok();
        port.string()
    }

    pub fn to_string_ss(&self) -> String {
        const SHARED_AWARE: bool = true;
        let mut port = StringOutputPort::new();
        port.display(*self, SHARED_AWARE).ok();
        port.string()
    }

    pub fn to_short_string(&self) -> String {
        let s = self.to_string();
        s[..min(s.len(), 40)].to_string()
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Object::False)
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Object::True)
    }

    pub fn is_list(&self) -> bool {
        Pair::is_list(*self)
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, Object::Pair(_))
    }

    pub fn is_bytevector(&self) -> bool {
        matches!(self, Object::Bytevector(_))
    }

    pub fn is_transcoder(&self) -> bool {
        matches!(self, Object::Transcoder(_))
    }

    pub fn is_vox(&self) -> bool {
        matches!(self, Object::Vox(_))
    }
    pub fn is_boolean(&self) -> bool {
        matches!(self, Object::True | Object::False)
    }
    pub fn is_bignum(&self) -> bool {
        matches!(self, Object::Bignum(_))
    }

    pub fn is_fixnum(&self) -> bool {
        matches!(self, Object::Fixnum(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Object::Char(_))
    }
    pub fn is_flonum(&self) -> bool {
        matches!(self, Object::Flonum(_))
    }
    pub fn is_ratnum(&self) -> bool {
        matches!(self, Object::Ratnum(_))
    }
    pub fn is_compnum(&self) -> bool {
        matches!(self, Object::Compnum(_))
    }
    pub fn is_procedure(&self) -> bool {
        matches!(self, Object::Procedure(_))
    }
    pub fn is_object_pointer(&self) -> bool {
        matches!(self, Object::ObjectPointer(_))
    }
    pub fn is_closure(&self) -> bool {
        matches!(self, Object::Closure(_))
    }
    pub fn is_callable(&self) -> bool {
        matches!(self, Object::Closure(_) | Object::Procedure(_))
    }
    pub fn is_regexp(&self) -> bool {
        matches!(self, Object::Regexp(_))
    }
    pub fn is_nil(&self) -> bool {
        matches!(self, Object::Nil)
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, Object::Symbol(_))
    }

    pub fn is_simple_struct(&self) -> bool {
        matches!(self, Object::SimpleStruct(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Object::String(_))
    }

    pub fn is_unspecified(&self) -> bool {
        matches!(self, Object::Unspecified)
    }

    pub fn scheme_eq(&self, other: &Self) -> bool {
        self == other
    }

    pub fn to_bool(self) -> bool {
        match self {
            Object::True => true,
            Object::False => false,
            _ => {
                bug!("Not a bool object")
            }
        }
    }
    pub fn to_isize(self) -> isize {
        if let Self::Fixnum(n) = self {
            n
        } else {
            bug!("Not a Object::Fixnum but {}", self)
        }
    }

    pub fn to_char(self) -> char {
        if let Self::Char(c) = self {
            c
        } else {
            bug!("Not a Object::Char but {}", self)
        }
    }
    pub fn to_eq_hashtable(self) -> GcRef<EqHashtable> {
        if let Self::EqHashtable(e) = self {
            e
        } else {
            bug!("Not a Object::EqHashtable")
        }
    }
    pub fn to_flonum(self) -> Flonum {
        if let Self::Flonum(fl) = self {
            fl
        } else {
            bug!("Not a Object::Flonum")
        }
    }
    pub fn to_sstring(self) -> GcRef<SString> {
        if let Self::String(s) = self {
            s
        } else {
            bug!("Not a Object::String")
        }
    }
    pub fn to_simple_struc(self) -> GcRef<SimpleStruct> {
        if let Self::SimpleStruct(s) = self {
            s
        } else {
            bug!("Not a Object::SimpleStruct")
        }
    }
    pub fn to_bignum(self) -> GcRef<Bignum> {
        if let Self::Bignum(b) = self {
            b
        } else {
            bug!("Not a Object::Bignum")
        }
    }
    pub fn to_bytevector_output_port(self) -> GcRef<BytevectorOutputPort> {
        if let Self::BytevectorOutputPort(b) = self {
            b
        } else {
            bug!("Not a Object::BytevectorOutputPort")
        }
    }
    pub fn to_transcoder(self) -> GcRef<Transcoder> {
        if let Self::Transcoder(b) = self {
            b
        } else {
            bug!("Not a Object::Transcoder")
        }
    }
    pub fn to_latin1_codec(self) -> GcRef<Latin1Codec> {
        if let Self::Latin1Codec(b) = self {
            b
        } else {
            bug!("Not a Object::Latin1Code")
        }
    }
    pub fn to_utf8_codec(self) -> GcRef<UTF8Codec> {
        if let Self::UTF8Codec(b) = self {
            b
        } else {
            bug!("Not a Object::UTF8Codec")
        }
    }
    pub fn to_utf16_codec(self) -> GcRef<UTF16Codec> {
        if let Self::UTF16Codec(b) = self {
            b
        } else {
            bug!("Not a Object::UTF16Codec")
        }
    }
    pub fn to_compnum(self) -> GcRef<Compnum> {
        if let Self::Compnum(c) = self {
            c
        } else {
            bug!("Not a Object::Compnum")
        }
    }
    pub fn to_simple_struct(self) -> GcRef<SimpleStruct> {
        if let Self::SimpleStruct(s) = self {
            s
        } else {
            bug!("Not a Object::SimpleStruct")
        }
    }
    pub fn to_instruction(self) -> Op {
        if let Self::Instruction(p) = self {
            p
        } else {
            bug!("Not a Object::Instruction {}", self)
        }
    }
    pub fn to_pair(self) -> GcRef<Pair> {
        if let Self::Pair(p) = self {
            p
        } else {
            bug!("Not a Object::Pair but got {}", self)
        }
    }
    pub fn to_continuation_stack(self) -> GcRef<ContinuationStack> {
        if let Self::ContinuationStack(c) = self {
            c
        } else {
            bug!("Not a Object::Pair")
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
            bug!("Not a Object::Symbol {}", self)
        }
    }

    pub fn to_vector(self) -> GcRef<Vector> {
        if let Self::Vector(v) = self {
            v
        } else {
            bug!("Not a Object::Vector")
        }
    }

    pub fn to_bytevector(self) -> GcRef<Bytevector> {
        if let Self::Bytevector(v) = self {
            v
        } else {
            bug!("Not a Object::Bytevector")
        }
    }

    pub fn to_vox(self) -> GcRef<Vox> {
        if let Self::Vox(v) = self {
            v
        } else {
            bug!("Not a Object::Vox")
        }
    }

    pub fn to_closure(self) -> GcRef<Closure> {
        if let Self::Closure(c) = self {
            c
        } else {
            bug!("Not a Object::Closure but {}", self)
        }
    }

    pub fn to_procedure(self) -> GcRef<Procedure> {
        if let Self::Procedure(p) = self {
            p
        } else {
            bug!("Not a Object::Procedure but {}", self)
        }
    }

    pub fn is_input_port(self) -> bool {
        matches!(self, Object::BinaryFileInputPort(_)
            | Object::BinaryFileInputOutputPort(_)
            | Object::FileInputPort(_)
            | Object::StdInputPort(_)
            | Object::CustomBinaryInputPort(_)
            | Object::CustomBinaryInputOutputPort(_)
            | Object::CustomTextInputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::TranscodedInputPort(_)
            | Object::BytevectorInputPort(_)
            | Object::StringInputPort(_))
    }

    pub fn is_binary_port(self) -> bool {
        matches!(self, Object::BinaryFileInputPort(_)
            | Object::StdInputPort(_)
            | Object::StdOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::BinaryFileInputOutputPort(_)
            | Object::CustomBinaryOutputPort(_)
            | Object::CustomBinaryInputPort(_)
            | Object::CustomBinaryInputOutputPort(_)
            | Object::BinaryFileOutputPort(_)
            | Object::BytevectorOutputPort(_)
            | Object::BytevectorInputPort(_))
    }

    pub fn is_port(self) -> bool {
        matches!(self, Object::BinaryFileInputPort(_)
            | Object::BinaryFileOutputPort(_)
            | Object::BinaryFileInputOutputPort(_)
            | Object::FileInputPort(_)
            | Object::FileOutputPort(_)
            | Object::CustomBinaryInputPort(_)
            | Object::CustomBinaryInputOutputPort(_)
            | Object::CustomBinaryOutputPort(_)
            | Object::CustomTextInputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::CustomTextOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::StdInputPort(_)
            | Object::TranscodedInputPort(_)
            | Object::TranscodedOutputPort(_)
            | Object::StdOutputPort(_)
            | Object::StringInputPort(_)
            | Object::StringOutputPort(_))
    }

    pub fn is_output_port(self) -> bool {
        matches!(self, Object::BinaryFileInputOutputPort(_)
            | Object::BinaryFileOutputPort(_)
            | Object::BytevectorOutputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::CustomTextOutputPort(_)
            | Object::CustomBinaryInputOutputPort(_)
            | Object::CustomBinaryOutputPort(_)
            | Object::FileInputPort(_)
            | Object::FileOutputPort(_)
            | Object::StdOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::StringOutputPort(_)
            | Object::TranscodedOutputPort(_)
            | Object::TranscodedInputOutputPort(_))
    }

    pub fn is_textual_port(self) -> bool {
        matches!(self, Object::FileInputPort(_)
            | Object::CustomTextInputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::CustomTextOutputPort(_)
            | Object::FileOutputPort(_)
            | Object::TranscodedInputPort(_)
            | Object::TranscodedOutputPort(_)
            | Object::StringInputPort(_)
            | Object::StringOutputPort(_))
    }

    pub fn is_textual_output_port(self) -> bool {
        matches!(self, Object::FileOutputPort(_)
            | Object::CustomTextOutputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::TranscodedOutputPort(_)
            | Object::StringOutputPort(_))
    }

    pub fn is_textual_input_port(self) -> bool {
        matches!(self, Object::TranscodedInputPort(_)
            | Object::FileInputPort(_)
            | Object::CustomTextInputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::StringInputPort(_))
    }

    pub fn is_binary_input_port(self) -> bool {
        matches!(self, Object::BinaryFileInputPort(_)
            | Object::BytevectorInputPort(_)
            | Object::CustomBinaryInputPort(_)
            | Object::StdInputPort(_)
            | Object::CustomBinaryInputOutputPort(_)
            | Object::BinaryFileInputOutputPort(_))
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
            match (*self, *other) {
                (Object::Latin1Codec(_), Object::Latin1Codec(_)) => true,
                (Object::UTF8Codec(_), Object::UTF8Codec(_)) => true,
                (Object::UTF16Codec(_), Object::UTF16Codec(_)) => true,
                _ => self.scheme_eq(other),
            }
        }
    }

    pub fn neg(&self, gc: &mut Box<Gc>) -> Self {
        match self {
            Object::Fixnum(n) => Object::Fixnum(n * -1),
            Object::Flonum(f) => Object::Flonum(Flonum::new(f.value() * -1.0)),
            Object::Ratnum(r) => Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(-r.ratio.clone()))),
            Object::Bignum(_) => numbers::negate(gc, *self),
            _ => todo!("{}", self),
        }
    }

    pub fn obj_type(&self) -> String {
        match self {
            Object::Bignum(_) => todo!(),
            Object::BinaryFileInputPort(_) => todo!(),
            Object::BinaryFileInputOutputPort(_) => todo!(),
            Object::BinaryFileOutputPort(_) => todo!(),
            Object::Bytevector(_) => todo!(),
            Object::BytevectorInputPort(_) => todo!(),
            Object::BytevectorOutputPort(_) => todo!(),
            Object::Char(_) => todo!(),
            Object::Closure(_) => todo!(),
            Object::Continuation(_) => todo!(),
            Object::ContinuationStack(_) => todo!(),
            Object::Compnum(_) => todo!(),
            Object::CustomBinaryInputPort(_) => todo!(),
            Object::CustomBinaryInputOutputPort(_) => todo!(),
            Object::CustomBinaryOutputPort(_) => todo!(),
            Object::CustomTextInputPort(_) => todo!(),
            Object::CustomTextInputOutputPort(_) => todo!(),
            Object::CustomTextOutputPort(_) => todo!(),
            Object::Eof => todo!(),
            Object::EqHashtable(_) => todo!(),
            Object::EqvHashtable(_) => todo!(),
            Object::False => todo!(),
            Object::FileInputPort(_) => todo!(),
            Object::FileOutputPort(_) => todo!(),
            Object::Fixnum(_) => todo!(),
            Object::Flonum(_) => todo!(),
            Object::GenericHashtable(_) => todo!(),
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
            Object::StdInputPort(_) => todo!(),
            Object::StdOutputPort(_) => todo!(),
            Object::String(_) => todo!(),
            Object::StringInputPort(_) => todo!(),
            Object::StringOutputPort(_) => todo!(),
            Object::Symbol(_) => todo!(),
            Object::True => todo!(),
            Object::Unspecified => todo!(),
            Object::Vector(_) => todo!(),
            Object::Vox(_) => todo!(),
            Object::DefinedShared(_) => todo!(),
            Object::Latin1Codec(_) => todo!(),
            Object::UTF8Codec(_) => todo!(),
            Object::UTF16Codec(_) => todo!(),
            Object::Transcoder(_) => todo!(),
            Object::TranscodedInputPort(_) => todo!(),
            Object::TranscodedOutputPort(_) => todo!(),
            Object::TranscodedInputOutputPort(_) => todo!(),
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
            Object::StdInputPort(port) => {
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
            Object::BytevectorOutputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Continuation(c) => {
                write!(f, "{}", unsafe { c.pointer.as_ref() })
            }
            Object::ContinuationStack(c) => {
                write!(f, "{}", unsafe { c.pointer.as_ref() })
            }
            Object::Latin1Codec(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::UTF8Codec(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::UTF16Codec(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Transcoder(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::TranscodedInputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::TranscodedOutputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::TranscodedInputOutputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Ratnum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::Compnum(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::CustomBinaryInputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::CustomBinaryInputOutputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::CustomBinaryOutputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::CustomTextInputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::CustomTextInputOutputPort(n) => {
                write!(f, "{}", unsafe { n.pointer.as_ref() })
            }
            Object::CustomTextOutputPort(n) => {
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
            Object::BinaryFileInputOutputPort(port) => {
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
                write!(f, "#\\x{:x}", *c as u32)
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
                
                let name = if closure.src.is_false() {
                    Object::False
                } else {
                    closure.src.cdr_unchecked()
                };
                write!(
                    f,
                    "#<closure {} {:?}>",
                    name.to_string(),
                    closure.pointer.as_ptr()
                )
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::EqvHashtable(table) => {
                write!(f, "#<eqv-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::GenericHashtable(table) => {
                write!(f, "#<hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::Pair(pair) => {
                write!(f, "{}", unsafe { pair.pointer.as_ref() })
            }
            Object::String(s) => {
                write!(f, "{:?}", unsafe { s.pointer.as_ref() })
            }
            Object::Symbol(symbol) => {
                write!(f, "{:?}", unsafe { symbol.pointer.as_ref() })
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
                write!(f, "{:?}", unsafe { vector.pointer.as_ref() })
            }
            Object::Bytevector(bytevector) => {
                write!(f, "{}", unsafe { bytevector.pointer.as_ref() })
            }
            Object::SimpleStruct(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
            Object::DefinedShared(_) => todo!(),
        }
    }
}

// This is for (display ...)
impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::StdInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
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
            Object::BinaryFileInputOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::BytevectorInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::BytevectorOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::FileOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::CustomBinaryInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::CustomBinaryInputOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::CustomBinaryOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::CustomTextInputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::CustomTextInputOutputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::CustomTextOutputPort(port) => {
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
            Object::Latin1Codec(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::UTF8Codec(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::UTF16Codec(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::Transcoder(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::TranscodedInputPort(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::TranscodedOutputPort(r) => {
                write!(f, "{}", unsafe { r.pointer.as_ref() })
            }
            Object::TranscodedInputOutputPort(r) => {
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
                
                let name = if closure.src.is_false() {
                    Object::False
                } else {
                    closure.src.cdr_unchecked()
                };
                write!(
                    f,
                    "#<closure {} {:?}>",
                    name.to_string(),
                    closure.pointer.as_ptr()
                )
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::EqvHashtable(table) => {
                write!(f, "#<eqv-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::GenericHashtable(table) => {
                write!(f, "#<hashtable {:?}>", table.pointer.as_ptr())
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
            Object::DefinedShared(_) => todo!(),
        }
    }
}

/// Vector
#[derive(Debug)]
#[repr(C)]
pub struct Vector {
    pub header: GcHeader,
    pub data: Vec<Object>,
}

impl Trace for Vector {
    fn trace(&self, gc: &mut Gc) {
        for obj in self.data.iter() {
            gc.mark_object(*obj);
        }
    }
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
#[repr(C)]
pub struct Bytevector {
    pub header: GcHeader,
    pub data: Vec<u8>,
}

impl Trace for Bytevector {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Bytevector {
    pub fn new(data: &Vec<u8>) -> Self {
        Bytevector {
            header: GcHeader::new(ObjectType::ByteVector),
            data: data.to_owned(),
        }
    }

    pub fn is_little_endian() -> bool {
        !cfg!(target_endian = "big")
    }

    pub fn from_list(list: Object) -> Option<Self> {
        let mut v: Vec<u8> = vec![];
        let mut obj = list;
        loop {
            match obj {
                Object::Pair(p) => match p.car {
                    Object::Fixnum(fx) if (0..=255).contains(&fx) => {
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

    pub fn ref_i8(&self, i: usize) -> Option<i8> {
        self.data.get(i).map(|v| *v as i8)
    }

    pub fn ref_u8(&self, i: usize) -> Option<u8> {
        self.data.get(i).copied()
    }

    pub fn ref_u8_unchecked(&self, i: usize) -> u8 {
        self.data[i]
    }

    pub fn set_u8_unchecked(&mut self, i: usize, v: u8) {
        self.data[i] = v;
    }

    pub fn set_i8_unchecked(&mut self, i: usize, v: i8) {
        self.data[i] = v as u8;
    }

    pub fn set_u16_little(&mut self, i: usize, v: u16) -> Option<()> {
        if i + 1 >= self.len() {
            None
        } else {
            self.data[i] = (v & 0xff) as u8;
            self.data[i + 1] = (v >> 8) as u8;
            Some(())
        }
    }

    pub fn set_u16_big(&mut self, i: usize, v: u16) -> Option<()> {
        if i + 1 >= self.len() {
            None
        } else {
            self.data[i + 1] = (v & 0xff) as u8;
            self.data[i] = (v >> 8) as u8;
            Some(())
        }
    }

    pub fn ref_u16_little(&self, i: usize) -> Option<u16> {
        match (self.data.get(i), self.data.get(i + 1)) {
            (Some(lhs), Some(rhs)) => {
                let lhs = *lhs as u16;
                let rhs = *rhs as u16;
                Some((rhs << 8) | lhs)
            }
            _ => None,
        }
    }

    pub fn ref_u16_big(&self, i: usize) -> Option<u16> {
        match (self.data.get(i), self.data.get(i + 1)) {
            (Some(lhs), Some(rhs)) => {
                let lhs = *lhs as u16;
                let rhs = *rhs as u16;
                Some((lhs << 8) | rhs)
            }
            _ => None,
        }
    }

    pub fn ref_s16_little(&self, i: usize) -> Option<i16> {
        self.ref_u16_little(i).map(|x| x as i16)
    }

    pub fn ref_s16_big(&self, i: usize) -> Option<i16> {
        self.ref_u16_big(i).map(|x| x as i16)
    }

    pub fn ref_f32_little(&self, i: usize) -> Option<f32> {
        let data: &[u8; 4] = match self.data[i..i + 4].try_into() {
            Ok(v) => v,
            Err(_) => return None,
        };
        Some(f32::from_le_bytes(*data))
    }

    pub fn ref_f32_big(&self, i: usize) -> Option<f32> {
        let data: &[u8; 4] = match self.data[i..i + 4].try_into() {
            Ok(v) => v,
            Err(_) => return None,
        };
        Some(f32::from_be_bytes(*data))
    }

    pub fn set_f32_little(&mut self, i: usize, v: f32) -> Option<()> {
        let data = v.to_le_bytes();
        match (&mut self.data[i..i + 4]).write(&data) {
            Ok(_) => Some(()),
            Err(_) => None,
        }
    }

    pub fn set_f32_big(&mut self, i: usize, v: f32) -> Option<()> {
        let data = v.to_be_bytes();
        match (&mut self.data[i..i + 4]).write(&data) {
            Ok(_) => Some(()),
            Err(_) => None,
        }
    }

    pub fn ref_f64_little(&self, i: usize) -> Option<f64> {
        let data: &[u8; 8] = match self.data[i..i + 8].try_into() {
            Ok(v) => v,
            Err(_) => return None,
        };
        Some(f64::from_le_bytes(*data))
    }

    pub fn ref_f64_big(&self, i: usize) -> Option<f64> {
        let data: &[u8; 8] = match self.data[i..i + 8].try_into() {
            Ok(v) => v,
            Err(_) => return None,
        };
        Some(f64::from_be_bytes(*data))
    }

    pub fn set_f64_little(&mut self, i: usize, v: f64) -> Option<()> {
        let data = v.to_le_bytes();
        match (&mut self.data[i..i + 8]).write(&data) {
            Ok(_) => Some(()),
            Err(_) => None,
        }
    }

    pub fn set_f64_big(&mut self, i: usize, v: f64) -> Option<()> {
        let data = v.to_be_bytes();
        match (&mut self.data[i..i + 8]).write(&data) {
            Ok(_) => Some(()),
            Err(_) => None,
        }
    }

    pub fn ref_u32_little(&self, i: usize) -> Option<u32> {
        if i + 3 >= self.len() {
            None
        } else {
            let a = (self.data[i + 3] as u32) << 24;
            let b = (self.data[i + 2] as u32) << 16;
            let c = (self.data[i + 1] as u32) << 8;
            let d = self.data[i] as u32;
            Some(a | b | c | d)
        }
    }

    pub fn ref_u32_big(&self, i: usize) -> Option<u32> {
        if i + 3 >= self.len() {
            None
        } else {
            let a = (self.data[i] as u32) << 24;
            let b = (self.data[i + 1] as u32) << 16;
            let c = (self.data[i + 2] as u32) << 8;
            let d = self.data[i + 3] as u32;
            Some(a | b | c | d)
        }
    }

    pub fn ref_s32_little(&self, i: usize) -> Option<i32> {
        self.ref_u32_little(i).map(|x| x as i32)
    }

    pub fn ref_s32_big(&self, i: usize) -> Option<i32> {
        self.ref_u32_big(i).map(|x| x as i32)
    }

    pub fn ref_u64_little(&self, i: usize) -> Option<u64> {
        if i + 7 >= self.len() {
            None
        } else {
            let a = (self.data[i + 7] as u64) << 56;
            let b = (self.data[i + 6] as u64) << 48;
            let c = (self.data[i + 5] as u64) << 40;
            let d = (self.data[i + 4] as u64) << 32;
            let e = (self.data[i + 3] as u64) << 24;
            let f = (self.data[i + 2] as u64) << 16;
            let g = (self.data[i + 1] as u64) << 8;
            let h = self.data[i] as u64;
            Some(a | b | c | d | e | f | g | h)
        }
    }

    pub fn ref_u64_big(&self, i: usize) -> Option<u64> {
        if i + 7 >= self.len() {
            None
        } else {
            let a = (self.data[i] as u64) << 56;
            let b = (self.data[i + 1] as u64) << 48;
            let c = (self.data[i + 2] as u64) << 40;
            let d = (self.data[i + 3] as u64) << 32;
            let e = (self.data[i + 4] as u64) << 24;
            let f = (self.data[i + 5] as u64) << 16;
            let g = (self.data[i + 6] as u64) << 8;
            let h = self.data[i + 7] as u64;
            Some(a | b | c | d | e | f | g | h)
        }
    }

    pub fn ref_s64_little(&self, i: usize) -> Option<i64> {
        self.ref_u64_little(i).map(|x| x as i64)
    }

    pub fn ref_s64_big(&self, i: usize) -> Option<i64> {
        self.ref_u64_big(i).map(|x| x as i64)
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }    

    pub fn fill(&mut self, value: u8) {
        for e in self.data.iter_mut() {
            *e = value;
        }
    }
}

impl Display for Bytevector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#vu8(")?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, " ")?;
            }
        }
        write!(f, ")")
    }
}

/// SimpleStruct
#[derive(Debug)]
#[repr(C)]
pub struct SimpleStruct {
    pub header: GcHeader,
    pub name: Object,
    pub data: Vec<Object>,
    len: usize,
}

impl Trace for SimpleStruct {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
        for obj in self.data.iter() {
            gc.mark_object(*obj)
        }
    }
}

impl SimpleStruct {
    pub fn new(name: Object, len: usize) -> Self {
        SimpleStruct {
            header: GcHeader::new(ObjectType::SimpleStruct),
            name,
            data: vec![Object::Unspecified; len],
            len,
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }    
}

impl Display for SimpleStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<simple-struct {} ", self.name)?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ">")
    }
}

/// Char
pub trait CharExt {
    const CR: char;
    const LF: char;
    const NEL: char;
    const LS: char;
}

impl CharExt for char {
    const CR: char = '\x0d';
    const LF: char = '\x0a';
    const NEL: char = '\u{85}';
    const LS: char = '\u{2028}';
}

/// Cons cell
#[derive(Debug)]
#[repr(C)]
pub struct Pair {
    pub header: GcHeader,
    pub car: Object,
    pub cdr: Object,
    pub src: Object,
}

impl Trace for Pair {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.car);
        gc.mark_object(self.cdr);
        gc.mark_object(self.src);
    }
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
                                    bug!("seen not a pair")
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
                        false
                    }
                    _ => {
                        false
                    }
                }
            }
            _ => {
                false
            }
        }
    }

    fn last_pair(p: Object) -> error::Result<Object> {
        let mut o = p;
        loop {
            match o {
                Object::Pair(pair) => {
                    let kdr = pair.cdr;
                    if kdr.is_nil() {
                        return Ok(o);
                    } else {
                        o = kdr;
                    }
                }
                _ => {
                    return Err(error::Error::new(
                        ErrorType::AssertionViolation,
                        "last_pair",
                        &format!("last_pair: pair requied but got {}", o),
                        &[o],
                    ));
                }
            }
        }
    }

    // append!
    pub fn append_destructive(l1: Object, l2: Object) -> error::Result<Object> {
        if l1.is_nil() {
            return Ok(l2);
        }
        let last = Self::last_pair(l1)?;
        match last {
            Object::Pair(mut pair) => {
                pair.cdr = l2;
                Ok(l1)
            }
            _ => {
                bug!("never reached");
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
        ret
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
                _ => bug!("should not reach"),
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
#[repr(C)]
pub struct Vox {
    pub header: GcHeader,
    pub value: Object,
}

impl Trace for Vox {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.value)
    }
}

impl Vox {
    pub fn new(value: Object) -> Self {
        Vox {
            header: GcHeader::new(ObjectType::Vox),
            value,
        }
    }
}

impl Display for Vox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Vox({})", self.value)
    }
}

/// SString (Sceheme String)
#[repr(C)]
pub struct SString {
    pub header: GcHeader,
    pub string: String,
}

impl Trace for SString {
    fn trace(&self, _gc: &mut Gc) {}
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

fn put_char_handle_special(f: &mut fmt::Formatter<'_>, c: char, in_string: bool) -> fmt::Result {
    const SPACE: char = '\u{20}';
    const DEL: char = '\u{7F}';
    if (c != '\u{a}'
        && c != '\u{d}'
        && c != '\t'
        && c != '\u{7}'
        && c != '\u{8}'
        && c != '\u{B}'
        && c != '\u{C}'
        && c < SPACE)
        || c == DEL
        || c == '\u{80}'
        || c == '\u{ff}'
        || c == '\u{D7FF}'
        || c == '\u{E000}'
        || c == '\u{10FFFF}'
    {
        if in_string {
            write!(f, "\\x{:x}", c as u32)
        } else {
            write!(f, "x{:x}", c as u32)
        }
    } else {
        write!(f, "{}", c)
    }
}

impl Debug for SString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const DOUBLE_QUOTE: char = '"';
        const ESCAPE: char = '\\';
        write!(f, "{}", DOUBLE_QUOTE)?;
        for ch in self.chars() {
            match ch {
                DOUBLE_QUOTE => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "{}", DOUBLE_QUOTE)?;
                }
                ESCAPE => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "{}", ESCAPE)?;
                }
                '\n' => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "n")?;
                }
                '\u{7}' => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "a")?;
                }
                '\u{8}' => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "b")?;
                }
                '\t' => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "t")?;
                }
                '\u{B}' => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "v")?;
                }
                '\r' => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "r")?;
                }
                '\u{C}' => {
                    write!(f, "{}", ESCAPE)?;
                    write!(f, "f")?;
                }
                _ => {
                    put_char_handle_special(f, ch, true)?;
                }
            }
        }
        write!(f, "{}", DOUBLE_QUOTE)
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
#[repr(C)]
pub struct Symbol {
    pub header: GcHeader,
    pub string: String,
}

impl Trace for Symbol {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Symbol {
    pub fn new(string: String) -> Self {
        Symbol {
            header: GcHeader::new(ObjectType::Symbol),
            string,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let content: Vec<char> = self.string.chars().collect();
        let start = content[0];
        let is_bar_symbol = (start == '|') && content[content.len() - 1] == '|';
        if ('0'..='9').contains(&start) || start == ' ' {
            write!(f, "\\x{:x};", start as u32)?;
        } else {
            write!(f, "{}", start)?;
        }

        for ch in content.iter().skip(1) {
            if !is_bar_symbol && *ch == ' ' {
                write!(f, "\\x{:x};", *ch as u32)?;
            } else {
                write!(f, "{}", ch)?;
            }
        }
        Ok(())
    }
}

/// Procedures written in Rust.
#[repr(C)]
pub struct Procedure {
    pub header: GcHeader,
    pub func: fn(&mut Vm, &mut [Object]) -> error::Result<Object>,
    pub name: String,
}

impl Trace for Procedure {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Procedure {
    pub fn new(func: fn(&mut Vm, &mut [Object]) -> error::Result<Object>, name: String) -> Self {
        Procedure {
            header: GcHeader::new(ObjectType::Procedure),
            func,
            name,
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
#[repr(C)]
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
#[repr(C)]
pub struct Continuation {
    pub header: GcHeader,
    pub shift_size: isize,
    pub stack: Object,
    pub winders: Object,
}

impl Trace for Continuation {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.stack);
        gc.mark_object(self.winders);
    }
}

impl Continuation {
    pub fn new(shift_size: isize, stack: Object, winders: Object) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Continuation),
            shift_size,
            stack,
            winders,
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

#[repr(C)]
pub struct ContinuationStack {
    pub header: GcHeader,
    pub data: Vec<Object>,
}

impl Trace for ContinuationStack {
    fn trace(&self, gc: &mut Gc) {
        for obj in self.data.iter() {
            gc.mark_object(*obj);
        }
    }
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

    pub fn restore(&self, dest: &mut [Object]) -> usize {
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
#[repr(C)]
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

impl Trace for Closure {
    fn trace(&self, gc: &mut Gc) {
        // We don't have to trace in closure body because every executed code is traced in other place.
        /*
        if self.ops != null() {
            for i in 0..self.ops_len {
                let op = unsafe { *self.ops.offset(i as isize) };
                gc.mark_object(op);
            }
        }*/

        for obj in self.free_vars.iter() {
            gc.mark_object(*obj);
        }

        gc.mark_object(self.prev);
        gc.mark_object(self.src);
    }
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
            ops_len,
            argc,
            is_optional_arg,
            free_vars,
            prev: Object::Unspecified,
            src,
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

pub trait Hashtable<'a>
where
    Self::Key: Eq + Hash,
{
    type Key;
    fn get_mut_map(&'a mut self) -> &'a mut HashMap<Self::Key, Object>;
    fn get_map(&'a self) -> &'a HashMap<Self::Key, Object>;
    fn is_mutable(&self) -> bool;

    fn get(&'a self, key: Self::Key, default: Object) -> Object {
        match self.get_map().get(&key) {
            Some(value) => *value,
            _ => default,
        }
    }

    fn contains(&'a self, key: Self::Key) -> bool {
        matches!(self.get_map().get(&key), Some(_))
    }

    fn set(&'a mut self, key: Self::Key, value: Object) {
        self.get_mut_map().insert(key, value);
    }

    fn size(&'a self) -> usize {
        self.get_map().len()
    }

    fn delte(&'a mut self, key: Self::Key) {
        self.get_mut_map().remove(&key);
    }

    fn clear(&'a mut self) {
        self.get_mut_map().clear()
    }
}

/// EqHashtable
#[derive(Debug)]
#[repr(C)]
pub struct EqHashtable {
    pub header: GcHeader,
    pub hash_map: HashMap<Object, Object>,
    pub is_mutable: bool,
}

impl Trace for EqHashtable {
    fn trace(&self, gc: &mut Gc) {
        for &obj in self.hash_map.values() {
            gc.mark_object(obj);
        }
        for &obj in self.hash_map.keys() {
            gc.mark_object(obj);
        }
    }
}

impl Default for EqHashtable {
    fn default() -> Self {
        Self::new()
    }
}

impl EqHashtable {
    pub fn new() -> Self {
        EqHashtable {
            header: GcHeader::new(ObjectType::EqHashtable),
            hash_map: HashMap::new(),
            is_mutable: true,
        }
    }

    pub fn copy(&self) -> Self {
        let mut h = Self::new();
        h.is_mutable = self.is_mutable;
        h.hash_map.clone_from(&self.hash_map);
        h
    }
}

impl Display for EqHashtable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<eq-hashtable>")
    }
}

impl<'a> Hashtable<'a> for EqHashtable
where
    Object: Eq + Hash,
{
    type Key = Object;
    fn get_mut_map(&'a mut self) -> &'a mut HashMap<Self::Key, Object> {
        &mut self.hash_map
    }
    fn get_map(&'a self) -> &'a HashMap<Self::Key, Object> {
        &self.hash_map
    }
    fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GenericHashKey {
    pub hash_obj: Object,
    pub org_key: Object,
}

impl GenericHashKey {
    pub fn new(hash_obj: Object, org_key: Object) -> Self {
        Self {
            hash_obj,
            org_key,
        }
    }
}

impl Hash for GenericHashKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // hash_obj is supposed to be exact integer returned by the scheme hash function.
        if self.hash_obj.is_number() {
            numbers::to_string(self.hash_obj, 10).hash(state)
        } else {
            self.hash_obj.hash(state)
        }
    }
}

impl PartialEq for GenericHashKey {
    fn eq(&self, other: &Self) -> bool {
        self.hash_obj.eqv(&other.hash_obj)
    }
}
impl Eq for GenericHashKey {}

/// GenericHashtable
#[derive(Debug)]
#[repr(C)]
pub struct GenericHashtable {
    pub header: GcHeader,
    pub hash_map: HashMap<GenericHashKey, Object>,
    pub hash_func: Object,
    pub eq_func: Object,
    pub is_mutable: bool,
}

impl Trace for GenericHashtable {
    fn trace(&self, gc: &mut Gc) {
        for &obj in self.hash_map.values() {
            gc.mark_object(obj);
        }
        for &key in self.hash_map.keys() {
            gc.mark_object(key.hash_obj);
            gc.mark_object(key.org_key);
        }
        gc.mark_object(self.hash_func);
        gc.mark_object(self.eq_func);
    }
}

impl GenericHashtable {
    pub fn new(hash_func: Object, eq_func: Object) -> Self {
        GenericHashtable {
            header: GcHeader::new(ObjectType::GenericHashtable),
            hash_map: HashMap::new(),
            hash_func,
            eq_func,
            is_mutable: true,
        }
    }

    pub fn copy(&self) -> Self {
        let mut h = Self::new(Object::Unspecified, Object::Unspecified);
        h.hash_func = self.hash_func;
        h.eq_func = self.eq_func;
        h.is_mutable = self.is_mutable;
        h.hash_map.clone_from(&self.hash_map);
        h
    }

    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

impl Display for GenericHashtable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<hashtable>")
    }
}

impl<'a> Hashtable<'a> for GenericHashtable
where
    GenericHashKey: Eq + Hash,
{
    type Key = GenericHashKey;
    fn get_mut_map(&'a mut self) -> &'a mut HashMap<Self::Key, Object> {
        &mut self.hash_map
    }
    fn get_map(&'a self) -> &'a HashMap<Self::Key, Object> {
        &self.hash_map
    }
    fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EqvKey {
    pub obj: Object,
}

impl EqvKey {
    pub fn new(obj: Object) -> Self {
        Self { obj }
    }
}

impl Hash for EqvKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.obj.is_number() {
            numbers::to_string(self.obj, 10).hash(state)
        } else {
            self.obj.hash(state)
        }
    }
}

impl PartialEq for EqvKey {
    fn eq(&self, other: &Self) -> bool {
        self.obj.eqv(&other.obj)
    }
}
impl Eq for EqvKey {}

impl<'a> Hashtable<'a> for EqvHashtable
where
    EqvKey: Eq + Hash,
{
    type Key = EqvKey;
    fn get_mut_map(&'a mut self) -> &'a mut HashMap<Self::Key, Object> {
        &mut self.hash_map
    }
    fn get_map(&'a self) -> &'a HashMap<Self::Key, Object> {
        &self.hash_map
    }
    fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

/// EqvHashtable
#[derive(Debug)]
#[repr(C)]
pub struct EqvHashtable {
    pub header: GcHeader,
    pub hash_map: HashMap<EqvKey, Object>,
    pub is_mutable: bool,
}

impl Trace for EqvHashtable {
    fn trace(&self, gc: &mut Gc) {
        for &obj in self.hash_map.values() {
            gc.mark_object(obj);
        }
        for &key in self.hash_map.keys() {
            gc.mark_object(key.obj);
        }
    }
}

impl Default for EqvHashtable {
    fn default() -> Self {
        Self::new()
    }
}

impl EqvHashtable {
    pub fn new() -> Self {
        EqvHashtable {
            header: GcHeader::new(ObjectType::EqvHashtable),
            hash_map: HashMap::new(),
            is_mutable: true,
        }
    }

    pub fn copy(&self) -> Self {
        let mut h = Self::new();
        h.is_mutable = self.is_mutable;
        h.hash_map.clone_from(&self.hash_map);
        h
    }

    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

impl Display for EqvHashtable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<eqv-hashtable>")
    }
}

/// Tests.
#[cfg(test)]
pub mod tests {

    use std::{mem, ptr::NonNull};

    use super::*;
    use crate::gc::Gc;
    use num_bigint::BigInt;
    use num_traits::FromPrimitive;
    use regex::Regex;

    // Helpers.
    fn procedure1(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
        assert_eq!(args.len(), 1);
        Ok(args[0])
    }

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
                bug!("not a symbol");
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
                bug!("Wrong return value");
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

    #[repr(C)]
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

    #[test]
    fn test_eqv_key() {
        let mut gc = Gc::new();
        let mut x: HashMap<EqvKey, Object> = HashMap::new();
        let s1 = Object::Bignum(gc.alloc(Bignum::new(BigInt::from_isize(0).unwrap())));
        let s2 = Object::Bignum(gc.alloc(Bignum::new(BigInt::from_isize(0).unwrap())));
        assert!(!s1.scheme_eq(&s2));
        assert!(s1.eqv(&s2));
        x.insert(EqvKey::new(s1), Object::Fixnum(3));
        println!("value={:?}", x.get(&EqvKey::new(s2)));
    }
}

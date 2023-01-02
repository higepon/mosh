use std::io::{self, Read};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{gc::Gc, objects::Object, op::Op};

#[derive(FromPrimitive)]
enum Tag {
    Fixnum = 0,
    True = 1,
    False = 2,
    Nil = 3,
    Char = 4,
    Symbol = 5,
    String = 6,
    Pair = 7,
    Vector = 8,
}

#[derive(FromPrimitive)]
enum OpTag {
    Constant = 10,
    Closure = 11,
    ReferLocalBranchNotNull = 12,
    ReferLocal = 13,
    Return = 14,
    Frame = 15,
    CarPush = 16,
    ReferLocalCall = 17,
    PushFrame = 18,
    ReferLocalPush = 19,
    CdrPush = 20,
    ReferGlobalCall = 21,
    Cons = 22,
    DefineGlobal = 23,
    Nop = 24,
    ReferFreePush = 25,
    ReferGlobal = 26,
    TailCall = 27,
    LetFrame = 28,
    Display = 29,
    ReferFreeCall = 30,
    PushEnter = 31,
    Test = 32,
    LocalJmp = 33,
    ConstantPush = 34,
    Push = 35,
    Leave = 36,
    PairP = 37,
    Enter = 38,
    ReferFree = 39,
    Shiftj = 40,
    Halt = 41,
    Undef = 42,
    Car = 43,
    Cdr = 44,
    Receive = 45,
    NumberEqual = 46,
    ReferLocalPushConstant = 47,
    NumberGt = 48,
    NumberLt = 49,
    Box = 50,
    Cdar = 51,
    BranchNotNull = 52,
    Indirect = 53,
    LocalTailCall = 54,
    LocalCall = 55,
    AssignLocal = 56,
    Caar = 57,
    BranchNotNumberEqual = 58,
    Append2 = 59,
    ReferGlobalPush = 60,
    BranchNotLt = 61,
    NumberAddPush = 62,
    Equal = 63,
    Eqv = 64,
    Eq = 65,
    Values = 66,
    BranchNotGe = 67,
    BranchNotEqual = 68,
    PushConstant = 69,
    NumberSubPush = 70,
    ReferLocalPushConstantBranchNotLe = 71,
    BranchNotGt = 72,
    BranchNotEq = 73,
    NumberAdd = 74,
    SetCar = 75,
    SetCdr = 76,
    Cddr = 77,
    NumberMul = 78,
    NumberDiv = 79,
    MakeContinuation = 80,
    Call = 81,
    AssignGlobal = 82,
    VectorLength = 83,
    BranchNotEqv = 84,
    VectorRef = 85,
    Cadr = 86,
    MakeVector = 87,
    BranchNotLe = 88,
    VectorSet = 89,
    AssignFree = 90,
    NotTest = 91,
    ReadChar = 92,
    Not = 93,
    NumberSub = 94,
    NumberGe = 95,
    SimpleStructRef = 96,
    SymbolP = 97,
    VectorP = 98,
    ReferLocalBranchNotLt = 99,
    NullP = 100,
    Vector = 101,
}

// S-expression serializer.
pub struct Fasl<'a> {
    pub bytes: &'a [u8],
}

#[macro_export]
macro_rules! read_sym_num {
    ($self:ident, $gc:ident, $op:ident) => {{
        let sym = $self.read_sexp($gc)?;
        let m = $self.read_sexp($gc)?;
        Ok(Op::$op(sym.to_symbol(), m.to_number()))
    }};
}

#[macro_export]
macro_rules! read_num_constant {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read_sexp($gc)?;
        let c = $self.read_sexp($gc)?;
        Ok(Op::$op(n.to_number(), c))
    }};
}

#[macro_export]
macro_rules! read_num_constant_num {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read_sexp($gc)?;
        let c = $self.read_sexp($gc)?;
        let o = $self.read_sexp($gc)?;
        Ok(Op::$op(n.to_number(), c, o.to_number()))
    }};
}

#[macro_export]
macro_rules! read_sym1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let s = $self.read_sexp($gc)?;
        Ok(Op::$op(s.to_symbol()))
    }};
}

#[macro_export]
macro_rules! read_const1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let c = $self.read_sexp($gc)?;
        Ok(Op::$op(c))
    }};
}

#[macro_export]
macro_rules! read_num1 {
    ($self:ident, $gc:ident, $op:ident, $size:ident) => {{
        let m = $self.read_sexp($gc)?;
        Ok(Op::$op(m.to_number() as $size))
    }};
}

#[macro_export]
macro_rules! read_num2 {
    ($self:ident, $gc:ident, $op:ident, $size:ident, $size2:ident) => {{
        let m = $self.read_sexp($gc)?;
        let n = $self.read_sexp($gc)?;
        Ok(Op::$op(m.to_number() as $size, n.to_number() as $size2))
    }};
}

#[macro_export]
macro_rules! read_num3 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let m = $self.read_sexp($gc)?;
        let n = $self.read_sexp($gc)?;
        let o = $self.read_sexp($gc)?;
        Ok(Op::$op(m.to_number(), n.to_number(), o.to_number()))
    }};
}

impl Fasl<'_> {
    pub fn read_op(&mut self, gc: &mut Gc) -> Result<Op, io::Error> {
        let tag = self.read_op_tag()?;
        match tag {
            OpTag::Append2 => Ok(Op::Append2),
            OpTag::AssignFree => read_num1!(self, gc, AssignFree, usize),
            OpTag::AssignGlobal => read_sym1!(self, gc, AssignGlobal),
            OpTag::AssignLocal => read_num1!(self, gc, AssignLocal, isize),
            OpTag::Box => read_num1!(self, gc, Box, isize),
            OpTag::BranchNotEq => read_num1!(self, gc, BranchNotEq, isize),
            OpTag::BranchNotEqual => read_num1!(self, gc, BranchNotEqual, isize),
            OpTag::BranchNotEqv => read_num1!(self, gc, BranchNotEqv, isize),
            OpTag::BranchNotGe => read_num1!(self, gc, BranchNotGe, isize),
            OpTag::BranchNotGt => read_num1!(self, gc, BranchNotGt, isize),
            OpTag::BranchNotLe => read_num1!(self, gc, BranchNotLe, isize),
            OpTag::BranchNotLt => read_num1!(self, gc, BranchNotLt, isize),
            OpTag::BranchNotNull => read_num1!(self, gc, BranchNotNull, isize),
            OpTag::BranchNotNumberEqual => read_num1!(self, gc, BranchNotNumberEqual, isize),
            OpTag::Caar => Ok(Op::Caar),
            OpTag::Cadr => Ok(Op::Cadr),
            OpTag::Call => read_num1!(self, gc, Call, isize),
            OpTag::Car => Ok(Op::Car),
            OpTag::CarPush => Ok(Op::CarPush),
            OpTag::Cdar => Ok(Op::Cdar),
            OpTag::Cddr => Ok(Op::Cddr),
            OpTag::Cdr => Ok(Op::Cdr),
            OpTag::CdrPush => Ok(Op::CdrPush),
            OpTag::Closure => self.read_closure_op(gc),
            OpTag::Cons => Ok(Op::Cons),
            OpTag::Constant => read_const1!(self, gc, Constant),
            OpTag::ConstantPush => read_const1!(self, gc, ConstantPush),
            OpTag::DefineGlobal => read_sym1!(self, gc, DefineGlobal),
            OpTag::Display => read_num1!(self, gc, Display, isize),
            OpTag::Enter => read_num1!(self, gc, Enter, isize),
            OpTag::Eq => Ok(Op::Eq),
            OpTag::Equal => Ok(Op::Equal),
            OpTag::Eqv => Ok(Op::Eqv),
            OpTag::Frame => read_num1!(self, gc, Frame, isize),
            OpTag::Halt => Ok(Op::Halt),
            OpTag::Indirect => Ok(Op::Indirect),
            OpTag::Leave => read_num1!(self, gc, Leave, isize),
            OpTag::LetFrame => read_num1!(self, gc, LetFrame, isize),
            OpTag::LocalCall => read_num1!(self, gc, LocalCall, isize),
            OpTag::LocalJmp => read_num1!(self, gc, LocalJmp, isize),
            OpTag::LocalTailCall => read_num2!(self, gc, LocalTailCall, isize, isize),
            OpTag::MakeContinuation => read_num1!(self, gc, MakeContinuation, isize),
            OpTag::MakeVector => Ok(Op::MakeVector),
            OpTag::Nop => Ok(Op::Nop),
            OpTag::Not => Ok(Op::Not),
            OpTag::NotTest => read_num1!(self, gc, NotTest, isize),
            OpTag::NumberAdd => Ok(Op::NumberAdd),
            OpTag::NumberAddPush => Ok(Op::NumberAddPush),
            OpTag::NumberDiv => Ok(Op::NumberDiv),
            OpTag::NumberEqual => Ok(Op::Equal),
            OpTag::NumberGe => Ok(Op::NumberGe),
            OpTag::NumberGt => Ok(Op::NumberGt),
            OpTag::NumberLt => Ok(Op::NumberLt),
            OpTag::NumberMul => Ok(Op::NumberMul),
            OpTag::NumberSub => Ok(Op::NumberSub),
            OpTag::NumberSubPush => Ok(Op::NumberSubPush),
            OpTag::PairP => Ok(Op::PairP),
            OpTag::Push => Ok(Op::Push),
            OpTag::PushConstant => read_const1!(self, gc, PushConstant),
            OpTag::PushEnter => read_num1!(self, gc, PushEnter, isize),
            OpTag::PushFrame => read_num1!(self, gc, PushFrame, isize),
            OpTag::ReadChar => Ok(Op::ReadChar),
            OpTag::Receive => read_num2!(self, gc, Receive, usize, usize),
            OpTag::ReferFree => read_num1!(self, gc, ReferFree, usize),
            OpTag::ReferFreeCall => read_num2!(self, gc, ReferFreeCall, usize, isize),
            OpTag::ReferFreePush => read_num1!(self, gc, ReferFreePush, usize),
            OpTag::ReferGlobal => read_sym1!(self, gc, ReferGlobal),
            OpTag::ReferGlobalCall => read_sym_num!(self, gc, ReferGlobalCall),
            OpTag::ReferGlobalPush => read_sym1!(self, gc, ReferGlobalPush),
            OpTag::ReferLocal => read_num1!(self, gc, ReferLocal, isize),
            OpTag::ReferLocalBranchNotNull => {
                read_num2!(self, gc, ReferLocalBranchNotNull, isize, isize)
            }
            OpTag::ReferLocalCall => read_num2!(self, gc, ReferLocalCall, isize, isize),
            OpTag::ReferLocalPush => read_num1!(self, gc, ReferLocalPush, isize),
            OpTag::ReferLocalPushConstant => read_num_constant!(self, gc, ReferLocalPushConstant),
            OpTag::ReferLocalPushConstantBranchNotLe => {
                read_num_constant_num!(self, gc, ReferLocalPushConstantBranchNotLe)
            }
            OpTag::Return => read_num1!(self, gc, Return, isize),
            OpTag::SetCar => Ok(Op::SetCar),
            OpTag::SetCdr => Ok(Op::SetCdr),
            OpTag::Shiftj => read_num3!(self, gc, Shiftj),
            OpTag::TailCall => read_num2!(self, gc, TailCall, isize, isize),
            OpTag::Test => read_num1!(self, gc, Test, isize),
            OpTag::Undef => Ok(Op::Undef),
            OpTag::Values => read_num1!(self, gc, Values, usize),
            OpTag::VectorLength => Ok(Op::VectorLength),
            OpTag::VectorRef => Ok(Op::VectorRef),
            OpTag::VectorSet => Ok(Op::VectorSet),
            OpTag::SimpleStructRef => Ok(Op::SimpleStructRef),
            OpTag::SymbolP => Ok(Op::SymbolP),
            OpTag::VectorP => Ok(Op::VectorP),
            OpTag::ReferLocalBranchNotLt => {
                read_num2!(self, gc, ReferLocalBranchNotLt, usize, isize)
            }
            OpTag::NullP => Ok(Op::NullP),
            OpTag::Vector => read_num1!(self, gc, Vector, usize),
        }
    }

    fn read_closure_op(&mut self, gc: &mut Gc) -> Result<Op, io::Error> {
        let size = self.read_sexp(gc)?;
        let arg_len = self.read_sexp(gc)?;
        let is_optional = !self.read_sexp(gc)?.is_false();
        let num_free_vars = self.read_sexp(gc)?;
        match (size, arg_len, num_free_vars) {
            (Object::Number(size), Object::Number(arg_len), Object::Number(num_free_vars)) => {
                Ok(Op::Closure {
                    size: size as usize,
                    arg_len: arg_len,
                    is_optional_arg: is_optional,
                    num_free_vars: num_free_vars,
                })
            }
            _ => Err(self.create_read_error("invalid closure")),
        }
    }

    pub fn read_sexp(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let tag = self.read_tag()?;
        match tag {
            Tag::Char => self.read_char(),
            Tag::Fixnum => self.read_fixnum(),
            Tag::String => self.read_string(gc),
            Tag::Symbol => self.read_symbol(gc),
            Tag::Pair => self.read_pair(gc),
            Tag::Vector => self.read_vector(gc),
            Tag::True => Ok(Object::True),
            Tag::False => Ok(Object::False),
            Tag::Nil => Ok(Object::Nil),
        }
    }

    fn read_fixnum(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 8];
        self.bytes.read_exact(&mut buf)?;
        let n = isize::from_le_bytes(buf);
        Ok(Object::Number(n))
    }

    fn read_symbol(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);
        let mut chars = vec![];
        for _ in 0..len {
            let mut buf = [0; 4];
            self.bytes.read_exact(&mut buf)?;
            let n = u32::from_le_bytes(buf);
            match char::from_u32(n) {
                Some(c) => chars.push(c),
                None => return Err(self.create_read_error("invalid char")),
            }
        }
        Ok(gc.symbol_intern(&String::from_iter(chars)))
    }

    fn read_string(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);
        let mut chars = vec![];
        for _ in 0..len {
            let mut buf = [0; 4];
            self.bytes.read_exact(&mut buf)?;
            let n = u32::from_le_bytes(buf);
            match char::from_u32(n) {
                Some(c) => chars.push(c),
                None => return Err(self.create_read_error("invalid char")),
            }
        }
        Ok(gc.new_string(&String::from_iter(chars)))
    }

    fn read_vector(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);
        let mut objs = vec![];
        for _ in 0..len {
            objs.push(self.read_sexp(gc)?);
        }
        Ok(gc.new_vector(&objs))
    }

    fn read_char(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 4];
        self.bytes.read_exact(&mut buf)?;
        let n = u32::from_le_bytes(buf);
        match char::from_u32(n) {
            Some(c) => Ok(Object::Char(c)),
            None => Err(self.create_read_error("invalid char")),
        }
    }

    fn read_pair(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let first = self.read_sexp(gc)?;
        let second = self.read_sexp(gc)?;
        Ok(gc.cons(first, second))
    }

    fn create_read_error(&self, reason: &str) -> io::Error {
        io::Error::new(io::ErrorKind::Other, reason)
    }

    fn read_tag(&mut self) -> Result<Tag, io::Error> {
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        Ok(FromPrimitive::from_u8(buf[0]).expect("unknown tag"))
    }

    fn read_op_tag(&mut self) -> Result<OpTag, io::Error> {
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        Ok(FromPrimitive::from_u8(buf[0]).expect("unknown tag"))
    }
}
/// Tests.
#[cfg(test)]
pub mod tests {
    use crate::{compiler, equal::Equal, gc::Gc, objects::Object, op::Op, vm::Vm};

    use super::Fasl;

    #[macro_export]
    macro_rules! assert_equal {
        ($gc:ident, $lhs:ident, $rhs:ident) => {{
            let e = Equal::new();
            if e.is_equal(&mut $gc, &$lhs, &$rhs) {
                assert!(true);
            } else {
                println!("{} is not equal to {}", $lhs, $rhs);
                assert!(false);
            }
        }};
    }

    #[test]
    fn test_constant_number() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[0, 3, 0, 0, 0, 0, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = Object::Number(3);
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_true() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[1];
        let mut fasl = Fasl { bytes };
        let expected = Object::True;
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_false() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[2];
        let mut fasl = Fasl { bytes };
        let expected = Object::False;
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }
    #[test]
    fn test_constant_nil() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[3];
        let mut fasl = Fasl { bytes };
        let expected = Object::Nil;
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }
    #[test]
    fn test_constant_char() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[4, 97, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = Object::Char('a');
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_symbol() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[
            5, 5, 0, 104, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 111, 0, 0, 0,
        ];
        let mut fasl = Fasl { bytes };
        let expected = gc.symbol_intern("hello");
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_string() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[6, 3, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = gc.new_string("abc");
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_simple_pair() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[7, 5, 1, 0, 97, 0, 0, 0, 3];
        let mut fasl = Fasl { bytes };
        let sym = gc.symbol_intern("a");
        let expected = gc.cons(sym, Object::Nil);
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_op() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[10, 7, 5, 1, 0, 97, 0, 0, 0, 3];
        let mut fasl = Fasl { bytes };
        let sym = gc.symbol_intern("a");
        let expected = gc.cons(sym, Object::Nil);
        match fasl.read_op(&mut gc).unwrap() {
            Op::Constant(v) => {
                assert_equal!(gc, expected, v);
            }
            _ => todo!(),
        }
    }

    #[test]
    fn test_closure_op() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[
            11, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 10, 0, 0, 0, 0, 0, 0,
            0,
        ];
        let mut fasl = Fasl { bytes };
        let expected = Op::Closure {
            size: 34,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 10,
        };

        assert_eq!(expected, fasl.read_op(&mut gc).unwrap());
    }

    #[test]
    fn test_refer_local_branch_not_null_op() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[12, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = Op::ReferLocalBranchNotNull(2, 5);
        assert_eq!(expected, fasl.read_op(&mut gc).unwrap());
    }

    #[test]
    fn test_refer_local_op() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[13, 0, 1, 0, 0, 0, 0, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = Op::ReferLocal(1);
        assert_eq!(expected, fasl.read_op(&mut gc).unwrap());
    }

    #[test]
    fn test_baselib() {
        let mut vm = Vm::new();
        let bytes: &[u8] = &[
            11, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 12, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 14, 0, 2, 0, 0, 0, 0, 0, 0, 0, 15, 0, 4, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0,
            0, 0, 0, 0, 16, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 18, 0, 5, 0,
            0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0, 0, 0, 0, 0, 20, 21,
            5, 4, 0, 109, 0, 0, 0, 97, 0, 0, 0, 112, 0, 0, 0, 49, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0,
            0, 22, 14, 0, 2, 0, 0, 0, 0, 0, 0, 0, 23, 5, 4, 0, 109, 0, 0, 0, 97, 0, 0, 0, 112, 0,
            0, 0, 49, 0, 0, 0, 24, 24, 24, 24, 24, 25, 0, 197, 0, 0, 0, 0, 0, 0, 0, 25, 0, 152, 0,
            0, 0, 0, 0, 0, 0, 25, 0, 2, 0, 0, 0, 0, 0, 0, 0, 11, 0, 39, 0, 0, 0, 0, 0, 0, 0, 0, 3,
            0, 0, 0, 0, 0, 0, 0, 1, 0, 3, 0, 0, 0, 0, 0, 0, 0, 12, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 6,
            0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 1, 0, 0, 0, 0, 0, 0, 0, 26,
            5, 9, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0,
            0, 108, 0, 0, 0, 45, 0, 0, 0, 49, 0, 0, 0, 27, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
            0, 0, 0, 0, 0, 14, 0, 3, 0, 0, 0, 0, 0, 0, 0, 28, 0, 11, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 19, 0, 1, 0, 0, 0, 0, 0, 0, 0, 19, 0, 2, 0, 0, 0, 0, 0, 0, 0, 25,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 0, 2, 0, 0, 0, 0, 0, 0, 0, 25, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 29, 0, 6, 0, 0, 0, 0, 0, 0, 0, 15, 0, 5, 0, 0, 0, 0, 0, 0, 0, 25, 0, 1, 0, 0, 0, 0,
            0, 0, 0, 19, 0, 1, 0, 0, 0, 0, 0, 0, 0, 19, 0, 2, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 31, 0, 1, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 32, 0, 6, 0, 0, 0, 0, 0, 0, 0, 25, 0, 5, 0, 0, 0, 0, 0, 0, 0, 19, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 26, 5, 15, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0,
            0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 45, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0,
            113, 0, 0, 0, 117, 0, 0, 0, 105, 0, 0, 0, 99, 0, 0, 0, 107, 0, 0, 0, 27, 0, 2, 0, 0, 0,
            0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 33, 0, 10, 0, 0, 0, 0, 0, 0, 0, 34, 5, 7, 0,
            102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108,
            0, 0, 0, 34, 6, 33, 0, 101, 0, 0, 0, 120, 0, 0, 0, 112, 0, 0, 0, 101, 0, 0, 0, 99, 0,
            0, 0, 116, 0, 0, 0, 101, 0, 0, 0, 100, 0, 0, 0, 32, 0, 0, 0, 115, 0, 0, 0, 97, 0, 0, 0,
            109, 0, 0, 0, 101, 0, 0, 0, 32, 0, 0, 0, 108, 0, 0, 0, 101, 0, 0, 0, 110, 0, 0, 0, 103,
            0, 0, 0, 116, 0, 0, 0, 104, 0, 0, 0, 32, 0, 0, 0, 112, 0, 0, 0, 114, 0, 0, 0, 111, 0,
            0, 0, 112, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 32, 0, 0, 0, 108, 0, 0, 0, 105, 0, 0,
            0, 115, 0, 0, 0, 116, 0, 0, 0, 115, 0, 0, 0, 15, 0, 4, 0, 0, 0, 0, 0, 0, 0, 25, 0, 4,
            0, 0, 0, 0, 0, 0, 0, 25, 0, 3, 0, 0, 0, 0, 0, 0, 0, 30, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            2, 0, 0, 0, 0, 0, 0, 0, 35, 26, 5, 19, 0, 97, 0, 0, 0, 115, 0, 0, 0, 115, 0, 0, 0, 101,
            0, 0, 0, 114, 0, 0, 0, 116, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0,
            0, 0, 118, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 108, 0, 0, 0, 97, 0, 0, 0, 116, 0, 0,
            0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 27, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0,
            0, 0, 0, 0, 0, 36, 0, 1, 0, 0, 0, 0, 0, 0, 0, 14, 0, 3, 0, 0, 0, 0, 0, 0, 0, 23, 5, 7,
            0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0,
            108, 0, 0, 0, 24, 24, 24, 24, 24, 24, 24, 25, 0, 48, 0, 0, 0, 0, 0, 0, 0, 25, 0, 89, 0,
            0, 0, 0, 0, 0, 0, 11, 0, 68, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2,
            0, 0, 0, 0, 0, 0, 0, 12, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 10, 1,
            14, 0, 2, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0, 0, 0, 0, 0, 37, 32, 0, 49, 0, 0, 0,
            0, 0, 0, 0, 28, 0, 14, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 25, 0, 1, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 29, 0,
            4, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0, 0, 0, 0, 0, 16, 13, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 20, 38, 0, 2, 0, 0, 0, 0, 0, 0, 0, 12, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0,
            0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 39, 0, 3, 0, 0, 0, 0, 0, 0, 0, 27, 0, 1, 0, 0,
            0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 33, 0, 31, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 37, 32, 0, 12, 0, 0, 0, 0, 0, 0, 0, 15, 0, 3, 0, 0, 0, 0, 0, 0, 0,
            19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 32, 0, 24, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0, 0, 0, 0, 0, 16, 13, 0, 1, 0, 0,
            0, 0, 0, 0, 0, 20, 40, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 33, 0, 239, 255, 255, 255, 255, 255, 255, 255, 33, 0, 17, 0, 0, 0, 0,
            0, 0, 0, 15, 0, 3, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 3, 0, 0,
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 32, 0, 13, 0, 0, 0, 0, 0, 0, 0, 34, 5, 7, 0,
            102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108,
            0, 0, 0, 15, 0, 4, 0, 0, 0, 0, 0, 0, 0, 34, 6, 40, 0, 116, 0, 0, 0, 114, 0, 0, 0, 97,
            0, 0, 0, 118, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 115, 0, 0, 0, 97, 0, 0, 0, 108, 0,
            0, 0, 32, 0, 0, 0, 114, 0, 0, 0, 101, 0, 0, 0, 97, 0, 0, 0, 99, 0, 0, 0, 104, 0, 0, 0,
            101, 0, 0, 0, 100, 0, 0, 0, 32, 0, 0, 0, 116, 0, 0, 0, 111, 0, 0, 0, 32, 0, 0, 0, 110,
            0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 112, 0, 0, 0, 97, 0, 0, 0, 105, 0, 0,
            0, 114, 0, 0, 0, 32, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 101, 0, 0, 0, 109, 0, 0, 0,
            101, 0, 0, 0, 110, 0, 0, 0, 116, 0, 0, 0, 32, 0, 0, 0, 126, 0, 0, 0, 115, 0, 0, 0, 19,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 30, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
            18, 0, 4, 0, 0, 0, 0, 0, 0, 0, 25, 0, 3, 0, 0, 0, 0, 0, 0, 0, 25, 0, 2, 0, 0, 0, 0, 0,
            0, 0, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 35, 26, 5, 19, 0, 97,
            0, 0, 0, 115, 0, 0, 0, 115, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 116, 0, 0, 0, 105, 0,
            0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 118, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0,
            0, 108, 0, 0, 0, 97, 0, 0, 0, 116, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0,
            27, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 36, 0, 2, 0, 0, 0, 0, 0, 0,
            0, 14, 0, 2, 0, 0, 0, 0, 0, 0, 0, 34, 5, 7, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0,
            0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 15, 0, 4, 0, 0, 0, 0, 0, 0, 0,
            34, 6, 50, 0, 101, 0, 0, 0, 120, 0, 0, 0, 112, 0, 0, 0, 101, 0, 0, 0, 99, 0, 0, 0, 116,
            0, 0, 0, 101, 0, 0, 0, 100, 0, 0, 0, 32, 0, 0, 0, 99, 0, 0, 0, 104, 0, 0, 0, 97, 0, 0,
            0, 105, 0, 0, 0, 110, 0, 0, 0, 32, 0, 0, 0, 111, 0, 0, 0, 102, 0, 0, 0, 32, 0, 0, 0,
            112, 0, 0, 0, 97, 0, 0, 0, 105, 0, 0, 0, 114, 0, 0, 0, 115, 0, 0, 0, 44, 0, 0, 0, 32,
            0, 0, 0, 98, 0, 0, 0, 117, 0, 0, 0, 116, 0, 0, 0, 32, 0, 0, 0, 103, 0, 0, 0, 111, 0, 0,
            0, 116, 0, 0, 0, 32, 0, 0, 0, 126, 0, 0, 0, 114, 0, 0, 0, 44, 0, 0, 0, 32, 0, 0, 0, 97,
            0, 0, 0, 115, 0, 0, 0, 32, 0, 0, 0, 97, 0, 0, 0, 114, 0, 0, 0, 103, 0, 0, 0, 117, 0, 0,
            0, 109, 0, 0, 0, 101, 0, 0, 0, 110, 0, 0, 0, 116, 0, 0, 0, 32, 0, 0, 0, 50, 0, 0, 0,
            19, 0, 1, 0, 0, 0, 0, 0, 0, 0, 30, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0,
            0, 18, 0, 4, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 1, 0, 0, 0, 0,
            0, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 35, 26, 5, 19, 0,
            97, 0, 0, 0, 115, 0, 0, 0, 115, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 116, 0, 0, 0, 105,
            0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 118, 0, 0, 0, 105, 0, 0, 0, 111, 0,
            0, 0, 108, 0, 0, 0, 97, 0, 0, 0, 116, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0,
            0, 27, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 14, 0, 2, 0, 0, 0, 0, 0,
            0, 0, 23, 5, 9, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0,
            108, 0, 0, 0, 108, 0, 0, 0, 45, 0, 0, 0, 49, 0, 0, 0, 24, 24, 24, 24, 24, 24, 24, 24,
            24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25, 0, 152, 0, 0, 0, 0, 0, 0, 0, 11, 0, 32, 0,
            0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 12, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 14, 0, 2, 0, 0, 0, 0, 0, 0, 0, 28, 0, 8,
            0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 29, 0, 3, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 16, 13, 0, 1, 0, 0, 0, 0, 0, 0, 0, 20, 38, 0, 2, 0, 0, 0, 0, 0, 0, 0, 12, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 25, 0, 2, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 39, 0, 1, 0, 0, 0, 0, 0, 0, 0, 27, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            6, 0, 0, 0, 0, 0, 0, 0, 33, 0, 12, 0, 0, 0, 0, 0, 0, 0, 15, 0, 4, 0, 0, 0, 0, 0, 0, 0,
            25, 0, 2, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 32, 0, 7, 0, 0, 0, 0, 0, 0, 0, 13, 0, 1, 0, 0, 0, 0,
            0, 0, 0, 16, 13, 0, 1, 0, 0, 0, 0, 0, 0, 0, 20, 40, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 33, 0, 240, 255, 255, 255, 255, 255, 255,
            255, 36, 0, 2, 0, 0, 0, 0, 0, 0, 0, 14, 0, 2, 0, 0, 0, 0, 0, 0, 0, 23, 5, 15, 0, 102,
            0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0,
            0, 45, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 113, 0, 0, 0, 117, 0, 0, 0, 105, 0, 0, 0,
            99, 0, 0, 0, 107, 0, 0, 0, 24, 24, 24, 24, 24, 24, 24, 24, 41,
        ];
        let mut fasl = Fasl { bytes };
        let mut ops = vec![];
        loop {
            match fasl.read_op(&mut vm.gc) {
                Ok(op) => {
                    ops.push(op);
                }
                Err(_) => {
                    break;
                }
            }
        }
        vm.register_baselib();
        for i in 0..ops.len() {
            if vm.lib_ops[i] != ops[i] {
                println!("{:?} is not equal to {:?}", vm.lib_ops[i], ops[i]);
            }
        }
        assert_eq!(vm.lib_ops.len(), ops.len());
    }
    #[test]
    fn test_compiler() {
        {
            let mut vm = Vm::new();
            let mut fasl = Fasl {
                bytes: compiler::BIN_COMPILER,
            };
            let mut ops = vec![];
            loop {
                match fasl.read_op(&mut vm.gc) {
                    Ok(op) => {
                        ops.push(op);
                    }
                    Err(_) => {
                        break;
                    }
                }
            }
        }
    }
}

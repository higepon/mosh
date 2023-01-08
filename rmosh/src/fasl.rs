use std::io::{self, Read};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{gc::Gc, objects::Object, op::OpOld};

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
    CompilerInsn = 9,
}

#[derive(FromPrimitive)]
enum OpTag {
    CompileError = 0,
    BranchNotLe = 1,
    BranchNotGe = 2,
    BranchNotLt = 3,
    BranchNotGt = 4,
    BranchNotNull = 5,
    BranchNotNumberEqual = 6,
    BranchNotEq = 7,
    BranchNotEqv = 8,
    BranchNotEqual = 9,
    Append2 = 10,
    Call = 11,
    Apply = 12,
    Push = 13,
    AssignFree = 14,
    AssignGlobal = 15,
    AssignLocal = 16,
    Box = 17,
    Caar = 18,
    Cadr = 19,
    Car = 20,
    Cdar = 21,
    Cddr = 22,
    Cdr = 23,
    Closure = 24,
    Cons = 25,
    Constant = 26,
    DefineGlobal = 27,
    Display = 28,
    Enter = 29,
    Eq = 30,
    Eqv = 31,
    Equal = 32,
    Frame = 33,
    Indirect = 34,
    Leave = 35,
    LetFrame = 36,
    List = 37,
    LocalJmp = 38,
    MakeContinuation = 39,
    MakeVector = 40,
    Nop = 41,
    Not = 42,
    NullP = 43,
    NumberAdd = 44,
    NumberEqual = 45,
    NumberGe = 46,
    NumberGt = 47,
    NumberLe = 48,
    NumberLt = 49,
    NumberMul = 50,
    NumberDiv = 51,
    NumberSub = 52,
    PairP = 53,
    Read = 54,
    ReadChar = 55,
    Reduce = 56,
    ReferFree = 57,
    ReferGlobal = 58,
    ReferLocal = 59,
    RestoreContinuation = 60,
    Return = 61,
    SetCar = 62,
    SetCdr = 63,
    Shift = 64,
    SymbolP = 65,
    Test = 66,
    Values = 67,
    Receive = 68,
    UnfixedJump = 69,
    Stop = 70,
    Shiftj = 71,
    Undef = 72,
    VectorLength = 73,
    VectorP = 74,
    VectorRef = 75,
    VectorSet = 76,
    PushEnter = 77,
    Halt = 78,
    ConstantPush = 79,
    NumberSubPush = 80,
    NumberAddPush = 81,
    PushConstant = 82,
    PushFrame = 83,
    CarPush = 84,
    CdrPush = 85,
    ShiftCall = 86,
    NotTest = 87,
    ReferGlobalCall = 88,
    ReferFreePush = 89,
    ReferLocalPush = 90,
    ReferLocalPushConstant = 91,
    ReferLocalPushConstantBranchNotLe = 92,
    ReferLocalPushConstantBranchNotGe = 93,
    ReferLocalPushConstantBranchNotNumberEqual = 94,
    ReferLocalBranchNotNull = 95,
    ReferLocalBranchNotLt = 96,
    ReferFreeCall = 97,
    ReferGlobalPush = 98,
    ReferLocalCall = 99,
    LocalCall = 100,
    Vector = 101,
    SimpleStructRef = 102,
    DynamicWinders = 103,
    TailCall = 104,
    LocalTailCall = 105,
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
        Ok(OpOld::$op(sym.to_symbol(), m.to_number()))
    }};
}

#[macro_export]
macro_rules! read_num_constant {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read_sexp($gc)?;
        let c = $self.read_sexp($gc)?;
        Ok(OpOld::$op(n.to_number(), c))
    }};
}

#[macro_export]
macro_rules! read_num_constant_num {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read_sexp($gc)?;
        let c = $self.read_sexp($gc)?;
        let o = $self.read_sexp($gc)?;
        Ok(OpOld::$op(n.to_number(), c, o.to_number()))
    }};
}

#[macro_export]
macro_rules! read_sym1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let s = $self.read_sexp($gc)?;
        Ok(OpOld::$op(s.to_symbol()))
    }};
}

#[macro_export]
macro_rules! read_const1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let c = $self.read_sexp($gc)?;
        Ok(OpOld::$op(c))
    }};
}

#[macro_export]
macro_rules! read_num1 {
    ($self:ident, $gc:ident, $op:ident, $size:ident) => {{
        let m = $self.read_sexp($gc)?;
        Ok(OpOld::$op(m.to_number() as $size))
    }};
}

#[macro_export]
macro_rules! read_num2 {
    ($self:ident, $gc:ident, $op:ident, $size:ident, $size2:ident) => {{
        let m = $self.read_sexp($gc)?;
        let n = $self.read_sexp($gc)?;
        Ok(OpOld::$op(m.to_number() as $size, n.to_number() as $size2))
    }};
}

#[macro_export]
macro_rules! read_num3 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let m = $self.read_sexp($gc)?;
        let n = $self.read_sexp($gc)?;
        let o = $self.read_sexp($gc)?;
        Ok(OpOld::$op(m.to_number(), n.to_number(), o.to_number()))
    }};
}

impl Fasl<'_> {
    pub fn read_op(&mut self, gc: &mut Gc) -> Result<OpOld, io::Error> {
        let tag = self.read_op_tag()?;
        match tag {
            OpTag::Append2 => Ok(OpOld::Append2),
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
            OpTag::Caar => Ok(OpOld::Caar),
            OpTag::Cadr => Ok(OpOld::Cadr),
            OpTag::Call => read_num1!(self, gc, Call, isize),
            OpTag::Car => Ok(OpOld::Car),
            OpTag::CarPush => Ok(OpOld::CarPush),
            OpTag::Cdar => Ok(OpOld::Cdar),
            OpTag::Cddr => Ok(OpOld::Cddr),
            OpTag::Cdr => Ok(OpOld::Cdr),
            OpTag::CdrPush => Ok(OpOld::CdrPush),
            OpTag::Closure => self.read_closure_op(gc),
            OpTag::Cons => Ok(OpOld::Cons),
            OpTag::Constant => read_const1!(self, gc, Constant),
            OpTag::ConstantPush => read_const1!(self, gc, ConstantPush),
            OpTag::DefineGlobal => read_sym1!(self, gc, DefineGlobal),
            OpTag::Display => read_num1!(self, gc, Display, isize),
            OpTag::Enter => read_num1!(self, gc, Enter, isize),
            OpTag::Eq => Ok(OpOld::Eq),
            OpTag::Equal => Ok(OpOld::Equal),
            OpTag::Eqv => Ok(OpOld::Eqv),
            OpTag::Frame => read_num1!(self, gc, Frame, isize),
            OpTag::Halt => Ok(OpOld::Halt),
            OpTag::Indirect => Ok(OpOld::Indirect),
            OpTag::Leave => read_num1!(self, gc, Leave, isize),
            OpTag::LetFrame => read_num1!(self, gc, LetFrame, isize),
            OpTag::LocalCall => read_num1!(self, gc, LocalCall, isize),
            OpTag::LocalJmp => read_num1!(self, gc, LocalJmp, isize),
            OpTag::LocalTailCall => read_num2!(self, gc, LocalTailCall, isize, isize),
            OpTag::MakeContinuation => read_num1!(self, gc, MakeContinuation, isize),
            OpTag::MakeVector => Ok(OpOld::MakeVector),
            OpTag::Nop => Ok(OpOld::Nop),
            OpTag::Not => Ok(OpOld::Not),
            OpTag::NotTest => read_num1!(self, gc, NotTest, isize),
            OpTag::NumberAdd => Ok(OpOld::NumberAdd),
            OpTag::NumberAddPush => Ok(OpOld::NumberAddPush),
            OpTag::NumberDiv => Ok(OpOld::NumberDiv),
            OpTag::NumberEqual => Ok(OpOld::NumberEqual),
            OpTag::NumberGe => Ok(OpOld::NumberGe),
            OpTag::NumberGt => Ok(OpOld::NumberGt),
            OpTag::NumberLt => Ok(OpOld::NumberLt),
            OpTag::NumberMul => Ok(OpOld::NumberMul),
            OpTag::NumberSub => Ok(OpOld::NumberSub),
            OpTag::NumberSubPush => Ok(OpOld::NumberSubPush),
            OpTag::PairP => Ok(OpOld::PairP),
            OpTag::Push => Ok(OpOld::Push),
            OpTag::PushConstant => read_const1!(self, gc, PushConstant),
            OpTag::PushEnter => read_num1!(self, gc, PushEnter, isize),
            OpTag::PushFrame => read_num1!(self, gc, PushFrame, isize),
            OpTag::ReadChar => Ok(OpOld::ReadChar),
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
            OpTag::SetCar => Ok(OpOld::SetCar),
            OpTag::SetCdr => Ok(OpOld::SetCdr),
            OpTag::Shiftj => read_num3!(self, gc, Shiftj),
            OpTag::TailCall => read_num2!(self, gc, TailCall, isize, isize),
            OpTag::Test => read_num1!(self, gc, Test, isize),
            OpTag::Undef => Ok(OpOld::Undef),
            OpTag::Values => read_num1!(self, gc, Values, usize),
            OpTag::VectorLength => Ok(OpOld::VectorLength),
            OpTag::VectorRef => Ok(OpOld::VectorRef),
            OpTag::VectorSet => Ok(OpOld::VectorSet),
            OpTag::SimpleStructRef => Ok(OpOld::SimpleStructRef),
            OpTag::SymbolP => Ok(OpOld::SymbolP),
            OpTag::VectorP => Ok(OpOld::VectorP),
            OpTag::ReferLocalBranchNotLt => {
                read_num2!(self, gc, ReferLocalBranchNotLt, isize, isize)
            }
            OpTag::NullP => Ok(OpOld::NullP),
            OpTag::Vector => read_num1!(self, gc, Vector, usize),
            OpTag::CompileError => todo!(),
            OpTag::Apply => todo!(),
            OpTag::List => todo!(),
            OpTag::NumberLe => todo!(),
            OpTag::Read => todo!(),
            OpTag::Reduce => todo!(),
            OpTag::RestoreContinuation => todo!(),
            OpTag::Shift => todo!(),
            OpTag::UnfixedJump => todo!(),
            OpTag::Stop => todo!(),
            OpTag::ShiftCall => todo!(),
            OpTag::ReferLocalPushConstantBranchNotGe => todo!(),
            OpTag::ReferLocalPushConstantBranchNotNumberEqual => todo!(),
            OpTag::DynamicWinders => todo!(),
        }
    }

    fn read_closure_op(&mut self, gc: &mut Gc) -> Result<OpOld, io::Error> {
        let size = self.read_sexp(gc)?;
        let arg_len = self.read_sexp(gc)?;
        let is_optional = !self.read_sexp(gc)?.is_false();
        let num_free_vars = self.read_sexp(gc)?;
        match (size, arg_len, num_free_vars) {
            (Object::Number(size), Object::Number(arg_len), Object::Number(num_free_vars)) => {
                Ok(OpOld::Closure {
                    size: size as usize,
                    arg_len: arg_len,
                    is_optional_arg: is_optional,
                    num_free_vars: num_free_vars,
                })
            }
            _ => Err(self.create_read_error("invalid closure")),
        }
    }

    pub fn read_all_sexp(&mut self, gc: &mut Gc) -> Vec<Object> {
        let mut objects = vec![];
        loop {
            match self.read_sexp(gc) {
                Ok(sexp) => {
                    objects.push(sexp);
                }
                Err(_) => {
                    break;
                }
            }
        }
        objects
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
            Tag::CompilerInsn => self.read_compiler_insn(),
        }
    }

    fn read_compiler_insn(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        let n = u8::from_le_bytes(buf);
        Ok(Object::Instruction(FromPrimitive::from_u8(buf[0]).expect("unknown Op")))
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
    use crate::{compiler, equal::Equal, gc::Gc, objects::Object, op::OpOld};

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
        let bytes: &[u8] = &[26, 7, 5, 1, 0, 97, 0, 0, 0, 3];
        let mut fasl = Fasl { bytes };
        let sym = gc.symbol_intern("a");
        let expected = gc.cons(sym, Object::Nil);
        match fasl.read_op(&mut gc).unwrap() {
            OpOld::Constant(v) => {
                assert_equal!(gc, expected, v);
            }
            _ => todo!(),
        }
    }

    #[test]
    fn test_closure_op() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[
            24, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 10, 0, 0, 0, 0, 0, 0,
            0,
        ];
        let mut fasl = Fasl { bytes };
        let expected = OpOld::Closure {
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
        let bytes: &[u8] = &[95, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = OpOld::ReferLocalBranchNotNull(2, 5);
        assert_eq!(expected, fasl.read_op(&mut gc).unwrap());
    }

    #[test]
    fn test_refer_local_op() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[59, 0, 1, 0, 0, 0, 0, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = OpOld::ReferLocal(1);
        assert_eq!(expected, fasl.read_op(&mut gc).unwrap());
    }
    /*
    #[test]
    fn test_baselib() {
        let mut vm = VmOld::new();
        let bytes: &[u8] = &[
            24, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 95, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 61, 0, 2, 0, 0, 0, 0, 0, 0, 0, 33, 0, 4, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0,
            0, 0, 0, 0, 84, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 83, 0, 5, 0,
            0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0, 0, 0, 0, 0, 85, 88,
            5, 4, 0, 109, 0, 0, 0, 97, 0, 0, 0, 112, 0, 0, 0, 49, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0,
            0, 25, 61, 0, 2, 0, 0, 0, 0, 0, 0, 0, 27, 5, 4, 0, 109, 0, 0, 0, 97, 0, 0, 0, 112, 0,
            0, 0, 49, 0, 0, 0, 41, 41, 41, 41, 41, 89, 0, 197, 0, 0, 0, 0, 0, 0, 0, 89, 0, 152, 0,
            0, 0, 0, 0, 0, 0, 89, 0, 2, 0, 0, 0, 0, 0, 0, 0, 24, 0, 39, 0, 0, 0, 0, 0, 0, 0, 0, 3,
            0, 0, 0, 0, 0, 0, 0, 1, 0, 3, 0, 0, 0, 0, 0, 0, 0, 95, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 6,
            0, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90, 0, 1, 0, 0, 0, 0, 0, 0, 0, 58,
            5, 9, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0,
            0, 108, 0, 0, 0, 45, 0, 0, 0, 49, 0, 0, 0, 104, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
            0, 0, 0, 0, 0, 61, 0, 3, 0, 0, 0, 0, 0, 0, 0, 36, 0, 11, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 90, 0, 1, 0, 0, 0, 0, 0, 0, 0, 90, 0, 2, 0, 0, 0, 0, 0, 0, 0, 89,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 0, 2, 0, 0, 0, 0, 0, 0, 0, 89, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 28, 0, 6, 0, 0, 0, 0, 0, 0, 0, 33, 0, 5, 0, 0, 0, 0, 0, 0, 0, 89, 0, 1, 0, 0, 0, 0,
            0, 0, 0, 90, 0, 1, 0, 0, 0, 0, 0, 0, 0, 90, 0, 2, 0, 0, 0, 0, 0, 0, 0, 97, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 77, 0, 1, 0, 0, 0, 0, 0, 0, 0, 59, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 66, 0, 6, 0, 0, 0, 0, 0, 0, 0, 89, 0, 5, 0, 0, 0, 0, 0, 0, 0, 90, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 58, 5, 15, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0,
            0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 45, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0,
            113, 0, 0, 0, 117, 0, 0, 0, 105, 0, 0, 0, 99, 0, 0, 0, 107, 0, 0, 0, 104, 0, 2, 0, 0,
            0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 38, 0, 10, 0, 0, 0, 0, 0, 0, 0, 79, 5, 7, 0,
            102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108,
            0, 0, 0, 79, 6, 33, 0, 101, 0, 0, 0, 120, 0, 0, 0, 112, 0, 0, 0, 101, 0, 0, 0, 99, 0,
            0, 0, 116, 0, 0, 0, 101, 0, 0, 0, 100, 0, 0, 0, 32, 0, 0, 0, 115, 0, 0, 0, 97, 0, 0, 0,
            109, 0, 0, 0, 101, 0, 0, 0, 32, 0, 0, 0, 108, 0, 0, 0, 101, 0, 0, 0, 110, 0, 0, 0, 103,
            0, 0, 0, 116, 0, 0, 0, 104, 0, 0, 0, 32, 0, 0, 0, 112, 0, 0, 0, 114, 0, 0, 0, 111, 0,
            0, 0, 112, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 32, 0, 0, 0, 108, 0, 0, 0, 105, 0, 0,
            0, 115, 0, 0, 0, 116, 0, 0, 0, 115, 0, 0, 0, 33, 0, 4, 0, 0, 0, 0, 0, 0, 0, 89, 0, 4,
            0, 0, 0, 0, 0, 0, 0, 89, 0, 3, 0, 0, 0, 0, 0, 0, 0, 97, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            2, 0, 0, 0, 0, 0, 0, 0, 13, 58, 5, 19, 0, 97, 0, 0, 0, 115, 0, 0, 0, 115, 0, 0, 0, 101,
            0, 0, 0, 114, 0, 0, 0, 116, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0,
            0, 0, 118, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 108, 0, 0, 0, 97, 0, 0, 0, 116, 0, 0,
            0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 104, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0,
            0, 0, 0, 0, 0, 0, 35, 0, 1, 0, 0, 0, 0, 0, 0, 0, 61, 0, 3, 0, 0, 0, 0, 0, 0, 0, 27, 5,
            7, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0,
            108, 0, 0, 0, 41, 41, 41, 41, 41, 41, 41, 89, 0, 48, 0, 0, 0, 0, 0, 0, 0, 89, 0, 89, 0,
            0, 0, 0, 0, 0, 0, 24, 0, 68, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2,
            0, 0, 0, 0, 0, 0, 0, 95, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 26, 1,
            61, 0, 2, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0, 0, 0, 0, 0, 53, 66, 0, 49, 0, 0, 0,
            0, 0, 0, 0, 36, 0, 14, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 89, 0, 1, 0, 0, 0, 0, 0, 0, 0, 89, 0, 0, 0, 0, 0, 0, 0, 0, 0, 28, 0,
            4, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0, 0, 0, 0, 0, 84, 59, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 85, 29, 0, 2, 0, 0, 0, 0, 0, 0, 0, 95, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0,
            0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 57, 0, 3, 0, 0, 0, 0, 0, 0, 0, 104, 0, 1, 0, 0,
            0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 38, 0, 31, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 53, 66, 0, 12, 0, 0, 0, 0, 0, 0, 0, 33, 0, 3, 0, 0, 0, 0, 0, 0, 0,
            90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 66, 0, 24, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0, 0, 0, 0, 0, 84, 59, 0, 1, 0, 0,
            0, 0, 0, 0, 0, 85, 71, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 38, 0, 239, 255, 255, 255, 255, 255, 255, 255, 38, 0, 17, 0, 0, 0, 0,
            0, 0, 0, 33, 0, 3, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 0, 3, 0, 0,
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 66, 0, 13, 0, 0, 0, 0, 0, 0, 0, 79, 5, 7, 0,
            102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108,
            0, 0, 0, 33, 0, 4, 0, 0, 0, 0, 0, 0, 0, 79, 6, 40, 0, 116, 0, 0, 0, 114, 0, 0, 0, 97,
            0, 0, 0, 118, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 115, 0, 0, 0, 97, 0, 0, 0, 108, 0,
            0, 0, 32, 0, 0, 0, 114, 0, 0, 0, 101, 0, 0, 0, 97, 0, 0, 0, 99, 0, 0, 0, 104, 0, 0, 0,
            101, 0, 0, 0, 100, 0, 0, 0, 32, 0, 0, 0, 116, 0, 0, 0, 111, 0, 0, 0, 32, 0, 0, 0, 110,
            0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 112, 0, 0, 0, 97, 0, 0, 0, 105, 0, 0,
            0, 114, 0, 0, 0, 32, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 101, 0, 0, 0, 109, 0, 0, 0,
            101, 0, 0, 0, 110, 0, 0, 0, 116, 0, 0, 0, 32, 0, 0, 0, 126, 0, 0, 0, 115, 0, 0, 0, 90,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 97, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
            83, 0, 4, 0, 0, 0, 0, 0, 0, 0, 89, 0, 3, 0, 0, 0, 0, 0, 0, 0, 89, 0, 2, 0, 0, 0, 0, 0,
            0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 13, 58, 5, 19, 0, 97,
            0, 0, 0, 115, 0, 0, 0, 115, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 116, 0, 0, 0, 105, 0,
            0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 118, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0,
            0, 108, 0, 0, 0, 97, 0, 0, 0, 116, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0,
            104, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 35, 0, 2, 0, 0, 0, 0, 0, 0,
            0, 61, 0, 2, 0, 0, 0, 0, 0, 0, 0, 79, 5, 7, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0,
            0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 33, 0, 4, 0, 0, 0, 0, 0, 0, 0,
            79, 6, 50, 0, 101, 0, 0, 0, 120, 0, 0, 0, 112, 0, 0, 0, 101, 0, 0, 0, 99, 0, 0, 0, 116,
            0, 0, 0, 101, 0, 0, 0, 100, 0, 0, 0, 32, 0, 0, 0, 99, 0, 0, 0, 104, 0, 0, 0, 97, 0, 0,
            0, 105, 0, 0, 0, 110, 0, 0, 0, 32, 0, 0, 0, 111, 0, 0, 0, 102, 0, 0, 0, 32, 0, 0, 0,
            112, 0, 0, 0, 97, 0, 0, 0, 105, 0, 0, 0, 114, 0, 0, 0, 115, 0, 0, 0, 44, 0, 0, 0, 32,
            0, 0, 0, 98, 0, 0, 0, 117, 0, 0, 0, 116, 0, 0, 0, 32, 0, 0, 0, 103, 0, 0, 0, 111, 0, 0,
            0, 116, 0, 0, 0, 32, 0, 0, 0, 126, 0, 0, 0, 114, 0, 0, 0, 44, 0, 0, 0, 32, 0, 0, 0, 97,
            0, 0, 0, 115, 0, 0, 0, 32, 0, 0, 0, 97, 0, 0, 0, 114, 0, 0, 0, 103, 0, 0, 0, 117, 0, 0,
            0, 109, 0, 0, 0, 101, 0, 0, 0, 110, 0, 0, 0, 116, 0, 0, 0, 32, 0, 0, 0, 50, 0, 0, 0,
            90, 0, 1, 0, 0, 0, 0, 0, 0, 0, 97, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0,
            0, 83, 0, 4, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90, 0, 1, 0, 0, 0, 0,
            0, 0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 13, 58, 5, 19, 0,
            97, 0, 0, 0, 115, 0, 0, 0, 115, 0, 0, 0, 101, 0, 0, 0, 114, 0, 0, 0, 116, 0, 0, 0, 105,
            0, 0, 0, 111, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 118, 0, 0, 0, 105, 0, 0, 0, 111, 0,
            0, 0, 108, 0, 0, 0, 97, 0, 0, 0, 116, 0, 0, 0, 105, 0, 0, 0, 111, 0, 0, 0, 110, 0, 0,
            0, 104, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 61, 0, 2, 0, 0, 0, 0, 0,
            0, 0, 27, 5, 9, 0, 102, 0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0,
            108, 0, 0, 0, 108, 0, 0, 0, 45, 0, 0, 0, 49, 0, 0, 0, 41, 41, 41, 41, 41, 41, 41, 41,
            41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 89, 0, 152, 0, 0, 0, 0, 0, 0, 0, 24, 0, 32, 0,
            0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 95, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 61, 0, 2, 0, 0, 0, 0, 0, 0, 0, 36, 0, 8,
            0, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 28, 0, 3, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 84, 59, 0, 1, 0, 0, 0, 0, 0, 0, 0, 85, 29, 0, 2, 0, 0, 0, 0, 0, 0, 0, 95, 0, 1, 0,
            0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 89, 0, 2, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 57, 0, 1, 0, 0, 0, 0, 0, 0, 0, 104, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            6, 0, 0, 0, 0, 0, 0, 0, 38, 0, 12, 0, 0, 0, 0, 0, 0, 0, 33, 0, 4, 0, 0, 0, 0, 0, 0, 0,
            89, 0, 2, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 66, 0, 7, 0, 0, 0, 0, 0, 0, 0, 59, 0, 1, 0, 0, 0, 0,
            0, 0, 0, 84, 59, 0, 1, 0, 0, 0, 0, 0, 0, 0, 85, 71, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 240, 255, 255, 255, 255, 255, 255,
            255, 35, 0, 2, 0, 0, 0, 0, 0, 0, 0, 61, 0, 2, 0, 0, 0, 0, 0, 0, 0, 27, 5, 15, 0, 102,
            0, 0, 0, 111, 0, 0, 0, 114, 0, 0, 0, 45, 0, 0, 0, 97, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0,
            0, 45, 0, 0, 0, 110, 0, 0, 0, 45, 0, 0, 0, 113, 0, 0, 0, 117, 0, 0, 0, 105, 0, 0, 0,
            99, 0, 0, 0, 107, 0, 0, 0, 41, 41, 41, 41, 41, 41, 41, 41, 78,
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
            let mut vm = VmOld::new();
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
    */
}

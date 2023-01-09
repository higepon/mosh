use std::io::{self, Read};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{gc::Gc, objects::Object};

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

// S-expression de-serializer.
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
        Ok(Object::Instruction(
            FromPrimitive::from_u8(buf[0]).expect("unknown Op"),
        ))
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
}
/// Tests.
#[cfg(test)]
pub mod tests {
    use crate::{equal::Equal, gc::Gc, objects::Object};

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
}

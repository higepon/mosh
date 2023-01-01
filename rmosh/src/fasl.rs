use core::num;
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
}

#[derive(FromPrimitive)]
enum OpTag {
    Constant = 10,
    Closure = 11,
}

// S-expression serializer.
pub struct Fasl<'a> {
    bytes: &'a [u8],
}

impl Fasl<'_> {
    pub fn read_op(&mut self, gc: &mut Gc) -> Result<Op, io::Error> {
        let tag = self.read_op_tag()?;
        match tag {
            OpTag::Constant => {
                let c = self.read_sexp(gc)?;
                Ok(Op::Constant(c))
            }
            OpTag::Closure => {
                let size = self.read_sexp(gc)?;
                let arg_len = self.read_sexp(gc)?;
                let is_optional = !self.read_sexp(gc)?.is_false();
                let num_free_vars = self.read_sexp(gc)?;
                match (size, arg_len, num_free_vars) {
                    (
                        Object::Number(size),
                        Object::Number(arg_len),
                        Object::Number(num_free_vars),
                    ) => Ok(Op::Closure {
                        size: size as usize,
                        arg_len: arg_len,
                        is_optional_arg: is_optional,
                        num_free_vars: num_free_vars,
                    }),
                    _ => Err(io::Error::new(io::ErrorKind::Other, "invalid char")),
                }
            }
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
                None => return Err(io::Error::new(io::ErrorKind::Other, "invalid char")),
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
                None => return Err(io::Error::new(io::ErrorKind::Other, "invalid char")),
            }
        }
        Ok(gc.new_string(&String::from_iter(chars)))
    }

    fn read_char(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 4];
        self.bytes.read_exact(&mut buf)?;
        let n = u32::from_le_bytes(buf);
        match char::from_u32(n) {
            Some(c) => Ok(Object::Char(c)),
            None => Err(io::Error::new(io::ErrorKind::Other, "invalid char")),
        }
    }

    fn read_pair(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let first = self.read_sexp(gc)?;
        let second = self.read_sexp(gc)?;
        Ok(gc.cons(first, second))
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
    use crate::{equal::Equal, gc::Gc, objects::Object, op::Op};

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
}

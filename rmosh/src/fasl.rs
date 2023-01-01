use std::io::{self, Read};

use crate::{gc::Gc, objects::Object};

// S-expression serializer.
pub struct Fasl<'a> {
    bytes: &'a [u8],
}

impl Fasl<'_> {
    pub fn read_sexp(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        if buf[0] == 0 {
            let mut buf = [0; 8];
            self.bytes.read_exact(&mut buf)?;
            let n = isize::from_le_bytes(buf);
            Ok(Object::Number(n))
        } else {
            Err(io::Error::new(io::ErrorKind::Other, "tag not found"))
        }
    }
}
/// Tests.
#[cfg(test)]
pub mod tests {
    use crate::{equal::Equal, gc::Gc, objects::Object, vm::Vm};

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
        let mut bytes: &[u8] = &[0, 3, 0, 0, 0, 0, 0, 0, 0];
        let mut fasl = Fasl { bytes };
        let expected = Object::Number(3);
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }
}

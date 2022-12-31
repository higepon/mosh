use crate::{objects::Object, gc::Gc};

// S-expression serializer.
pub struct Fasl<'a> {
    bytes: &'a [u8],
}

impl Fasl<'_> {


    pub fn read_sexp(&self, gc: &mut Gc) -> Result<Object, &'static str> {
        Ok(gc.new_string("s"))
    }
}
/// Tests.
#[cfg(test)]
pub mod tests {
    use crate::{objects::Object, equal::Equal, vm::Vm, gc::Gc};

    use super::Fasl;

    #[macro_export]
    macro_rules! assert_equal {
        ($gc:ident, $lhs:ident, $rhs:ident) => {
            {
                let e = Equal::new();
                if e.is_equal(&mut $gc, &$lhs, &$rhs) {
                    assert!(true);
                } else {
                    println!("{} is not equal to {}", $lhs, $rhs);
                    assert!(false);
                }
            }
        };
    }    

    #[test]
    fn test_constant_number() {
        let mut gc = Box::new(Gc::new());
        let mut bytes: &[u8] = &[0, 3, 0, 0, 0, 0, 0, 0, 0];
        let fasl  = Fasl {
            bytes,
        };
        let expected = Object::Number(3);
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }
}
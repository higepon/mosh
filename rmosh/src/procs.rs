/// Scheme procedures written in Rust.
use crate::objects::Object;

pub fn numberp(args: &[Object]) -> Object {
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Number(_) => Object::True,
        _ => Object::False,
    }
}

pub fn write(_args: &[Object]) -> Object {
    Object::Undef
}

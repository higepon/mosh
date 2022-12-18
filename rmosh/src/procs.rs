/// Scheme procedures written in Rust.
use crate::objects::Object;

pub fn numberp(args: &[Object]) -> Object {
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Number(_) => Object::True,
        _ => Object::False,
    }
}

pub fn car(args: &[Object]) -> Object {
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Pair(pair) => pair.first,
        _ => {
            panic!("car: pair required")
        }
    }
}

pub fn write(_args: &[Object]) -> Object {
    Object::Unspecified
}

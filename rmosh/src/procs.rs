/// Scheme procedures written in Rust.
use crate::objects::Object;

pub fn numberp(object: Object) -> Object {
    match object {
        Object::Number(_) => Object::True,
        _ => Object::False,
    }
}

pub fn write(object: Object) -> Object {
    println!("{:?}", object);
    Object::Undef
}

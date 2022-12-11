/// Scheme procedures written in Rust.
use crate::objects::Object;

pub fn scm_write(value: Object) -> Object {
    println!("{:?}", value);
    Object::Undef
}
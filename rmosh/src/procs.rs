/// Scheme procedures written in Rust.
use crate::{
    gc::Gc,
    objects::{Object, Procedure},
};

pub fn default_free_vars(gc: &mut Gc) -> Vec<Object> {
    let free_vars = vec![
        Object::Procedure(gc.alloc(Procedure::new(numberp, "number?".to_owned()))),
        Object::Procedure(gc.alloc(Procedure::new(write, "cons".to_owned()))),
        Object::Procedure(gc.alloc(Procedure::new(write, "cons*".to_owned()))),
        Object::Procedure(gc.alloc(Procedure::new(car, "car".to_owned()))),
    ];
    free_vars
}


fn numberp(args: &[Object]) -> Object {
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Number(_) => Object::True,
        _ => Object::False,
    }
}

fn car(args: &[Object]) -> Object {
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Pair(pair) => pair.first,
        _ => {
            panic!("car: pair required")
        }
    }
}

fn write(_args: &[Object]) -> Object {
    Object::Unspecified
}

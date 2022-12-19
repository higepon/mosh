/// Scheme procedures written in Rust.
/// The procedures will be exposed to the VM via free vars.
use crate::{
    gc::Gc,
    objects::{Object, Procedure},
};

pub fn default_free_vars(gc: &mut Gc) -> Vec<Object> {
    let free_vars = vec![
        gc.new_procedure(numberp, "number?"),
        gc.new_procedure(write, "cons"),        
        gc.new_procedure(write, "cons*"),                
        gc.new_procedure(car, "car"),                        
    ];
    free_vars
}

fn hoge(args: &[Object]) -> Object {
    const NAME: &str = "null?";
    panic!("{} not implemented", NAME)
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

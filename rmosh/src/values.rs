use std::fmt::{self, Display};

use crate::{
    gc::GcRef,
    objects::{Closure, Pair, Procedure, Symbol},
};

#[derive(Copy, Clone, Debug)]
pub enum Value {
    Closure(GcRef<Closure>),
    False,
    Number(isize),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    Symbol(GcRef<Symbol>),
    Undef,
    VMStackPointer(*mut Value),
}

impl Value {
    pub fn is_false(&self) -> bool {
        match self {
            Value::False => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => {
                write!(f, "{}", n)
            }
            Value::Closure(closure) => {
                write!(f, "{}", closure)
            }
            Value::Pair(pair) => {
                write!(f, "{}", pair)
            }
            Value::Symbol(symbol) => {
                write!(f, "{}", symbol)
            }
            Value::False => {
                write!(f, "false")
            }
            Value::VMStackPointer(_) => {
                write!(f, "<stack pointer>")
            }
            Value::Undef => {
                write!(f, "<undefined>")
            }
            Value::Procedure(_) => {
                write!(f, "<procedure>")
            }
        }
    }
}

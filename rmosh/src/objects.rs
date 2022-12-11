use std::fmt::{self, Display};

use crate::{
    gc::{GcHeader, ObjectType},
    values::Value,
};

/// Cons cell
#[derive(Debug)]
pub struct Pair {
    pub header: GcHeader,
    pub first: Value,
    pub second: Value,
}

impl Pair {
    pub fn new(first: Value, second: Value) -> Self {
        Pair {
            header: GcHeader::new(ObjectType::Pair),
            first: first,
            second: second,
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.first, self.second)
    }
}

/// Symbol
#[derive(Debug)]
pub struct Symbol {
    pub header: GcHeader,
    pub string: String,
}

impl Symbol {
    pub fn new(string: String) -> Self {
        Symbol {
            header: GcHeader::new(ObjectType::Symbol),
            string: string,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.string)
    }
}

/// Procedures written in Rust.
#[derive(Debug)]
pub struct Procedure {
    pub header: GcHeader,
    // TODO(higepon): Multiples arugments.
    pub func: fn(Value) -> Value,
}

impl Procedure {
    pub fn new(func: fn(Value) -> Value) -> Self {
        Procedure {
            header: GcHeader::new(ObjectType::Procedure),
            func: func,
        }
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<procedure>")
    }
}

/// Closure
#[derive(Debug)]
pub struct Closure {
    pub header: GcHeader,
    pub pc: usize,
    pub arg_len: isize,
    pub is_optional_arg: bool,
    size: usize,
    pub free_vars: Vec<Value>,
    pub prev: Value,
}

impl Closure {
    pub fn new(
        pc: usize,
        arg_len: isize,
        is_optional_arg: bool,
        size: usize,
        free_vars: Vec<Value>,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            pc: pc,
            arg_len: arg_len,
            is_optional_arg: is_optional_arg,
            size: size,
            free_vars: free_vars,
            prev: Value::Undef,
        }
    }

    pub fn refer_free(&mut self, n: usize) -> Value {
        self.free_vars[n]
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

/// Tests.
#[cfg(test)]
pub mod tests {

    use crate::gc::Gc;

    use super::*;

    // Helpers.
    pub fn procedure1(value: Value) -> Value {
        value
    }

    #[test]
    fn test_symbol() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("define".to_owned()));
        let symbol = Value::Symbol(symbol);
        match symbol {
            Value::Symbol(s) => {
                assert_eq!(s.string, "define");
            }
            _ => {
                panic!("not a symbo");
            }
        }
    }

    #[test]
    fn test_procedure() {
        let mut gc = Gc::new();
        let p = gc.alloc(Procedure::new(procedure1));
        match (p.func)(Value::False) {
            Value::False => {}
            _ => {
                panic!("Wrong return value");
            }
        }
    }
}

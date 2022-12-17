use std::fmt::{self, Display};

use crate::gc::{GcHeader, ObjectType};

use crate::gc::GcRef;

/// Wrapper of heap allocated or simple stack objects.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Object {
    Vox(GcRef<Vox>),
    Closure(GcRef<Closure>),
    True,
    False,
    Number(isize),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    Symbol(GcRef<Symbol>),
    Nil,
    Undef,
    VMStackPointer(*mut Object),
}

impl Object {
    pub fn is_false(&self) -> bool {
        match self {
            Object::False => true,
            _ => false,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Number(n) => {
                write!(f, "{}", n)
            }
            Object::Vox(obj) => {
                write!(f, "{}", obj)
            }            
            Object::Closure(closure) => {
                write!(f, "{}", closure)
            }
            Object::Pair(pair) => {
                write!(f, "{}", pair)
            }
            Object::Symbol(symbol) => {
                write!(f, "{}", symbol)
            }
            Object::True => {
                write!(f, "true")
            }            
            Object::False => {
                write!(f, "false")
            }
            Object::VMStackPointer(_) => {
                write!(f, "<stack pointer>")
            }
            Object::Undef => {
                write!(f, "<undefined>")
            }
            Object::Nil => {
                write!(f, "()")
            }            
            Object::Procedure(_) => {
                write!(f, "<procedure>")
            }
        }
    }
}

/// Cons cell
#[derive(Debug)]
pub struct Pair {
    pub header: GcHeader,
    pub first: Object,
    pub second: Object,
}

impl Pair {
    pub fn new(first: Object, second: Object) -> Self {
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

/// Vox
#[derive(Debug)]
pub struct Vox {
    pub header: GcHeader,
    pub value: Object,
}

impl Vox {
    pub fn new(value: Object) -> Self {
        Vox {
            header: GcHeader::new(ObjectType::Vox),
            value: value,
        }
    }
}

impl Display for Vox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Vox({})", self.value)
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
    pub func: fn(Object) -> Object,
}

impl Procedure {
    pub fn new(func: fn(Object) -> Object) -> Self {
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
    //size: usize,
    pub free_vars: Vec<Object>,
    pub prev: Object,
}

impl Closure {
    pub fn new(
        pc: usize,
        arg_len: isize,
        is_optional_arg: bool,
       // size: usize,
        free_vars: Vec<Object>,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            pc: pc,
            arg_len: arg_len,
            is_optional_arg: is_optional_arg,
            //size: size,
            free_vars: free_vars,
            prev: Object::Undef,
        }
    }

    pub fn refer_free(&mut self, n: usize) -> Object {
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
    fn procedure1(value: Object) -> Object {
        value
    }

    #[test]
    fn test_symbol() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("define".to_owned()));
        let symbol = Object::Symbol(symbol);
        match symbol {
            Object::Symbol(s) => {
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
        match (p.func)(Object::False) {
            Object::False => {}
            _ => {
                panic!("Wrong return value");
            }
        }
    }
}

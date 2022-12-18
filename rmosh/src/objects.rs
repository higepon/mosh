use std::fmt::{self, Display};
use regex::Regex;
use crate::gc::{GcHeader, ObjectType};

use crate::gc::GcRef;

/// Wrapper of heap allocated or simple stack objects.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Object {
    Closure(GcRef<Closure>),
    False,
    Nil,
    Number(isize),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    Symbol(GcRef<Symbol>),
    True,
    Unspecified,
    VMStackPointer(*mut Object),
    Vox(GcRef<Vox>),
}

impl Object {
    pub fn is_false(&self) -> bool {
        match self {
            Object::False => true,
            _ => false,
        }
    }

    pub fn make_bool(pred: bool) -> Self {
        if pred {
            Object::True
        } else {
            Object::False
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
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::Pair(pair) => {
                write!(f, "pair{}", pair)
            }
            Object::Symbol(symbol) => {
                 write!(f, "{}", unsafe { symbol.pointer.as_ref() }) 
            }
            Object::True => {
                write!(f, "#t")
            }
            Object::False => {
                write!(f, "#f")
            }
            Object::VMStackPointer(_) => {
                write!(f, "<stack pointer>")
            }
            Object::Unspecified => {
                write!(f, "#<unspecified>")
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

impl PartialEq for Pair {
    fn eq(&self, other: &Self) -> bool {
        (self.first == other.first) && (self.second == other.second)
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
        write!(f, "{}", self.string)
    }
}

/// Procedures written in Rust.
pub struct Procedure {
    pub header: GcHeader,
    pub func: fn(&[Object]) -> Object,
}

impl Procedure {
    pub fn new(func: fn(&[Object]) -> Object) -> Self {
        Procedure {
            header: GcHeader::new(ObjectType::Procedure),
            func: func,
        }
    }
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<procedure>")
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
    pub argc: isize,
    pub is_optional_arg: bool,
    //size: usize,
    pub free_vars: Vec<Object>,
    pub prev: Object,
}

impl Closure {
    pub fn new(
        pc: usize,
        argc: isize,
        is_optional_arg: bool,
        // size: usize,
        free_vars: Vec<Object>,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            pc: pc,
            argc: argc,
            is_optional_arg: is_optional_arg,
            //size: size,
            free_vars: free_vars,
            prev: Object::Unspecified,
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
    fn procedure1(args: &[Object]) -> Object {
        assert_eq!(args.len(), 1);
        args[0]
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
        let stack = [Object::Number(1), Object::Number(2)];
        match (p.func)(&stack[0..1]) {
            Object::Number(1) => {}
            _ => {
                panic!("Wrong return value");
            }
        }
    }

    #[test]
    fn test_simple_to_string() {
        assert_eq!("101", Object::Number(101).to_string());
        assert_eq!("#t", Object::True.to_string());
        assert_eq!("#f", Object::False.to_string());
        assert_eq!("#<unspecified>", Object::Unspecified.to_string());
    }

    #[test]
    fn test_symbol_to_string() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("hello".to_owned()));
        let symbol = Object::Symbol(symbol);
        assert_eq!("hello", symbol.to_string());
    }

    #[test]
    fn test_closure_to_string() {
        let mut gc = Gc::new();
        let closure = gc.alloc(Closure::new(0, 0, false, vec![]));
        let closure = Object::Closure(closure);

        let re = Regex::new(r"^#<closure\s[^>]+>$").unwrap();
        assert!(re.is_match(&closure.to_string()));        
    }    
}

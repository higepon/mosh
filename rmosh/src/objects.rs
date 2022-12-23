use crate::gc::GcRef;
use crate::gc::{GcHeader, ObjectType};
use crate::op::Op;
use crate::vm::Vm;
use std::fmt::{self, Display};

/// Wrapper of heap allocated or simple stack objects.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Object {
    Char(char),
    Closure(GcRef<Closure>),
    False,
    Nil,
    Number(isize),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    String(GcRef<SString>),
    Symbol(GcRef<Symbol>),
    True,
    Unspecified,
    StackPointer(*mut Object),
    OpPointer(*const Op),
    Vector(GcRef<Vector>),
    Vox(GcRef<Vox>),
}

impl Object {
    pub fn is_false(&self) -> bool {
        match self {
            Object::False => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        Pair::is_list(*self)
    }
    pub fn is_pair(&self) -> bool {
        match self {
            Object::Pair(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Object::Nil => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Object::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_unspecified(&self) -> bool {
        match self {
            Object::Unspecified => true,
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

    pub fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::OpPointer(op) => {
                write!(f, "{:?}", *op)
            }
            Object::Char(c) => {
                write!(f, "{}", c)
            }
            Object::Number(n) => {
                write!(f, "{}", n)
            }
            Object::Vox(obj) => {
                write!(f, "#<vox {}>", obj.value)
            }
            Object::Closure(closure) => {
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::Pair(pair) => {
                write!(f, "{}", unsafe { pair.pointer.as_ref() })
            }
            Object::String(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
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
            Object::StackPointer(v) => {
                write!(f, "#<stack pointer {:?}>", v)
            }
            Object::Unspecified => {
                write!(f, "#<unspecified>")
            }
            Object::Nil => {
                write!(f, "()")
            }
            Object::Procedure(proc) => {
                write!(f, "#<procedure {}>", proc.name)
            }
            Object::Vector(vector) => {
                write!(f, "{}", unsafe { vector.pointer.as_ref() })
            }
        }
    }
}

/// Vector
#[derive(Debug)]
pub struct Vector {
    pub header: GcHeader,
    pub data: Vec<Object>,
}

impl Vector {
    pub fn new(data: &Vec<Object>) -> Self {
        Vector {
            header: GcHeader::new(ObjectType::Vector),
            data: data.to_owned(),
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#[")?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

/// Cons cell
#[derive(Debug)]
pub struct Pair {
    pub header: GcHeader,
    pub car: Object,
    pub cdr: Object,
}

impl Pair {
    pub fn new(first: Object, second: Object) -> Self {
        Pair {
            header: GcHeader::new(ObjectType::Pair),
            car: first,
            cdr: second,
        }
    }

    pub fn is_list(obj: Object) -> bool {
        let mut obj = obj;
        let mut seen = obj;
        loop {
            if obj.is_nil() {
                return true;
            }
            match obj {
                Object::Pair(pair) => {
                    obj = pair.cdr;
                    if obj.is_nil() {
                        return true;
                    }

                    match obj {
                        Object::Pair(pair) => {
                            obj = pair.cdr;
                            match seen {
                                Object::Pair(pair) => {
                                    seen = pair.cdr;
                                    if obj == seen {
                                        // Circular
                                        return false;
                                    }
                                }
                                _ => {
                                    panic!("seen not a pair")
                                }
                            }
                        }
                        _ => {
                            // Dot pair
                            return false;
                        }
                    }
                }
                _ => {
                    // Dot pair.
                    return false;
                }
            }
        }
    }

    fn print_abbreviated(&self, f: &mut fmt::Formatter<'_>) -> bool {
        match self.cdr {
            Object::Pair(cdr) => {
                if !cdr.cdr.is_nil() {
                    return false;
                }
                let car = self.car;
                match car {
                    Object::Symbol(symbol) => {
                        if symbol.string.eq("quote") {
                            match write!(f, "'") {
                                Ok(_) => {
                                    return true;
                                }
                                Err(_) => {
                                    return false;
                                }
                            }
                        }
                        return false;
                    }
                    _ => {
                        return false;
                    }
                }
            }
            _ => {
                return false;
            }
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let abbreviated = self.print_abbreviated(f);
        let mut e = self.cdr;
        if abbreviated {
            match e {
                Object::Pair(pair) => {
                    let car_str = pair.car.to_string();
                    write!(f, "{}", car_str)?;
                    e = pair.cdr;
                }
                _ => panic!("should not reach"),
            }
        } else {
            let car_str = self.car.to_string();
            write!(f, "({}", car_str)?;
        }

        loop {
            match e {
                Object::Pair(pair) => {
                    write!(f, " ")?;
                    write!(f, "{}", pair.car)?;
                    e = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    write!(f, " . ")?;
                    write!(f, "{}", e)?;
                    break;
                }
            }
        }
        if !abbreviated {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl PartialEq for Pair {
    fn eq(&self, other: &Self) -> bool {
        (self.car == other.car) && (self.cdr == other.cdr)
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

/// SString (Sceheme String)
#[derive(Debug)]
pub struct SString {
    pub header: GcHeader,
    pub string: String,
}

impl SString {
    pub fn new(string: &str) -> Self {
        SString {
            header: GcHeader::new(ObjectType::String),
            string: string.to_string(),
        }
    }
}

impl Display for SString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.string)
    }
}

impl PartialEq for SString {
    fn eq(&self, other: &Self) -> bool {
        self.string.eq(&other.string)
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
    pub func: fn(&mut Vm, &[Object]) -> Object,
    pub name: String,
}

impl Procedure {
    pub fn new(func: fn(&mut Vm, &[Object]) -> Object, name: String) -> Self {
        Procedure {
            header: GcHeader::new(ObjectType::Procedure),
            func: func,
            name: name,
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
    pub ops: *const Op,
    pub ops_len: usize,
    pub argc: isize,
    pub is_optional_arg: bool,
    pub free_vars: Vec<Object>,
    pub prev: Object,
}

impl Closure {
    pub fn new(
        ops: *const Op,
        ops_len: usize,
        argc: isize,
        is_optional_arg: bool,
        free_vars: Vec<Object>,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            ops: ops,
            ops_len: ops_len,
            argc: argc,
            is_optional_arg: is_optional_arg,
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

    use super::*;
    use crate::gc::Gc;
    use regex::Regex;

    // Helpers.
    fn procedure1(_vm: &mut Vm, args: &[Object]) -> Object {
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
        let mut vm = Vm::new();
        let p = vm.gc.alloc(Procedure::new(procedure1, "proc1".to_owned()));
        let stack = [Object::Number(1), Object::Number(2)];
        match (p.func)(&mut vm, &stack[0..1]) {
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
        assert_eq!("()", Object::Nil.to_string());
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
        let closure = gc.alloc(Closure::new(&[] as *const Op, 0, 0, false, vec![]));
        let closure = Object::Closure(closure);

        let re = Regex::new(r"^#<closure\s[^>]+>$").unwrap();
        assert!(re.is_match(&closure.to_string()));
    }

    #[test]
    fn test_procedure_to_string() {
        let mut gc = Gc::new();
        let proc = gc.alloc(Procedure::new(procedure1, "number?".to_owned()));
        let proc = Object::Procedure(proc);
        assert_eq!("#<procedure number?>", proc.to_string());
    }

    #[test]
    fn test_stack_pointer_to_string() {
        let obj = Object::Number(10);
        let pointer: *mut Object = &obj as *const Object as *mut Object;
        let stack_pointer = Object::StackPointer(pointer);
        let re = Regex::new(r"^#<stack pointer\s[^>]+>$").unwrap();
        assert!(re.is_match(&stack_pointer.to_string()));
    }

    #[test]
    fn test_vox_to_string() {
        let mut gc = Gc::new();
        let vox = gc.alloc(Vox::new(Object::Number(101)));
        let vox = Object::Vox(vox);
        assert_eq!("#<vox 101>", vox.to_string());

        let symbol = gc.alloc(Symbol::new("my-symbol".to_owned()));
        let symbol = Object::Symbol(symbol);
        let vox = gc.alloc(Vox::new(symbol));
        let vox = Object::Vox(vox);
        assert_eq!("#<vox my-symbol>", vox.to_string());
    }

    #[test]
    fn test_dot_pair_to_string() {
        let mut gc = Gc::new();
        let pair = gc.cons(Object::Number(1), Object::Number(2));
        assert_eq!("(1 . 2)", pair.to_string());
    }

    #[test]
    fn test_simple_pair_to_string() {
        let mut gc = Gc::new();
        let pair1 = gc.cons(Object::Number(2), Object::Nil);
        let pair2 = gc.cons(Object::Number(1), pair1);
        assert_eq!("(1 2)", pair2.to_string());
    }

    #[test]
    fn test_pair_to_string() {
        let mut gc = Gc::new();
        let pair1 = gc.cons(Object::Number(3), Object::Nil);
        let pair2 = gc.cons(Object::Number(2), pair1);
        let pair3 = gc.cons(Object::Number(1), pair2);
        assert_eq!("(1 2 3)", pair3.to_string());
    }

    #[test]
    fn test_quote_to_string() {
        let mut gc = Gc::new();
        let sym_quote = gc.symbol_intern("quote");
        let sym_a = gc.symbol_intern("a");
        let pair = gc.list2(sym_quote, sym_a);
        assert_eq!("'a", pair.to_string());
    }

    #[test]
    fn test_quote_list_to_string() {
        let mut gc = Gc::new();
        let sym_quote = gc.symbol_intern("quote");
        let sym_a = gc.symbol_intern("a");
        let sym_b = gc.symbol_intern("b");
        let list = gc.list2(sym_a, sym_b);
        let pair = gc.list2(sym_quote, list);
        assert_eq!("'(a b)", pair.to_string());
    }

    #[test]
    fn test_sstring_eq() {
        let a = SString::new("abc");
        let b = SString::new("abc");
        assert_eq!(a, b);
    }

    #[test]
    fn test_vector_to_string() {
        let mut gc = Gc::new();
        let data = vec![Object::Number(1), Object::Number(2)];
        let v = gc.new_vector(&data);
        assert_eq!("#[1, 2]", v.to_string());
    }
}

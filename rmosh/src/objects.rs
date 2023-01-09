use crate::gc::GcRef;
use crate::gc::{GcHeader, ObjectType};
use crate::vm::Vm;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;

#[repr(u8)]
#[derive(Clone, Copy, Debug, FromPrimitive, Hash, PartialEq)]
pub enum Op {
    CompileError = 0,
    BranchNotLe = 1,
    BranchNotGe = 2,
    BranchNotLt = 3,
    BranchNotGt = 4,
    BranchNotNull = 5,
    BranchNotNumberEqual = 6,
    BranchNotEq = 7,
    BranchNotEqv = 8,
    BranchNotEqual = 9,
    Append2 = 10,
    Call = 11,
    Apply = 12,
    Push = 13,
    AssignFree = 14,
    AssignGlobal = 15,
    AssignLocal = 16,
    Box = 17,
    Caar = 18,
    Cadr = 19,
    Car = 20,
    Cdar = 21,
    Cddr = 22,
    Cdr = 23,
    Closure = 24,
    Cons = 25,
    Constant = 26,
    DefineGlobal = 27,
    Display = 28,
    Enter = 29,
    Eq = 30,
    Eqv = 31,
    Equal = 32,
    Frame = 33,
    Indirect = 34,
    Leave = 35,
    LetFrame = 36,
    List = 37,
    LocalJmp = 38,
    MakeContinuation = 39,
    MakeVector = 40,
    Nop = 41,
    Not = 42,
    NullP = 43,
    NumberAdd = 44,
    NumberEqual = 45,
    NumberGe = 46,
    NumberGt = 47,
    NumberLe = 48,
    NumberLt = 49,
    NumberMul = 50,
    NumberDiv = 51,
    NumberSub = 52,
    PairP = 53,
    Read = 54,
    ReadChar = 55,
    Reduce = 56,
    ReferFree = 57,
    ReferGlobal = 58,
    ReferLocal = 59,
    RestoreContinuation = 60,
    Return = 61,
    SetCar = 62,
    SetCdr = 63,
    Shift = 64,
    SymbolP = 65,
    Test = 66,
    Values = 67,
    Receive = 68,
    UnfixedJump = 69,
    Stop = 70,
    Shiftj = 71,
    Undef = 72,
    VectorLength = 73,
    VectorP = 74,
    VectorRef = 75,
    VectorSet = 76,
    PushEnter = 77,
    Halt = 78,
    ConstantPush = 79,
    NumberSubPush = 80,
    NumberAddPush = 81,
    PushConstant = 82,
    PushFrame = 83,
    CarPush = 84,
    CdrPush = 85,
    ShiftCall = 86,
    NotTest = 87,
    ReferGlobalCall = 88,
    ReferFreePush = 89,
    ReferLocalPush = 90,
    ReferLocalPushConstant = 91,
    ReferLocalPushConstantBranchNotLe = 92,
    ReferLocalPushConstantBranchNotGe = 93,
    ReferLocalPushConstantBranchNotNumberEqual = 94,
    ReferLocalBranchNotNull = 95,
    ReferLocalBranchNotLt = 96,
    ReferFreeCall = 97,
    ReferGlobalPush = 98,
    ReferLocalCall = 99,
    LocalCall = 100,
    Vector = 101,
    SimpleStructRef = 102,
    DynamicWinders = 103,
    TailCall = 104,
    LocalTailCall = 105,
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Wrapper of heap allocated or simple stack objects.
#[derive(Copy, Clone, PartialEq, Hash)]
pub enum Object {
    Char(char),
    Closure(GcRef<Closure>),
    Eof,
    EqHashtable(GcRef<EqHashtable>),
    False,
    InputPort(GcRef<InputPort>),
    Instruction(Op),
    Nil,
    Number(isize),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    SimpleStruct(GcRef<SimpleStruct>),
    String(GcRef<SString>),
    Symbol(GcRef<Symbol>),
    True,
    Unspecified,
    ObjectPointer(*mut Object),
    ProgramCounter(*const Object),
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

    pub fn is_vox(&self) -> bool {
        match self {
            Object::Vox(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Object::Number(_) => true,
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

    pub fn to_bool(self) -> bool {
        match self {
            Object::True => true,
            Object::False => false,
            _ => {
                panic!("Not a bool object")
            }
        }
    }
    pub fn to_number(self) -> isize {
        if let Self::Number(n) = self {
            n
        } else {
            panic!("Not a Object::Number")
        }
    }
    pub fn to_instruction(self) -> Op {
        if let Self::Instruction(p) = self {
            p
        } else {
            panic!("Not a Object::Instruction")
        }
    }
    pub fn to_pair(self) -> GcRef<Pair> {
        if let Self::Pair(p) = self {
            p
        } else {
            panic!("Not a Object::Pair")
        }
    }
    pub fn to_symbol(self) -> GcRef<Symbol> {
        if let Self::Symbol(s) = self {
            s
        } else {
            panic!("Not a Object::Symbol {}", self)
        }
    }
    pub fn to_vox(self) -> GcRef<Vox> {
        if let Self::Vox(v) = self {
            v
        } else {
            panic!("Not a Object::Vox")
        }
    }

    pub fn to_closure(self) -> GcRef<Closure> {
        if let Self::Closure(c) = self {
            c
        } else {
            panic!("Not a Object::Closure")
        }
    }

    // TODO: Implement eqv?
    pub fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => {
                return a == b;
            }
            _ => {
                return self.eq(other);
            }
        }
    }
}

// For HashMap<Object, Object>
impl Eq for Object {}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::InputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::Char(c) => {
                write!(f, "{}", c)
            }
            Object::Number(n) => {
                write!(f, "{}", n)
            }
            Object::Instruction(op) => {
                write!(f, "#<instruction {}>", op)
            }
            Object::Vox(obj) => {
                write!(f, "#<vox {}>", obj.value)
            }
            Object::Closure(closure) => {
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
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
            Object::Eof => {
                write!(f, "#<eof>")
            }
            Object::True => {
                write!(f, "#t")
            }
            Object::False => {
                write!(f, "#f")
            }
            Object::ObjectPointer(v) => {
                write!(f, "#<stack pointer {:?}>", v)
            }
            Object::ProgramCounter(v) => {
                write!(f, "#<program counter {:?}>", v)
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
            Object::SimpleStruct(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::InputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::Char(c) => {
                write!(f, "{}", c)
            }
            Object::Number(n) => {
                write!(f, "{}", n)
            }
            Object::Instruction(op) => {
                write!(f, "#<instruction {}>", op)
            }
            Object::Vox(obj) => {
                write!(f, "#<vox {}>", obj.value)
            }
            Object::Closure(closure) => {
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
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
            Object::Eof => {
                write!(f, "#<eof>")
            }
            Object::True => {
                write!(f, "#t")
            }
            Object::False => {
                write!(f, "#f")
            }
            Object::ObjectPointer(v) => {
                write!(f, "#<stack pointer {:?}>", v)
            }
            Object::ProgramCounter(v) => {
                write!(f, "#<program counter {:?}>", v)
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
            Object::SimpleStruct(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
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

/// SimpleStruct
#[derive(Debug)]
pub struct SimpleStruct {
    pub header: GcHeader,
    pub name: Object,
    pub data: Vec<Object>,
    len: usize,
}

impl SimpleStruct {
    pub fn new(name: Object, len: usize) -> Self {
        SimpleStruct {
            header: GcHeader::new(ObjectType::SimpleStruct),
            name: name,
            data: vec![Object::Unspecified; len],
            len: len,
        }
    }

    pub fn initialize(&mut self, args: Object) {
        let mut args = args;
        for i in 0..self.len {
            if args.is_nil() {
                self.data[i] = Object::Unspecified;
            } else {
                let p = args.to_pair();
                self.data[i] = p.car;
                args = p.cdr;
            }
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl Display for SimpleStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#< simple-struct {} ", self.name)?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ">")
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

    // Caller should check if obj is_list().
    pub fn list_len(obj: Object) -> usize {
        let mut obj = obj;
        let mut len = 0;
        loop {
            if obj.is_nil() {
                break;
            }
            obj = obj.to_pair().cdr;
            len += 1;
        }
        len
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

    fn last_pair(p: Object) -> Object {
        let mut o = p;
        loop {
            match o {
                Object::Pair(pair) => {
                    let kdr = pair.cdr;
                    if kdr.is_nil() {
                        return o;
                    } else {
                        o = kdr;
                    }
                }
                _ => {
                    panic!("last_pair: pair requied but got {}", o);
                }
            }
        }
    }

    // append!
    pub fn append_destructive(l1: Object, l2: Object) -> Object {
        if l1.is_nil() {
            return l2;
        }
        let last = Self::last_pair(l1);
        match last {
            Object::Pair(mut pair) => {
                pair.cdr = l2;
                return l1;
            }
            _ => {
                panic!("never reached");
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
    pub ops: *const Object,
    pub ops_len: usize,
    pub argc: isize,
    pub is_optional_arg: bool,
    pub free_vars: Vec<Object>,
    pub prev: Object,
}

impl Closure {
    pub fn new(
        ops: *const Object,
        ops_len: usize,
        argc: isize,
        is_optional_arg: bool,
        free_vars: Vec<Object>,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            ops,
            ops_len: ops_len,
            argc: argc,
            is_optional_arg: is_optional_arg,
            free_vars: free_vars,
            prev: Object::Unspecified,
        }
    }

    pub fn refer_free(&self, n: usize) -> Object {
        self.free_vars[n]
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

/// EqHashtable
#[derive(Debug)]
pub struct EqHashtable {
    pub header: GcHeader,
    pub hash_map: HashMap<Object, Object>,
    pub is_mutable: bool,
}

impl EqHashtable {
    pub fn new() -> Self {
        EqHashtable {
            header: GcHeader::new(ObjectType::EqHashtable),
            hash_map: HashMap::new(),
            is_mutable: true,
        }
    }

    pub fn get(&self, key: Object, default: Object) -> Object {
        match self.hash_map.get(&key) {
            Some(value) => *value,
            _ => default,
        }
    }

    pub fn contains(&self, key: Object) -> bool {
        match self.hash_map.get(&key) {
            Some(_) => true,
            _ => false,
        }
    }

    pub fn set(&mut self, key: Object, value: Object) {
        match self.hash_map.insert(key, value) {
            _ => (),
        }
    }

    pub fn size(&self) -> usize {
        self.hash_map.len()
    }

    pub fn delte(&mut self, key: Object) {
        match self.hash_map.remove(&key) {
            _ => (),
        }
    }

    pub fn is_mutable(&self) -> bool {
        return self.is_mutable;
    }

    pub fn clear(&mut self) {
        self.hash_map.clear()
    }
}

impl Display for EqHashtable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<eq-hashtable>")
    }
}

/// InputPort
#[derive(Debug)]
pub struct InputPort {
    pub header: GcHeader,
    source: String,
    idx: usize,
}

impl InputPort {
    fn new(source: &str) -> Self {
        InputPort {
            header: GcHeader::new(ObjectType::InputPort),
            source: source.to_owned(),
            idx: 0,
        }
    }
    pub fn open(source: &str) -> std::io::Result<InputPort> {
        Ok(InputPort::new(source))
    }

    pub fn read_char(&mut self) -> Option<char> {
        let mut chars = self.source.chars();
        let ret = chars.nth(self.idx);
        self.idx = self.idx + 1;
        ret
    }
}

impl Display for InputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<input port>")
    }
}

/// Tests.
#[cfg(test)]
pub mod tests {

    use std::{mem, ptr::NonNull};

    use super::*;
    use crate::gc::Gc;
    use regex::Regex;

    // Helpers.
    fn procedure1(_vm: &mut Vm, args: &[Object]) -> Object {
        assert_eq!(args.len(), 1);
        args[0]
    }

    /*
    #[test]
    fn test_input_port() {
        match InputPort::open("file_not_exists") {
            Ok(_) => {}
            Err(e) => {
                println!("port error {:?}", e);
            }
        }
    }
    */

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
        println!("hoge symbol{:?}", symbol);
        assert_eq!("hello", symbol.to_string());
    }

    #[test]
    fn test_closure_to_string() {
        let mut gc = Gc::new();
        let closure = gc.alloc(Closure::new([].as_ptr(), 0, 0, false, vec![]));
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
        let stack_pointer = Object::ObjectPointer(pointer);
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

    trait InputPort {
        fn read(&self);
    }

    struct StringInputPort {}

    impl Drop for StringInputPort {
        fn drop(&mut self) {
            println!("StringInputPort dropped");
        }
    }

    impl InputPort for StringInputPort {
        fn read(&self) {
            println!("StringInputPort::read called");
        }
    }

    fn do_something(input_port: &mut dyn InputPort) {
        // Do something with input_port.
        // We don't care if it is StringInputPort or not.
        input_port.read();

        // Now we are free the object as InputPort.
        let pointer = input_port as *mut dyn InputPort;
        unsafe { drop(Box::from_raw(pointer)) }
    }

    #[test]
    fn test_string_input_port() {
        let boxed = Box::new(StringInputPort {});
        // The GC manages the pointer going forward.
        let pointer = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        let mut string_input_port: NonNull<StringInputPort> =
            unsafe { mem::transmute(pointer.as_ref()) };

        // Call read() as StringInputPort
        unsafe { string_input_port.as_ref().read() };

        // Use the object as InputPort.
        unsafe { do_something(string_input_port.as_mut()) };
    }

    #[test]
    fn test_ref_ref() {
        {
            let mut a: isize = 3;

            let a_ref: &isize = &a;
            assert_eq!(3, *a_ref);

            let a_ref: &mut isize = &mut a;
            *a_ref = 4;
            assert_eq!(4, *a_ref);
        }

        {
            let a: isize = 3;
            let b: isize = 4;

            let a_ref: &isize = &a;
            assert_eq!(3, *a_ref);

            let b_ref: &isize = &b;
            assert_eq!(4, *b_ref);

            let a_ref_ref: &&isize = &&a;
            assert_eq!(3, **a_ref_ref);

            let ref_ref: &mut &isize = &mut &a;
            assert_eq!(3, **ref_ref);
            *ref_ref = &b;
            assert_eq!(4, **ref_ref);
        }
    }
}

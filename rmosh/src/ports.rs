use std::io::Write;
use std::{
    collections::HashMap,
    fmt::{self, Display},
    fs::File,
    io::{self, Read},
};

use lalrpop_util::ParseError;

pub type ReadError = ParseError<usize, lexer::Token, LexicalError>;

use crate::{
    gc::{Gc, GcHeader, GcRef, ObjectType},
    lexer::{self, LexicalError},
    objects::{Object, Pair, SimpleStruct, Vector},
    reader::DatumParser,
};

// Trait for TextInputPort.
pub trait TextInputPort {
    // The only methods you have to implement.
    fn read_to_string(&mut self, str: &mut String) -> io::Result<usize>;
    fn set_parsed(&mut self, obj: Object);
    fn parsed(&self) -> Object;

    // (read ...)
    // LALRPOP doesn't support multiple calls of parse.
    // We parse all S-Expressions once then store them.
    fn read(&mut self, gc: &mut Box<Gc>) -> Result<Object, ReadError> {
        //
        if self.parsed().is_unspecified() {
            let mut s = String::new();
            match self.read_to_string(&mut s) {
                Ok(_) => {}
                Err(err) => {
                    return Err(ParseError::User {
                        error: LexicalError {
                            start: 0,
                            end: 0,
                            token: err.to_string(),
                        },
                    });
                }
            }
            let s = "(".to_string() + &s;
            // re2c assumes null terminated string.
            let s = s + ")\0";
            let chars: Vec<char> = s.chars().collect();

            self.set_parsed(DatumParser::new().parse(gc, lexer::Lexer::new(&chars))?);
        }
        if self.parsed().is_nil() {
            return Ok(Object::Eof);
        } else {
            let obj = self.parsed().car_unchecked();
            self.set_parsed(self.parsed().cdr_unchecked());
            return Ok(obj);
        }
    }
}

#[derive(Debug)]
pub struct FileInputPort {
    pub header: GcHeader,
    pub file: File,
    is_closed: bool,
    pub parsed: Object,
}

impl FileInputPort {
    fn new(file: File) -> Self {
        FileInputPort {
            header: GcHeader::new(ObjectType::FileInputPort),
            file: file,
            is_closed: false,
            parsed: Object::Unspecified,
        }
    }
    pub fn open(path: &str) -> std::io::Result<FileInputPort> {
        let file = File::open(path)?;
        Ok(FileInputPort::new(file))
    }

    pub fn close(&mut self) {
        self.is_closed = true;
    }
}

impl Display for FileInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<file-input-port>")
    }
}

impl TextInputPort for FileInputPort {
    fn read_to_string(&mut self, str: &mut String) -> std::io::Result<usize> {
        self.file.read_to_string(str)
    }
    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

#[derive(Debug)]
pub struct StringInputPort {
    pub header: GcHeader,
    source: String,
    idx: usize,
    ahead_char: Option<char>,
    pub parsed: Object,
}

impl StringInputPort {
    pub fn new(source: &str) -> Self {
        StringInputPort {
            header: GcHeader::new(ObjectType::StringInputPort),
            source: source.to_owned(),
            idx: 0,
            ahead_char: None,
            parsed: Object::Unspecified,
        }
    }

    pub fn read_char(&mut self) -> Option<char> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Some(c)
            }
            None => {
                let mut chars = self.source.chars();
                let ret = chars.nth(self.idx);
                self.idx = self.idx + 1;
                ret
            }
        }
    }

    pub fn lookahead_char(&mut self) -> Option<char> {
        match self.ahead_char {
            Some(ch) => {
                self.ahead_char = None;
                Some(ch)
            }
            None => self.read_char(),
        }
    }

    pub fn unget_char(&mut self, c: char) {
        assert!(self.ahead_char == None);
        self.ahead_char = Some(c);
    }
}

impl Display for StringInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<string-input-port>")
    }
}

impl TextInputPort for StringInputPort {
    fn read_to_string(&mut self, str: &mut String) -> std::io::Result<usize> {
        str.push_str(&self.source);
        Ok(self.source.len())
    }
    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

// Trait for TextOutputPort.
pub trait TextOutputPort {
    // The only method you have to implement :)
    fn put_string(&mut self, s: &str) -> Result<(), std::io::Error>;

    // (write obj): Machine readable print.
    fn write(&mut self, obj: Object) -> Result<(), std::io::Error> {
        let mut shared_id = 1;
        let mut seen: HashMap<Object, Object> = HashMap::new();
        self.scan(obj, &mut seen);
        self.display_one(obj, &mut seen, &mut shared_id)
    }

    // (display obj): Human readable print.
    fn display(&mut self, obj: Object) -> Result<(), std::io::Error> {
        let mut shared_id = 1;
        let mut seen: HashMap<Object, Object> = HashMap::new();
        self.scan(obj, &mut seen);
        self.display_one(obj, &mut seen, &mut shared_id)
    }

    fn display_one(
        &mut self,
        obj: Object,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
    ) -> Result<(), std::io::Error> {
        let seen_state = match seen.get(&obj) {
            Some(val) => *val,
            None => Object::False,
        };
        if seen_state.is_true() {
            seen.insert(obj, Object::Fixnum(*shared_id));
            self.put_string(&format!("#{}=", shared_id))?;
            *shared_id += 1;
        } else if seen_state.is_number() {
            return self.put_string(&format!("#{}#", seen_state.to_isize()));
        }
        match obj {
            Object::Pair(p) => self.display_pair(p, seen, shared_id),
            Object::Vector(v) => self.display_vector(v, seen, shared_id),
            Object::SimpleStruct(s) => self.display_struct(s, seen, shared_id),
            Object::Bytevector(_)
            | Object::BytevectorInputPort(_)            
            | Object::Closure(_)
            | Object::Continuation(_)
            | Object::ContinuationStack(_)
            | Object::Vox(_)
            | Object::ProgramCounter(_)
            | Object::ObjectPointer(_)
            | Object::Unspecified
            | Object::True
            | Object::Procedure(_)
            | Object::Char(_)
            | Object::EqHashtable(_)
            | Object::Bignum(_)
            | Object::Compnum(_)
            | Object::Ratnum(_)
            | Object::Regexp(_)
            | Object::False
            | Object::Flonum(_)
            | Object::StringInputPort(_)
            | Object::FileInputPort(_)
            | Object::Eof
            | Object::BinaryFileInputPort(_)
            | Object::BinaryFileOutputPort(_)
            | Object::FileOutputPort(_)
            | Object::StringOutputPort(_)
            | Object::StdOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::Instruction(_)
            | Object::Nil
            | Object::Symbol(_)
            | Object::String(_)
            | Object::Fixnum(_) => self.as_display(obj),
        }
    }

    fn display_abbreviated(&mut self, obj: Object) -> bool {
        if let Object::Symbol(s) = obj {
            if s.string.eq("quote") {
                self.put_string("'").ok();
                return true;
            } else if s.string.eq("unquote") {
                self.put_string(",").ok();
                return true;
            } else if s.string.eq("unquote-splicing") {
                self.put_string(",@").ok();
                return true;
            } else if s.string.eq("quasiquote") {
                self.put_string("`").ok();
                return true;
            }
        }
        return false;
    }

    fn display_pair(
        &mut self,
        p: GcRef<Pair>,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
    ) -> Result<(), std::io::Error> {
        let mut p = p;
        let abbreviated =
            p.cdr.is_pair() && p.cdr.cdr_unchecked().is_nil() && self.display_abbreviated(p.car);
        if abbreviated {
            p = p.cdr.to_pair();
        } else {
            self.put_string("(")?;
        }
        self.display_one(p.car, seen, shared_id)?;

        let mut obj = p.cdr;
        loop {
            let seen_state = match seen.get(&obj) {
                Some(v) => *v,
                None => Object::False,
            };
            match obj {
                Object::Pair(pair) if seen_state.is_false() => {
                    self.put_string(" ")?;
                    self.display_one(pair.car, seen, shared_id)?;
                    obj = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    self.put_string(" . ")?;
                    self.display_one(obj, seen, shared_id)?;
                    break;
                }
            }
        }
        if !abbreviated {
            return self.put_string(")");
        } else {
            Ok(())
        }
    }

    fn as_display(&mut self, obj: Object) -> Result<(), std::io::Error> {
        self.put_string(&format!("{}", obj))
    }

    fn display_vector(
        &mut self,
        v: GcRef<Vector>,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
    ) -> Result<(), std::io::Error> {
        self.put_string("#(")?;
        for i in 0..v.len() {
            self.display_one(v.data[i], seen, shared_id)?;
            if i != v.len() - 1 {
                self.put_string(" ")?;
            }
        }
        self.put_string(")")
    }

    fn display_struct(
        &mut self,
        s: GcRef<SimpleStruct>,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
    ) -> Result<(), std::io::Error> {
        self.put_string("#<simple-stuct ")?;
        for i in 0..s.len() {
            self.display_one(s.field(i), seen, shared_id)?;
            if i != s.len() - 1 {
                self.put_string(" ")?;
            }
        }
        self.put_string(">")
    }

    fn scan(&mut self, obj: Object, seen: &mut HashMap<Object, Object>) {
        let mut o = obj;
        loop {
            match o {
                Object::Bytevector(_)
                | Object::Bignum(_)
                | Object::BinaryFileInputPort(_)
                | Object::BinaryFileOutputPort(_)
                | Object::BytevectorInputPort(_)                
                | Object::Char(_)
                | Object::Closure(_)
                | Object::Compnum(_)
                | Object::Continuation(_)
                | Object::ContinuationStack(_)
                | Object::Eof
                | Object::EqHashtable(_)
                | Object::False
                | Object::FileInputPort(_)
                | Object::FileOutputPort(_)
                | Object::Flonum(_)
                | Object::Instruction(_)
                | Object::Nil
                | Object::ObjectPointer(_)
                | Object::Procedure(_)
                | Object::ProgramCounter(_)
                | Object::Ratnum(_)
                | Object::Regexp(_)
                | Object::StdErrorPort(_)
                | Object::StdOutputPort(_)
                | Object::String(_)
                | Object::StringInputPort(_)
                | Object::StringOutputPort(_)
                | Object::Symbol(_)
                | Object::True
                | Object::Unspecified
                | Object::Vox(_)
                | Object::Fixnum(_) => return,
                Object::Pair(p) => {
                    let val = match seen.get(&o) {
                        Some(v) => *v,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        seen.insert(obj, Object::False);
                    }
                    self.scan(p.car, seen);
                    o = p.cdr;
                    continue;
                }
                Object::Vector(v) => {
                    let val = match seen.get(&o) {
                        Some(found) => *found,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        seen.insert(obj, Object::False);
                    }
                    for obj in v.data.iter() {
                        self.scan(*obj, seen);
                    }
                    break;
                }
                Object::SimpleStruct(s) => {
                    let val = match seen.get(&o) {
                        Some(v) => *v,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        seen.insert(obj, Object::False);
                    }
                    for i in 0..s.len() {
                        self.scan(s.field(i), seen);
                    }
                    break;
                }
            }
        }
    }

    // (format ...).
    fn format(&mut self, fmt: &str, args: &mut [Object]) {
        let mut chars = fmt.chars();
        let mut i = 0;
        while let Some(c) = chars.next() {
            if c == '~' {
                if let Some(c) = chars.next() {
                    if c == 'a' || c == 'd' || c == 'e' {
                        if i < args.len() {
                            self.display(args[i]).ok();
                            i += 1;
                        } else {
                            panic!("format: not enough arguments");
                        }
                    } else if c == 's' {
                        if i < args.len() {
                            self.write(args[i]).ok();
                            i += 1;
                        } else {
                            panic!("format: not enough arguments");
                        }
                    } else {
                        panic!("format: unknown ~{}", c);
                    }
                } else {
                    break;
                }
            } else {
                self.put_string(&format!("{}", c)).ok();
            }
        }
    }
}

// BytevectorInputPort
#[derive(Debug)]
pub struct BytevectorInputPort {
    pub header: GcHeader,
    is_closed: bool,
}

impl BytevectorInputPort {
    pub fn new(_bv: &[u8]) -> Self {
        BytevectorInputPort {
            header: GcHeader::new(ObjectType::BytevectorInputPort),
            is_closed: false,
        }
    }
}


impl Display for BytevectorInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<bytevector-input-port>")
    }
}


// BinaryFileInputPort
#[derive(Debug)]
pub struct BinaryFileInputPort {
    pub header: GcHeader,
    file: File,
    is_closed: bool,
}

impl BinaryFileInputPort {
    pub fn new(file: File) -> Self {
        BinaryFileInputPort {
            header: GcHeader::new(ObjectType::BinaryFileInputPort),
            is_closed: false,
            file: file,
        }
    }

    pub fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        self.file.read_to_end(buf)
    }

    pub fn close(&mut self) {
        self.is_closed = true;
    }
}

impl Display for BinaryFileInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<binary-file-input-port {:?}>", self.file)
    }
}

// FileOutputPort
#[derive(Debug)]
pub struct FileOutputPort {
    pub header: GcHeader,
    file: File,
    is_closed: bool,
}

impl FileOutputPort {
    pub fn new(file: File) -> Self {
        FileOutputPort {
            header: GcHeader::new(ObjectType::FileOutputPort),
            is_closed: false,
            file: file,
        }
    }

    pub fn close(&mut self) {
        self.is_closed = true;
    }
}

impl Display for FileOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<file-output-port>")
    }
}

impl TextOutputPort for FileOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), std::io::Error> {
        write!(self.file, "{}", s)
    }
}

// StdOutputPort
pub struct StdOutputPort {
    pub header: GcHeader,
}

impl StdOutputPort {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::StdOutputPort),
        }
    }
}

impl Display for StdOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<std-output-port>")
    }
}

impl TextOutputPort for StdOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), std::io::Error> {
        print!("{}", s);
        Ok(())
    }
}

// StdOutputPort
pub struct StdErrorPort {
    pub header: GcHeader,
}

impl StdErrorPort {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::StdErrorPort),
        }
    }
}

impl Display for StdErrorPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<stderr-output-port>")
    }
}

impl TextOutputPort for StdErrorPort {
    fn put_string(&mut self, s: &str) -> Result<(), std::io::Error> {
        eprint!("{}", s);
        Ok(())
    }
}

// StringOutputPort
#[derive(Debug)]
pub struct StringOutputPort {
    pub header: GcHeader,
    string: String,
    is_closed: bool,
}

impl StringOutputPort {
    pub fn new() -> Self {
        StringOutputPort {
            header: GcHeader::new(ObjectType::StringOutputPort),
            is_closed: false,
            string: "".to_string(),
        }
    }
    pub fn open(_path: &str) -> std::io::Result<StringOutputPort> {
        Ok(StringOutputPort::new())
    }

    pub fn close(&mut self) {
        self.is_closed = true;
    }

    pub fn string(&self) -> String {
        self.string.to_owned()
    }
}

impl Display for StringOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<string-output-port>")
    }
}

impl TextOutputPort for StringOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), std::io::Error> {
        self.string.push_str(s);
        Ok(())
    }
}

// BinaryFileOutputPort
#[derive(Debug)]
pub struct BinaryFileOutputPort {
    pub header: GcHeader,
    file: File,
    is_closed: bool,
}

impl BinaryFileOutputPort {
    pub fn new(file: File) -> Self {
        BinaryFileOutputPort {
            header: GcHeader::new(ObjectType::BinaryFileOutputPort),
            is_closed: false,
            file: file,
        }
    }

    pub fn put_u8(&mut self, value: u8) -> io::Result<usize> {
        self.file.write(&[value])
    }

    pub fn put_u16(&mut self, value: u16) -> io::Result<usize> {
        self.file.write(&value.to_le_bytes())
    }

    pub fn put_u32(&mut self, value: u32) -> io::Result<usize> {
        self.file.write(&value.to_le_bytes())
    }

    pub fn put_u64(&mut self, value: u64) -> io::Result<usize> {
        self.file.write(&value.to_le_bytes())
    }

    pub fn close(&mut self) {
        self.is_closed = true;
    }
}

impl Display for BinaryFileOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<binary-file-output-port>")
    }
}

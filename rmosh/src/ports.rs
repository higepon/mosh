use std::cmp::{max, min};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::{
    collections::HashMap,
    fmt::{self, Display},
    fs::File,
    io::{self, Read},
};

use crate::error;
use crate::{
    gc::{Gc, GcHeader, GcRef, ObjectType},
    lexer::{self},
    objects::{CharExt, Object, Pair, SimpleStruct, Vector},
    reader::DatumParser,
    reader_util::ReadError,
};
use lalrpop_util::ParseError;

// Trait for Port.
pub trait Port {
    fn is_open(&self) -> bool;
    fn close(&mut self);
}

pub trait OutputPort: Port {
    fn flush(&mut self);
}

// Trait for TextInputPort.
pub trait TextInputPort {
    // The methods you have to implement.
    fn read_to_string(&mut self, str: &mut String) -> io::Result<usize>;
    fn read_n_to_string(&mut self, str: &mut String, n: usize) -> io::Result<usize>;
    fn read_char(&mut self) -> Option<char>;
    fn ahead_char(&self) -> Option<char>;
    fn set_ahead_char(&mut self, c: Option<char>);
    fn input_src(&self) -> String;

    fn read_line(&mut self, str: &mut String) -> io::Result<usize>;
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
                    return Err(ReadError::ContentNotFound {
                        description: err.to_string(),
                    });
                }
            }
            let s = "(".to_string() + &s;
            //println!("TRY to read {}", s);
            // re2c assumes null terminated string.
            let s = s + ")\0";
            let chars: Vec<char> = s.chars().collect();
            // Whether if we found #1# style.
            let mut shared_map: HashMap<u32, Object> = HashMap::new();
            match DatumParser::new().parse(
                gc,
                &mut shared_map,
                &self.input_src(),
                &s,
                lexer::Lexer::new(&chars),
            ) {
                Ok(parsed) => {
                    if shared_map.len() > 0 {
                        self.link_shared(gc, &shared_map, parsed);
                    }
                    self.set_parsed(parsed);
                }
                Err(ParseError::User { error }) => {
                    return Err(error);
                }
                Err(ParseError::InvalidToken { location }) => {
                    return Err(ReadError::LalrpopInvalidToken { location: location })
                }
                Err(ParseError::UnrecognizedEOF {
                    location,
                    expected: _,
                }) => return Err(ReadError::UnrecognizedEOF { location: location }),
                Err(ParseError::UnrecognizedToken { token, expected }) => {
                    let context_start = max(0, (token.0 as isize) - 10) as usize;
                    // Show what is causing this error.
                    let context = format!("reader: {}", &s[context_start..token.2]);
                    return Err(ReadError::UnrecognizedToken {
                        token: token.1,
                        expected: expected,
                        context: context.to_string(),
                    });
                }
                Err(ParseError::ExtraToken { token }) => {
                    return Err(ReadError::ExtraToken { token: token.1 })
                }
            }
        }
        if self.parsed().is_nil() {
            return Ok(Object::Eof);
        } else {
            let obj = self.parsed().car_unchecked();
            self.set_parsed(self.parsed().cdr_unchecked());
            return Ok(obj);
        }
    }

    fn link_shared(&self, gc: &mut Box<Gc>, shared_map: &HashMap<u32, Object>, obj: Object) {
        match obj {
            Object::Pair(mut p) => {
                if let Object::DefinedShared(index) = p.car {
                    match shared_map.get(&index) {
                        Some(v) => {
                            p.car = *v;
                        }
                        None => panic!(),
                    }
                } else {
                    self.link_shared(gc, shared_map, p.car);
                }
                if let Object::DefinedShared(index) = p.cdr {
                    match shared_map.get(&index) {
                        Some(v) => {
                            p.cdr = *v;
                        }
                        None => panic!(),
                    }
                } else {
                    self.link_shared(gc, shared_map, p.cdr);
                }
            }
            Object::Vector(mut v) => {
                for i in 0..v.len() {
                    let obj = v.data[i];
                    if let Object::DefinedShared(index) = obj {
                        match shared_map.get(&index) {
                            Some(value) => {
                                v.data[i] = *value;
                            }
                            None => panic!(),
                        }
                    } else {
                        self.link_shared(gc, shared_map, obj);
                    }
                }
            }
            _ => (),
        }
    }

    fn lookahead_char(&mut self) -> Option<char> {
        match self.ahead_char() {
            Some(ch) => Some(ch),
            None => match self.read_char() {
                Some(c) => {
                    self.unget_char(c);
                    Some(c)
                }
                None => None,
            },
        }
    }

    fn unget_char(&mut self, c: char) {
        assert!(self.ahead_char() == None);
        self.set_ahead_char(Some(c));
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct StdInputPort {
    pub header: GcHeader,
    pub parsed: Object,
    ahead_char: Option<char>,
}

impl StdInputPort {
    pub fn new() -> Self {
        StdInputPort {
            header: GcHeader::new(ObjectType::StdInputPort),
            ahead_char: None,
            parsed: Object::Unspecified,
        }
    }
}

impl Display for StdInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<std-input-port>")
    }
}

impl Port for StdInputPort {
    fn is_open(&self) -> bool {
        true
    }

    fn close(&mut self) {}
}

impl TextInputPort for StdInputPort {
    fn read_to_string(&mut self, str: &mut String) -> std::io::Result<usize> {
        io::stdin().read_to_string(str)
    }
    fn input_src(&self) -> String {
        "#<stdin>".to_string()
    }
    fn read_char(&mut self) -> Option<char> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Some(c)
            }
            None => {
                let mut str = String::new();
                match self.read_n_to_string(&mut str, 1) {
                    Ok(_) => {
                        if str.len() == 0 {
                            None
                        } else {
                            let mut chars = str.chars();
                            chars.nth(0)
                        }
                    }
                    Err(_) => None,
                }
            }
        }
    }

    fn ahead_char(&self) -> Option<char> {
        self.ahead_char
    }

    fn set_ahead_char(&mut self, c: Option<char>) {
        self.ahead_char = c;
    }
    fn read_n_to_string(&mut self, str: &mut String, n: usize) -> io::Result<usize> {
        let mut buf = vec![0; n];
        match io::stdin().read(&mut buf) {
            Ok(_) => {
                // TODO: This doesn't work for non ascii.
                match std::str::from_utf8(&buf) {
                    Ok(s) => {
                        str.push_str(s);
                        Ok(str.len())
                    }
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "can't read")),
                }
            }
            Err(_) => Err(io::Error::new(io::ErrorKind::Other, "can't read")),
        }
    }
    fn read_line(&mut self, str: &mut String) -> std::io::Result<usize> {
        io::stdin().lock().read_line(str)
    }

    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct FileInputPort {
    pub header: GcHeader,
    pub reader: BufReader<File>,
    is_closed: bool,
    ahead_char: Option<char>,
    path: String,
    pub parsed: Object,
}

impl FileInputPort {
    fn new(file: File, path: &str) -> Self {
        FileInputPort {
            header: GcHeader::new(ObjectType::FileInputPort),
            reader: BufReader::new(file),
            is_closed: false,
            ahead_char: None,
            path: path.to_string(),
            parsed: Object::Unspecified,
        }
    }
    pub fn open(path: &str) -> std::io::Result<FileInputPort> {
        let file = File::open(path)?;
        Ok(FileInputPort::new(file, path))
    }
}

impl Display for FileInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<file-input-port>")
    }
}

impl Port for FileInputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }

    fn close(&mut self) {
        self.is_closed = true;
    }
}

impl TextInputPort for FileInputPort {
    fn read_to_string(&mut self, str: &mut String) -> std::io::Result<usize> {
        self.reader.read_to_string(str)
    }

    fn input_src(&self) -> String {
        self.path.to_string()
    }

    fn ahead_char(&self) -> Option<char> {
        self.ahead_char
    }

    fn set_ahead_char(&mut self, c: Option<char>) {
        self.ahead_char = c;
    }

    fn read_char(&mut self) -> Option<char> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Some(c)
            }
            None => {
                let mut str = String::new();
                match self.read_n_to_string(&mut str, 1) {
                    Ok(_) => {
                        if str.len() == 0 {
                            None
                        } else {
                            let mut chars = str.chars();
                            chars.nth(0)
                        }
                    }
                    Err(_) => None,
                }
            }
        }
    }
    fn read_n_to_string(&mut self, str: &mut String, n: usize) -> io::Result<usize> {
        let mut buf = vec![0; n];
        match self.reader.read(&mut buf) {
            Ok(size) => {
                // TODO: This doesn't work for non ascii.
                match std::str::from_utf8(&buf[0..size]) {
                    Ok(s) => {
                        str.push_str(s);
                        Ok(str.len())
                    }
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "can't read")),
                }
            }
            Err(_) => Err(io::Error::new(io::ErrorKind::Other, "can't read")),
        }
    }
    fn read_line(&mut self, str: &mut String) -> std::io::Result<usize> {
        self.reader.read_to_string(str)
    }

    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct StringInputPort {
    pub header: GcHeader,
    source: String,
    idx: usize,
    ahead_char: Option<char>,
    is_closed: bool,
    pub parsed: Object,
}

impl StringInputPort {
    pub fn new(source: &str) -> Self {
        StringInputPort {
            header: GcHeader::new(ObjectType::StringInputPort),
            source: source.to_owned(),
            idx: 0,
            ahead_char: None,
            is_closed: false,
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

    fn input_src(&self) -> String {
        "#<string-input-port>".to_string()
    }

    fn ahead_char(&self) -> Option<char> {
        self.ahead_char
    }

    fn set_ahead_char(&mut self, c: Option<char>) {
        self.ahead_char = c;
    }
    fn read_char(&mut self) -> Option<char> {
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
    fn read_n_to_string(&mut self, str: &mut String, n: usize) -> io::Result<usize> {
        let end = min(self.source.len(), self.idx + n);
        let s = &self.source[self.idx..end];
        str.push_str(&s);
        Ok(s.len())
    }
    fn read_line(&mut self, str: &mut String) -> std::io::Result<usize> {
        let s = &self.source[self.idx..];
        match s.lines().next() {
            Some(line) => {
                str.push_str(line);
                // line doesn't include \n so we add 1 here.
                self.idx += line.len() + 1;
                Ok(line.len() + 1)
            }
            None => Err(io::Error::new(io::ErrorKind::Other, "can't read line")),
        }
    }

    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

impl Port for StringInputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }

    fn close(&mut self) {
        self.is_closed = true
    }
}

// Trait for TextOutputPort.
pub trait TextOutputPort: Port {
    fn put_string(&mut self, s: &str) -> Result<(), std::io::Error>;
    fn flush(&mut self);

    // (write-char c).
    fn write_char(&mut self, c: char) -> Result<(), std::io::Error> {
        if self.is_open() {
            self.put_string(&c.to_string())
        } else {
            Err(io::Error::new(io::ErrorKind::Other, "port is closed"))
        }
    }

    // (write obj): Machine readable print.
    fn write(&mut self, obj: Object, shared_aware: bool) -> Result<(), std::io::Error> {
        if shared_aware {
            let mut shared_id = 1;
            let mut seen: HashMap<Object, Object> = HashMap::new();
            self.scan(obj, &mut seen);
            self.display_shared_one(obj, &mut seen, &mut shared_id, false)
        } else {
            self.display_one(obj, false)
        }
    }

    // (display obj): Human readable print.
    fn display(&mut self, obj: Object, shared_aware: bool) -> Result<(), std::io::Error> {
        if shared_aware {
            let mut shared_id = 1;
            let mut seen: HashMap<Object, Object> = HashMap::new();
            self.scan(obj, &mut seen);
            self.display_shared_one(obj, &mut seen, &mut shared_id, true)
        } else {
            self.display_one(obj, true)
        }
    }

    fn display_one(&mut self, obj: Object, human_readable: bool) -> Result<(), std::io::Error> {
        match obj {
            Object::Pair(p) => self.display_pair(p, human_readable),
            Object::Vector(v) => self.display_vector(v, human_readable),
            Object::SimpleStruct(s) => self.display_struct(s, human_readable),
            Object::Bytevector(_)
            | Object::BytevectorInputPort(_)
            | Object::BytevectorOutputPort(_)
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
            | Object::EqvHashtable(_)
            | Object::GenericHashtable(_)
            | Object::Bignum(_)
            | Object::Latin1Codec(_)
            | Object::Transcoder(_)
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
            | Object::StdInputPort(_)
            | Object::StdOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::Instruction(_)
            | Object::Nil
            | Object::Symbol(_)
            | Object::String(_)
            | Object::Fixnum(_) => {
                if human_readable {
                    self.as_display(obj)
                } else {
                    self.as_write(obj)
                }
            }
            Object::DefinedShared(_) => todo!(),
        }
    }

    fn display_shared_one(
        &mut self,
        obj: Object,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
        human_readable: bool,
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
            Object::Pair(p) => self.display_shared_pair(p, seen, shared_id, human_readable),
            Object::Vector(v) => self.display_shared_vector(v, seen, shared_id, human_readable),
            Object::SimpleStruct(s) => {
                self.display_shared_struct(s, seen, shared_id, human_readable)
            }
            Object::Bytevector(_)
            | Object::BytevectorInputPort(_)
            | Object::BytevectorOutputPort(_)
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
            | Object::EqvHashtable(_)
            | Object::GenericHashtable(_)
            | Object::Bignum(_)
            | Object::Compnum(_)
            | Object::Latin1Codec(_)
            | Object::Transcoder(_)
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
            | Object::StdInputPort(_)
            | Object::StdOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::Instruction(_)
            | Object::Nil
            | Object::Symbol(_)
            | Object::String(_)
            | Object::Fixnum(_) => {
                if human_readable {
                    self.as_display(obj)
                } else {
                    self.as_write(obj)
                }
            }
            Object::DefinedShared(_) => todo!(),
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

    fn display_pair(&mut self, p: GcRef<Pair>, human_readable: bool) -> Result<(), std::io::Error> {
        let mut p = p;
        let abbreviated =
            p.cdr.is_pair() && p.cdr.cdr_unchecked().is_nil() && self.display_abbreviated(p.car);
        if abbreviated {
            p = p.cdr.to_pair();
        } else {
            self.put_string("(")?;
        }
        self.display_one(p.car, human_readable)?;

        let mut obj = p.cdr;
        loop {
            match obj {
                Object::Pair(pair) => {
                    self.put_string(" ")?;
                    self.display_one(pair.car, human_readable)?;
                    obj = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    self.put_string(" . ")?;
                    self.display_one(obj, human_readable)?;
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

    fn display_shared_pair(
        &mut self,
        p: GcRef<Pair>,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
        human_readable: bool,
    ) -> Result<(), std::io::Error> {
        let mut p = p;
        let abbreviated =
            p.cdr.is_pair() && p.cdr.cdr_unchecked().is_nil() && self.display_abbreviated(p.car);
        if abbreviated {
            p = p.cdr.to_pair();
        } else {
            self.put_string("(")?;
        }
        self.display_shared_one(p.car, seen, shared_id, human_readable)?;

        let mut obj = p.cdr;
        loop {
            let seen_state = match seen.get(&obj) {
                Some(v) => *v,
                None => Object::False,
            };
            match obj {
                Object::Pair(pair) if seen_state.is_false() => {
                    self.put_string(" ")?;
                    self.display_shared_one(pair.car, seen, shared_id, human_readable)?;
                    obj = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    self.put_string(" . ")?;
                    self.display_shared_one(obj, seen, shared_id, human_readable)?;
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

    fn as_write(&mut self, obj: Object) -> Result<(), std::io::Error> {
        self.put_string(&format!("{:?}", obj))
    }

    fn display_vector(
        &mut self,
        v: GcRef<Vector>,
        human_readable: bool,
    ) -> Result<(), std::io::Error> {
        self.put_string("#(")?;
        for i in 0..v.len() {
            self.display_one(v.data[i], human_readable)?;
            if i != v.len() - 1 {
                self.put_string(" ")?;
            }
        }
        self.put_string(")")
    }

    fn display_shared_vector(
        &mut self,
        v: GcRef<Vector>,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
        human_readable: bool,
    ) -> Result<(), std::io::Error> {
        self.put_string("#(")?;
        for i in 0..v.len() {
            self.display_shared_one(v.data[i], seen, shared_id, human_readable)?;
            if i != v.len() - 1 {
                self.put_string(" ")?;
            }
        }
        self.put_string(")")
    }

    fn display_struct(
        &mut self,
        s: GcRef<SimpleStruct>,
        human_readable: bool,
    ) -> Result<(), std::io::Error> {
        self.put_string(&format!("#<simple-stuct {} ", s.name))?;
        for i in 0..s.len() {
            self.display_one(s.field(i), human_readable)?;
            if i != s.len() - 1 {
                self.put_string(" ")?;
            }
        }
        self.put_string(">")
    }

    fn display_shared_struct(
        &mut self,
        s: GcRef<SimpleStruct>,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
        human_readable: bool,
    ) -> Result<(), std::io::Error> {
        self.put_string(&format!("#<simple-stuct {} ", s.name))?;
        for i in 0..s.len() {
            self.display_shared_one(s.field(i), seen, shared_id, human_readable)?;
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
                | Object::BytevectorOutputPort(_)
                | Object::Char(_)
                | Object::Closure(_)
                | Object::Compnum(_)
                | Object::Continuation(_)
                | Object::ContinuationStack(_)
                | Object::Eof
                | Object::EqHashtable(_)
                | Object::EqvHashtable(_)
                | Object::GenericHashtable(_)
                | Object::False
                | Object::FileInputPort(_)
                | Object::FileOutputPort(_)
                | Object::Flonum(_)
                | Object::Instruction(_)
                | Object::Latin1Codec(_)
                | Object::Transcoder(_)
                | Object::Nil
                | Object::ObjectPointer(_)
                | Object::Procedure(_)
                | Object::ProgramCounter(_)
                | Object::Ratnum(_)
                | Object::Regexp(_)
                | Object::StdErrorPort(_)
                | Object::StdInputPort(_)
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
                Object::DefinedShared(_) => todo!(),
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
                        let shared_aware = false;
                        if i < args.len() {
                            self.display(args[i], shared_aware).ok();
                            i += 1;
                        } else {
                            panic!("format: not enough arguments");
                        }
                    } else if c == 's' {
                        let shared_aware = false;
                        if i < args.len() {
                            self.write(args[i], shared_aware).ok();
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

// Trait for Port.
pub trait BinaryInputPort {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize>;

    fn read_u8(&mut self) -> io::Result<Option<u8>> {
        let mut buf = [0; 1];
        let size = self.read(&mut buf)?;
        if size == 1 {
            Ok(Some(buf[0]))
        } else {
            Ok(None)
        }
    }
}

// BytevectorInputPort
#[derive(Debug)]
#[repr(C)]
pub struct BytevectorInputPort {
    pub header: GcHeader,
    is_closed: bool,
    idx: usize,
    data: Vec<u8>,
}

impl BytevectorInputPort {
    pub fn new(data: &[u8]) -> Self {
        BytevectorInputPort {
            header: GcHeader::new(ObjectType::BytevectorInputPort),
            is_closed: false,
            idx: 0,
            data: data.to_owned(),
        }
    }
}

impl Port for BytevectorInputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
}

impl BinaryInputPort for BytevectorInputPort {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let size = min(buf.len(), self.data.len() - self.idx);
        buf[0..size].copy_from_slice(&self.data[self.idx..self.idx + size]);
        self.idx += size;
        Ok(size)
    }
}

impl Display for BytevectorInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Don't change this name. This is used in u8-ready?
        write!(f, "#<byte-array-input-port>")
    }
}

// Trait for binary output port.
pub trait BinaryOutputPort {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize>;

    fn put_u8(&mut self, value: u8) -> io::Result<usize> {
        self.write(&[value])
    }

    fn put_u16(&mut self, value: u16) -> io::Result<usize> {
        self.write(&value.to_le_bytes())
    }

    fn put_u32(&mut self, value: u32) -> io::Result<usize> {
        self.write(&value.to_le_bytes())
    }

    fn put_u64(&mut self, value: u64) -> io::Result<usize> {
        self.write(&value.to_le_bytes())
    }

    fn put_i64(&mut self, value: i64) -> io::Result<usize> {
        self.write(&value.to_le_bytes())
    }
}

// BytevectorOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct BytevectorOutputPort {
    pub header: GcHeader,
    is_closed: bool,
    pub data: Vec<u8>,
}

impl BytevectorOutputPort {
    pub fn new() -> Self {
        BytevectorOutputPort {
            header: GcHeader::new(ObjectType::BytevectorOutputPort),
            is_closed: false,
            data: vec![],
        }
    }

    pub fn to_bytevector(&self, gc: &mut Box<Gc>) -> Object {
        gc.new_bytevector_u8(&self.data)
    }
}

impl Port for BytevectorOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
}

impl BinaryOutputPort for BytevectorOutputPort {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.data.extend_from_slice(buf);
        Ok(buf.len())
    }
}

impl OutputPort for BytevectorOutputPort {
    fn flush(&mut self) {}
}

impl Display for BytevectorOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<bytevector-output-port>")
    }
}

// BinaryFileInputPort
#[derive(Debug)]
#[repr(C)]
pub struct BinaryFileInputPort {
    pub header: GcHeader,
    pub reader: BufReader<File>,
    is_closed: bool,
}

impl BinaryFileInputPort {
    pub fn new(file: File) -> Self {
        BinaryFileInputPort {
            header: GcHeader::new(ObjectType::BinaryFileInputPort),
            is_closed: false,
            reader: BufReader::new(file),
        }
    }

    pub fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        self.reader.read_to_end(buf)
    }

    pub fn close(&mut self) {
        self.is_closed = true;
    }
}

impl Port for BinaryFileInputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
}

impl BinaryInputPort for BinaryFileInputPort {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.reader.read(buf)
    }
}

impl Display for BinaryFileInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<binary-file-input-port {:?}>", self.reader.get_ref())
    }
}

// FileOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct FileOutputPort {
    pub header: GcHeader,
    writer: BufWriter<File>,
    is_closed: bool,
}

impl FileOutputPort {
    pub fn new(file: File) -> Self {
        FileOutputPort {
            header: GcHeader::new(ObjectType::FileOutputPort),
            is_closed: false,
            writer: BufWriter::new(file),
        }
    }
}

impl Port for FileOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.writer.flush().unwrap();
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
        write!(self.writer, "{}", s)
    }
    fn flush(&mut self) {
        self.writer.flush().unwrap_or(())
    }
}

// StdOutputPort
#[repr(C)]
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

impl Port for StdOutputPort {
    fn is_open(&self) -> bool {
        true
    }
    fn close(&mut self) {}
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
    fn flush(&mut self) {
        // There's nothing we can do here if flush resutns error.
        io::stdout().flush().unwrap_or(())
    }
}

// StdOutputPort
#[repr(C)]
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

impl Port for StdErrorPort {
    fn is_open(&self) -> bool {
        true
    }
    fn close(&mut self) {}
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
    fn flush(&mut self) {
        // There's nothing we can do here if flush resutns error.
        io::stdout().flush().unwrap_or(())
    }
}

// StringOutputPort
#[derive(Debug)]
#[repr(C)]
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

impl Port for StringOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
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
    fn flush(&mut self) {}
}

// BinaryFileOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct BinaryFileOutputPort {
    pub header: GcHeader,
    pub writer: BufWriter<File>,
    is_closed: bool,
}

impl BinaryFileOutputPort {
    pub fn new(file: File) -> Self {
        BinaryFileOutputPort {
            header: GcHeader::new(ObjectType::BinaryFileOutputPort),
            is_closed: false,
            writer: BufWriter::new(file),
        }
    }

    pub fn close(&mut self) {
        self.writer.flush().unwrap_or(());
        self.is_closed = true;
    }
}

impl Port for BinaryFileOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.flush();
        self.is_closed = true;
    }
}

impl OutputPort for BinaryFileOutputPort {
    fn flush(&mut self) {
        self.writer.flush().unwrap_or(())
    }
}

impl BinaryOutputPort for BinaryFileOutputPort {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }
}

impl Display for BinaryFileOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<binary-file-output-port>")
    }
}

/// Codec
#[derive(Debug)]
#[repr(C)]
pub struct Latin1Codec {
    pub header: GcHeader,
}

impl Latin1Codec {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::Latin1Codec),
        }
    }

    pub fn read_char(
        &self,
        port: &mut dyn BinaryInputPort,
        mode: ErrorHandlingMode,
        _should_check_bom: bool,
    ) -> error::Result<Option<char>> {
        loop {
            match port.read_u8() {
                Ok(Some(u)) => return Ok(Some(u as char)),
                Ok(None) => return Ok(None),
                Err(_) => match mode {
                    ErrorHandlingMode::IgnoreError => {
                        continue;
                    }
                    ErrorHandlingMode::RaiseError => {
                        return error::Error::io_decoding_error(
                            "latin-1-code",
                            "invalid latin-1 byte sequence",
                            &[],
                        )
                    }
                    ErrorHandlingMode::ReplaceError => return Ok(Some('\u{FFFD}')),
                },
            }
        }
        /*
        retry:
            const int f = port->getU8();
            if (f == EOF) return EOF;
            if (f <= 0xff) {
                return (ucs4char)f;
             } else {
                if (mode == ErrorHandlingMode(RAISE_ERROR)) {
                    throwIOError2(IOError::DECODE, UC("invalid latin-1 byte sequence"));
                } else if (mode == ErrorHandlingMode(REPLACE_ERROR)) {
                    return 0xFFFD;
                } else {
                    MOSH_ASSERT(mode == ErrorHandlingMode(IGNORE_ERROR));
                    goto retry;
                }
            }
            return ' ';
         */
    }
}

impl Display for Latin1Codec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<latin-1-codec>")
    }
}

/// Transcoder
#[derive(Debug)]
#[repr(C)]
pub struct Transcoder {
    pub header: GcHeader,
    pub codec: Object,
    eol_style: EolStyle,
    mode: ErrorHandlingMode,
    lineno: usize,
    is_beginning: bool,
    buffer: Vec<char>,
}

impl Transcoder {
    pub fn new(codec: Object, eol_style: EolStyle, mode: ErrorHandlingMode) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Transcoder),
            codec: codec,
            eol_style: eol_style,
            mode: mode,
            lineno: 1,
            is_beginning: true,
            buffer: vec![],
        }
    }

    pub fn read_string(&mut self, port: &mut dyn BinaryInputPort) -> error::Result<String> {
        let mut s = String::new();
        loop {
            let ch = self.read_char(port)?;
            match ch {
                Some(ch) => {
                    s.push(ch);
                }
                // EOF.
                None => break,
            }
        }
        Ok(s)
    }

    // TODO: remove GC. this is not a good design.
    pub fn read_char(&mut self, port: &mut dyn BinaryInputPort) -> error::Result<Option<char>> {
        let ch = self.read_char_raw(port)?;

        match ch {
            Some(ch) => {
                if self.eol_style == EolStyle::ENone {
                    if ch == char::LF {
                        self.lineno += 1;
                    }
                    return Ok(Some(ch));
                }
            }
            _ => {}
        }
        match ch {
            Some(char::LF) | Some(char::NEL) | Some(char::LS) => {
                self.lineno += 1;
                return Ok(Some(char::LF));
            }
            Some(char::CR) => {
                let ch2 = self.read_char_raw(port)?;
                self.lineno += 1;
                match ch2 {
                    Some(char::LF) | Some(char::NEL) => {
                        return Ok(Some(char::LF));
                    }
                    _ => {
                        self.unget_char(ch2);
                        Ok(Some(char::LF))
                    }
                }
            }
            _ => Ok(ch),
        }
    }

    fn read_char_raw(&mut self, port: &mut dyn BinaryInputPort) -> error::Result<Option<char>> {
        // In the beginning of input, we have to check the BOM.
        if self.is_beginning {
            self.is_beginning = false;
            self.codec.to_latin1_code().read_char(port, self.mode, true)
        } else if self.buffer.is_empty() {
            self.codec
                .to_latin1_code()
                .read_char(port, self.mode, false)
        } else {
            Ok(self.buffer.pop())
        }
    }

    fn unget_char(&mut self, _ch: Option<char>) {
        todo!()
    }
}

impl Display for Transcoder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<transcoder>")
    }
}

#[derive(Debug, PartialEq)]
pub enum EolStyle {
    Lf,
    Cr,
    Nel,
    Ls,
    CrNel,
    CrLf,
    ENone,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ErrorHandlingMode {
    IgnoreError,
    RaiseError,
    ReplaceError,
}

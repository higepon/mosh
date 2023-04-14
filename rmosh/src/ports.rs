use crate::error::{self, SchemeError};
use crate::gc::Trace;
use crate::numbers::{GcObjectExt, ObjectExt};
use crate::vm::{Vm, CURRENT_VM};
use crate::{
    bug, obj_as_binary_input_port_mut_or_panic, obj_as_binary_output_port_mut,
    obj_as_binary_output_port_mut_or_panic,
};
use crate::{
    gc::{Gc, GcHeader, GcRef, ObjectType},
    lexer::{self},
    objects::{CharExt, Object, Pair, SimpleStruct, Vector},
    reader::DatumParser,
};
use core::panic;
use lalrpop_util::ParseError;
use rust_embed::RustEmbed;
use std::cmp::max;
use std::io::{BufReader, BufWriter, Write};
use std::io::{Seek, SeekFrom};
use std::{
    collections::HashMap,
    fmt::{self, Display},
    fs::File,
    io::{self, Read},
};

// We embed libraries written in Scheme in the binary.
#[derive(RustEmbed)]
#[folder = "./stdlib/"]
#[prefix = "/embed/stdlib/"]
pub struct StdLib;

pub trait StdLibExt {
    fn exists(s: &str) -> bool;
}

impl StdLibExt for StdLib {
    fn exists(s: &str) -> bool {
        if s.eq("/embed/stdlib") || s.eq("/embed/stdlib/") {
            true
        } else if s.starts_with("/embed/stdlib") {
            Self::get(s).is_some()
        } else {
            false
        }
    }
}

// Trait for Port.
pub trait Port {
    fn is_open(&self) -> bool;
    fn close(&mut self);
    fn has_position(&self) -> bool {
        false
    }
    fn position(&mut self, _vm: &mut Vm) -> Result<usize, SchemeError> {
        Err(SchemeError::implementation_restriction_violation(
            "position",
            "not supported",
            &[],
        ))
    }
    fn has_set_position(&self) -> bool {
        false
    }
    fn set_position(&mut self, _vm: &mut Vm, _pos: usize) -> Result<usize, SchemeError> {
        Err(SchemeError::implementation_restriction_violation(
            "set-position",
            "not supported",
            &[],
        ))
    }

    fn buffer_mode(&self) -> BufferMode {
        BufferMode::None
    }
}

pub trait OutputPort: Port {
    fn flush(&mut self);
}

// Trait for TextInputPort.
pub trait TextInputPort: Port {
    // The methods you have to implement.
    fn read_to_string(&mut self, vm: &mut Vm, str: &mut String) -> Result<usize, SchemeError>;
    fn read_n_to_string(
        &mut self,
        vm: &mut Vm,
        str: &mut String,
        n: usize,
    ) -> Result<usize, SchemeError>;
    fn read_char(&mut self, vm: &mut Vm) -> Result<Option<char>, SchemeError>;
    fn ahead_char(&self) -> Option<char>;
    fn set_ahead_char(&mut self, c: Option<char>);
    fn input_src(&self) -> String;

    fn read_line(&mut self, vm: &mut Vm, str: &mut String) -> Result<usize, SchemeError> {
        loop {
            match self.read_char(vm) {
                Ok(Some(ch)) => {
                    if ch == '\n' {
                        break;
                    }
                    str.push(ch)
                }
                Ok(None) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(str.len())
    }
    fn set_parsed(&mut self, obj: Object);
    fn parsed(&self) -> Object;

    // (read ...)
    // LALRPOP doesn't support multiple calls of parse.
    // We parse all S-Expressions once then store them.
    fn read(&mut self, vm: &mut Vm) -> Result<Object, SchemeError> {
        //
        if self.parsed().is_unspecified() {
            let mut s = String::new();
            self.read_to_string(vm, &mut s)?;
            let s = "(".to_string() + &s;
            //println!("TRY to read {}", s);
            // re2c assumes null terminated string.
            let s = s + ")\0";
            let chars: Vec<char> = s.chars().collect();
            // Whether if we found #1# style.
            let mut shared_map: HashMap<u32, Object> = HashMap::new();
            match DatumParser::new().parse(
                &mut vm.gc,
                &mut shared_map,
                &self.input_src(),
                &s,
                lexer::Lexer::new(&chars),
            ) {
                Ok(parsed) => {
                    if !shared_map.is_empty() {
                        self.link_shared(&mut vm.gc, &shared_map, parsed);
                    }
                    self.set_parsed(parsed);
                }
                Err(ParseError::User { error }) => {
                    return Err(error);
                }
                Err(ParseError::InvalidToken { location }) => {
                    return Err(SchemeError::lexical_violation_read_error(
                        "read",
                        &format!("invalid token at {}", location),
                    ));
                }
                Err(ParseError::UnrecognizedEOF {
                    location,
                    expected: _,
                }) => {
                    return Err(SchemeError::lexical_violation_read_error(
                        "read",
                        &format!("unexpected EOF at {}", location),
                    ));
                }
                Err(ParseError::UnrecognizedToken { token, expected }) => {
                    let context_start = max(0, (token.0 as isize) - 10) as usize;
                    // Show what is causing this error.
                    let context = format!("reader: {}", &s[context_start..token.2]);
                    return Err(SchemeError::lexical_violation_read_error(
                        "read",
                        &format!(
                            "unrecognized token: {:?} expected: {:?} context: {}",
                            token.1, expected, context
                        ),
                    ));
                }
                Err(ParseError::ExtraToken { token }) => {
                    return Err(SchemeError::lexical_violation_read_error(
                        "read",
                        &format!("found extra token {:?}", token.1,),
                    ));
                }
            }
        }
        if self.parsed().is_nil() {
            Ok(Object::Eof)
        } else {
            let obj = self.parsed().car_unchecked();
            self.set_parsed(self.parsed().cdr_unchecked());
            Ok(obj)
        }
    }

    fn link_shared(&self, _gc: &mut Box<Gc>, shared_map: &HashMap<u32, Object>, obj: Object) {
        match obj {
            Object::Pair(mut p) => {
                if let Object::DefinedShared(index) = p.car {
                    match shared_map.get(&index) {
                        Some(v) => {
                            p.car = *v;
                        }
                        None => bug!(),
                    }
                } else {
                    self.link_shared(_gc, shared_map, p.car);
                }
                if let Object::DefinedShared(index) = p.cdr {
                    match shared_map.get(&index) {
                        Some(v) => {
                            p.cdr = *v;
                        }
                        None => bug!(),
                    }
                } else {
                    self.link_shared(_gc, shared_map, p.cdr);
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
                            None => bug!(),
                        }
                    } else {
                        self.link_shared(_gc, shared_map, obj);
                    }
                }
            }
            _ => (),
        }
    }

    fn lookahead_char(&mut self, vm: &mut Vm) -> Result<Option<char>, SchemeError> {
        match self.ahead_char() {
            Some(ch) => Ok(Some(ch)),
            None => match self.read_char(vm) {
                Ok(Some(c)) => {
                    self.unget_char(c);
                    Ok(Some(c))
                }
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            },
        }
    }

    fn unget_char(&mut self, c: char) {
        assert!(self.ahead_char().is_none());
        self.set_ahead_char(Some(c));
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct StdInputPort {
    pub header: GcHeader,
    ahead_u8: Option<u8>,
}

impl Trace for StdInputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for StdInputPort {
    fn default() -> Self {
        Self::new()
    }
}

impl StdInputPort {
    pub fn new() -> Self {
        StdInputPort {
            header: GcHeader::new(ObjectType::StdInputPort),
            ahead_u8: None,
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

impl BinaryInputPort for StdInputPort {
    fn read(&mut self, _vm: &mut Vm, buf: &mut [u8]) -> Result<usize, SchemeError> {
        io::stdin()
            .read(buf)
            .map_err(|e| SchemeError::io_error("read", &e.to_string(), &[]))
    }

    fn ahead_u8(&self) -> Option<u8> {
        self.ahead_u8
    }

    fn set_ahead_u8(&mut self, u: Option<u8>) {
        self.ahead_u8 = u;
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

impl Trace for FileInputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.parsed)
    }
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
    fn read_to_string(&mut self, _vm: &mut Vm, str: &mut String) -> Result<usize, SchemeError> {
        self.reader.read_to_string(str).map_err(|e| {
            error::SchemeError::lexical_violation_read_error("read", &format!("{}", e))
        })
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

    fn read_char(&mut self, vm: &mut Vm) -> Result<Option<char>, SchemeError> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Ok(Some(c))
            }
            None => {
                let mut str = String::new();
                match self.read_n_to_string(vm, &mut str, 1) {
                    Ok(_) => {
                        if str.is_empty() {
                            Ok(None)
                        } else {
                            let mut chars = str.chars();
                            Ok(chars.next())
                        }
                    }
                    Err(e) => Err(e),
                }
            }
        }
    }
    fn read_n_to_string(
        &mut self,
        _vm: &mut Vm,
        str: &mut String,
        n: usize,
    ) -> Result<usize, SchemeError> {
        let mut buf = vec![0; n];
        match self.reader.read(&mut buf) {
            Ok(size) => {
                // TODO: This doesn't work for non ascii.
                match std::str::from_utf8(&buf[0..size]) {
                    Ok(s) => {
                        str.push_str(s);
                        Ok(str.len())
                    }
                    Err(e) => Err(SchemeError::io_decoding_error(
                        "read",
                        &format!("{}", e),
                        &[],
                    )),
                }
            }
            Err(e) => Err(SchemeError::io_error("read", &format!("{}", e), &[])),
        }
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

impl Trace for StringInputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.parsed);
    }
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

    /*pub fn read_char(&mut self) -> Option<char> {
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
    }*/
}

impl Display for StringInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<string-input-port>")
    }
}

impl TextInputPort for StringInputPort {
    fn read_to_string(&mut self, vm: &mut Vm, str: &mut String) -> Result<usize, SchemeError> {
        let mut read_size = 0;
        loop {
            match self.read_char(vm) {
                Ok(Some(ch)) => str.push(ch),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
            read_size += 1;
        }
        Ok(read_size)
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
    fn read_char(&mut self, _vm: &mut Vm) -> Result<Option<char>, SchemeError> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Ok(Some(c))
            }
            None => {
                let mut chars = self.source.chars();
                let ret = chars.nth(self.idx);
                self.idx += 1;
                Ok(ret)
            }
        }
    }
    fn read_n_to_string(
        &mut self,
        vm: &mut Vm,
        str: &mut String,
        n: usize,
    ) -> Result<usize, SchemeError> {
        let mut read_size = 0;
        loop {
            if read_size >= n {
                break;
            }
            match self.read_char(vm) {
                Ok(Some(ch)) => str.push(ch),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
            read_size += 1;
        }
        Ok(read_size)
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

pub trait TextOutputPort: OutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), SchemeError>;

    // (write-char c).
    fn write_char(&mut self, c: char) -> Result<(), SchemeError> {
        if self.is_open() {
            self.put_string(&c.to_string())
        } else {
            Err(SchemeError::io_error("write-char", "port is closed", &[]))
        }
    }

    // (write obj): Machine readable print.
    fn write(&mut self, obj: Object, shared_aware: bool) -> Result<(), SchemeError> {
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
    fn display(&mut self, obj: Object, shared_aware: bool) -> Result<(), SchemeError> {
        if shared_aware {
            let mut shared_id = 1;
            let mut seen: HashMap<Object, Object> = HashMap::new();
            self.scan(obj, &mut seen);
            self.display_shared_one(obj, &mut seen, &mut shared_id, true)
        } else {
            self.display_one(obj, true)
        }
    }

    fn display_one(&mut self, obj: Object, human_readable: bool) -> Result<(), SchemeError> {
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
            | Object::CustomBinaryInputPort(_)
            | Object::CustomBinaryInputOutputPort(_)
            | Object::CustomBinaryOutputPort(_)
            | Object::CustomTextInputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::CustomTextOutputPort(_)
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
            | Object::UTF8Codec(_)
            | Object::UTF16Codec(_)
            | Object::Transcoder(_)
            | Object::TranscodedOutputPort(_)
            | Object::TranscodedInputPort(_)
            | Object::TranscodedInputOutputPort(_)
            | Object::Compnum(_)
            | Object::Ratnum(_)
            | Object::Regexp(_)
            | Object::False
            | Object::Flonum(_)
            | Object::StringInputPort(_)
            | Object::FileInputPort(_)
            | Object::Eof
            | Object::BinaryFileInputPort(_)
            | Object::BinaryFileInputOutputPort(_)
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
    ) -> Result<(), SchemeError> {
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
            | Object::Bignum(_)
            | Object::BinaryFileInputOutputPort(_)
            | Object::BinaryFileInputPort(_)
            | Object::BinaryFileOutputPort(_)
            | Object::BytevectorInputPort(_)
            | Object::BytevectorOutputPort(_)
            | Object::Char(_)
            | Object::Closure(_)
            | Object::Compnum(_)
            | Object::Continuation(_)
            | Object::ContinuationStack(_)
            | Object::CustomBinaryInputPort(_)
            | Object::CustomBinaryInputOutputPort(_)
            | Object::CustomBinaryOutputPort(_)
            | Object::CustomTextInputPort(_)
            | Object::CustomTextInputOutputPort(_)
            | Object::CustomTextOutputPort(_)
            | Object::Eof
            | Object::EqHashtable(_)
            | Object::EqvHashtable(_)
            | Object::False
            | Object::FileInputPort(_)
            | Object::FileOutputPort(_)
            | Object::Flonum(_)
            | Object::GenericHashtable(_)
            | Object::Instruction(_)
            | Object::Latin1Codec(_)
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
            | Object::TranscodedInputPort(_)
            | Object::TranscodedOutputPort(_)
            | Object::TranscodedInputOutputPort(_)
            | Object::Transcoder(_)
            | Object::True
            | Object::Unspecified
            | Object::UTF16Codec(_)
            | Object::UTF8Codec(_)
            | Object::Vox(_)
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
        false
    }

    fn display_pair(&mut self, p: GcRef<Pair>, human_readable: bool) -> Result<(), SchemeError> {
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
            self.put_string(")")
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
    ) -> Result<(), SchemeError> {
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
            self.put_string(")")
        } else {
            Ok(())
        }
    }

    fn as_display(&mut self, obj: Object) -> Result<(), SchemeError> {
        self.put_string(&format!("{}", obj))
    }

    fn as_write(&mut self, obj: Object) -> Result<(), SchemeError> {
        self.put_string(&format!("{:?}", obj))
    }

    fn display_vector(
        &mut self,
        v: GcRef<Vector>,
        human_readable: bool,
    ) -> Result<(), SchemeError> {
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
    ) -> Result<(), SchemeError> {
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
    ) -> Result<(), SchemeError> {
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
    ) -> Result<(), SchemeError> {
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
                | Object::BinaryFileInputOutputPort(_)
                | Object::BinaryFileInputPort(_)
                | Object::BinaryFileOutputPort(_)
                | Object::BytevectorInputPort(_)
                | Object::BytevectorOutputPort(_)
                | Object::Char(_)
                | Object::Closure(_)
                | Object::Compnum(_)
                | Object::Continuation(_)
                | Object::ContinuationStack(_)
                | Object::CustomBinaryInputPort(_)
                | Object::CustomBinaryInputOutputPort(_)
                | Object::CustomBinaryOutputPort(_)
                | Object::CustomTextInputPort(_)
                | Object::CustomTextInputOutputPort(_)
                | Object::CustomTextOutputPort(_)
                | Object::Eof
                | Object::EqHashtable(_)
                | Object::EqvHashtable(_)
                | Object::False
                | Object::FileInputPort(_)
                | Object::FileOutputPort(_)
                | Object::Flonum(_)
                | Object::GenericHashtable(_)
                | Object::Instruction(_)
                | Object::Latin1Codec(_)
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
                | Object::TranscodedInputPort(_)
                | Object::TranscodedOutputPort(_)
                | Object::TranscodedInputOutputPort(_)
                | Object::Transcoder(_)
                | Object::True
                | Object::Unspecified
                | Object::UTF16Codec(_)
                | Object::UTF8Codec(_)
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
    fn format(&mut self, fmt: &str, args: &mut [Object]) -> Result<(), SchemeError> {
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
                            return Err(SchemeError::io_error(
                                "format",
                                "not enough arguments",
                                &[],
                            ));
                        }
                    } else if c == 's' {
                        let shared_aware = false;
                        if i < args.len() {
                            self.write(args[i], shared_aware).ok();
                            i += 1;
                        } else {
                            return Err(SchemeError::io_error(
                                "format",
                                "not enough arguments",
                                &[],
                            ));
                        }
                    } else {
                        return Err(SchemeError::io_error(
                            "format",
                            &format!("unknown format specifier ~{}", c),
                            &[],
                        ));
                    }
                } else {
                    break;
                }
            } else {
                self.put_string(&format!("{}", c)).ok();
            }
        }
        Ok(())
    }
}

// Trait for Port.
pub trait BinaryInputPort: Port {
    fn read(&mut self, vm: &mut Vm, buf: &mut [u8]) -> Result<usize, SchemeError>;
    fn ahead_u8(&self) -> Option<u8>;
    fn set_ahead_u8(&mut self, c: Option<u8>);

    fn read_u8(&mut self, vm: &mut Vm) -> Result<Option<u8>, SchemeError> {
        match self.ahead_u8() {
            Some(u) => {
                self.set_ahead_u8(None);
                Ok(Some(u))
            }
            None => {
                let mut buf = [0; 1];
                let size = self.read(vm, &mut buf)?;
                if size == 1 {
                    Ok(Some(buf[0]))
                } else {
                    Ok(None)
                }
            }
        }
    }

    // This default implementation is not very efficient :)
    fn read_all(&mut self, vm: &mut Vm, buf: &mut Vec<u8>) -> Result<usize, SchemeError> {
        let mut size = 0;

        while let Ok(Some(u)) = self.read_u8(vm) {
            buf.push(u);
            size += 1;
        }

        Ok(size)
    }

    fn lookahead_u8(&mut self, vm: &mut Vm) -> Result<Option<u8>, SchemeError> {
        match self.ahead_u8() {
            Some(u) => Ok(Some(u)),
            None => match self.read_u8(vm) {
                Ok(Some(u)) => {
                    self.unget_u8(u);
                    Ok(Some(u))
                }
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            },
        }
    }

    fn unget_u8(&mut self, u: u8) {
        assert!(self.ahead_u8().is_none());
        self.set_ahead_u8(Some(u));
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
    ahead_u8: Option<u8>,
}

impl Trace for BytevectorInputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl BytevectorInputPort {
    pub fn new(data: &[u8]) -> Self {
        BytevectorInputPort {
            header: GcHeader::new(ObjectType::BytevectorInputPort),
            is_closed: false,
            idx: 0,
            data: data.to_owned(),
            ahead_u8: None,
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
    fn read_u8(&mut self, _vm: &mut Vm) -> Result<Option<u8>, SchemeError> {
        match self.ahead_u8() {
            Some(u) => {
                self.set_ahead_u8(None);
                Ok(Some(u))
            }
            None => match self.data.get(self.idx) {
                Some(u) => {
                    self.idx += 1;
                    Ok(Some(*u))
                }
                None => Ok(None),
            },
        }
    }

    fn read(&mut self, vm: &mut Vm, buf: &mut [u8]) -> Result<usize, SchemeError> {
        let size = buf.len();
        let mut i = 0;
        loop {
            if i >= size {
                break;
            }
            match self.read_u8(vm) {
                Ok(Some(u)) => {
                    buf[i] = u;
                    i += 1;
                }
                Ok(None) => {
                    break;
                }
                Err(e) => return Err(e),
            }
        }
        Ok(i)
    }

    fn ahead_u8(&self) -> Option<u8> {
        self.ahead_u8
    }

    fn set_ahead_u8(&mut self, u: Option<u8>) {
        self.ahead_u8 = u;
    }
}

impl Display for BytevectorInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Don't change this name. This is used in u8-ready?
        write!(f, "#<byte-array-input-port>")
    }
}

// Trait for binary output port.
pub trait BinaryOutputPort: OutputPort {
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError>;

    fn put_u8(&mut self, value: u8) -> Result<usize, SchemeError> {
        self.write(&[value])
    }

    fn put_u16(&mut self, value: u16) -> Result<usize, SchemeError> {
        self.write(&value.to_le_bytes())
    }

    fn put_u32(&mut self, value: u32) -> Result<usize, SchemeError> {
        self.write(&value.to_le_bytes())
    }

    fn put_u64(&mut self, value: u64) -> Result<usize, SchemeError> {
        self.write(&value.to_le_bytes())
    }

    fn put_i64(&mut self, value: i64) -> Result<usize, SchemeError> {
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
impl Trace for BytevectorOutputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for BytevectorOutputPort {
    fn default() -> Self {
        Self::new()
    }
}

impl BytevectorOutputPort {
    pub fn new() -> Self {
        BytevectorOutputPort {
            header: GcHeader::new(ObjectType::BytevectorOutputPort),
            is_closed: false,
            data: vec![],
        }
    }

    pub fn to_bytevector(&mut self, gc: &mut Box<Gc>) -> Object {
        let ret = gc.new_bytevector_u8(&self.data);
        self.data.clear();
        ret
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
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
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
    ahead_u8: Option<u8>,
    buffer_mode: BufferMode,
}

impl Trace for BinaryFileInputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl BinaryFileInputPort {
    pub fn new(file: File, buffer_mode: BufferMode) -> Self {
        BinaryFileInputPort {
            header: GcHeader::new(ObjectType::BinaryFileInputPort),
            is_closed: false,
            reader: BufReader::new(file),
            ahead_u8: None,
            buffer_mode,
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
    fn buffer_mode(&self) -> BufferMode {
        self.buffer_mode
    }

    fn has_set_position(&self) -> bool {
        true
    }

    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        match self.reader.seek(SeekFrom::Start(pos as u64)) {
            Ok(pos) => Ok(pos as usize),
            Err(e) => Err(SchemeError::io_invalid_position(
                "set-position!",
                &format!("invalid position: {}", e),
                &[pos.to_obj(&mut vm.gc)],
                pos as isize,
            )),
        }
    }
}

impl BinaryInputPort for BinaryFileInputPort {
    fn read(&mut self, _vm: &mut Vm, buf: &mut [u8]) -> Result<usize, SchemeError> {
        let read_start: usize;
        match self.ahead_u8() {
            Some(u) => {
                if !buf.is_empty() {
                    buf[0] = u;
                    read_start = 1
                } else {
                    read_start = 0;
                }
            }
            None => read_start = 0,
        }
        self.reader
            .read(&mut buf[read_start..])
            .map_err(|e| SchemeError::io_error("read", &e.to_string(), &[]))
    }

    fn ahead_u8(&self) -> Option<u8> {
        self.ahead_u8
    }

    fn set_ahead_u8(&mut self, u: Option<u8>) {
        self.ahead_u8 = u;
    }
}

impl Display for BinaryFileInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<binary-file-input-port {:?}>", self.reader.get_ref())
    }
}

// BinaryFileInputOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct BinaryFileInputOutputPort {
    pub header: GcHeader,
    pub file: File,
    is_closed: bool,
    buffer_mode: BufferMode,
}

impl Trace for BinaryFileInputOutputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl BinaryFileInputOutputPort {
    pub fn new(file: File, buffer_mode: BufferMode) -> Self {
        BinaryFileInputOutputPort {
            header: GcHeader::new(ObjectType::BinaryFileInputOutputPort),
            is_closed: false,
            file,
            buffer_mode,
        }
    }

    pub fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        self.file.read_to_end(buf)
    }

    pub fn close(&mut self) {
        self.is_closed = true;
    }
}

impl Port for BinaryFileInputOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.file.flush().ok();
        self.is_closed = true;
    }
    fn buffer_mode(&self) -> BufferMode {
        self.buffer_mode
    }
    fn has_position(&self) -> bool {
        true
    }
    fn has_set_position(&self) -> bool {
        true
    }
    fn position(&mut self, _vm: &mut Vm) -> Result<usize, SchemeError> {
        let current_position = self.file.seek(SeekFrom::Current(0)).map_err(|e| {
            SchemeError::assertion_violation(
                "port-position",
                &format!("failed get port-position {}", e),
                &[],
            )
        })?;
        Ok(current_position as usize)
    }
    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        match self.file.seek(SeekFrom::Start(pos as u64)) {
            Ok(pos) => Ok(pos as usize),
            Err(e) => Err(SchemeError::io_invalid_position(
                "set-position!",
                &format!("invalid position: {}", e),
                &[pos.to_obj(&mut vm.gc)],
                pos as isize,
            )),
        }
    }
}

impl BinaryInputPort for BinaryFileInputOutputPort {
    fn read(&mut self, _vm: &mut Vm, buf: &mut [u8]) -> Result<usize, SchemeError> {
        self.file
            .read(buf)
            .map_err(|e| SchemeError::io_error("read", &e.to_string(), &[]))
    }

    fn ahead_u8(&self) -> Option<u8> {
        None
    }

    fn set_ahead_u8(&mut self, _c: Option<u8>) {}

    fn lookahead_u8(&mut self, vm: &mut Vm) -> Result<Option<u8>, SchemeError> {
        let pos = self.position(vm)?;
        match self.read_u8(vm) {
            Ok(Some(u)) => {
                self.set_position(vm, pos)?;
                Ok(Some(u))
            }
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

impl BinaryOutputPort for BinaryFileInputOutputPort {
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        self.file
            .write(buf)
            .map_err(|e| SchemeError::io_error("write", &e.to_string(), &[]))
    }
}

impl OutputPort for BinaryFileInputOutputPort {
    fn flush(&mut self) {
        self.file.flush().unwrap_or(())
    }
}

impl Display for BinaryFileInputOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<binary-file-input-output-port {:?}>", self.file)
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

impl Trace for FileOutputPort {
    fn trace(&self, _gc: &mut Gc) {}
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

impl OutputPort for FileOutputPort {
    fn flush(&mut self) {
        self.writer.flush().unwrap_or(())
    }
}

impl TextOutputPort for FileOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), SchemeError> {
        write!(self.writer, "{}", s)
            .map_err(|e| SchemeError::io_error("put-string", &format!("write error {}", e), &[]))
    }
}

// StdOutputPort
#[repr(C)]
pub struct StdOutputPort {
    pub header: GcHeader,
}

impl Trace for StdOutputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for StdOutputPort {
    fn default() -> Self {
        Self::new()
    }
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

impl OutputPort for StdOutputPort {
    fn flush(&mut self) {
        // There's nothing we can do here if flush resutns error.
        io::stdout().flush().unwrap_or(())
    }
}

impl BinaryOutputPort for StdOutputPort {
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        let ret = io::stdout()
            .write(buf)
            .map_err(|e| SchemeError::io_error("write", &e.to_string(), &[]));
        self.flush();
        ret
    }
}

// StdErrorPort
#[repr(C)]
pub struct StdErrorPort {
    pub header: GcHeader,
}

impl Trace for StdErrorPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for StdErrorPort {
    fn default() -> Self {
        Self::new()
    }
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

impl OutputPort for StdErrorPort {
    fn flush(&mut self) {
        // There's nothing we can do here if flush resutns error.
        io::stdout().flush().unwrap_or(())
    }
}

impl BinaryOutputPort for StdErrorPort {
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        io::stderr()
            .write(buf)
            .map_err(|e| SchemeError::io_error("write", &e.to_string(), &[]))
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

impl Trace for StringOutputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for StringOutputPort {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn string(&mut self) -> String {
        let ret = self.string.to_string();
        self.string.clear();
        ret
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
impl OutputPort for StringOutputPort {
    fn flush(&mut self) {}
}

impl TextOutputPort for StringOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), SchemeError> {
        self.string.push_str(s);
        Ok(())
    }
}

// TranscodedInputPort
#[derive(Debug)]
#[repr(C)]
pub struct TranscodedInputPort {
    pub header: GcHeader,
    is_closed: bool,
    pub in_port: Object,
    pub transcoder: Object,
    ahead_char: Option<char>,
    pub parsed: Object,
}

impl Trace for TranscodedInputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.in_port);
        gc.mark_object(self.transcoder);
        gc.mark_object(self.parsed);
    }
}

impl TranscodedInputPort {
    pub fn new(in_port: Object, transcoder: Object) -> Self {
        TranscodedInputPort {
            header: GcHeader::new(ObjectType::TranscodedInputPort),
            is_closed: false,
            in_port,
            transcoder,
            ahead_char: None,
            parsed: Object::Unspecified,
        }
    }
}

impl Port for TranscodedInputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
    fn buffer_mode(&self) -> BufferMode {
        let port = obj_as_binary_input_port_mut_or_panic!(self.in_port);
        port.buffer_mode()
    }
}

impl Display for TranscodedInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<transcoded-input-port>")
    }
}

impl TextInputPort for TranscodedInputPort {
    fn read_to_string(&mut self, vm: &mut Vm, str: &mut String) -> Result<usize, SchemeError> {
        let mut i: usize = 1;
        loop {
            match self.read_char(vm) {
                Ok(Some(ch)) => str.push(ch),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
            i += 1;
        }
        Ok(i)
    }

    fn read_n_to_string(
        &mut self,
        vm: &mut Vm,
        str: &mut String,
        n: usize,
    ) -> Result<usize, SchemeError> {
        let mut size = 0;
        loop {
            match self.read_char(vm) {
                Ok(Some(ch)) => str.push(ch),
                Ok(None) => return Ok(size),
                Err(e) => return Err(e),
            }
            size += 1;
            if size == n {
                break;
            }
        }
        Ok(n)
    }

    fn read_char(&mut self, vm: &mut Vm) -> Result<Option<char>, SchemeError> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Ok(Some(c))
            }
            None => {
                let port = obj_as_binary_input_port_mut_or_panic!(self.in_port);
                let mut t = self.transcoder.to_transcoder();
                match t.read_char(vm, port) {
                    Ok(Some(ch)) => Ok(Some(ch)),
                    Ok(None) => Ok(None),
                    Err(e) => Err(e),
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

    fn input_src(&self) -> String {
        "todo: input source".to_string()
    }

    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

// TranscodedOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct TranscodedOutputPort {
    pub header: GcHeader,
    is_closed: bool,
    pub out_port: Object,
    pub transcoder: Object,
}

impl Trace for TranscodedOutputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.out_port);
        gc.mark_object(self.transcoder);
    }
}

impl TranscodedOutputPort {
    pub fn new(out_port: Object, transcoder: Object) -> Self {
        TranscodedOutputPort {
            header: GcHeader::new(ObjectType::TranscodedOutputPort),
            is_closed: false,
            out_port,
            transcoder,
        }
    }
}

impl Port for TranscodedOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        let port = obj_as_binary_output_port_mut_or_panic!(self.out_port);
        port.close();
        self.is_closed = true;
    }
    fn buffer_mode(&self) -> BufferMode {
        let port = obj_as_binary_output_port_mut_or_panic!(self.out_port);
        port.buffer_mode()
    }
}

impl Display for TranscodedOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<transcoded-output-port>")
    }
}

impl OutputPort for TranscodedOutputPort {
    fn flush(&mut self) {
        let port = obj_as_binary_output_port_mut_or_panic!(self.out_port);
        port.flush()
    }
}

impl TextOutputPort for TranscodedOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), SchemeError> {
        let port = obj_as_binary_output_port_mut_or_panic!(self.out_port);
        let mut transcoder = self.transcoder.to_transcoder();
        transcoder.write_string(port, s)
    }
}

// TranscodedInputOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct TranscodedInputOutputPort {
    pub header: GcHeader,
    is_closed: bool,
    pub port: Object,
    pub transcoder: Object,
    ahead_char: Option<char>,
    pub parsed: Object,
}

impl Trace for TranscodedInputOutputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.port);
        gc.mark_object(self.transcoder);
        gc.mark_object(self.parsed);
    }
}

impl TranscodedInputOutputPort {
    pub fn new(port: Object, transcoder: Object) -> Self {
        TranscodedInputOutputPort {
            header: GcHeader::new(ObjectType::TranscodedInputOutputPort),
            is_closed: false,
            port,
            transcoder,
            ahead_char: None,
            parsed: Object::Unspecified,
        }
    }
}

impl Port for TranscodedInputOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
    fn buffer_mode(&self) -> BufferMode {
        let port = obj_as_binary_input_port_mut_or_panic!(self.port);
        port.buffer_mode()
    }
}

impl Display for TranscodedInputOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<transcoded-input/output-port>")
    }
}

impl TextInputPort for TranscodedInputOutputPort {
    fn read_to_string(&mut self, vm: &mut Vm, str: &mut String) -> Result<usize, SchemeError> {
        let mut i: usize = 1;
        loop {
            match self.read_char(vm) {
                Ok(Some(ch)) => str.push(ch),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
            i += 1;
        }
        Ok(i)
    }

    fn read_n_to_string(
        &mut self,
        vm: &mut Vm,
        str: &mut String,
        n: usize,
    ) -> Result<usize, SchemeError> {
        let mut size = 0;
        loop {
            match self.read_char(vm) {
                Ok(Some(ch)) => str.push(ch),
                Ok(None) => return Ok(size),
                Err(e) => return Err(e),
            }
            size += 1;
            if size == n {
                break;
            }
        }
        Ok(n)
    }

    fn read_char(&mut self, vm: &mut Vm) -> Result<Option<char>, SchemeError> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Ok(Some(c))
            }
            None => {
                let port = obj_as_binary_input_port_mut_or_panic!(self.port);
                let mut t = self.transcoder.to_transcoder();
                match t.read_char(vm, port) {
                    Ok(Some(ch)) => Ok(Some(ch)),
                    Ok(None) => Ok(None),
                    Err(e) => Err(e),
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

    fn input_src(&self) -> String {
        "todo: input source".to_string()
    }

    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

impl OutputPort for TranscodedInputOutputPort {
    fn flush(&mut self) {
        // todo
        //self.file.flush().unwrap_or(())
    }
}

impl TextOutputPort for TranscodedInputOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), SchemeError> {
        let port = obj_as_binary_output_port_mut!("put-string", self.port);
        let mut transcoder = self.transcoder.to_transcoder();
        transcoder.write_string(port, s)
    }
}

// BinaryFileOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct BinaryFileOutputPort {
    pub header: GcHeader,
    pub writer: BufWriter<File>,
    is_closed: bool,
    buffer_mode: BufferMode,
}

impl Trace for BinaryFileOutputPort {
    fn trace(&self, _gc: &mut Gc) {}
}

impl BinaryFileOutputPort {
    pub fn new(file: File, buffer_mode: BufferMode) -> Self {
        BinaryFileOutputPort {
            header: GcHeader::new(ObjectType::BinaryFileOutputPort),
            is_closed: false,
            writer: BufWriter::new(file),
            buffer_mode,
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

    fn has_position(&self) -> bool {
        true
    }
    fn position(&mut self, _vm: &mut Vm) -> Result<usize, SchemeError> {
        self.writer.flush().ok();
        match self.writer.stream_position() {
            Ok(pos) => Ok(pos as usize),
            Err(e) => Err(SchemeError::io_error("position", &e.to_string(), &[])),
        }
    }
    fn has_set_position(&self) -> bool {
        true
    }
    fn set_position(&mut self, _vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        self.writer.flush().ok();
        match self.writer.seek(SeekFrom::Start(pos as u64)) {
            Ok(pos) => Ok(pos as usize),
            Err(e) => Err(SchemeError::io_error("set-position!", &e.to_string(), &[])),
        }
    }

    fn buffer_mode(&self) -> BufferMode {
        self.buffer_mode
    }
}

impl OutputPort for BinaryFileOutputPort {
    fn flush(&mut self) {
        self.writer.flush().unwrap_or(())
    }
}

impl BinaryOutputPort for BinaryFileOutputPort {
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        self.writer
            .write(buf)
            .map_err(|e| SchemeError::io_error("write", &e.to_string(), &[]))
    }
}

impl Display for BinaryFileOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<binary-file-output-port>")
    }
}

/// Codec
pub trait Codec {
    fn read_char(
        &mut self,
        vm: &mut Vm,
        port: &mut dyn BinaryInputPort,
        mode: ErrorHandlingMode,
        should_check_bom: bool,
    ) -> Result<Option<char>, SchemeError>;

    fn write_char(
        &mut self,
        port: &mut dyn BinaryOutputPort,
        ch: char,
        mode: ErrorHandlingMode,
    ) -> Result<usize, SchemeError>;
}

/// Latin1Codec
#[derive(Debug)]
#[repr(C)]
pub struct Latin1Codec {
    pub header: GcHeader,
}

impl Trace for Latin1Codec {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for Latin1Codec {
    fn default() -> Self {
        Self::new()
    }
}

impl Latin1Codec {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::Latin1Codec),
        }
    }
}

impl Codec for Latin1Codec {
    fn read_char(
        &mut self,
        vm: &mut Vm,
        port: &mut dyn BinaryInputPort,
        mode: ErrorHandlingMode,
        _should_check_bom: bool,
    ) -> Result<Option<char>, SchemeError> {
        loop {
            match port.read_u8(vm) {
                Ok(Some(u)) => return Ok(Some(u as char)),
                Ok(None) => {
                    return Ok(None);
                }
                Err(e) => match mode {
                    ErrorHandlingMode::IgnoreError => {
                        continue;
                    }
                    ErrorHandlingMode::RaiseError => {
                        return Err(e);
                    }
                    ErrorHandlingMode::ReplaceError => return Ok(Some('\u{FFFD}')),
                },
            }
        }
    }
    fn write_char(
        &mut self,
        port: &mut dyn BinaryOutputPort,
        ch: char,
        mode: ErrorHandlingMode,
    ) -> Result<usize, SchemeError> {
        let u = ch as u32;
        let mut buf: Vec<u8> = vec![];
        if u < 0xff {
            buf.push(u as u8);
        } else {
            match mode {
                ErrorHandlingMode::IgnoreError => return Ok(0),
                ErrorHandlingMode::RaiseError => {
                    return Err(SchemeError::io_encoding_error(
                        "latin-1-codec",
                        "char should be smaller 0xff",
                        &[Object::Char(ch)],
                    ))
                }
                ErrorHandlingMode::ReplaceError => {
                    buf.push(b'?');
                }
            }
        }
        port.write(&buf)
    }
}

impl Display for Latin1Codec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<latin-1-codec>")
    }
}

/// UTF8Codec
#[derive(Debug)]
#[repr(C)]
pub struct UTF8Codec {
    pub header: GcHeader,
}

impl Trace for UTF8Codec {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for UTF8Codec {
    fn default() -> Self {
        Self::new()
    }
}

impl UTF8Codec {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::UTF8Codec),
        }
    }

    fn is_utf8_tail(&self, u: u8) -> bool {
        (0x80..=0xbf).contains(&u)
    }

    fn decoding_error(&self, msg: &str) -> Result<Option<char>, SchemeError> {
        Err(SchemeError::io_decoding_error(
            "utf-8-codec",
            &format!("invalid utf8 sequence: {}", msg),
            &[],
        ))
    }
}

impl Codec for UTF8Codec {
    fn read_char(
        &mut self,
        vm: &mut Vm,
        port: &mut dyn BinaryInputPort,
        mode: ErrorHandlingMode,
        _should_check_bom: bool,
    ) -> Result<Option<char>, SchemeError> {
        'retry: loop {
            match port.read_u8(vm) {
                Ok(Some(first)) => {
                    match first {
                        // UTF8-1(ascii) = %x00-7F
                        0..=0x7F => return Ok(Some(first as char)),
                        // UTF8-2 = %xC2-DF UTF8-tail
                        0xC2..=0xDF => match port.read_u8(vm) {
                            Ok(Some(second)) => {
                                if self.is_utf8_tail(second) {
                                    let u =
                                        (((first as u32) & 0x1f) << 6) | ((second as u32) & 0x3f);
                                    match char::from_u32(u) {
                                        Some(ch) => return Ok(Some(ch)),
                                        None => match mode {
                                            ErrorHandlingMode::IgnoreError => continue 'retry,
                                            ErrorHandlingMode::RaiseError => {
                                                return self.decoding_error(&format!(
                                                    "can't convert {} to char",
                                                    u
                                                ))
                                            }
                                            ErrorHandlingMode::ReplaceError => {
                                                return Ok(Some('\u{FFFD}'))
                                            }
                                        },
                                    }
                                } else {
                                    match mode {
                                        ErrorHandlingMode::IgnoreError => continue 'retry,
                                        ErrorHandlingMode::RaiseError => {
                                            return self.decoding_error(&format!(
                                                "second byte <{}> is not utf8 tail",
                                                second
                                            ));
                                        }
                                        ErrorHandlingMode::ReplaceError => {
                                            return Ok(Some('\u{FFFD}'))
                                        }
                                    }
                                }
                            }
                            // EOF
                            Ok(None) | Err(_) => return Ok(None),
                        },
                        // UTF8-3 = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
                        //          %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
                        0xE0..=0xEF => match (port.read_u8(vm), port.read_u8(vm)) {
                            (Ok(Some(second)), Ok(Some(third))) => {
                                if !self.is_utf8_tail(second) {
                                    match mode {
                                        ErrorHandlingMode::IgnoreError => continue 'retry,
                                        ErrorHandlingMode::RaiseError => {
                                            return self.decoding_error(&format!(
                                                "second byte <{}> is not utf8 tail",
                                                second
                                            ));
                                        }
                                        ErrorHandlingMode::ReplaceError => {
                                            return Ok(Some('\u{FFFD}'))
                                        }
                                    }
                                } else if (0xe0 == first && (0xa0..=0xbf).contains(&second))
                                    || (0xed == first && (0x80..=0x9f).contains(&second))
                                    || ((0xe1..=0xec).contains(&first) && self.is_utf8_tail(second))
                                    || ((0xee == first || 0xef == first)
                                        && self.is_utf8_tail(second))
                                {
                                    let u = (((first as u32) & 0xf) << 12)
                                        | (((second as u32) & 0x3f) << 6)
                                        | ((third as u32) & 0x3f);
                                    match char::from_u32(u) {
                                        Some(ch) => return Ok(Some(ch)),
                                        None => match mode {
                                            ErrorHandlingMode::IgnoreError => continue 'retry,
                                            ErrorHandlingMode::RaiseError => {
                                                return self.decoding_error(&format!(
                                                    "can't convert {} to char",
                                                    u
                                                ))
                                            }
                                            ErrorHandlingMode::ReplaceError => {
                                                return Ok(Some('\u{FFFD}'))
                                            }
                                        },
                                    }
                                } else {
                                    match mode {
                                        ErrorHandlingMode::IgnoreError => continue 'retry,
                                        ErrorHandlingMode::RaiseError => {
                                            return self.decoding_error(&format!(
                                            "malformed bytes second byte <{}>, third byte <{}> ",
                                            second, third
                                        ));
                                        }
                                        ErrorHandlingMode::ReplaceError => {
                                            return Ok(Some('\u{FFFD}'))
                                        }
                                    }
                                }
                            }
                            _ => return Ok(None),
                        },
                        // UTF8-4 = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
                        //          %xF4 %x80-8F 2( UTF8-tail )
                        0xf0..=0xf4 => match (port.read_u8(vm), port.read_u8(vm), port.read_u8(vm))
                        {
                            (Ok(Some(second)), Ok(Some(third)), Ok(Some(fourth))) => {
                                if !self.is_utf8_tail(third) || !self.is_utf8_tail(fourth) {
                                    match mode {
                                        ErrorHandlingMode::IgnoreError => continue 'retry,
                                        ErrorHandlingMode::RaiseError => {
                                            return self.decoding_error(&format!(
                                            "third byte <{}> and forth byte <{}> are not utf8 tail",
                                            third, fourth,
                                        ));
                                        }
                                        ErrorHandlingMode::ReplaceError => {
                                            return Ok(Some('\u{FFFD}'))
                                        }
                                    }
                                } else if (0xf0 == first && (0x90..=0xbf).contains(&second))
                                    || (0xf4 == first && (0x80..=0x8f).contains(&second))
                                    || ((0xf1..=0xf3).contains(&first) && self.is_utf8_tail(second))
                                {
                                    let u = (((first as u32) & 0x7) << 18)
                                        | (((second as u32) & 0x3f) << 12)
                                        | (((third as u32) & 0x3f) << 6)
                                        | (fourth as u32);
                                    match char::from_u32(u) {
                                        Some(ch) => return Ok(Some(ch)),
                                        None => match mode {
                                            ErrorHandlingMode::IgnoreError => continue 'retry,
                                            ErrorHandlingMode::RaiseError => {
                                                return self.decoding_error(&format!(
                                                    "can't convert {} to char",
                                                    u
                                                ))
                                            }
                                            ErrorHandlingMode::ReplaceError => {
                                                return Ok(Some('\u{FFFD}'))
                                            }
                                        },
                                    }
                                } else {
                                    match mode {
                                        ErrorHandlingMode::IgnoreError => continue 'retry,
                                        ErrorHandlingMode::RaiseError => {
                                            return self.decoding_error(&format!(
                                                "malformed utf8 {} {} {} {}",
                                                first, second, third, fourth,
                                            ));
                                        }
                                        ErrorHandlingMode::ReplaceError => {
                                            return Ok(Some('\u{FFFD}'))
                                        }
                                    }
                                }
                            }
                            _ => return Ok(None),
                        },
                        _ => match mode {
                            ErrorHandlingMode::IgnoreError => continue 'retry,
                            ErrorHandlingMode::RaiseError => {
                                return self.decoding_error(&format!(
                                    "malformed utf8 first byte was {}",
                                    first
                                ))
                            }
                            ErrorHandlingMode::ReplaceError => return Ok(Some('\u{FFFD}')),
                        },
                    }
                }
                // EOF
                Ok(None) => return Ok(None),
                Err(e) => return self.decoding_error(&format!("{:?}", e)),
            }
        }
    }

    fn write_char(
        &mut self,
        port: &mut dyn BinaryOutputPort,
        ch: char,
        mode: ErrorHandlingMode,
    ) -> Result<usize, SchemeError> {
        let u = ch as u32;
        let mut buf: Vec<u8> = vec![];
        // UTF8-1
        if u < 0x80 {
            buf.push(u as u8);
        } else if u < 0x7ff {
            buf.push((0xc0 | ((u >> 6) & 0x1f)) as u8);
            buf.push((0x80 | (u & 0x3f)) as u8);
        } else if u < 0xffff {
            buf.push((0xe0 | ((u >> 12) & 0xf)) as u8);
            buf.push((0x80 | ((u >> 6) & 0x3f)) as u8);
            buf.push((0x80 | (u & 0x3f)) as u8);
        } else if u < 0x10ffff {
            buf.push((0xf0 | ((u >> 18) & 0x7)) as u8);
            buf.push((0x80 | ((u >> 12) & 0x3f)) as u8);
            buf.push((0x80 | ((u >> 6) & 0x3f)) as u8);
            buf.push((0x80 | (u & 0x3f)) as u8);
        } else {
            match mode {
                ErrorHandlingMode::IgnoreError => return Ok(buf.len()),
                ErrorHandlingMode::RaiseError => {
                    return Err(SchemeError::io_decoding_error(
                        "utf-8-codec",
                        &format!("invalid utf-8 sequence: {}", u),
                        &[],
                    ))
                }
                ErrorHandlingMode::ReplaceError => {
                    buf.push(0xff);
                    buf.push(0xfd);
                }
            }
        }
        port.write(&buf)
    }
}

impl Display for UTF8Codec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<utf-8-codec>")
    }
}

/// UTF16Codec
#[derive(Debug)]
#[repr(C)]
pub struct UTF16Codec {
    pub header: GcHeader,
    dont_check_bom: bool,
    is_little_endian: bool,
    is_native_little_endian: bool,
}

impl Trace for UTF16Codec {
    fn trace(&self, _gc: &mut Gc) {}
}

impl Default for UTF16Codec {
    fn default() -> Self {
        Self::new()
    }
}

impl UTF16Codec {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::UTF16Codec),
            dont_check_bom: false,
            is_little_endian: false,
            is_native_little_endian: false,
        }
    }

    fn decoding_error(&self) -> Result<Option<char>, SchemeError> {
        Err(SchemeError::io_decoding_error(
            "utf-16-codec",
            "invalid utf16 sequence",
            &[],
        ))
    }
}

impl Codec for UTF16Codec {
    fn read_char(
        &mut self,
        vm: &mut Vm,
        port: &mut dyn BinaryInputPort,
        mode: ErrorHandlingMode,
        should_check_bom: bool,
    ) -> Result<Option<char>, SchemeError> {
        'retry: loop {
            match (port.read_u8(vm), port.read_u8(vm)) {
                (Ok(None), _) => return Ok(None),
                (_, Ok(None)) | (Err(_), _) | (_, Err(_)) => match mode {
                    ErrorHandlingMode::IgnoreError => continue 'retry,
                    ErrorHandlingMode::RaiseError => return self.decoding_error(),
                    ErrorHandlingMode::ReplaceError => return Ok(Some('\u{FFFD}')),
                },
                (Ok(Some(a)), Ok(Some(b))) => {
                    if should_check_bom && !self.dont_check_bom {
                        if a == 0xFE && b == 0xFF {
                            self.is_little_endian = false;
                            return self.read_char(vm, port, mode, false);
                        } else if a == 0xFF && b == 0xFE {
                            self.is_little_endian = true;
                            return self.read_char(vm, port, mode, false);
                        } else {
                            self.is_little_endian = self.is_native_little_endian;
                            // fall through.
                        }
                    }
                    let a = a as u16;
                    let b = b as u16;

                    let val1 = if self.is_little_endian {
                        (b << 8) | a
                    } else {
                        (a << 8) | b
                    };
                    if !(0xD800..=0xDFFF).contains(&val1) {
                        match char::from_u32(val1 as u32) {
                            Some(ch) => return Ok(Some(ch)),
                            None => match mode {
                                ErrorHandlingMode::IgnoreError => continue 'retry,
                                ErrorHandlingMode::RaiseError => return self.decoding_error(),
                                ErrorHandlingMode::ReplaceError => return Ok(Some('\u{FFFD}')),
                            },
                        }
                    }
                    match (port.read_u8(vm), port.read_u8(vm)) {
                        (Ok(None), _) | (_, Ok(None)) | (Err(_), _) | (_, Err(_)) => {
                            return Ok(None)
                        }
                        (Ok(Some(c)), Ok(Some(d))) => {
                            let c = c as u16;
                            let d = d as u16;
                            let val2 = if self.is_little_endian {
                                (d << 8) | c
                            } else {
                                (c << 8) | d
                            };
                            // http://unicode.org/faq/utf_bom.html#utf16-3
                            let hi = val1 as u32;
                            let lo = val2 as u32;
                            let x = (hi & ((1 << 6) - 1)) << 10 | (lo & ((1 << 10) - 1));
                            let w = (hi >> 6) & ((1 << 5) - 1);
                            let u = w + 1;
                            let c = u << 16 | x;
                            match char::from_u32(c) {
                                Some(ch) => return Ok(Some(ch)),
                                None => match mode {
                                    ErrorHandlingMode::IgnoreError => continue 'retry,
                                    ErrorHandlingMode::RaiseError => return self.decoding_error(),
                                    ErrorHandlingMode::ReplaceError => return Ok(Some('\u{FFFD}')),
                                },
                            }
                        }
                    }
                }
            }
        }
    }
    fn write_char(
        &mut self,
        port: &mut dyn BinaryOutputPort,
        ch: char,
        mode: ErrorHandlingMode,
    ) -> Result<usize, SchemeError> {
        let u = ch as u32;
        let mut buf: Vec<u8> = vec![];
        if u > 0x10FFFF {
            match mode {
                ErrorHandlingMode::IgnoreError => {
                    return Ok(0);
                }
                ErrorHandlingMode::RaiseError => {
                    return Err(SchemeError::io_encoding_error(
                        "utf-16-codec",
                        "character out of utf16 range",
                        &[Object::Char(ch)],
                    ))
                }
                ErrorHandlingMode::ReplaceError => {
                    buf.push(0xff);
                    buf.push(0xfd);
                }
            }
        }
        if u < 0x10000 {
            if self.is_little_endian {
                buf.push((u & 0xff) as u8);
                buf.push((u >> 8) as u8);
            } else {
                buf.push((u >> 8) as u8);
                buf.push((u & 0xff) as u8);
            }
        } else {
            // http://unicode.org/faq/utf_bom.html#utf16-3
            const HI_SURROGATE_START: u16 = 0xD800;
            let x = u as u16;
            let uu: u32 = (u >> 16) & ((1 << 5) - 1);
            let w: u16 = (uu as u16) - 1;
            let hi_surrogate = HI_SURROGATE_START | (w << 6) | x >> 10;

            const LO_SURROGATE_START: u16 = 0xDC00;
            let x = u as u16;
            let lo_surrogate = LO_SURROGATE_START | (x & ((1 << 10) - 1));
            if self.is_little_endian {
                buf.push((hi_surrogate & 0xff) as u8);
                buf.push((hi_surrogate >> 8) as u8);
                buf.push((lo_surrogate & 0xff) as u8);
                buf.push((lo_surrogate >> 8) as u8);
            } else {
                buf.push((hi_surrogate >> 8) as u8);
                buf.push((hi_surrogate & 0xff) as u8);
                buf.push((lo_surrogate >> 8) as u8);
                buf.push((lo_surrogate & 0xff) as u8);
            }
        }
        port.write(&buf)
    }
}

impl Display for UTF16Codec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<utf-16-codec>")
    }
}

/// Transcoder
#[derive(Debug)]
#[repr(C)]
pub struct Transcoder {
    pub header: GcHeader,
    pub codec: Object,
    pub eol_style: EolStyle,
    pub mode: ErrorHandlingMode,
    lineno: usize,
    is_beginning: bool,
    buffer: Vec<char>,
}

impl Trace for Transcoder {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.codec);
    }
}

impl Transcoder {
    pub fn new(codec: Object, eol_style: EolStyle, mode: ErrorHandlingMode) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Transcoder),
            codec,
            eol_style,
            mode,
            lineno: 1,
            is_beginning: true,
            buffer: vec![],
        }
    }

    pub fn write_char(
        &mut self,
        port: &mut dyn BinaryOutputPort,
        ch: char,
    ) -> Result<usize, SchemeError> {
        if !self.buffer.is_empty() {
            self.buffer.pop();
        }
        let codec = match self.codec {
            Object::Latin1Codec(mut codec) => {
                let codec: &mut dyn Codec = unsafe { codec.pointer.as_mut() };
                codec
            }
            Object::UTF8Codec(mut codec) => {
                let codec: &mut dyn Codec = unsafe { codec.pointer.as_mut() };
                codec
            }
            Object::UTF16Codec(mut codec) => {
                let codec: &mut dyn Codec = unsafe { codec.pointer.as_mut() };
                codec
            }
            _ => todo!(),
        };
        if self.eol_style == EolStyle::ENone {
            codec.write_char(port, ch, self.mode)
        } else if ch == char::LF {
            match self.eol_style {
                EolStyle::Lf => return codec.write_char(port, char::LF, self.mode),
                EolStyle::Cr => return codec.write_char(port, char::CR, self.mode),
                EolStyle::Nel => return codec.write_char(port, char::NEL, self.mode),
                EolStyle::Ls => return codec.write_char(port, char::LS, self.mode),
                EolStyle::ENone => return codec.write_char(port, ch, self.mode),
                EolStyle::CrNel => {
                    codec.write_char(port, char::CR, self.mode)?;
                    codec.write_char(port, char::NEL, self.mode)
                }
                EolStyle::CrLf => {
                    codec.write_char(port, char::CR, self.mode)?;
                    codec.write_char(port, char::LF, self.mode)
                }
            }
        } else {
            codec.write_char(port, ch, self.mode)
        }
    }

    pub fn write_string(
        &mut self,
        port: &mut dyn BinaryOutputPort,
        s: &str,
    ) -> Result<(), SchemeError> {
        for ch in s.chars() {
            self.write_char(port, ch)?;
        }
        Ok(())
    }

    pub fn read_string(
        &mut self,
        vm: &mut Vm,
        port: &mut dyn BinaryInputPort,
    ) -> Result<String, SchemeError> {
        let mut s = String::new();
        loop {
            let ch = self.read_char(vm, port)?;
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

    pub fn read_char(
        &mut self,
        vm: &mut Vm,
        port: &mut dyn BinaryInputPort,
    ) -> Result<Option<char>, SchemeError> {
        let ch = self.read_char_raw(vm, port)?;

        if let Some(ch) = ch {
            if self.eol_style == EolStyle::ENone {
                if ch == char::LF {
                    self.lineno += 1;
                }
                return Ok(Some(ch));
            }
        }
        match ch {
            Some(char::LF) | Some(char::NEL) | Some(char::LS) => {
                self.lineno += 1;
                Ok(Some(char::LF))
            }
            Some(char::CR) => {
                let ch2 = self.read_char_raw(vm, port)?;
                self.lineno += 1;
                match ch2 {
                    Some(char::LF) | Some(char::NEL) => Ok(Some(char::LF)),
                    _ => {
                        self.unget_char(ch2);
                        Ok(Some(char::LF))
                    }
                }
            }
            _ => Ok(ch),
        }
    }

    fn read_char_raw(
        &mut self,
        vm: &mut Vm,
        port: &mut dyn BinaryInputPort,
    ) -> Result<Option<char>, SchemeError> {
        let codec = match self.codec {
            Object::Latin1Codec(mut codec) => {
                let codec: &mut dyn Codec = unsafe { codec.pointer.as_mut() };
                codec
            }
            Object::UTF8Codec(mut codec) => {
                let codec: &mut dyn Codec = unsafe { codec.pointer.as_mut() };
                codec
            }
            Object::UTF16Codec(mut codec) => {
                let codec: &mut dyn Codec = unsafe { codec.pointer.as_mut() };
                codec
            }
            _ => todo!(),
        };
        // In the beginning of input, we have to check the BOM.
        if self.is_beginning {
            self.is_beginning = false;
            codec.read_char(vm, port, self.mode, true)
        } else if self.buffer.is_empty() {
            codec.read_char(vm, port, self.mode, false)
        } else {
            Ok(self.buffer.pop())
        }
    }

    fn unget_char(&mut self, ch: Option<char>) {
        if let Some(ch) = ch {
            self.buffer.push(ch);
            if ch == char::LF {
                self.lineno += 1;
            }
        }
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BufferMode {
    None,
    Line,
    Block,
}

impl Display for BufferMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BufferMode::None => write!(f, "none"),
            BufferMode::Line => write!(f, "line"),
            BufferMode::Block => write!(f, "block"),
        }
    }
}

// CustomBinaryInputPort
#[derive(Debug)]
#[repr(C)]
pub struct CustomBinaryInputPort {
    pub header: GcHeader,
    is_closed: bool,
    ahead_u8: Option<u8>,
    id: String,
    pub read_proc: Object,
    pub pos_proc: Object,
    pub set_pos_proc: Object,
    pub close_proc: Object,
}

impl Trace for CustomBinaryInputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.read_proc);
        gc.mark_object(self.pos_proc);
        gc.mark_object(self.set_pos_proc);
        gc.mark_object(self.close_proc);
    }
}

impl CustomBinaryInputPort {
    pub fn new(
        id: &str,
        read_proc: Object,
        pos_proc: Object,
        set_pos_proc: Object,
        close_proc: Object,
    ) -> Self {
        CustomBinaryInputPort {
            header: GcHeader::new(ObjectType::CustomBinaryInputPort),
            is_closed: false,
            ahead_u8: None,
            id: id.to_string(),
            read_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
        }
    }
}

impl Port for CustomBinaryInputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }

    fn has_position(&self) -> bool {
        !self.pos_proc.is_false()
    }

    fn has_set_position(&self) -> bool {
        !self.set_pos_proc.is_false()
    }

    fn position(&mut self, vm: &mut Vm) -> Result<usize, SchemeError> {
        if self.has_position() {
            match vm.call_closure0(self.pos_proc) {
                Ok(Object::Fixnum(pos)) => match self.ahead_u8() {
                    Some(_) => Ok((pos - 1) as usize),
                    None => Ok(pos as usize),
                },
                Ok(obj) => Err(SchemeError::io_error(
                    "position",
                    &format!("pos procedure returned non integer {}", obj),
                    &[self.pos_proc, obj],
                )),
                Err(e) => Err(e),
            }
        } else {
            Err(SchemeError::io_error(
                "position",
                "position! not supported",
                &[],
            ))
        }
    }

    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        if self.has_set_position() {
            self.set_ahead_u8(None);
            let pos = pos.to_obj(&mut vm.gc);
            vm.call_closure1(self.set_pos_proc, pos)?;
            Ok(0)
        } else {
            Err(SchemeError::io_error(
                "set-position!",
                "set-position! not supported",
                &[],
            ))
        }
    }
}

impl BinaryInputPort for CustomBinaryInputPort {
    fn read_u8(&mut self, vm: &mut Vm) -> Result<Option<u8>, SchemeError> {
        match self.ahead_u8() {
            Some(u) => {
                self.set_ahead_u8(None);
                Ok(Some(u))
            }
            None => {
                let size: usize = 1;
                let read_buf: Vec<u8> = vec![0; 1];
                let bv = vm.gc.new_bytevector_u8(&read_buf);
                let start = 0_isize.to_obj();
                let count = size.to_obj(&mut vm.gc);
                match vm.call_closure3(self.read_proc, bv, start, count) {
                    Ok(Object::Fixnum(n)) => {
                        if n == 0 {
                            Ok(None)
                        } else {
                            Ok(Some(bv.to_bytevector().data[0]))
                        }
                    }
                    Ok(obj) => {
                        Err(SchemeError::io_error(
                            "read_u8",
                            &format!("custom-binary-input-port read procedure should return exact integer but got {}", obj),
                            &[obj],
                        ))
                    }
                    Err(e) => Err(e),
                }
            }
        }
    }

    fn read(&mut self, vm: &mut Vm, buf: &mut [u8]) -> Result<usize, SchemeError> {
        let size = buf.len();
        let mut i = 0;
        loop {
            if i >= size {
                break;
            }
            match self.read_u8(vm) {
                Ok(Some(u)) => buf[i] = u,
                Ok(None) => break,
                Err(e) => return Err(e),
            }
            i += 1;
        }
        Ok(i)
    }

    fn ahead_u8(&self) -> Option<u8> {
        self.ahead_u8
    }

    fn set_ahead_u8(&mut self, u: Option<u8>) {
        self.ahead_u8 = u;
    }
}

impl Display for CustomBinaryInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<custom-binary-input-port>")
    }
}

// CustomTextInputPort
#[derive(Debug)]
#[repr(C)]
pub struct CustomTextInputPort {
    pub header: GcHeader,
    is_closed: bool,
    id: String,
    ahead_char: Option<char>,
    pub read_proc: Object,
    pub pos_proc: Object,
    pub set_pos_proc: Object,
    pub close_proc: Object,
    pub parsed: Object,
}

impl Trace for CustomTextInputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.read_proc);
        gc.mark_object(self.pos_proc);
        gc.mark_object(self.set_pos_proc);
        gc.mark_object(self.close_proc);
        gc.mark_object(self.parsed);
    }
}

impl CustomTextInputPort {
    pub fn new(
        id: &str,
        read_proc: Object,
        pos_proc: Object,
        set_pos_proc: Object,
        close_proc: Object,
    ) -> Self {
        CustomTextInputPort {
            header: GcHeader::new(ObjectType::CustomTextInputPort),
            is_closed: false,
            ahead_char: None,
            id: id.to_string(),
            read_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
            parsed: Object::Unspecified,
        }
    }
}

impl Port for CustomTextInputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
    fn has_position(&self) -> bool {
        !self.pos_proc.is_false()
    }

    fn has_set_position(&self) -> bool {
        !self.set_pos_proc.is_false()
    }
    fn position(&mut self, vm: &mut Vm) -> Result<usize, SchemeError> {
        if self.has_position() {
            match vm.call_closure0(self.pos_proc) {
                Ok(Object::Fixnum(pos)) => Ok(pos as usize),
                Ok(obj) => Err(SchemeError::io_error(
                    "position",
                    &format!("pos procedure returned non integer {}", obj),
                    &[self.pos_proc, obj],
                )),
                Err(e) => Err(e),
            }
        } else {
            Err(SchemeError::io_error(
                "position",
                "position not supported",
                &[],
            ))
        }
    }

    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        if self.has_set_position() {
            let pos = pos.to_obj(&mut vm.gc);
            vm.call_closure1(self.set_pos_proc, pos)?;
            Ok(0)
        } else {
            Err(SchemeError::io_error(
                "set-position!",
                "set-position! not supported",
                &[],
            ))
        }
    }
}

impl TextInputPort for CustomTextInputPort {
    fn read_to_string(&mut self, _vm: &mut Vm, _str: &mut String) -> Result<usize, SchemeError> {
        todo!();
    }

    fn input_src(&self) -> String {
        "custom text port".to_string()
    }

    fn ahead_char(&self) -> Option<char> {
        self.ahead_char
    }

    fn set_ahead_char(&mut self, c: Option<char>) {
        self.ahead_char = c;
    }

    fn read_char(&mut self, vm: &mut Vm) -> Result<Option<char>, SchemeError> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Ok(Some(c))
            }
            None => {
                let size: usize = 1;
                let s = vm
                    .gc
                    .new_string(&(0..size).map(|_| " ").collect::<String>());
                let start = 0_isize.to_obj();
                let count = size.to_obj(&mut vm.gc);
                let read_size = match vm.call_closure3(self.read_proc, s, start, count) {
                    Ok(obj) => {
                        if !obj.is_fixnum() {
                            return Err(SchemeError::io_error(
                                "read_u8",
                                &format!("custom-text-input-port read procedure should return exact integer but got {}", obj),
                                &[obj],
                            ));
                        }
                        obj.to_isize() as usize
                    }
                    Err(e) => return Err(e),
                };
                if read_size == 0 {
                    Ok(None)
                } else {
                    Ok(s.to_sstring().string.chars().next())
                }
            }
        }
    }
    fn read_n_to_string(
        &mut self,
        vm: &mut Vm,
        dst: &mut String,
        n: usize,
    ) -> Result<usize, SchemeError> {
        let mut size = 0;
        loop {
            if size >= n {
                break;
            }
            match self.read_char(vm) {
                Ok(Some(ch)) => dst.push(ch),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
            size += 1;
        }
        Ok(size)
    }

    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

impl Display for CustomTextInputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<custom-text-input-port>")
    }
}

// CustomBinaryOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct CustomBinaryOutputPort {
    pub header: GcHeader,
    is_closed: bool,
    id: String,
    pub write_proc: Object,
    pub pos_proc: Object,
    pub set_pos_proc: Object,
    pub close_proc: Object,
}

impl Trace for CustomBinaryOutputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.write_proc);
        gc.mark_object(self.pos_proc);
        gc.mark_object(self.set_pos_proc);
        gc.mark_object(self.close_proc);
    }
}

impl CustomBinaryOutputPort {
    pub fn new(
        id: &str,
        write_proc: Object,
        pos_proc: Object,
        set_pos_proc: Object,
        close_proc: Object,
    ) -> Self {
        CustomBinaryOutputPort {
            header: GcHeader::new(ObjectType::CustomBinaryOutputPort),
            is_closed: false,
            id: id.to_string(),
            write_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
        }
    }
}

impl Port for CustomBinaryOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
    fn has_position(&self) -> bool {
        !self.pos_proc.is_false()
    }

    fn has_set_position(&self) -> bool {
        !self.set_pos_proc.is_false()
    }
    fn position(&mut self, vm: &mut Vm) -> Result<usize, SchemeError> {
        if self.has_position() {
            match vm.call_closure0(self.pos_proc) {
                Ok(Object::Fixnum(pos)) => Ok(pos as usize),
                Ok(obj) => Err(SchemeError::io_error(
                    "position",
                    &format!("pos procedure returned non integer {}", obj),
                    &[self.pos_proc, obj],
                )),
                Err(e) => Err(e),
            }
        } else {
            Err(SchemeError::io_error(
                "position",
                "position not supported",
                &[],
            ))
        }
    }

    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        if self.has_set_position() {
            let pos = pos.to_obj(&mut vm.gc);
            vm.call_closure1(self.set_pos_proc, pos)?;
            Ok(0)
        } else {
            Err(SchemeError::io_error(
                "set-position!",
                "set-position! not supported",
                &[],
            ))
        }
    }
}

impl BinaryOutputPort for CustomBinaryOutputPort {
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        let vm = unsafe { &mut CURRENT_VM };
        let write_buf = buf.to_vec();
        let bv = vm.gc.new_bytevector_u8(&write_buf);
        let start = 0_isize.to_obj();
        let count = write_buf.len().to_obj(&mut vm.gc);
        let write_size = match vm.call_closure3(self.write_proc, bv, start, count) {
            Ok(obj) => {
                if !obj.is_fixnum() {
                    return Err(SchemeError::io_error(
                        "write",
                        &format!("custom-binary-input/output-port write procedure should return exact integer but got {}", obj),
                        &[obj],
                    ));
                }
                obj.to_isize() as usize
            }
            Err(e) => return Err(e),
        };
        Ok(write_size)
    }
}

impl OutputPort for CustomBinaryOutputPort {
    fn flush(&mut self) {}
}

impl Display for CustomBinaryOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<custom-binary-input-port>")
    }
}

// CustomTextOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct CustomTextOutputPort {
    pub header: GcHeader,
    is_closed: bool,
    id: String,
    pub write_proc: Object,
    pub pos_proc: Object,
    pub set_pos_proc: Object,
    pub close_proc: Object,
}

impl Trace for CustomTextOutputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.write_proc);
        gc.mark_object(self.pos_proc);
        gc.mark_object(self.set_pos_proc);
        gc.mark_object(self.close_proc);
    }
}

impl CustomTextOutputPort {
    pub fn new(
        id: &str,
        write_proc: Object,
        pos_proc: Object,
        set_pos_proc: Object,
        close_proc: Object,
    ) -> Self {
        CustomTextOutputPort {
            header: GcHeader::new(ObjectType::CustomTextOutputPort),
            is_closed: false,
            id: id.to_string(),
            write_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
        }
    }
}

impl Port for CustomTextOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
    fn has_position(&self) -> bool {
        !self.pos_proc.is_false()
    }

    fn has_set_position(&self) -> bool {
        !self.set_pos_proc.is_false()
    }
    fn position(&mut self, vm: &mut Vm) -> Result<usize, SchemeError> {
        if self.has_position() {
            match vm.call_closure0(self.pos_proc) {
                Ok(Object::Fixnum(pos)) => Ok(pos as usize),
                Ok(obj) => Err(SchemeError::io_error(
                    "position",
                    &format!("pos procedure returned non integer {}", obj),
                    &[self.pos_proc, obj],
                )),
                Err(e) => Err(e),
            }
        } else {
            Err(SchemeError::io_error(
                "position",
                "position not supported",
                &[],
            ))
        }
    }

    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        if self.has_set_position() {
            let pos = pos.to_obj(&mut vm.gc);
            vm.call_closure1(self.set_pos_proc, pos)?;
            Ok(0)
        } else {
            Err(SchemeError::io_error(
                "set-position!",
                "set-position! not supported",
                &[],
            ))
        }
    }
}

impl TextOutputPort for CustomTextOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), SchemeError> {
        let vm = unsafe { &mut CURRENT_VM };
        let str = vm.gc.new_string(s);
        let start = 0_isize.to_obj();
        let count = s.len().to_obj(&mut vm.gc);
        let _write_size = match vm.call_closure3(self.write_proc, str, start, count) {
            Ok(obj) => {
                if !obj.is_fixnum() {
                    return Err(SchemeError::io_error(
                        "read",
                        &format!("custom-binary-input/output-port write procedure should return exact integer but got {}", obj),
                        &[obj],
                    ));
                }
                obj.to_isize() as usize
            }
            Err(_) => 0,
        };
        Ok(())
    }
}

impl OutputPort for CustomTextOutputPort {
    fn flush(&mut self) {}
}

impl Display for CustomTextOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<custom-text-output-port>")
    }
}

// CustomBinaryInputOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct CustomBinaryInputOutputPort {
    pub header: GcHeader,
    is_closed: bool,
    ahead_u8: Option<u8>,
    id: String,
    pub read_proc: Object,
    pub write_proc: Object,
    pub pos_proc: Object,
    pub set_pos_proc: Object,
    pub close_proc: Object,
}

impl Trace for CustomBinaryInputOutputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.read_proc);
        gc.mark_object(self.write_proc);
        gc.mark_object(self.pos_proc);
        gc.mark_object(self.set_pos_proc);
        gc.mark_object(self.close_proc);
    }
}

impl CustomBinaryInputOutputPort {
    pub fn new(
        id: &str,
        read_proc: Object,
        write_proc: Object,
        pos_proc: Object,
        set_pos_proc: Object,
        close_proc: Object,
    ) -> Self {
        CustomBinaryInputOutputPort {
            header: GcHeader::new(ObjectType::CustomBinaryInputOutputPort),
            is_closed: false,
            ahead_u8: None,
            id: id.to_string(),
            read_proc,
            write_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
        }
    }
}

impl Port for CustomBinaryInputOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
    fn has_position(&self) -> bool {
        !self.pos_proc.is_false()
    }

    fn has_set_position(&self) -> bool {
        !self.set_pos_proc.is_false()
    }
    fn position(&mut self, vm: &mut Vm) -> Result<usize, SchemeError> {
        if self.has_position() {
            match vm.call_closure0(self.pos_proc) {
                Ok(Object::Fixnum(pos)) => Ok(pos as usize),
                Ok(obj) => Err(SchemeError::io_error(
                    "position",
                    &format!("pos procedure returned non integer {}", obj),
                    &[self.pos_proc, obj],
                )),
                Err(e) => Err(e),
            }
        } else {
            Err(SchemeError::io_error(
                "position",
                "position not supported",
                &[],
            ))
        }
    }

    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        if self.has_set_position() {
            self.set_ahead_u8(None);
            let pos = pos.to_obj(&mut vm.gc);
            vm.call_closure1(self.set_pos_proc, pos)?;
            Ok(0)
        } else {
            Err(SchemeError::io_error(
                "set-position!",
                "set-position! not supported",
                &[],
            ))
        }
    }
}

impl BinaryOutputPort for CustomBinaryInputOutputPort {
    fn write(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        let vm = unsafe { &mut CURRENT_VM };
        let write_buf = buf.to_vec();
        let bv = vm.gc.new_bytevector_u8(&write_buf);
        let start = 0_isize.to_obj();
        let count = write_buf.len().to_obj(&mut vm.gc);
        let write_size = match vm.call_closure3(self.write_proc, bv, start, count) {
            Ok(obj) => {
                if !obj.is_fixnum() {
                    return Err(SchemeError::io_error(
                        "write",
                        &format!("custom-binary-input/output-port write procedure should return exact integer but got {}", obj),
                        &[obj],
                    ));
                }
                obj.to_isize() as usize
            }
            Err(_) => 0,
        };
        Ok(write_size)
    }
}

impl BinaryInputPort for CustomBinaryInputOutputPort {
    fn read_u8(&mut self, vm: &mut Vm) -> Result<Option<u8>, SchemeError> {
        match self.ahead_u8() {
            Some(u) => {
                self.set_ahead_u8(None);
                Ok(Some(u))
            }
            None => {
                let size: usize = 1;
                let read_buf: Vec<u8> = vec![0; 1];
                let bv = vm.gc.new_bytevector_u8(&read_buf);
                let start = 0_isize.to_obj();
                let count = size.to_obj(&mut vm.gc);
                match vm.call_closure3(self.read_proc, bv, start, count) {
                    Ok(Object::Fixnum(n)) => {
                        if n == 0 {
                            Ok(None)
                        } else {
                            Ok(Some(bv.to_bytevector().data[0]))
                        }
                    }
                    Ok(obj) => {
                        Err(SchemeError::io_error(
                            "read_u8",
                            &format!("custom-binary-input/output-port read procedure should return exact integer but got {}", obj),
                            &[obj],
                        ))
                    }
                    Err(e) => Err(e)
                }
            }
        }
    }

    fn read(&mut self, vm: &mut Vm, buf: &mut [u8]) -> Result<usize, SchemeError> {
        let size = buf.len();
        let mut i = 0;
        loop {
            if i >= size {
                break;
            }
            match self.read_u8(vm) {
                Ok(Some(u)) => buf[i] = u,
                Ok(None) => break,
                Err(_) => break,
            }
            i += 1;
        }
        Ok(i)
    }

    fn ahead_u8(&self) -> Option<u8> {
        self.ahead_u8
    }

    fn set_ahead_u8(&mut self, u: Option<u8>) {
        self.ahead_u8 = u;
    }
}

impl OutputPort for CustomBinaryInputOutputPort {
    fn flush(&mut self) {}
}

impl Display for CustomBinaryInputOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<custom-binary-input/output-port>")
    }
}

// CustomTextInputOutputPort
#[derive(Debug)]
#[repr(C)]
pub struct CustomTextInputOutputPort {
    pub header: GcHeader,
    is_closed: bool,
    id: String,
    ahead_char: Option<char>,
    pub parsed: Object,
    pub read_proc: Object,
    pub write_proc: Object,
    pub pos_proc: Object,
    pub set_pos_proc: Object,
    pub close_proc: Object,
}

impl Trace for CustomTextInputOutputPort {
    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.parsed);
        gc.mark_object(self.read_proc);
        gc.mark_object(self.write_proc);
        gc.mark_object(self.pos_proc);
        gc.mark_object(self.set_pos_proc);
        gc.mark_object(self.close_proc);
    }
}

impl CustomTextInputOutputPort {
    pub fn new(
        id: &str,
        read_proc: Object,
        write_proc: Object,
        pos_proc: Object,
        set_pos_proc: Object,
        close_proc: Object,
    ) -> Self {
        CustomTextInputOutputPort {
            header: GcHeader::new(ObjectType::CustomTextInputOutputPort),
            is_closed: false,
            id: id.to_string(),
            ahead_char: None,
            parsed: Object::Unspecified,
            read_proc,
            write_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
        }
    }
}

impl Port for CustomTextInputOutputPort {
    fn is_open(&self) -> bool {
        !self.is_closed
    }
    fn close(&mut self) {
        self.is_closed = true;
    }
    fn has_position(&self) -> bool {
        !self.pos_proc.is_false()
    }

    fn has_set_position(&self) -> bool {
        !self.set_pos_proc.is_false()
    }
    fn position(&mut self, vm: &mut Vm) -> Result<usize, SchemeError> {
        if self.has_position() {
            match vm.call_closure0(self.pos_proc) {
                Ok(Object::Fixnum(pos)) => Ok(pos as usize),
                Ok(obj) => Err(SchemeError::io_error(
                    "position",
                    &format!("pos procedure returned non integer {}", obj),
                    &[self.pos_proc, obj],
                )),
                Err(e) => Err(e),
            }
        } else {
            Err(SchemeError::io_error(
                "position",
                "position not supported",
                &[],
            ))
        }
    }

    fn set_position(&mut self, vm: &mut Vm, pos: usize) -> Result<usize, SchemeError> {
        if self.has_set_position() {
            let pos = pos.to_obj(&mut vm.gc);
            vm.call_closure1(self.set_pos_proc, pos)?;
            Ok(0)
        } else {
            Err(SchemeError::io_error(
                "set-position!",
                "set-position! not supported",
                &[],
            ))
        }
    }
}

impl TextOutputPort for CustomTextInputOutputPort {
    fn put_string(&mut self, s: &str) -> Result<(), SchemeError> {
        let vm = unsafe { &mut CURRENT_VM };
        let str = vm.gc.new_string(s);
        let start = 0_isize.to_obj();
        let count = s.len().to_obj(&mut vm.gc);
        let _write_size = match vm.call_closure3(self.write_proc, str, start, count) {
            Ok(obj) => {
                if !obj.is_fixnum() {
                    return Err(SchemeError::io_error(
                        "write",
                        &format!("custom-binary-input/output-port write procedure should return exact integer but got {}", obj),
                        &[obj],
                    ));
                }
                obj.to_isize() as usize
            }
            Err(_) => 0,
        };
        Ok(())
    }
}

impl OutputPort for CustomTextInputOutputPort {
    fn flush(&mut self) {}
}

impl Display for CustomTextInputOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<custom-text-input/output-port>")
    }
}

impl TextInputPort for CustomTextInputOutputPort {
    fn read_to_string(&mut self, _vm: &mut Vm, _str: &mut String) -> Result<usize, SchemeError> {
        todo!();
    }

    fn input_src(&self) -> String {
        "custom text port".to_string()
    }

    fn ahead_char(&self) -> Option<char> {
        self.ahead_char
    }

    fn set_ahead_char(&mut self, c: Option<char>) {
        self.ahead_char = c;
    }

    fn read_char(&mut self, vm: &mut Vm) -> Result<Option<char>, SchemeError> {
        match self.ahead_char {
            Some(c) => {
                self.ahead_char = None;
                Ok(Some(c))
            }
            None => {
                let mut str = String::new();
                match self.read_n_to_string(vm, &mut str, 1) {
                    Ok(_) => {
                        if str.is_empty() {
                            Ok(None)
                        } else {
                            let mut chars = str.chars();
                            Ok(chars.next())
                        }
                    }
                    Err(e) => Err(e),
                }
            }
        }
    }
    fn read_n_to_string(
        &mut self,
        vm: &mut Vm,
        dst: &mut String,
        n: usize,
    ) -> Result<usize, SchemeError> {
        if n == 0 {
            return Ok(0);
        }

        let read_start: usize = match self.ahead_char {
            Some(ch) => {
                dst.push(ch);
                self.set_ahead_char(None);

                if n == 1 {
                    return Ok(1);
                }
                1
            }
            None => 0,
        };

        let size = n - read_start;

        let s = vm
            .gc
            .new_string(&(0..size).map(|_| " ").collect::<String>());
        let start = 0_isize.to_obj();
        let count = size.to_obj(&mut vm.gc);
        let read_size = match vm.call_closure3(self.read_proc, s, start, count) {
            Ok(obj) => {
                if !obj.is_fixnum() {
                    return Err(SchemeError::io_error(
                        "read",
                        &format!("custom-binary-input/output-port write procedure should return exact integer but got {}", obj),
                        &[obj],
                    ));
                }
                obj.to_isize() as usize
            }
            Err(_) => 0,
        };

        let source = &s.to_sstring().string;
        let _i = 0;
        for (i, ch) in source.chars().enumerate() {
            if i >= read_size {
                break;
            }
            dst.push(ch);
        }
        Ok(read_start + read_size)
    }

    fn set_parsed(&mut self, obj: Object) {
        self.parsed = obj;
    }
    fn parsed(&self) -> Object {
        self.parsed
    }
}

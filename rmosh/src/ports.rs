use std::{fmt::{self, Display}, collections::HashMap};

use crate::{
    gc::{GcHeader, ObjectType, GcRef},
    objects::{Object, Vector, SimpleStruct, Pair},
};

// Trait for TextOutputPort.
pub trait TextOutputPort {
    fn put_string(&mut self, s: &str);

    // (write obj): Machine readable print.
    fn write(&mut self, obj: Object) {
        let mut shared_id = 1;
        let mut seen: HashMap<Object, Object> = HashMap::new();
        self.scan(obj, &mut seen);
        self.display_one(obj, &mut seen, &mut shared_id);
    }

    // (display obj): Human readable print.
    fn display(&mut self, obj: Object) {
        let mut shared_id = 1;
        let mut seen: HashMap<Object, Object> = HashMap::new();
        self.scan(obj, &mut seen);
        self.display_one(obj, &mut seen, &mut shared_id);
    }

    fn display_one(&mut self, obj: Object, seen: &mut HashMap<Object, Object>, shared_id: &mut isize) {
        let seen_state = match seen.get(&obj) {
            Some(val) => *val,
            None => Object::False,
        };
        if seen_state.is_true() {
            seen.insert(obj, Object::Number(*shared_id));
            self.put_string(&format!("#{}=", shared_id));
            *shared_id += 1;
        } else if seen_state.is_number() {
            self.put_string(&format!("#{}#", seen_state.to_number()));
            return;
        }
        match obj {
            Object::Pair(p) => self.display_pair(p, seen, shared_id),
            Object::Vector(v) => self.display_vector(v, seen, shared_id),
            Object::SimpleStruct(s) => self.display_struct(s, seen, shared_id),
            Object::ByteVector(_)
            | Object::Closure(_)
            | Object::Vox(_)
            | Object::ProgramCounter(_)
            | Object::ObjectPointer(_)
            | Object::Unspecified
            | Object::True
            | Object::Procedure(_)
            | Object::Char(_)
            | Object::EqHashtable(_)
            | Object::False
            | Object::Float(_)
            | Object::StringInputPort(_)
            | Object::FileInputPort(_)
            | Object::Eof
            | Object::FileOutputPort(_)
            | Object::StringOutputPort(_)
            | Object::StdOutputPort(_)
            | Object::StdErrorPort(_)
            | Object::Instruction(_)
            | Object::Nil
            | Object::Symbol(_)
            | Object::String(_)
            | Object::Number(_) => self.as_display(obj),
        }
    }


    fn display_pair(&mut self, p: GcRef<Pair>, seen: &mut HashMap<Object, Object>, shared_id: &mut isize) {
        self.put_string("(");
        self.display_one(p.car, seen, shared_id);

        let mut obj = p.cdr;
        loop {
            let seen_state = match seen.get(&obj) {
                Some(v) => *v,
                None => Object::False,
            };
            match obj {
                Object::Pair(pair) if seen_state.is_false() => {
                    self.put_string(" ");
                    self.display_one(pair.car, seen, shared_id);
                    obj = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    self.put_string(" . ");
                    self.display_one(obj, seen, shared_id);
                    break;
                }
            }
        }
        self.put_string(")");
    }

    fn as_display(&mut self, obj: Object) {
        self.put_string(&format!("{}", obj));
    }    

    fn display_vector(&mut self, v: GcRef<Vector>, seen: &mut HashMap<Object, Object>, shared_id: &mut isize) {
        self.put_string("#(");
        for i in 0..v.len() {
            self.display_one(v.data[i], seen, shared_id);
            if i != v.len() - 1 {
                self.put_string(" ");
            }
        }
        self.put_string(")");
    }

    fn display_struct(&mut self, s: GcRef<SimpleStruct>, seen: &mut HashMap<Object, Object>, shared_id: &mut isize) {
        self.put_string("#<simple-stuct ");
        for i in 0..s.len() {
            self.display_one(s.field(i), seen, shared_id);
            if i != s.len() - 1 {
                self.put_string(" ");
            }
        }
        self.put_string(">");
    }

    fn scan(&mut self, obj: Object, seen: &mut HashMap<Object, Object>) {
        let mut o = obj;
        loop {
            match o {
                Object::ByteVector(_)
                | Object::Closure(_)
                | Object::Vox(_)
                | Object::ProgramCounter(_)
                | Object::ObjectPointer(_)
                | Object::Unspecified
                | Object::True
                | Object::Procedure(_)
                | Object::Char(_)
                | Object::EqHashtable(_)
                | Object::False
                | Object::Float(_)
                | Object::StringInputPort(_)
                | Object::FileInputPort(_)
                | Object::Eof
                | Object::FileOutputPort(_)
                | Object::StringOutputPort(_)
                | Object::StdOutputPort(_)
                | Object::StdErrorPort(_)
                | Object::Instruction(_)
                | Object::Nil
                | Object::Symbol(_)
                | Object::String(_)
                | Object::Number(_) => return,
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
                    for i in 0..v.len() {
                        self.scan(v.data[i], seen);
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
                    if c == 'a' || c == 'd' {
                        if i < args.len() {
                            self.display(args[i]);
                            i += 1;
                        } else {
                            panic!("format: not enough arguments");
                        }
                    } else if c == 's' {
                        if i < args.len() {
                            self.write(args[i]);
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
                print!("{}", c)
            }
        }
    }
}

// FileOutputPort
#[derive(Debug)]
pub struct FileOutputPort {
    pub header: GcHeader,
    is_closed: bool,
}

impl FileOutputPort {
    fn new() -> Self {
        FileOutputPort {
            header: GcHeader::new(ObjectType::FileOutputPort),
            is_closed: false,
        }
    }
    pub fn open(_path: &str) -> std::io::Result<FileOutputPort> {
        Ok(FileOutputPort::new())
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
    fn put_string(&mut self, _s: &str) {
        todo!()
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
    fn put_string(&mut self, s: &str) {
        print!("{}", s)
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
    fn put_string(&mut self, s: &str) {
        eprint!("{}", s);
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
        write!(f, "#<file-output-port>")
    }
}

impl TextOutputPort for StringOutputPort {
    fn put_string(&mut self, s: &str) {
        self.string.push_str(s);
    }
}

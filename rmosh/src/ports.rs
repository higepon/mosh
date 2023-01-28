use std::fmt::{Display, self};

use crate::{objects::Object, gc::{GcHeader, ObjectType}};


pub trait TextOutputPort {
    fn put_string(&mut self, s: &str);

    fn display(&mut self, obj: Object) {
        self.put_string(&format!("{}", obj))
    }

    fn write(&mut self, obj: Object) {
        self.put_string(&format!("{:?}", obj))
    }

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




impl TextOutputPort for StdOutputPort {
    fn put_string(&mut self, s: &str) {
        print!("{}", s);
    }
}

impl TextOutputPort for StdErrorPort {
    fn put_string(&mut self, s: &str) {
        eprint!("{}", s);
    }
}

pub struct StdOutputPort {
    pub header: GcHeader,
}
impl StdOutputPort {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::StdOutputPort),
        }
    }

    pub fn close(&mut self) {}

    /*
    pub fn write(&mut self, obj: Object) {
        let written = format!("{:?}", obj);
        print!("{}", written)
    }

    // TODO: Make this human readable.
    pub fn display(&mut self, obj: Object) {
        let written = format!("{}", obj);
        print!("{}", written)
    }
    */
}

impl Display for StdOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<std-output-port>")
    }
}

pub struct StdErrorPort {
    pub header: GcHeader,
}
impl StdErrorPort {
    pub fn new() -> Self {
        Self {
            header: GcHeader::new(ObjectType::StdErrOutputPort),
        }
    }

    pub fn close(&mut self) {}
/*
    pub fn write(&mut self, obj: Object) {
        let written = format!("{:?}", obj);
        eprint!("{}", written)
    }

    // TODO: Make this human readable.
    pub fn display(&mut self, obj: Object) {
        let written = format!("{}", obj);
        eprint!("{}", written)
    }
*/
}

impl Display for StdErrorPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<stderr-output-port>")
    }
}

#[derive(Debug)]
pub struct StringOutputPort {
    pub header: GcHeader,
    string: String,
    is_closed: bool,
}

impl TextOutputPort for StringOutputPort {
    fn put_string(&mut self, s: &str) {
        self.string.push_str(s);
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
/*
    pub fn write(&mut self, obj: Object) {
        let written = format!("{:?}", obj);
        self.string.push_str(&written);
    }

    // TODO: Make this human readable.
    pub fn display(&mut self, obj: Object) {
        let written = format!("{}", obj);
        self.string.push_str(&written);
    }
*/

    pub fn string(&self) -> String {
        self.string.to_owned()
    }
}

impl Display for StringOutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<file-output-port>")
    }
}
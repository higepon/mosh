use crate::gc::{GcHeader, ObjectType, Trace};
use std::fmt::{self, Debug, Display};

#[repr(C)]
pub struct Regexp {
    pub header: GcHeader,
    string: String,
}

impl Trace for Regexp {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Regexp {
    pub fn new(s: &str) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Regexp),
            string: s.to_string(),
        }
    }
}

impl Debug for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("#<regexp>")
    }
}

impl Display for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<regexp>")
    }
}

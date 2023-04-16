use crate::{
    error::SchemeError,
    gc::{GcHeader, ObjectType, Trace},
};
use onig::Regex;
use std::fmt::{self, Debug, Display};

#[repr(C)]
pub struct Regexp {
    pub header: GcHeader,
    string: String,
    regex: Regex,
}

impl Trace for Regexp {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Regexp {
    pub fn new(s: &str) -> Result<Self, SchemeError> {
        let regex = Regex::new(s).map_err(|e| {
            SchemeError::assertion_violation("lexer", &format!("regexp errpr {}", e), &[])
        })?;

        Ok(Self {
            header: GcHeader::new(ObjectType::Regexp),
            string: s.to_string(),
            regex: regex,
        })
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

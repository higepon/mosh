use crate::{
    error::SchemeError,
    gc::{GcHeader, ObjectType, Trace},
};
use onig::{MatchParam, Regex, Region, SearchOptions};
use std::fmt::{self, Debug, Display};

#[repr(C)]
pub struct Regexp {
    pub header: GcHeader,
    pub pattern: String,
    regex: Regex,
}

impl Trace for Regexp {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Regexp {
    pub fn new(s: &str) -> Result<Self, SchemeError> {
        let regex = Regex::new(s).map_err(|e| {
            SchemeError::assertion_violation("lexer", &format!("regexp error {}", e), &[])
        })?;

        Ok(Self {
            header: GcHeader::new(ObjectType::Regexp),
            pattern: s.to_string(),
            regex: regex,
        })
    }

    pub fn rxmatch(&self, text: &str) -> Result<RegMatch, SchemeError> {
        self.match_internal(text)
            .map(|region| RegMatch::new(region, text))
    }

    fn match_internal(&self, text: &str) -> Result<Region, SchemeError> {
        let mut region = Region::new();
        match self.regex.search_with_param(
            text,
            0,
            text.len(),
            SearchOptions::SEARCH_OPTION_NONE,
            Some(&mut region),
            MatchParam::default(),
        ) {
            Ok(Some(_size)) => Ok(region),
            Ok(_) => Err(SchemeError::assertion_violation("rxmatch", "no match", &[])),
            Err(err) => Err(SchemeError::assertion_violation(
                "rxmatch",
                &err.to_string(),
                &[],
            )),
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

#[repr(C)]
pub struct RegMatch {
    pub header: GcHeader,
    region: Region,
    text: String,
}

impl Trace for RegMatch {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl RegMatch {
    pub fn new(region: Region, text: &str) -> Self {
        Self {
            header: GcHeader::new(ObjectType::RegMatch),
            region: region,
            text: text.to_string(),
        }
    }

    pub fn match_start(&self, index: usize) -> Result<usize, SchemeError> {
        match self.region.pos(index) {
            Some(start_end) => Ok(start_end.0),
            None => Err(SchemeError::assertion_violation(
                "rxmatch-start",
                &format!("submatch index {} out of range", index),
                &[],
            )),
        }
    }

    pub fn match_end(&self, index: usize) -> Result<usize, SchemeError> {
        match self.region.pos(index) {
            Some(start_end) => Ok(start_end.1),
            None => Err(SchemeError::assertion_violation(
                "rxmatch-end",
                &format!("submatch index {} out of range", index),
                &[],
            )),
        }
    }
}

impl Debug for RegMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("#<regmatch>")
    }
}

impl Display for RegMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<regmatch>")
    }
}

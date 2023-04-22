use crate::{
    error::SchemeError,
    gc::{GcHeader, ObjectType, Trace},
};
use onig::{MatchParam, Regex, RegexOptions, Region, SearchOptions, Syntax};
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
        let options = RegexOptions::REGEX_OPTION_NONE;
        let regex = Regex::with_options(s, options, Syntax::ruby()).map_err(|e| {
            SchemeError::assertion_violation("lexer", &format!("regexp error {}", e), &[])
        })?;

        Ok(Self {
            header: GcHeader::new(ObjectType::Regexp),
            pattern: s.to_string(),
            regex,
        })
    }

    pub fn with_options(
        s: &str,
        case_fold: bool,
        is_single_line: bool,
    ) -> Result<Self, SchemeError> {
        let mut options = RegexOptions::REGEX_OPTION_NONE;
        if case_fold {
            options |= RegexOptions::REGEX_OPTION_IGNORECASE;
        }
        if is_single_line {
            options |= RegexOptions::REGEX_OPTION_SINGLELINE;
        }
        let regex = Regex::with_options(s, options, Syntax::ruby()).map_err(|e| {
            SchemeError::assertion_violation("lexer", &format!("regexp error {}", e), &[])
        })?;

        Ok(Self {
            header: GcHeader::new(ObjectType::Regexp),
            pattern: s.to_string(),
            regex,
        })
    }

    pub fn rxmatch(&self, text: &str) -> Result<RegMatch, SchemeError> {
        match self.match_internal(text) {
            Ok(Some(region)) => Ok(RegMatch::new(region, text)),
            Ok(None) => Err(SchemeError::assertion_violation("rxmatch", "no match", &[])),
            Err(err) => Err(err),
        }
    }

    fn match_internal(&self, text: &str) -> Result<Option<Region>, SchemeError> {
        let mut region = Region::new();
        match self.regex.search_with_param(
            text,
            0,
            text.len(),
            SearchOptions::SEARCH_OPTION_NONE,
            Some(&mut region),
            MatchParam::default(),
        ) {
            Ok(Some(_size)) => Ok(Some(region)),
            Ok(None) => Ok(None),
            Err(err) => Err(SchemeError::assertion_violation(
                "rxmatch",
                &err.to_string(),
                &[],
            )),
        }
    }

    pub fn replace_all(&self, target: &str, substitute: &str) -> Result<String, SchemeError> { 
        if substitute.is_empty() {
            return Ok(target);
        }       
        let mut ret = String::new();
        let mut target_str = target;
        loop {
            match self.match_internal(target_str) {
                Ok(Some(region)) => match region.pos(0) {
                    Some((start, end)) => {
                        let pre = &target_str[0..start];
                        ret.push_str(pre);
                        let post = &target_str[end..];
                        ret.push_str(substitute);
                        target_str = post;
                    }
                    None => {
                        return Err(SchemeError::assertion_violation(
                            "regex-replace-all",
                            &format!("submatch index {} out of range", 0),
                            &[],
                        ))
                    }
                },
                Ok(None) => {
                    ret.push_str(target_str);
                    break;
                }
                Err(err) => return Err(err),
            }
        }
        Ok(ret)
    }
}

impl Debug for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#/{}/", self.pattern)
    }
}

impl Display for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#/{}/", self.pattern)
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
            region,
            text: text.to_string(),
        }
    }

    pub fn match_start(&self, index: usize) -> Result<usize, SchemeError> {
        match self.region.pos(index) {
            Some((start, _end)) => Ok(start),
            None => Err(SchemeError::assertion_violation(
                "rxmatch-start",
                &format!("submatch index {} out of range", index),
                &[],
            )),
        }
    }

    pub fn match_end(&self, index: usize) -> Result<usize, SchemeError> {
        match self.region.pos(index) {
            Some((_start, end)) => Ok(end),
            None => Err(SchemeError::assertion_violation(
                "rxmatch-end",
                &format!("submatch index {} out of range", index),
                &[],
            )),
        }
    }

    pub fn match_after(&self, index: usize) -> Result<String, SchemeError> {
        match self.region.pos(index) {
            Some((_start, end)) => Ok(self.text[end..].to_string()),
            None => Err(SchemeError::assertion_violation(
                "rxmatch-after",
                &format!("submatch index {} out of range", index),
                &[],
            )),
        }
    }

    pub fn match_before(&self, index: usize) -> Result<String, SchemeError> {
        match self.region.pos(index) {
            Some((start, _end)) => Ok(self.text[0..start].to_string()),
            None => Err(SchemeError::assertion_violation(
                "rxmatch-before",
                &format!("submatch index {} out of range", index),
                &[],
            )),
        }
    }

    pub fn match_substring(&self, index: usize) -> Result<Option<String>, SchemeError> {
        match self.region.pos(index) {
            Some((start, end)) => {
                if start == end {
                    Ok(None)
                } else {
                    Ok(Some(self.text[start..end].to_string()))
                }
            }
            None => Err(SchemeError::assertion_violation(
                "rxmatch-substring",
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
        write!(f, "#<reg-match>")
    }
}

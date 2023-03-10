use crate::gc::Gc;
use crate::objects::Object;
use core::fmt;
use std::error;
use std::fmt::Debug;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorType {
    AssertionViolation,
    ImplementationRestrictionViolation,
    IoError,
    IoDecodingError,
    Error,
}

pub struct Error {
    pub error_type: ErrorType,
    pub who: Object,
    pub message: Object,
    pub irritants: Object,
}

impl Error {
    pub fn new(error_type: ErrorType, who: Object, message: Object, irritants: Object) -> Self {
        Self {
            error_type: error_type,
            who: who,
            message: message,
            irritants: irritants,
        }
    }

    pub fn assertion_violation_obj(
        who: Object,
        message: Object,
        irritants: Object,
    ) -> Result<Object> {
        Err(Self {
            error_type: ErrorType::AssertionViolation,
            who: who,
            message: message,
            irritants: irritants,
        })
    }

    pub fn assertion_violation(
        gc: &mut Box<Gc>,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> Result<Object> {
        Self::new_err(gc, ErrorType::AssertionViolation, who, message, irritants)
    }

    pub fn error(
        gc: &mut Box<Gc>,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> Result<Object> {
        Self::new_err(gc, ErrorType::Error, who, message, irritants)
    }

    pub fn implementation_restriction_violation(
        gc: &mut Box<Gc>,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> Result<Object> {
        Self::new_err(
            gc,
            ErrorType::ImplementationRestrictionViolation,
            who,
            message,
            irritants,
        )
    }

    pub fn io_decoding_error(
        gc: &mut Box<Gc>,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> Result<Option<char>> {
        let who = gc.new_string(who);
        let message = gc.new_string(message);
        let irritants = gc.listn(irritants);
        Err(Self {
            error_type: ErrorType::IoDecodingError,
            who: who,
            message: message,
            irritants: irritants,
        })
    }

    fn new_err(
        gc: &mut Box<Gc>,
        error_type: ErrorType,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> Result<Object> {
        let who = gc.new_string(who);
        let message = gc.new_string(message);
        let irritants = gc.listn(irritants);
        Err(Self {
            error_type: error_type,
            who: who,
            message: message,
            irritants: irritants,
        })
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "type: {:?} who: {} message: {} irritants {}",
            self.error_type,
            self.who.to_string(),
            self.message.to_string(),
            self.irritants.to_string(),
        )
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "who: {} message: {} irritants {}",
            self.who.to_string(),
            self.message.to_string(),
            self.irritants.to_string(),
        )
    }
}

impl error::Error for Error {}

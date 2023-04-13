use crate::objects::Object;
use core::fmt;
use std::error;
use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum SchemeError {
    // Errors which have corresponding Scheme errors.
    AssertionViolation {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    ImplementationRestrictionViolation {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    IoError {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    IoEncodingError {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    IoDecodingError {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    IoFileNotExist {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    IoFileAlreadyExist {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    IoInvalidPosition {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    LexicalViolationReadError {
        who: String,
        message: String,
    },
    Error {
        who: String,
        message: String,
        irritants: Vec<Object>,
    },
    // Non-Scheme errors. This will be converted to assertion-violation.
    Div0,
    NonZeroRequired,
    NanOrInfinite,
    Overflow,
}

impl SchemeError {
    pub fn lexical_violation_read_error(who: &str, message: &str) -> Self {
        Self::LexicalViolationReadError {
            who: who.to_string(),
            message: message.to_string(),
        }
    }

    pub fn assertion_violation(who: &str, message: &str, irritants: &[Object]) -> Self {
        Self::AssertionViolation {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }

    pub fn division_by_zero(who: &str, message: &str, irritants: &[Object]) -> Self {
        // Note we don't use Div0 here.
        Self::AssertionViolation {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }    

    pub fn io_file_already_exist(who: &str, message: &str, irritants: &[Object]) -> Self {
        Self::IoFileAlreadyExist {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }

    pub fn io_file_not_exist(who: &str, message: &str, irritants: &[Object]) -> Self {
        Self::IoFileNotExist {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }

    pub fn io_error(who: &str, message: &str, irritants: &[Object]) -> Self {
        Self::IoError {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }

    pub fn io_encoding_error(who: &str, message: &str, irritants: &[Object]) -> Self {
        Self::IoEncodingError {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }

    pub fn io_decoding_error(who: &str, message: &str, irritants: &[Object]) -> Self {
        Self::IoDecodingError {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }

    pub fn implementation_restriction_violation(
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> Self {
        Self::ImplementationRestrictionViolation {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    AssertionViolation,
    ImplementationRestrictionViolation,
    IoError,
    IoEncodingError,
    IoDecodingError,
    IoFileNotExist,
    IoFileAlreadyExist,
    IoInvalidPosition,
    LexicalViolationReadError,
    Error,
}

#[derive(Clone, PartialEq)]
pub struct Error {
    pub error_type: ErrorType,
    pub who: String,
    pub message: String,
    pub irritants: Vec<Object>,
}

impl Error {
    pub fn new(error_type: ErrorType, who: &str, message: &str, irritants: &[Object]) -> Self {
        Self {
            error_type,
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_owned(),
        }
    }

    pub fn assertion_violation(who: &str, message: &str, irritants: &[Object]) -> Result<Object> {
        Err(Self::new(
            ErrorType::AssertionViolation,
            who,
            message,
            irritants,
        ))
    }

    pub fn lexical_violation_read_error(who: &str, message: &str) -> Result<Object> {
        Err(Self::new(
            ErrorType::LexicalViolationReadError,
            who,
            message,
            &[],
        ))
    }

    pub fn scheme_error(who: &str, message: &str, irritants: &[Object]) -> Result<Object> {
        Err(Self::new(ErrorType::Error, who, message, irritants))
    }

    pub fn implementation_restriction_violation(
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> Result<Object> {
        Err(Self::new(
            ErrorType::ImplementationRestrictionViolation,
            who,
            message,
            irritants,
        ))
    }

    pub fn io_decoding_error(who: &str, message: &str, port: &[Object]) -> Result<Option<char>> {
        Err(Self::new(ErrorType::IoDecodingError, who, message, port))
    }
    pub fn io_encoding_error(who: &str, message: &str, port: &[Object]) -> Result<Option<char>> {
        Err(Self::new(ErrorType::IoEncodingError, who, message, port))
    }

    pub fn io_file_not_exist(who: &str, message: &str, irritants: &[Object]) -> Result<Object> {
        Err(Self::new(
            ErrorType::IoFileNotExist,
            who,
            message,
            irritants,
        ))
    }

    pub fn io_file_already_exist(who: &str, message: &str, irritants: &[Object]) -> Result<Object> {
        Err(Self::new(
            ErrorType::IoFileAlreadyExist,
            who,
            message,
            irritants,
        ))
    }

    pub fn io_invalid_position(who: &str, message: &str, irritants: &[Object]) -> Result<Object> {
        Err(Self::new(
            ErrorType::IoInvalidPosition,
            who,
            message,
            irritants,
        ))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "type: {:?} who: {} message: {} irritants {:?}",
            self.error_type, self.who, self.message, self.irritants,
        )
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "who: {} message: {} irritants {:?}",
            self.who, self.message, self.irritants,
        )
    }
}

impl error::Error for Error {}

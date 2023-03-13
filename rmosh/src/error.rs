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
    IoEncodingError,
    IoDecodingError,
    IoFileNotExist,
    IoFileAlreadyExist,
    IoInvalidPosition,
    Error,
}

pub struct Error {
    pub error_type: ErrorType,
    pub who: String,
    pub message: String,
    pub irritants: Vec<Object>,
}

impl Error {
    pub fn new(error_type: ErrorType, who: &str, message: &str, irritants: &[Object]) -> Self {
        Self {
            error_type: error_type,
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

    pub fn error(who: &str, message: &str, irritants: &[Object]) -> Result<Object> {
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
            self.error_type,
            self.who.to_string(),
            self.message.to_string(),
            self.irritants,
        )
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "who: {} message: {} irritants {:?}",
            self.who.to_string(),
            self.message.to_string(),
            self.irritants,
        )
    }
}

impl error::Error for Error {}

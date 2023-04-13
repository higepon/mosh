use crate::objects::Object;
use std::fmt::Debug;

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
        position: isize,
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

    pub fn io_invalid_position(
        who: &str,
        message: &str,
        irritants: &[Object],
        position: isize,
    ) -> Self {
        Self::IoInvalidPosition {
            who: who.to_string(),
            message: message.to_string(),
            irritants: irritants.to_vec(),
            position,
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

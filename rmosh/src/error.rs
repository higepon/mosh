use crate::objects::Object;
use core::fmt;
use std::error;
use std::fmt::Debug;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

pub struct Error {
    pub who: Object,
    pub message: Object,
    pub irritants: Object,
}

impl Error {
    pub fn new(who: Object, message: Object, irritants: Object) -> Self {
        Self {
            who: who,
            message: message,
            irritants: irritants,
        }
    }
}

impl Display for Error {
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

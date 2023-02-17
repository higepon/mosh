use crate::gc::Gc;
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
    pub fn new_from_string(gc: &mut Box<Gc>, who: &str, message: &str, irritants: Object) -> Self {
        let who = gc.new_string(who);
        let message = gc.new_string(message);        
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

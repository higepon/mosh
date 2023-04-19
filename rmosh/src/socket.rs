use std::fmt::{self, Display};

use crate::{
    error::SchemeError,
    gc::{GcHeader, ObjectType, Trace},
};
use std::fmt::Debug;

#[repr(C)]
pub struct Socket {
    pub header: GcHeader,
}

impl Trace for Socket {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Socket {
    pub fn create_client_socket() -> Result<Self, SchemeError> {
        Ok(Self {
            header: GcHeader::new(ObjectType::Socket),
        })
    }

    pub fn send(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        todo!()
    }
}

impl Debug for Socket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("#<socket>")
    }
}

impl Display for Socket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<socket>")
    }
}

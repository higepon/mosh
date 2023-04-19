use std::{
    fmt::{self, Display},
    io::Write,
    net::TcpStream,
};

use crate::{
    error::SchemeError,
    gc::{GcHeader, ObjectType, Trace},
};
use std::fmt::Debug;

#[repr(C)]
pub struct Socket {
    pub header: GcHeader,
    pub stream: TcpStream,
}

impl Trace for Socket {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Socket {
    pub fn create_client_socket() -> Result<Self, SchemeError> {
        let mut stream = TcpStream::connect("neverssl.com:80").expect("failed to connect server");
        Ok(Self {
            header: GcHeader::new(ObjectType::Socket),
            stream: stream,
        })
    }

    pub fn send(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        self.stream
            .write(buf)
            .map_err(|e| SchemeError::io_error("send", &e.to_string(), &[]))
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

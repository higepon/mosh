use std::{
    fmt::{self, Display},
    io::{Read, Write},
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
    pub fn create_client_socket(node: &str, service: &str) -> Result<Self, SchemeError> {
        let stream = TcpStream::connect(format!("{}:{}", node, service)).map_err(|e| {
            SchemeError::io_error("connect", &format!("{}: {} {}", e, node, service), &[])
        })?;

        Ok(Self {
            header: GcHeader::new(ObjectType::Socket),
            stream,
        })
    }

    pub fn send(&mut self, buf: &[u8]) -> Result<usize, SchemeError> {
        self.stream
            .write(buf)
            .map_err(|e| SchemeError::io_error("send", &e.to_string(), &[]))
    }

    pub fn receive(&mut self, buf: &mut [u8]) -> Result<usize, SchemeError> {
        self.stream
            .read(buf)
            .map_err(|e| SchemeError::io_error("receive", &e.to_string(), &[]))
    }

    pub fn close(&mut self) -> Result<(), SchemeError> {
        self.stream
            .shutdown(std::net::Shutdown::Both)
            .map_err(|e| SchemeError::io_error("close", &e.to_string(), &[]))
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

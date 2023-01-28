pub mod alloc;
pub mod compiler;
pub mod equal;
pub mod fasl;
pub mod gc;
pub mod lexer;
pub mod lexer_iter;
pub mod objects;
pub mod op;
pub mod ports;
pub mod procs;
pub mod psyntax;
pub mod read;
pub mod vm;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub reader); // synthesized by LALRPOP

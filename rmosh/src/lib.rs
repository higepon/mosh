pub mod alloc;
pub mod compiler;
pub mod equal;
pub mod fasl;
pub mod gc;
pub mod objects;
pub mod op;
pub mod procs;
pub mod vm;
pub mod lexer_iter;
pub mod lexer;
pub mod read;
#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub reader); // synthesized by LALRPOP
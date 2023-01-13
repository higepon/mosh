extern crate num_derive;
#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub reader); // synthesized by LALRPOP
pub mod alloc;
pub mod compiler;
pub mod equal;
pub mod fasl;
pub mod gc;
pub mod lexer;
pub mod lexer_iter;
pub mod objects;
pub mod op;
pub mod procs;
pub mod vm;
pub mod read;

fn main() {}

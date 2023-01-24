use std::env;

use rmosh::objects::Object;
use rmosh::vm::Vm;
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
pub mod psyntax;
pub mod read;
pub mod write;
pub mod vm;

fn main() {
    let args: Vec<String> = env::args().collect();
    //if args.len() == 2 {
    let mut vm = Vm::new();
    vm.should_load_compiler = true;
    let mut vargs: Vec<Object> = vec![];
    for i in 1..args.len() {
        vargs.push(vm.gc.new_string(&args[i]));
    }
    vargs.push(vm.gc.new_string("fib.scm"));
    let vargs = vm.gc.listn(&vargs);
    vm.enable_r7rs(vargs);
}

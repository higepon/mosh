use std::env;

use rmosh::objects::Object;
use rmosh::vm::Vm;
extern crate num_derive;
#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub reader); // synthesized by LALRPOP
lalrpop_mod!(pub number_reader); // synthesized by LALRPOP
pub mod alloc;
pub mod compiler;
pub mod equal;
pub mod error;
pub mod fasl;
pub mod gc;
pub mod lexer;
pub mod lexer_iter;
pub mod number_lexer;
pub mod number_lexer_iter;
pub mod numbers;
pub mod objects;
pub mod op;
pub mod ports;
pub mod procs;
pub mod procs_util;
pub mod psyntax;
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
    //vargs.push(vm.gc.new_string("/root/mosh.git/tests/r7rs/r7rs-tests.scm"));
    vargs.push(vm.gc.new_string("/root/cont.sc"));
    let vargs = vm.gc.listn(&vargs);
    match vm.enable_r7rs(vargs) {
        Ok(_ret) => println!("normal exit"),
        Err(e) => {
            println!("e={}", e);
        }
    }
}

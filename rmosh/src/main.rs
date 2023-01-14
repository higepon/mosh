use std::{env, fs};

use rmosh::objects::Object;
use rmosh::op::Op;
use rmosh::read::read;
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
pub mod read;
pub mod vm;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        let mut vm = Vm::new();
        vm.should_load_compiler = true;

        let text = fs::read_to_string(args[1].to_owned()).unwrap();
        let text = "(".to_string() + &text;
        let text = text + ")";
        let mut sexps = read(&mut vm.gc, &text).unwrap();
        loop {
            if sexps.is_nil() {
                break;
            }

            //println!("text={}", sexp);
            let ops = vec![
                Object::Instruction(Op::Frame),
                Object::Number(8),
                Object::Instruction(Op::Constant),
                sexps.to_pair().car,
                Object::Instruction(Op::Push),
                Object::Instruction(Op::ReferGlobal),
                vm.gc.symbol_intern("compile-no-optimize"),
                Object::Instruction(Op::Call),
                Object::Number(1),
                Object::Instruction(Op::Halt),
            ];
            let ret = vm.run(ops.as_ptr(), ops.len());
            //println!("code={}", ret);
            match ret {
                Object::Vector(v) => {
                    vm.run(v.data.as_ptr(), v.data.len());
                }
                _ => {}
            }
            sexps = sexps.to_pair().cdr;
        }
    }
}

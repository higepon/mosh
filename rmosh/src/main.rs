use clap::Parser;
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
pub mod reader_util;
pub mod vm;

/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    #[arg(long = "loadpath", help = "Add library loadpath.")]
    loadpath: Option<String>,

    #[arg(help = "Path to a Scheme program file.")]
    file: Option<String>,
}

fn main() {
    let args = Cli::parse();
    let mut vm = Vm::new();
    vm.should_load_compiler = true;
    let mut vargs: Vec<Object> = vec![];
    // The main program file.
    if let Some(file) = args.file {
        vargs.push(vm.gc.new_string(&file));
    } else {
        //vargs.push(vm.gc.new_string("/root/mosh.git/tests/r7rs/r7rs-tests.scm"));
        //vargs.push(vm.gc.new_string("/root/cont.scm"));
        vargs.push(
          vm.gc
            .new_string("/root/mosh.git/tests/r6rs-test-suite/tests/r6rs/run/arithmetic/fixnums.sps"),
        )
    }

    let vargs = vm.gc.listn(&vargs);
    //    let loadpath = args.loadpath;
    let loadpath = Some("/root/mosh.git/tests/r6rs-test-suite/".to_string());
    match vm.enable_r7rs(vargs, loadpath) {
        Ok(_ret) => (),
        Err(e) => {
            eprintln!("Abnormal exit {:?}", e);
        }
    }
}

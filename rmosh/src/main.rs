use clap::Parser;
extern crate num_derive;
#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(
    #[allow(clippy::all)]
    pub reader); // synthesized by LALRPOP
lalrpop_mod!(
    #[allow(clippy::all)]
    pub number_reader); // synthesized by LALRPOP
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

/// rmosh R7RS & R6RS Scheme intepreter.
#[derive(Parser)]
struct Cli {
    #[arg(long = "loadpath", help = "Add library loadpath.")]
    loadpath: Option<String>,

    #[arg(help = "Path to a Scheme program file.")]
    file: Option<String>,

    #[clap(long, short, action)]
    show_gc_stats: bool,
}

fn main() {
    let args = Cli::parse();
    let vm = unsafe { &mut vm::CURRENT_VM };
    vm.should_load_compiler = true;
    let mut vargs: Vec<objects::Object> = vec![];
    // The main program file.
    if let Some(file) = args.file {
        vargs.push(vm.gc.new_string(&file));
    } else {
        //vargs.push(vm.gc.new_string("/root/mosh.git/tests/srfi/srfi-194.scm"));
        vargs.push(vm.gc.new_string("/root/mosh.git/tests/unicode.scm"));
        //vargs.push(vm.gc.new_string("/root/cont.scm"));
        /*
        vargs.push(
            vm.gc
                .new_string("/root/mosh.git/tests/r6rs-test-suite/tests/r6rs/run/io/ports.sps"),
        )*/
        // vargs.push(
        //   vm.gc
        //     .new_string("/root/mosh.git/tests/r6rs-test-suite/tests/r6rs/run/contrib.sps"),
        //)
    }

    vm.gc.show_stats = args.show_gc_stats;
    let vargs = vm.gc.listn(&vargs);
    let loadpath = args.loadpath;
    //let loadpath = Some("/root/mosh.git/tests/r6rs-test-suite/".to_string());
    match vm.enable_r7rs(vargs, loadpath) {
        Ok(_ret) => (),
        Err(e) => {
            eprintln!("Abnormal exit {:?}", e);
        }
    }
}

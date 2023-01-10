extern crate num_derive;
#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub calculator1); // synthesized by LALRPOP
pub mod alloc;
pub mod compiler;
pub mod equal;
pub mod fasl;
pub mod gc;
pub mod objects;
pub mod op;
pub mod procs;
pub mod vm;

fn main() {}




#[test]
fn calculator1() {
    assert!(calculator1::TermParser::new().parse("22").is_ok());
    assert!(calculator1::TermParser::new().parse("(22)").is_ok());
    assert!(calculator1::TermParser::new().parse("((((22))))").is_ok());
    assert!(calculator1::TermParser::new().parse("((22)").is_err());
}

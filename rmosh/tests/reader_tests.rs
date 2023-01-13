
use rmosh::{lexer, objects::Object, reader::DatumParser, vm::Vm};

#[test]
fn parse_boolean() {
    let mut vm = Vm::new();
    assert_eq!(
        Object::False,
        DatumParser::new()
            .parse(&mut vm.gc, lexer::Lexer::new(b"#false\0"))
            .unwrap()
    );       
} 

#[test]
fn parse_boolean2() {
    let mut vm = Vm::new();
    assert_eq!(
        Object::True,
        DatumParser::new()
            .parse(&mut vm.gc, lexer::Lexer::new(b"#t\0"))
            .unwrap()
    );
}

#[test]
fn parse_boolean3() {
    let mut vm = Vm::new();
    assert_eq!(
        Object::True,
        DatumParser::new()
            .parse(&mut vm.gc, lexer::Lexer::new(b"#true\0"))
            .unwrap()
    );

}
 
#[test]
fn parse_symbol() {
    let mut vm = Vm::new();
    assert_eq!(
        vm.gc.symbol_intern("abc"),
        DatumParser::new()
            .parse(&mut vm.gc, lexer::Lexer::new(b"abc\0"))
            .unwrap()
    );

}
 

    /*
    assert_eq!(
        Object::False,
        DatumParser::new().parse(&mut vm.gc, lexer).unwrap()
    );
    assert_eq!(
        Object::False,
        DatumParser::new().parse(&mut vm.gc, lexer).unwrap()
    );
    */

/*
#[test]
fn parse_symbol() {
    let mut vm = Vm::new();
    assert_eq!(
        vm.gc.symbol_intern("abc"),
        DatumParser::new().parse(&mut vm.gc, "abc").unwrap()
    );

    assert_eq!(
        vm.gc.symbol_intern("$seq1-"),
        DatumParser::new().parse(&mut vm.gc, "$seq1-").unwrap()
    );
}
*/

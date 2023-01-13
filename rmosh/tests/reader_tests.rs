use rmosh::{gc::Gc, lexer, objects::Object, reader::DatumParser, vm::Vm, read::read};

#[test]
fn parse_boolean() {
    let mut vm = Vm::new();
    assert_eq!(Object::False, read(&mut vm.gc, "#false"));
}

#[test]
fn parse_boolean2() {
    let mut vm = Vm::new();
    assert_eq!(Object::True, read(&mut vm.gc, "#t"));
}

#[test]
fn parse_boolean3() {
    let mut vm = Vm::new();
    assert_eq!(Object::True, read(&mut vm.gc, "#true"));
}

#[test]
fn parse_symbol() {
    let mut vm = Vm::new();
    assert_eq!(vm.gc.symbol_intern("abc"), read(&mut vm.gc, "abc"));
}

/*
#[test]
fn handle_error() {
    let mut vm = Vm::new();
    assert_eq!(
        Object::False,
        DatumParser::new()
            .parse(&mut vm.gc, lexer::Lexer::new(b"3\0"))
            .unwrap()
    );

}
 */

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

use rmosh::{objects::Object, reader::DatumParser, vm::Vm};

#[test]
fn parse_boolean() {
    let mut vm = Vm::new();
    assert_eq!(
        Object::True,
        DatumParser::new().parse(&mut vm.gc, "#t").unwrap()
    );
    assert_eq!(
        Object::True,
        DatumParser::new().parse(&mut vm.gc, "#true").unwrap()
    );
    assert_eq!(
        Object::False,
        DatumParser::new().parse(&mut vm.gc, "#f").unwrap()
    );
    assert_eq!(
        Object::False,
        DatumParser::new().parse(&mut vm.gc, "#false").unwrap()
    );
}

#[test]
fn parse_symbol() {
    let mut vm = Vm::new();
    assert_eq!(
        vm.gc.symbol_intern("abc"),
        DatumParser::new().parse(&mut vm.gc, "abc").unwrap()
    );

    assert_eq!(
        vm.gc.symbol_intern("$seq1"),
        DatumParser::new().parse(&mut vm.gc, "$seq1").unwrap()
    );    
}

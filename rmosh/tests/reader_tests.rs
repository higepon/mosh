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

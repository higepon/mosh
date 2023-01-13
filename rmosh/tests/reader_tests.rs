use rmosh::{objects::Object, read::read, vm::Vm};

#[test]
fn parse_true() {
    let mut vm = Vm::new();
    assert_eq!(Object::True, read(&mut vm.gc, "#t").unwrap());
    assert_eq!(Object::True, read(&mut vm.gc, "#true").unwrap());
}

#[test]
fn parse_false() {
    let mut vm = Vm::new();
    assert_eq!(Object::False, read(&mut vm.gc, "#f").unwrap());
    assert_eq!(Object::False, read(&mut vm.gc, "#false").unwrap());
}

#[test]
fn parse_symbol() {
    let mut vm = Vm::new();
    assert_eq!(vm.gc.symbol_intern("abc"), read(&mut vm.gc, "abc").unwrap());
    assert_eq!(vm.gc.symbol_intern("$seq"), read(&mut vm.gc, "$seq").unwrap());    
}

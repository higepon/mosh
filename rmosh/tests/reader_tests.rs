use rmosh::{objects::Object, read::read, vm::Vm, equal::Equal};

#[macro_export]
macro_rules! assert_equal {
    ($gc:expr, $lhs:expr, $rhs:expr) => {{
        let e = Equal::new();
        if e.is_equal(&mut $gc, &$lhs, &$rhs) {
            assert!(true);
        } else {
            println!("{} is not equal to {}", $lhs, $rhs);
            assert!(false);
        }
    }};
}

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
    assert_eq!(vm.gc.symbol_intern("$seq--"), read(&mut vm.gc, "$seq--").unwrap());
    assert_eq!(vm.gc.symbol_intern("|xy z|"), read(&mut vm.gc, "|xy z|").unwrap());
    assert_eq!(vm.gc.symbol_intern(".abc"), read(&mut vm.gc, ".abc").unwrap());
}

#[test]
fn parse_list() {
    let mut vm = Vm::new();
    let expected = vm.gc.list1(Object::True);
    let obj = read(&mut vm.gc, "(#t)").unwrap();
    assert_equal!(vm.gc, expected, obj);

}

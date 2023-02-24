use lalrpop_util::ParseError;
use rmosh::number_lexer::NumberLexer;
use rmosh::{
    equal::Equal,
    gc::Gc,
    number_reader::NumberParser,
    objects::Object,
    ports::{StringInputPort, TextInputPort},
    reader_util::ReadError,
    vm::Vm,
};

fn read(gc: &mut Box<Gc>, s: &str) -> Result<Object, ReadError> {
    let mut port = StringInputPort::new(s);
    port.read(gc)
}

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
    assert_eq!(
        vm.gc.symbol_intern("$seq"),
        read(&mut vm.gc, "$seq").unwrap()
    );
    assert_eq!(
        vm.gc.symbol_intern("$seq--"),
        read(&mut vm.gc, "$seq--").unwrap()
    );
    assert_eq!(
        vm.gc.symbol_intern("|xy z|"),
        read(&mut vm.gc, "|xy z|").unwrap()
    );
    assert_eq!(
        vm.gc.symbol_intern(".abc"),
        read(&mut vm.gc, ".abc").unwrap()
    );
}

#[test]
fn parse_list() {
    let mut vm = Vm::new();
    let expected = Object::Nil;
    let obj = read(&mut vm.gc, "()").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_list1() {
    let mut vm = Vm::new();
    let expected = vm.gc.list1(Object::True);
    let obj = read(&mut vm.gc, "(#t)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_list2() {
    let mut vm = Vm::new();
    let expected = vm.gc.list2(Object::True, Object::False);
    let obj = read(&mut vm.gc, "(#t #f)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_nested_list() {
    let mut vm = Vm::new();
    let list1 = vm.gc.list1(Object::True);
    let expected = vm.gc.list2(list1, Object::False);
    let obj = read(&mut vm.gc, "((#t) #f)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_string() {
    let mut vm = Vm::new();
    let expected = vm.gc.new_string("hello");
    let obj = read(&mut vm.gc, "\"hello\"").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_number() {
    let mut vm = Vm::new();
    let expected = Object::Fixnum(101);
    let obj = read(&mut vm.gc, "101").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_number_list1() {
    let mut vm = Vm::new();
    let expected = vm.gc.list1(Object::Fixnum(3));
    let obj = read(&mut vm.gc, "(3)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_number_comment_list1() {
    let mut vm = Vm::new();
    let expected = vm.gc.list1(Object::Fixnum(3));
    let obj = read(&mut vm.gc, "; comment\n(3)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_number_list2() {
    let mut vm = Vm::new();
    let expected = vm.gc.list2(Object::Fixnum(3), Object::Fixnum(4));
    let obj = read(&mut vm.gc, "(3 4)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_dot_pair() {
    let mut vm = Vm::new();
    let expected = vm.gc.cons(Object::Fixnum(3), Object::False);
    let obj = read(&mut vm.gc, "(3 . #f)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_empty_vector() {
    let mut vm = Vm::new();
    let expected = vm.gc.new_vector(&vec![]);
    let obj = read(&mut vm.gc, "#()").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_vector() {
    let mut vm = Vm::new();
    let expected = vm.gc.new_vector(&vec![
        Object::Fixnum(3),
        Object::Fixnum(4),
        Object::Fixnum(5),
    ]);
    let obj = read(&mut vm.gc, "#(3 4 5)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_bytevector() {
    let mut vm = Vm::new();
    let expected = vm.gc.new_bytevector(&vec![
        Object::Fixnum(3),
        Object::Fixnum(4),
        Object::Fixnum(5),
    ]);
    let obj = read(&mut vm.gc, "#u8(3 4 5)").unwrap();
    assert_equal!(vm.gc, expected, obj);
}

#[test]
fn parse_chars() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "#\\a").unwrap();
        assert_equal!(vm.gc, Object::Char('a'), obj);
    }
}

#[test]
fn parse_chars2() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "#\\あ").unwrap();
        assert_equal!(vm.gc, Object::Char('あ'), obj);
    }
}

#[test]
fn parse_hex_chars() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "#\\x41").unwrap();
        assert_equal!(vm.gc, Object::Char('A'), obj);
    }
}

#[test]
fn parse_hex_number() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "#x7F").unwrap();
        assert_equal!(vm.gc, Object::Fixnum(127), obj);
    }
}

#[test]
fn read_quote() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "'a").unwrap();
        let quote = vm.gc.symbol_intern("quote");
        let symbol = vm.gc.symbol_intern("a");
        let expected = vm.gc.list2(quote, symbol);
        assert_equal!(vm.gc, expected, obj);
    }
}

#[test]
fn read_quasiquote() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "`a").unwrap();
        let quote = vm.gc.symbol_intern("quasiquote");
        let symbol = vm.gc.symbol_intern("a");
        let expected = vm.gc.list2(quote, symbol);
        assert_equal!(vm.gc, expected, obj);
    }
}

#[test]
fn read_unquote() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, ",a").unwrap();
        let quote = vm.gc.symbol_intern("unquote");
        let symbol = vm.gc.symbol_intern("a");
        let expected = vm.gc.list2(quote, symbol);
        assert_equal!(vm.gc, expected, obj);
    }
}

#[test]
fn read_datum_comment() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "#; 3 4").unwrap();
        let expected = Object::Fixnum(4);
        assert_equal!(vm.gc, expected, obj);
    }

    {
        let obj = read(&mut vm.gc, "(3 #; 4)").unwrap();
        let expected = vm.gc.list1(Object::Fixnum(3));
        assert_equal!(vm.gc, expected, obj);
    }

    {
        let obj = read(&mut vm.gc, "(3 #;(9))").unwrap();
        let expected = vm.gc.list1(Object::Fixnum(3));
        assert_equal!(vm.gc, expected, obj);
    }

    {
        let obj = read(&mut vm.gc, "(3 #;8 #;9)").unwrap();
        let expected = vm.gc.list1(Object::Fixnum(3));
        assert_equal!(vm.gc, expected, obj);
    }
}

#[test]
fn parse_multiple() {
    let mut vm = Vm::new();
    let mut port = StringInputPort::new("(3) (4)");

    let expected = vm.gc.list1(Object::Fixnum(3));
    let parsed = port.read(&mut vm.gc).unwrap();
    assert_equal!(vm.gc, expected, parsed);

    let expected = vm.gc.list1(Object::Fixnum(4));
    let parsed = port.read(&mut vm.gc).unwrap();
    assert_equal!(vm.gc, expected, parsed);

    let expected = Object::Eof;
    let parsed = port.read(&mut vm.gc).unwrap();
    assert_equal!(vm.gc, expected, parsed);
}

#[test]
fn parse_special_chars() {
    let mut vm = Vm::new();
    {
        let obj = read(&mut vm.gc, "#\\alarm").unwrap();
        assert_equal!(vm.gc, Object::Char(char::from(7)), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\backspace").unwrap();
        assert_equal!(vm.gc, Object::Char(char::from(8)), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\delete").unwrap();
        assert_equal!(vm.gc, Object::Char(char::from(0x7f)), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\escape").unwrap();
        assert_equal!(vm.gc, Object::Char(char::from(0x1b)), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\newline").unwrap();
        assert_equal!(vm.gc, Object::Char('\n'), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\null").unwrap();
        assert_equal!(vm.gc, Object::Char('\0'), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\return").unwrap();
        assert_equal!(vm.gc, Object::Char(char::from(0x0d)), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\space").unwrap();
        assert_equal!(vm.gc, Object::Char(' '), obj);
    }

    {
        let obj = read(&mut vm.gc, "#\\tab").unwrap();
        assert_equal!(vm.gc, Object::Char('\t'), obj);
    }
}

#[test]
fn propagate_lexer_error() {
    // Lexter finds an invalid token and bubble up the error to the parser.
    let s = "?3";
    let mut vm = Vm::new();
    let mut chars: Vec<char> = s.chars().collect();
    chars.push('\0');
    match NumberParser::new().parse(&mut vm.gc, NumberLexer::new(&chars)) {
        Ok(_) => {
            assert!(false);
        }
        Err(ParseError::User {
            error: ReadError::InvalidToken { start, end, token },
        }) => {
            assert_eq!(0, start);
            assert_eq!(1, end);
            assert_eq!("?", token);
        }
        _ => {
            assert!(false);
        }
    }
}

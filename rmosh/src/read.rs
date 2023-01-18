use lalrpop_util::ParseError;

use crate::{
    gc::Gc,
    lexer::{self, LexicalError},
    objects::Object,
    reader::DatumParser,
};

type ReadError = ParseError<usize, lexer::Token, LexicalError>;

pub fn read(gc: &mut Box<Gc>, s: &str) -> Result<Object, ReadError> {
    let mut s = s.to_string();
    // re2c assumes null terminated string.
    s.push('\0');
    let chars: Vec<char> = s.chars().collect();
    DatumParser::new().parse(gc, lexer::Lexer::new(&chars))
}

pub fn read_sexps(gc: &mut Box<Gc>, text: &str) -> Vec<Object> {
    let mut objs = vec![];
    let text = "(".to_string() + &text;
    let text = text + ")";
    let mut sexps = read(gc, &text).unwrap();
    println!("before");
    loop {
        if sexps.is_nil() {
            break;
        }
        objs.push(sexps.to_pair().car);
        sexps = sexps.to_pair().cdr;
    }
    println!("after");    
    return objs;
}

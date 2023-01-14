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
    DatumParser::new().parse(gc, lexer::Lexer::new(s.as_bytes()))
}

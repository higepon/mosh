use crate::{gc::Gc, objects::Object, lexer, reader::DatumParser};

pub fn read(gc: &mut Box<Gc>, s: &str) -> Object {
    let mut s = s.to_string();
    s.push('\0');
    DatumParser::new()
        .parse(gc, lexer::Lexer::new(s.as_bytes()))
        .unwrap()
}
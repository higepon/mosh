use lalrpop_util::ParseError;

use crate::{
    gc::Gc,
    lexer::{self, LexicalError},
    objects::Object,
    reader::DatumParser,
};

// LALRPOP doesn't support multiple call of parse.
// We parse all S-Expressions once then store them in this reader.
#[derive(Debug)]
pub struct Reader {
    text: String,
    pub parsed: Object,
}

impl Reader {
    pub fn new(text: &str) -> Self {
        Self {
            text: text.to_string(),
            parsed: Object::Unspecified,
        }
    }

    pub fn read(&mut self, gc: &mut Box<Gc>) -> Result<Object, ReadError> {
        // Parse
        if self.parsed.is_unspecified() {
            let text = self.text.to_string();
            let text = "(".to_string() + &text;
            // re2c assumes null terminated string.
            let text = text + ")\0";
            let chars: Vec<char> = text.chars().collect();

            self.parsed = DatumParser::new().parse(gc, lexer::Lexer::new(&chars))?;
        }
        if self.parsed.is_nil() {
            return Ok(Object::Eof);
        } else {
            let obj = self.parsed.to_pair().car;
            self.parsed = self.parsed.to_pair().cdr;
            return Ok(obj);
        }
    }
}

pub type ReadError = ParseError<usize, lexer::Token, LexicalError>;

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

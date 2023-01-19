use lalrpop_util::ParseError;

use crate::{
    gc::Gc,
    lexer::{self, LexicalError},
    objects::Object,
    reader::DatumParser,
};

pub type ReadError = ParseError<usize, lexer::Token, LexicalError>;

// LALRPOP doesn't support multiple call of parse.
// We parse all S-Expressions once then store them in this reader.
#[derive(Debug)]
pub struct Reader {
    text: String,
    pub parsed: Object,
}

// helper for testing.
pub fn read(gc: &mut Box<Gc>, s: &str) -> Result<Object, ReadError> {
    let mut reader = Reader::new(s);
    reader.read(gc)
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

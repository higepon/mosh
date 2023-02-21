use crate::ports::ReadError;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Digit { value: String },
    HexDigit { value: String },
    Radix10,
    Radix16,
    Dot,
    Exact,
    Exponent,
    Imag,
    Inexact,
    Minus,
    MinusInf,
    MinusNan,
    Plus,
    PlusInf,
    PlusNan,
    Slash,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerItem = Spanned<Token, usize, ReadError>;

#[derive(Clone, Debug)]
pub struct NumberLexer<'input> {
    pub s: &'input [char],
    pub cursor: usize,
    pub marker: usize,
    pub limit: usize,
    pub tok: usize,
}

// TODO:
// - Fix range in Some.
impl<'input> NumberLexer<'input> {
    pub fn new(input: &'input [char]) -> Self {
        //println!("number lexer for <{:?}>", input);
        Self {
            s: input,
            cursor: 0,
            marker: 0,
            tok: 0,
            limit: input.len() - 1,
        }
    }

    pub fn with_location(&self, token: Token) -> Option<LexerItem> {
        Some(Ok((self.tok, token, self.cursor)))
    }

    pub fn extract_token(&self) -> String {
        self.s[self.tok..self.cursor].iter().collect()
    }
}

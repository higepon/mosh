use crate::reader_util::{ReadError, read_string};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    AbbrevQuasiquote,
    AbbrevQuote,
    AbbrevUnquote,
    AbbrevUnquoteSplicing,
    AbbrevQuasisyntax,
    AbbrevSyntax,
    AbbrevUnsyntax,
    AbbrevUnsyntaxSplicing,
    Character { value: char },
    DatumComment,
    Dot,
    False,
    Identifier { value: String },
    LeftParen,
    Number10 { value: String },
    Number8 { value: String },    
    Number16 { value: String },
    RightParen,
    Regexp { value: String },
    String { value: String },
    True,
    ByteVectorStart,
    VectorStart,
    Digit { value: String },
    HexDigit { value: String },
    OctDigit { value: String },    
    Radix10,
    Radix8,    
    Radix16,
    Exact,
    Exponent { value: String },
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
pub struct Lexer<'input> {
    pub s: &'input [char],
    pub cursor: usize,
    pub marker: usize,
    pub limit: usize,
    pub tok: usize,
}

// TODO:
// - Fix range in Some.
impl<'input> Lexer<'input> {
    pub fn new(input: &'input [char]) -> Self {
        Self {
            s: input,
            cursor: 0,
            marker: 0,
            tok: 0,
            limit: input.len() - 1,
        }
    }

    // todo pub
    pub fn with_location(&self, token: Token) -> Option<LexerItem> {
        Some(Ok((self.tok, token, self.cursor)))
    }

    pub fn extract_token(&self) -> String {
        self.s[self.tok..self.cursor].iter().collect()
    }

    pub fn extract_character(&self) -> char {
        // Actual character is at index = 2 #\a.
        self.s[self.tok + 2]
    }

    pub fn extract_hex_character(&self) -> char {
        // #\xAB
        let hex_str: String = self.s[self.tok + 3..self.cursor].iter().collect();
        match u32::from_str_radix(&hex_str, 16) {
            Ok(n) => match char::from_u32(n) {
                Some(c) => c,
                None => {
                    panic!("malformed hex scalar value character")
                }
            },
            Err(e) => {
                panic!("malformed hex scalar value character: {} in {}", e, hex_str)
            }
        }
    }

    pub fn extract_string(&self) -> String {
        // Remove double quotes.
        let s: String = self.s[self.tok + 1..self.cursor - 1].iter().collect();
        read_string(&s)
    }

    pub fn extract_regexp(&self) -> String {
        // Remove #/ and /
        self.s[self.tok + 2..self.cursor - 1].iter().collect()
    }
}

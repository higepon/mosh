use crate::reader_util::{read_string, ReadError};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    AbbrevQuasiquote,
    AbbrevQuasisyntax,
    AbbrevQuote,
    AbbrevSyntax,
    AbbrevUnquote,
    AbbrevUnquoteSplicing,
    AbbrevUnsyntax,
    AbbrevUnsyntaxSplicing,
    At,
    BinDigit { value: String },
    ByteVectorStart,
    Character { value: char },
    DatumComment,
    DefinedShared { value: String },
    DefiningShared { value: String },
    Digit { value: String },
    Dot,
    Exact,
    Exponent { value: String },
    False,
    HexDigit { value: String },
    Identifier { value: String },
    Imag,
    Inexact,
    LeftParen,
    Minus,
    MinusInf,
    MinusNan,
    Number10 { value: String },
    Number16 { value: String },
    Number8 { value: String },
    Number2 { value: String },
    OctDigit { value: String },
    Plus,
    PlusInf,
    PlusNan,
    Radix10,
    Radix16,
    Radix2,
    Radix8,
    Regexp { value: String },
    RightParen,
    Slash,
    String { value: String },
    True,
    VectorStart,
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
    pub(super) is_fold_case: bool,
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
            is_fold_case: false,
        }
    }

    // todo pub
    pub fn with_location(&self, token: Token) -> Option<LexerItem> {
        Some(Ok((self.tok, token, self.cursor)))
    }

    pub fn extract_token(&self) -> String {
        let token: String = self.s[self.tok..self.cursor].iter().collect();
        if self.is_fold_case {
            token.to_ascii_lowercase()
        } else {
            token
        }
    }

    pub fn extract_character(&self) -> char {
        // Actual character is at index = 2 #\a.
        self.s[self.tok + 2]
    }

    pub fn extract_defining_shared(&self) -> String {
        self.s[self.tok + 1..self.cursor - 1].iter().collect()
    }

    pub fn extract_defined_shared(&self) -> String {
        // #33#
        self.s[self.tok + 1..self.cursor - 1].iter().collect()
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

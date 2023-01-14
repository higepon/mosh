#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    AbbrevQuote,
    Character { value: char },
    Dot,
    False,
    Identifier { value: String },
    LeftParen,
    Number10 { value: String },
    RightParen,
    String { value: String },
    True,
    VectorStart,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerItem = Spanned<Token, usize, LexicalError>;

#[derive(Clone, Debug)]
pub struct Lexer<'input> {
    pub s: &'input [u8],
    pub cursor: usize,
    pub marker: usize,
    pub limit: usize,
    pub tok: usize,
}

// TODO:
// - Fix range in Some.
impl<'input> Lexer<'input> {
    pub fn new(input: &'input [u8]) -> Self {
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
        match std::str::from_utf8(&self.s[self.tok..self.cursor]) {
            Ok(s) => s.to_string(),
            Err(_) => {
                panic!("malformed utf8 string")
            }
        }
    }

    pub fn extract_character(&self) -> char {
        // Actual character is at index = 2 #\a.
        match std::char::from_u32(self.s[self.tok + 2] as u32) {
            Some(c) => c,
            None => {
                panic!("malformed char")
            }
        }
    }

    pub fn extract_hex_character(&self) -> char {
        // #\xAB
        match std::str::from_utf8(&self.s[self.tok + 3..self.cursor]) {
            Ok(hex_str) => match u32::from_str_radix(hex_str, 16) {
                Ok(n) => match char::from_u32(n) {
                    Some(c) => c,
                    None => {
                        panic!("malformed hex scalar value character")
                    }
                },
                Err(e) => {
                    panic!("malformed hex scalar value character: {} in {}", e, hex_str)
                }
            },
            Err(e) => {
                panic!("malformed hex scalar value character: {}", e)
            }
        }
    }

    pub fn extract_string(&self) -> String {
        // Remove double quotes.
        match std::str::from_utf8(&self.s[self.tok + 1..self.cursor - 1]) {
            Ok(s) => s.to_string(),
            Err(_) => {
                panic!("malformed utf8 string")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LexicalError {
    pub start: usize,
    pub end: usize,
    pub token: String,
}

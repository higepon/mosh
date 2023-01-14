#[derive(Clone, Debug, PartialEq)]
pub enum Token {
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

    pub fn extract_token(&self) -> String {
        match std::str::from_utf8(&self.s[self.tok..self.cursor]) {
            Ok(s) => s.to_string(),
            Err(_) => {
                panic!("malformed utf8 string")
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
pub enum LexicalError {}

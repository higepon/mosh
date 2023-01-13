#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    True,
    False,
    LeftParen,
    RightParen,
    Identifier { value: String },
    String { value: String },
    Error,
}
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub struct Lexer<'input> {
    /*   chars: CharIndices<'input>,*/
    pub s: &'input [u8],
    pub cursor: usize,
    pub marker: usize,
    pub limit: usize,
    pub tok: usize, // todo rename
}

// todo
// Extract out most of the code to rs file.
// parse true/false as lexer
// parse true/false as parser.
// Handle errror.
// Fix range in Some.
// Handle identifier.
impl<'input> Lexer<'input> {
    pub fn new(input: &'input [u8]) -> Self {
        // TODO: RE2Rust assumes strings are nul terminated.
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
pub enum LexicalError {
    // Not possible
}

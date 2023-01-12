#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    True,
    False,
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
            limit: input.len() - 1,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexicalError {
    // Not possible
}

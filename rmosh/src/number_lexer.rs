#[derive(Clone, Debug, PartialEq)]
pub enum Token {

}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerItem = Spanned<Token, usize, LexicalError>;

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
        Self {
            s: input,
            cursor: 0,
            marker: 0,
            tok: 0,
            limit: input.len() - 1,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NumberLexicalError {
    pub start: usize,
    pub end: usize,
    pub token: String,
}

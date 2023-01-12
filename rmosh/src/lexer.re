#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    True,
    False,
    Error,
}
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
use std::str::CharIndices;

#[derive(Clone, Debug)]
pub struct Lexer<'input> {
    chars: CharIndices<'input>,
    s: &'input [u8],
    cursor: usize,
    marker: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer { chars: input.char_indices() , s: input.as_bytes(), cursor: 0, marker:0}
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {

    /*!re2c
        re2c:define:YYCTYPE = u8;
        re2c:define:YYPEEK = "*self.s.get_unchecked(self.cursor)";
        re2c:define:YYSKIP = "self.cursor += 1;";
        re2c:define:YYBACKUP = "self.marker = self.cursor;";
        re2c:define:YYRESTORE = "self.cursor = self.marker;";
        re2c:yyfill:enable = 0;
        TRUE = "#"[tT] | "#true";
        FALSE = "#"[fF] | "#false";        
        TRUE { return  Some(Ok((0, Token::True, 1))); }
        FALSE { return  Some(Ok((0, Token::False, 1))) }        
        * { return  None; }

        */
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexicalError {
    // Not possible
}



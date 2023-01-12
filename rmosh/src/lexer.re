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
 /*   chars: CharIndices<'input>,*/
    s: &'input [u8],
    cursor: usize,
    marker: usize,
    limit: usize,
}

// todo
// parse true/false as lexer
// parse true/false as parser.
// Handle errror.
// Fix range in Some.
// Handle identifier.
impl<'input> Lexer<'input> {
    pub fn new(input: &'input [u8]) -> Self {
        // TODO: RE2Rust assumes strings are nul terminated.
        Self { s: input, cursor: 0, marker:0, limit:input.len() - 1}
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
        re2c:define:YYLESSTHAN = "self.cursor >= self.limit";
        re2c:yyfill:enable = 0;
        re2c:eof = 0;
        TRUE = "#"[tT] | "#true";
        FALSE = "#"[fF] | "#false";        
        TRUE { println!("****<true> cursor={}", self.cursor);return Some(Ok((0, Token::True, 2))); }
        FALSE { println!("****<false>");return Some(Ok((0, Token::False, 2))); }        
        $ { println!("$$$$");return  None; }        
        * { println!("else else");return  None; }
       */
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexicalError {
    // Not possible
}



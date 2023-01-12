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
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer { chars: input.char_indices() }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Ok((0, Token::True, 1)))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexicalError {
    // Not possible
}

fn lex(s: &[u8]) -> Token {
    let mut cursor = 0;
    let mut marker = 0;
    /*!re2c
        re2c:define:YYCTYPE = u8;
        re2c:define:YYPEEK = "*s.get_unchecked(cursor)";
        re2c:define:YYSKIP = "cursor += 1;";
        re2c:define:YYBACKUP = "marker = cursor;";
        re2c:define:YYRESTORE = "cursor = marker;";
        re2c:yyfill:enable = 0;
        TRUE = "#"[tT] | "#true";
        FALSE = "#"[fF] | "#false";        
        TRUE { return Token::True; }
        FALSE { return Token::False; }        
        * { return Token::Error; }
    */
}


/// Tests.
#[cfg(test)]
pub mod tests {
    use crate::lexer::{lex, Token};

    #[test]
    fn test_lex() {
		assert_eq!(Token::True, lex(b"#t"));
		assert_eq!(Token::True, lex(b"#true"));		
		assert_eq!(Token::False, lex(b"#f"));			        
	}
}
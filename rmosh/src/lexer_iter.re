use crate::lexer::{Lexer, Spanned, Token, LexicalError};
use std::str;
impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tok = self.cursor;
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
        LETTER                 = [A-Za-z]; // Conforms R7RS.        
        SPECIAL_INITIAL        = [!\$%&\*\/\:\<=\>\?\^\_~]; // Conforms R7RS.        
        INITIAL                = LETTER | SPECIAL_INITIAL; // Conforms R7RS.
        DIGIT                  = [0-9]; // Conforms R7RS.       
        EXPLICIT_SIGN          = "+" | "-"; // Conforms R7RS.
        SPECIAL_SUBSEQUENT     = EXPLICIT_SIGN | "." | "@"; // Conforms R7RS.        
        SUBSEQUENT             = INITIAL | DIGIT | SPECIAL_SUBSEQUENT; //  // Conforms R7RS.

        IDENTIFIER = (INITIAL (SUBSEQUENT)*);
        TRUE { println!("****<true> cursor={}", self.cursor);return Some(Ok((0, Token::True, 2))); }
        FALSE { println!("****<false>");return Some(Ok((0, Token::False, 2))); }        
        IDENTIFIER { println!("ident!!!!{:?}", str::from_utf8(&self.s[self.tok..self.cursor])); return Some(Ok((0, Token::Identifier{value: self.token()}, 2)));}
        $ { println!("$$$$");return  None; }        
        * { println!("else else");return  None; }
       */
    }
}



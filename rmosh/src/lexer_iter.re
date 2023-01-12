use crate::lexer::{Lexer, Spanned, Token, LexicalError};

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



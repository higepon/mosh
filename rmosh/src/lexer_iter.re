use crate::lexer::{Lexer, Spanned, Token, LexicalError};

/*!re2c
    re2c:define:YYCTYPE = usize; // We have Vec<char> and treat char as usize.
    re2c:define:YYPEEK = "*self.s.get_unchecked(self.cursor) as usize";
    re2c:define:YYSKIP = "self.cursor += 1;";
    re2c:define:YYBACKUP = "self.marker = self.cursor;";
    re2c:define:YYRESTORE = "self.cursor = self.marker;";
    re2c:define:YYLESSTHAN = "self.cursor >= self.limit";
    re2c:yyfill:enable = 0;
    re2c:eof = 0;

    // Conforms to R7RS.
    ANY_CHARACTER          = [^];
    TRUE                   = "#t" | "#true";
    FALSE                  = "#f" | "#false";
    LETTER                 = [A-Za-z];
    SPECIAL_INITIAL        = [!\$%&\*\/\:\<=\>\?\^\_~];
    INITIAL                = LETTER | SPECIAL_INITIAL;
    DIGIT                  = [0-9];
    EXPLICIT_SIGN          = "+" | "-";
    SPECIAL_SUBSEQUENT     = EXPLICIT_SIGN | "." | "@";
    SUBSEQUENT             = INITIAL | DIGIT | SPECIAL_SUBSEQUENT;
    VERTICAL_LINE          = "|";
    HEX_DIGIT              = DIGIT | [A-Fa-f];
    HEX_SCALAR_VALUE       = HEX_DIGIT +;
    INLINE_HEX_ESCAPE      = "\\x" HEX_SCALAR_VALUE ";";
    SIGN_SUBSEQUENT        = INITIAL | EXPLICIT_SIGN | "@";
    DOT                    = ".";
    VECTOR_START           = "#(";
    BYTEVECTOR_START       = "#u8(" | "#vu8(";
    DOT_SUBSEQUENT         = SIGN_SUBSEQUENT | DOT;
    // Per R7RS Small Errata, we allow \\\\ and \\\" here.
    MNEMONIC_ESCAPE        = ('\\' [abtnr\\\"]);
    PECULIAR_IDENTIFIER    = EXPLICIT_SIGN | EXPLICIT_SIGN SIGN_SUBSEQUENT SUBSEQUENT * | EXPLICIT_SIGN "." DOT_SUBSEQUENT SUBSEQUENT * | "." DOT_SUBSEQUENT SUBSEQUENT *;
    SYMBOL_ELEMENT         = [^\|\\] | "\\|" | INLINE_HEX_ESCAPE | MNEMONIC_ESCAPE;
    IDENTIFIER             = (INITIAL (SUBSEQUENT)*) | VERTICAL_LINE SYMBOL_ELEMENT * VERTICAL_LINE | PECULIAR_IDENTIFIER;
    LEFT_PAREN             = "(" | "[";
    RIGHT_PAREN            = ")" | "]";
    RETURN                 = "\r";
    NEWLINE                = "\n";
    INTRA_LINE_WHITE_SPACE = " " | "\t";
    LINE_ENDING            = NEWLINE | RETURN NEWLINE | RETURN RETURN;
    WHITE_SPACE            = INTRA_LINE_WHITE_SPACE | LINE_ENDING;
    DELIMITER              = WHITE_SPACE | VERTICAL_LINE | LEFT_PAREN | RIGHT_PAREN | '"' | ";" | "\x00";
    STRING_ELEMENT         = [^\"\\] | MNEMONIC_ESCAPE | '\\"' | '\\\\' | '\\' INTRA_LINE_WHITE_SPACE * LINE_ENDING INTRA_LINE_WHITE_SPACE * | INLINE_HEX_ESCAPE;
    STRING                 = '"' STRING_ELEMENT * '"';
    REGEXP_ELEMENT         = "\\\/" | [^/];
    REGEXP                 = '#/' REGEXP_ELEMENT * '/';
    DIGIT_10               = DIGIT;
    INF_NAN                = "+inf.0" | "-inf.0" | "+nan.0" | "-nan.0";
    EXACTNESS              = ("#"[ie])?;
    SIGN                   = [\+\-]?;
    EXPONENT_MARKER        = "e";
    SUFFIX                 = (EXPONENT_MARKER SIGN (DIGIT_10)+)?;
    UINTEGER_10            = DIGIT_10 +;
    DECIMAL_10             = (UINTEGER_10 SUFFIX) | ("." (DIGIT_10)+ SUFFIX) | ((DIGIT_10)+ "." (DIGIT_10)* SUFFIX);
    UREAL_10               = UINTEGER_10 | (UINTEGER_10 "/" UINTEGER_10) | DECIMAL_10;
    REAL_10                = (SIGN UREAL_10) | INF_NAN;
    RADIX_10               = "#d" ?;
    COMPLEX_10             = REAL_10 | (REAL_10 "@" REAL_10) | (REAL_10 [\+\-] UREAL_10 'i') | (REAL_10 [\+\-] INF_NAN 'i') | (REAL_10 [\+\-] 'i') | ([\+\-] UREAL_10 'i') | ([\+\-] INF_NAN 'i') | ([\+\-] 'i');
    PREFIX_10              = (RADIX_10 EXACTNESS) | (EXACTNESS RADIX_10);
    NUM_10                 = PREFIX_10 COMPLEX_10;
    EOS                    = "\X0000";
    COMMENT                = (";"[^\n\X0000]* (LINE_ENDING | EOS));
*/

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {       
        loop {
            let should_skip_comment;
            let mut comment_level = 1;             
            'lex: loop {
                self.tok = self.cursor;
                /*!re2c
                    LEFT_PAREN { return self.with_location(Token::LeftParen); }
                    RIGHT_PAREN { return self.with_location(Token::RightParen); }
                    TRUE  { return self.with_location(Token::True); }
                    FALSE { return self.with_location(Token::False); }
                    IDENTIFIER {
                        return self.with_location(Token::Identifier { value: self.extract_token() });
                    }
                    REGEXP {
                        return self.with_location(Token::Regexp { value: self.extract_regexp() });
                    }                
                    STRING {
                        return self.with_location(Token::String{value: self.extract_string()});
                    }
                    NUM_10 {
                        return self.with_location(Token::Number10{value: self.extract_token()});
                    }
                    DOT {
                        return self.with_location(Token::Dot);
                    }
                    BYTEVECTOR_START {
                        return self.with_location(Token::ByteVectorStart);
                    }
                    VECTOR_START {
                        return self.with_location(Token::VectorStart);
                    }
                    "#\\alarm" {
                        return self.with_location(Token::Character { value: char::from(7) });
                    }
                    "#\\backspace" {
                        return self.with_location(Token::Character { value: char::from(8) });
                    }
                    "#\\delete" {
                        return self.with_location(Token::Character { value: char::from(0x7f) });
                    }
                    "#\\escape" {
                        return self.with_location(Token::Character { value: char::from(0x1b) });
                    }
                    "#\\newline" {
                        return self.with_location(Token::Character { value: '\n' });
                    }
                    "#\\null" {
                        return self.with_location(Token::Character { value: '\0' });
                    }
                    "#\\return" {
                        return self.with_location(Token::Character { value: char::from(0x0d) });
                    }
                    "#\\space" {
                        return self.with_location(Token::Character { value: ' ' });
                    }
                    "#\\tab" {
                        return self.with_location(Token::Character { value: '\t' });
                    }
                    "#\\" ANY_CHARACTER {
                        return self.with_location(Token::Character{value: self.extract_character()});
                    }
                    "#\\x" HEX_SCALAR_VALUE {
                        return self.with_location(Token::Character{value: self.extract_hex_character()});
                    }
                    "'" {
                        return self.with_location(Token::AbbrevQuote);
                    }
                    "`" {
                        return self.with_location(Token::AbbrevQuasiquote);
                    }
                    "," {
                        return self.with_location(Token::AbbrevUnquote);
                    }
                    "@" {
                        return self.with_location(Token::AbbrevUnquoteSplicing);
                    }
                    "#'" {
                        return self.with_location(Token::AbbrevSyntax);
                    }
                    "#`" {
                        return self.with_location(Token::AbbrevQuasisyntax);
                    }
                    "#," {
                        return self.with_location(Token::AbbrevUnsyntax);
                    }
                    "#@" {
                        return self.with_location(Token::AbbrevUnsyntaxSplicing);
                    }
                    DELIMITER {
                        continue 'lex;
                    }
                    COMMENT {
                        continue 'lex;
                    }
                    "#|" {
                        should_skip_comment = true;
                        break 'lex;
                    }
                    $ { return None; }
                    * { return Some(Err(LexicalError {
                            start: self.tok,
                            end: self.cursor,
                            token: self.extract_token()
                        })); }
                */
            }
            if should_skip_comment {
                'skip_comment: loop {
                    /*!re2c
                    "|#" {
                        comment_level -= 1;
                        if comment_level == 0 {
                            break 'skip_comment;
                        }
                        continue 'skip_comment;   
                    }
                    "#|" {
                        comment_level += 1;
                        continue 'skip_comment;
                    }
                    "EOS" {
                        return None;
                    }
                    ANY_CHARACTER {
                        continue 'skip_comment;                   
                    }            
                    $ { return None; }            
                    */                    
                }
            }
        }
    }
}



use crate::lexer::{Lexer, Spanned, Token, LexicalError};

/*!re2c
    re2c:define:YYCTYPE = u8;
    re2c:define:YYPEEK = "*self.s.get_unchecked(self.cursor)";
    re2c:define:YYSKIP = "self.cursor += 1;";
    re2c:define:YYBACKUP = "self.marker = self.cursor;";
    re2c:define:YYRESTORE = "self.cursor = self.marker;";
    re2c:define:YYLESSTHAN = "self.cursor >= self.limit";
    re2c:yyfill:enable = 0;
    re2c:eof = 0;

    // Conforms R7RS.
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
    DOT_SUBSEQUENT         = SIGN_SUBSEQUENT | DOT;
    // Per R7RS Small Errata, we allow \\\\ and \\\" here.
    MNEMONIC_ESCAPE        = ('\\' [abtnr\\\"]);
    PECULIAR_IDENTIFIER    = EXPLICIT_SIGN | EXPLICIT_SIGN SIGN_SUBSEQUENT SUBSEQUENT * | EXPLICIT_SIGN "." DOT_SUBSEQUENT SUBSEQUENT * | "." DOT_SUBSEQUENT SUBSEQUENT *;
    SYMBOL_ELEMENT         = [^\|\\] | "\\|" | INLINE_HEX_ESCAPE | MNEMONIC_ESCAPE;
    IDENTIFIER             = (INITIAL (SUBSEQUENT)*) | VERTICAL_LINE SYMBOL_ELEMENT * VERTICAL_LINE | PECULIAR_IDENTIFIER;
    LEFT_PAREN             = "(";
    RIGHT_PAREN            = ")";
    RETURN                 = "\r";
    NEWLINE                = "\n";
    INTRA_LINE_WHITE_SPACE = " " | "\t";
    LINE_ENDING            = NEWLINE | RETURN NEWLINE | RETURN RETURN;
    WHITE_SPACE            = INTRA_LINE_WHITE_SPACE | LINE_ENDING;
    DELIMITER              = WHITE_SPACE | VERTICAL_LINE | LEFT_PAREN | RIGHT_PAREN | '"' | ";" | "\x00";
    STRING_ELEMENT         = [^\"\\] | MNEMONIC_ESCAPE | '\\"' | '\\\\' | '\\' INTRA_LINE_WHITE_SPACE * LINE_ENDING INTRA_LINE_WHITE_SPACE * | INLINE_HEX_ESCAPE;
    STRING                 = '"' STRING_ELEMENT * '"';
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
*/

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        'lex: loop {
            self.tok = self.cursor;
            /*!re2c
                LEFT_PAREN { return Some(Ok((0, Token::LeftParen, 2))); }
                RIGHT_PAREN { return Some(Ok((0, Token::RightParen, 2))); }
                TRUE  { return Some(Ok((0, Token::True, 2))); }
                FALSE { return Some(Ok((0, Token::False, 2))); }
                IDENTIFIER {
                    return Some(Ok((0, Token::Identifier{value: self.extract_token()}, 2)));
                }
                STRING {
                    return Some(Ok((0, Token::String{value: self.extract_string()}, 2)));
                }
                NUM_10 {
                    return Some(Ok((0, Token::Number10{value: self.extract_token()}, 2)));
                }
                DOT {
                    return Some(Ok((0, Token::Dot, 2)));
                }
                VECTOR_START {
                    return Some(Ok((0, Token::VectorStart, 2)));
                }
                "#\\alarm" {
                    return Some(Ok((0, Token::Character { value: char::from(7) }, 2)));
                }
                "#\\backspace" {
                    return Some(Ok((0, Token::Character { value: char::from(8) }, 2)));
                }
                "#\\delete" {
                    return Some(Ok((0, Token::Character { value: char::from(0x7f) }, 2)));
                }
                "#\\escape" {
                    return Some(Ok((0, Token::Character { value: char::from(0x1b) }, 2)));
                }
                "#\\newline" {
                    return Some(Ok((0, Token::Character { value: '\n' }, 2)));
                }
                "#\\null" {
                    return Some(Ok((0, Token::Character { value: '\0' }, 2)));
                }
                "#\\return" {
                    return Some(Ok((0, Token::Character { value: char::from(0x0d) }, 2)));
                }
                "#\\space" {
                    return Some(Ok((0, Token::Character { value: ' ' }, 2)));
                }
                "#\\tab" {
                    return Some(Ok((0, Token::Character { value: '\t' }, 2)));
                }
                "#\\" ANY_CHARACTER {
                    return Some(Ok((0, Token::Character{value: self.extract_character()}, 2)));
                }
                DELIMITER {
                    continue 'lex;
                }
                $ { return None; }
                * { return  None; }
            */
        }
    }
}



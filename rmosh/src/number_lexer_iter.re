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
    CHARACTER_TABULATION   = "\X0009"; // Not in R7RS
    LINE_TABULATION        = "\X000B"; // Not in R7RS
    FORM_FEED              = "\X000C"; // Not in R7RS.
    INTRA_LINE_WHITE_SPACE = " " | "\t";
    LINE_ENDING            = NEWLINE | RETURN NEWLINE | RETURN RETURN;
    WHITE_SPACE            = INTRA_LINE_WHITE_SPACE | LINE_ENDING | CHARACTER_TABULATION | LINE_TABULATION | FORM_FEED;
    DELIMITER              = WHITE_SPACE | VERTICAL_LINE | LEFT_PAREN | RIGHT_PAREN | '"' | ";" | "\x00";
    STRING_ELEMENT         = [^\"\\] | MNEMONIC_ESCAPE | '\\"' | '\\\\' | '\\' INTRA_LINE_WHITE_SPACE * LINE_ENDING INTRA_LINE_WHITE_SPACE * | INLINE_HEX_ESCAPE;
    STRING                 = '"' STRING_ELEMENT * '"';
    REGEXP_ELEMENT         = "\\\/" | [^/];
    REGEXP                 = '#/' REGEXP_ELEMENT * '/';
    DIGIT_10               = DIGIT;
    DIGIT_16               = HEX_DIGIT;
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
    UINTEGER_16            = DIGIT_16 +;
    UREAL_16               = UINTEGER_16 | (UINTEGER_16 "/" UINTEGER_16);
    REAL_16                = (SIGN UREAL_16) | INF_NAN;
    RADIX_16               = "#x" ?;
    COMPLEX_16             = REAL_16 | (REAL_16 "@" REAL_16) | (REAL_16 [\+\-] UREAL_16 'i') | (REAL_16 [\+\-] INF_NAN 'i') | (REAL_16 [\+\-] 'i') | ([\+\-] UREAL_16 'i') | ([\+\-] INF_NAN 'i') | ([\+\-] 'i');
    PREFIX_16              = (RADIX_16 EXACTNESS) | (EXACTNESS RADIX_16);
    NUM_16                 = PREFIX_16 COMPLEX_16;    
    EOS                    = "\X0000";
    DIRECTIVE              = "#!fold-case" | "#!no-fold-case" | "#!r6rs";
    DATUM_COMMENT          = "#;";
    COMMENT                = (";"[^\n\X0000]* (LINE_ENDING | EOS));
*/

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {       
        loop {
            'lex: loop {
                self.tok = self.cursor;
                /*!re2c
                    NUM_10 {
                        todo!();
                    }
                    $ { return None; }
                    * { return Some(Err(NumberLexicalError {
                            start: self.tok,
                            end: self.cursor,
                            token: self.extract_token()
                        })); }
                */
            }
        }
    }
}



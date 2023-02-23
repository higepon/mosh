use crate::lexer::Spanned;
use crate::number_lexer::NumberLexer;
use crate::lexer::Token;
use crate::reader_util::ReadError;

/*!re2c
    re2c:define:YYCTYPE = usize; // We have Vec<char> and treat char as usize.
    re2c:define:YYPEEK = "*self.s.get_unchecked(self.cursor) as usize";
    re2c:define:YYSKIP = "self.cursor += 1;";
    re2c:define:YYBACKUP = "self.marker = self.cursor;";
    re2c:define:YYRESTORE = "self.cursor = self.marker;";
    re2c:define:YYLESSTHAN = "self.cursor >= self.limit";
    re2c:define:YYGETCONDITION = "unsafe {COND}";
    re2c:define:YYSETCONDITION = "unsafe {COND = @@;}";    
    re2c:yyfill:enable = 0;
    re2c:eof = 0;

    // Conforms to R7RS.
    DIGIT                  = [0-9];
    HEX_DIGIT              = DIGIT | [A-Fa-f];
    OCT_DIGIT              = [0-7];
    DIGIT_10               = DIGIT;
    DIGIT_8                = OCT_DIGIT;
    DIGIT_16               = HEX_DIGIT;
    INF_NAN                = "+inf.0" | "-inf.0" | "+nan.0" | "-nan.0";
    EXACTNESS              = ("#"[ie])?;
    SIGN                   = [\+\-]?;
    EXPONENT_MARKER        = [eEsSfFdDlL] [\+\-]? DIGIT_10+;
    SUFFIX                 = (EXPONENT_MARKER SIGN (DIGIT_10)+)?;
    UINTEGER_10            = DIGIT_10 +;
    DECIMAL_10             = (UINTEGER_10 SUFFIX) | ("." (DIGIT_10)+ SUFFIX) | ((DIGIT_10)+ "." (DIGIT_10)* SUFFIX);
    UREAL_10               = UINTEGER_10 | (UINTEGER_10 "/" UINTEGER_10) | DECIMAL_10;
    REAL_10                = (SIGN UREAL_10) | INF_NAN;
    RADIX_10               = '#d' ?;
    COMPLEX_10             = REAL_10 | (REAL_10 "@" REAL_10) | (REAL_10 [\+\-] UREAL_10 'i') | (REAL_10 [\+\-] INF_NAN 'i') | (REAL_10 [\+\-] 'i') | ([\+\-] UREAL_10 'i') | ([\+\-] INF_NAN 'i') | ([\+\-] 'i');
    PREFIX_10              = (RADIX_10 EXACTNESS) | (EXACTNESS RADIX_10);
    NUM_10                 = PREFIX_10 COMPLEX_10;
    UINTEGER_8             = DIGIT_8 +;
    UREAL_8                = UINTEGER_8 | (UINTEGER_8 "/" UINTEGER_8);
    REAL_8                 = (SIGN UREAL_8) | INF_NAN;
    RADIX_8                = '#o' ?;
    COMPLEX_8              = REAL_8 | (REAL_8 "@" REAL_8) | (REAL_8 [\+\-] UREAL_8 'i') | (REAL_8 [\+\-] INF_NAN 'i') | (REAL_8 [\+\-] 'i') | ([\+\-] UREAL_8 'i') | ([\+\-] INF_NAN 'i') | ([\+\-] 'i');
    PREFIX_8               = (RADIX_8 EXACTNESS) | (EXACTNESS RADIX_8);
    NUM_8                  = PREFIX_8 COMPLEX_8;
    UINTEGER_16            = DIGIT_16 +;
    UREAL_16               = UINTEGER_16 | (UINTEGER_16 "/" UINTEGER_16);
    REAL_16                = (SIGN UREAL_16) | INF_NAN;
    RADIX_16               = '#x' ?;
    COMPLEX_16             = REAL_16 | (REAL_16 "@" REAL_16) | (REAL_16 [\+\-] UREAL_16 'i') | (REAL_16 [\+\-] INF_NAN 'i') | (REAL_16 [\+\-] 'i') | ([\+\-] UREAL_16 'i') | ([\+\-] INF_NAN 'i') | ([\+\-] 'i');
    PREFIX_16              = (RADIX_16 EXACTNESS) | (EXACTNESS RADIX_16);
    NUM_16                 = PREFIX_16 COMPLEX_16;
    EOS                    = "\X0000";
*/

/*!conditions:re2c*/
static mut COND: usize = YYC_INIT;

impl<'input> Iterator for NumberLexer<'input> {
    type Item = Spanned<Token, usize, ReadError>;

    fn next(&mut self) -> Option<Self::Item> {
      
        loop {
            loop {
                self.tok = self.cursor;
                /*!re2c
                    <INIT> EXPONENT_MARKER {
                        let token = self.extract_token();
                        let mut marker = String::new();
                        marker.push_str("e");
                        marker.push_str(&token[1..token.len()]);
                        return self.with_location(Token::Exponent { value: marker });
                    }
                    <INIT, HEX> "+inf.0" { return self.with_location(Token::PlusInf); }
                    <INIT, HEX> "-inf.0" { return self.with_location(Token::MinusInf); }
                    <INIT, HEX> "+nan.0" { return self.with_location(Token::PlusNan); }
                    <INIT, HEX> "-nan.0" { return self.with_location(Token::MinusNan); }
                    <INIT, HEX> "/" { return self.with_location(Token::Slash); }
                    <INIT, HEX> "+" { return self.with_location(Token::Plus); }
                    <INIT, HEX> "-" { return self.with_location(Token::Minus); }
                    <INIT, HEX> "." { return self.with_location(Token::Dot); }
                    <INIT, HEX> 'i' { return self.with_location(Token::Imag); }
                    <INIT, HEX> '#d' { return self.with_location(Token::Radix10); }
                    <INIT, HEX> '#o' => OCT { return self.with_location(Token::Radix8); }
                    <INIT> '#x'=> HEX { return self.with_location(Token::Radix16); }
                    <INIT, HEX> '#i' { return self.with_location(Token::Inexact); }
                    <INIT, HEX> "#e" { return self.with_location(Token::Exact); }                                        
                    <INIT> DIGIT {
                        return self.with_location(Token::Digit { value: self.extract_token() });
                    }
                    <INIT, OCT> OCT_DIGIT {
                        return self.with_location(Token::OctDigit { value: self.extract_token() });
                    }                    
                    <INIT, HEX> HEX_DIGIT {
                        return self.with_location(Token::HexDigit { value: self.extract_token() });
                    }
                    <*> "EOS" => INIT { return None; }
                    <*> $ => INIT { return None; }
                    <*> * => INIT { return Some(Err(ReadError::InvalidToken {
                            start: self.tok,
                            end: self.cursor,
                            token: self.extract_token()
                        })); }
                */
            }
        }
    }
}



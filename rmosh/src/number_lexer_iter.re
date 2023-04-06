#![allow(clippy::all)]
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
    BIN_DIGIT              = [01];
    HEX_DIGIT              = DIGIT | [A-Fa-f];
    OCT_DIGIT              = [0-7];
    DIGIT_10               = DIGIT;
    DIGIT_8                = OCT_DIGIT;
    DIGIT_16               = HEX_DIGIT;
    EXPONENT_MARKER        = [eEsSfFdDlL] [\+\-]? DIGIT_10+;
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
                    <*> '+inf.0' => INIT { return self.with_location(Token::PlusInf); }
                    <*> '-inf.0' => INIT { return self.with_location(Token::MinusInf); }
                    <*> '+nan.0' => INIT { return self.with_location(Token::PlusNan); }
                    <*> '-nan.0' => INIT { return self.with_location(Token::MinusNan); }
                    <*> "/" { return self.with_location(Token::Slash); }
                    <*> "+" { return self.with_location(Token::Plus); }
                    <*> "-" { return self.with_location(Token::Minus); }
                    <*> "." { return self.with_location(Token::Dot); }
                    <*> 'i' { return self.with_location(Token::Imag); }
                    <*> "@" { return self.with_location(Token::At); }
                    <*> '#b' => BIN { return self.with_location(Token::Radix2); }
                    <*> '#d' { return self.with_location(Token::Radix10); }
                    <*> '#o' => OCT { return self.with_location(Token::Radix8); }
                    <*> '#x'=> HEX { return self.with_location(Token::Radix16); }
                    <*> '#i' { return self.with_location(Token::Inexact); }
                    <*> "#e" { return self.with_location(Token::Exact); }
                    <BIN> BIN_DIGIT {
                        return self.with_location(Token::BinDigit { value: self.extract_token() });
                    }
                    <*> DIGIT {
                        return self.with_location(Token::Digit { value: self.extract_token() });
                    }
                    <OCT> OCT_DIGIT {
                        return self.with_location(Token::OctDigit { value: self.extract_token() });
                    }
                    <HEX> HEX_DIGIT {
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



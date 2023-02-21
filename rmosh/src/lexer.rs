use crate::ports::ReadError2;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    AbbrevQuasiquote,
    AbbrevQuote,
    AbbrevUnquote,
    AbbrevUnquoteSplicing,
    AbbrevQuasisyntax,
    AbbrevSyntax,
    AbbrevUnsyntax,
    AbbrevUnsyntaxSplicing,
    Character { value: char },
    DatumComment,
    Dot,
    False,
    Identifier { value: String },
    LeftParen,
    Number10 { value: String },
    Number16 { value: String },
    RightParen,
    Regexp { value: String },
    String { value: String },
    True,
    ByteVectorStart,
    VectorStart,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerItem = Spanned<Token, usize, ReadError2>;

#[derive(Clone, Debug)]
pub struct Lexer<'input> {
    pub s: &'input [char],
    pub cursor: usize,
    pub marker: usize,
    pub limit: usize,
    pub tok: usize,
}

// TODO:
// - Fix range in Some.
impl<'input> Lexer<'input> {
    pub fn new(input: &'input [char]) -> Self {
        Self {
            s: input,
            cursor: 0,
            marker: 0,
            tok: 0,
            limit: input.len() - 1,
        }
    }

    // todo pub
    pub fn with_location(&self, token: Token) -> Option<LexerItem> {
        Some(Ok((self.tok, token, self.cursor)))
    }

    pub fn extract_token(&self) -> String {
        self.s[self.tok..self.cursor].iter().collect()
    }

    pub fn extract_character(&self) -> char {
        // Actual character is at index = 2 #\a.
        self.s[self.tok + 2]
    }

    pub fn extract_hex_character(&self) -> char {
        // #\xAB
        let hex_str: String = self.s[self.tok + 3..self.cursor].iter().collect();
        match u32::from_str_radix(&hex_str, 16) {
            Ok(n) => match char::from_u32(n) {
                Some(c) => c,
                None => {
                    panic!("malformed hex scalar value character")
                }
            },
            Err(e) => {
                panic!("malformed hex scalar value character: {} in {}", e, hex_str)
            }
        }
    }

    pub fn extract_string(&self) -> String {
        // Remove double quotes.
        let s: String = self.s[self.tok + 1..self.cursor - 1].iter().collect();
        let chars: Vec<char> = s.chars().collect();
        let mut ret = String::new();
        let mut i: usize = 0;
        loop {
            let ch = match chars.get(i) {
                Some(c) => c,
                None => break,
            };
            i += 1;
            if *ch == '\\' {
                let ch2 = match chars.get(i) {
                    Some(c) => c,
                    None => break,
                };
                i += 1;
                if *ch2 == '"' {
                    ret.push('"');
                } else if *ch2 == '\\' {
                    ret.push('\\');
                } else if *ch2 == 'a' {
                    ret.push(7 as char);
                } else if *ch2 == 'b' {
                    ret.push(8 as char);
                } else if *ch2 == 't' {
                    ret.push(9 as char);
                } else if *ch2 == 'n' {
                    ret.push(0xa as char);
                } else if *ch2 == 'v' {
                    ret.push(0xb as char);
                } else if *ch2 == 'f' {
                    ret.push(0xc as char);
                } else if *ch2 == 'r' {
                    ret.push(0xd as char);
                } else if *ch2 == 't' {
                    ret.push(9 as char);
                } else if *ch2 == 't' {
                    ret.push(9 as char);
                } else if *ch2 == 't' {
                    ret.push(9 as char);
                } else if *ch2 == 'x' {
                    let mut current_ch = 0 as char;
                    loop {
                        let hex_ch = match chars.get(i) {
                            Some(c) => c,
                            None => {
                                eprintln!("invalid \\x in string end");
                                break;
                            }
                        };
                        i += 1;
                        if *hex_ch == ';' {
                            ret.push(current_ch);
                            break;
                        } else if hex_ch.is_digit(10) {
                            let lhs = (current_ch as u32) << 4;
                            let rhs = (*hex_ch as u32) - ('0' as u32);
                            current_ch = char::from_u32(lhs | rhs).unwrap_or('*');
                        } else if 'a' <= *hex_ch && *hex_ch <= 'f' {
                            let lhs = (current_ch as u32) << 4;
                            let rhs = (*hex_ch as u32) - ('a' as u32) + 10;
                            current_ch = char::from_u32(lhs | rhs).unwrap_or('*');
                        } else if 'A' <= *hex_ch && *hex_ch <= 'F' {
                            let lhs = (current_ch as u32) << 4;
                            let rhs = (*hex_ch as u32) - ('A' as u32) + 10;
                            current_ch = char::from_u32(lhs | rhs).unwrap_or('*');
                        } else {
                            eprintln!("invalid \\x instring {}", hex_ch);
                        }
                    }
                } else {
                    // <intraline whitespace>*<line ending>
                    // <intraline whitespace>*
                    // NB: Lexical syntax has already checked by the scanner.
                    loop {
                        let ch3 = match chars.get(i) {
                            Some(c) => c,
                            None => break,
                        };
                        // <line ending>
                        if *ch3 == '\r' ||
                           *ch3 == '\n' ||
                           *ch3 == '\t' || // <intraline whitespace>
                           (*ch3 as u32) ==  0x0085 || // next line
                           (*ch3 as u32) ==  0x2028 || // line separator
                           (*ch3 as u32) ==  0x0020 || // <Unicode Zs>
                           (*ch3 as u32) ==  0x00a0 ||
                           (*ch3 as u32) ==  0x1680 ||
                           (*ch3 as u32) ==  0x180e ||
                           (*ch3 as u32) ==  0x202f ||
                           (*ch3 as u32) ==  0x205f ||
                           (*ch3 as u32) ==  0x3000 ||
                           ((0x2000 <= (*ch3 as u32)) && (((*ch3 as u32)) <= 0x200a))
                        {
                            i += 1;
                            continue;
                        } else {
                            i -= 1;
                            break;
                        }
                    }
                }
            } else {
                ret.push(*ch)
            }
        }
        ret
    }

    pub fn extract_regexp(&self) -> String {
        // Remove #/ and /
        self.s[self.tok + 2..self.cursor - 1].iter().collect()
    }
}

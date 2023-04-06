#[allow(clippy)]
use crate::error;
use crate::gc::Gc;
use crate::lexer;
use crate::numbers::Bignum;
use crate::objects::Object;
use lalrpop_util::ParseError;
use num_bigint::BigInt;
use num_bigint::ParseBigIntError;
use num_traits::Num;
use std::cmp::max;
use std::str::FromStr;

use crate::{lexer::Token, number_lexer::NumberLexer, number_reader::NumberParser};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadError {
    ContentNotFound {
        description: String,
    },
    InvalidToken {
        start: usize,
        end: usize,
        token: String,
    },
    MalformedBytevector {
        start: usize,
        end: usize,
        token: String,
    },
    NumberParseError {
        token: String,
        description: String,
    },
    SymbolParseError {
        token: String,
        description: String,
    },
    DivisionByZero {
        token: String,
        description: String,
    },
    LalrpopInvalidToken {
        location: usize,
    },
    UnrecognizedEOF {
        location: usize,
    },
    UnrecognizedToken {
        token: Token,
        expected: Vec<String>,
        context: String,
    },
    ExtraToken {
        token: Token,
    },
}

pub fn count_lineno(content: &str, position: usize) -> usize {
    let chars: Vec<char> = content.chars().collect();
    let mut lineno: usize = 1;
    for i in 0..chars.len() {
        if i == position {
            break;
        }
        let ch = chars[i];
        if ch == '\n' {
            lineno += 1;
        }
    }
    lineno
}

pub fn read_uinteger2(gc: &mut Box<Gc>, s: &str) -> Result<Object, ParseBigIntError> {
    match isize::from_str_radix(s, 2) {
        Ok(fx) => Ok(Object::Fixnum(fx)),
        Err(_) => match BigInt::from_str_radix(s, 2) {
            Ok(b) => Ok(Object::Bignum(gc.alloc(Bignum::new(b)))),
            Err(e) => Err(e),
        },
    }
}

pub fn read_uinteger8(gc: &mut Box<Gc>, s: &str) -> Result<Object, ParseBigIntError> {
    match isize::from_str_radix(s, 8) {
        Ok(fx) => Ok(Object::Fixnum(fx)),
        Err(_) => match BigInt::from_str_radix(s, 8) {
            Ok(b) => Ok(Object::Bignum(gc.alloc(Bignum::new(b)))),
            Err(e) => Err(e),
        },
    }
}

pub fn read_uinteger10(gc: &mut Box<Gc>, s: &str) -> Result<Object, ParseBigIntError> {
    match isize::from_str(s) {
        Ok(fx) => Ok(Object::Fixnum(fx)),
        Err(_) => match BigInt::from_str(s) {
            Ok(b) => Ok(Object::Bignum(gc.alloc(Bignum::new(b)))),
            Err(e) => Err(e),
        },
    }
}

pub fn read_uinteger16(gc: &mut Box<Gc>, s: &str) -> Result<Object, ParseBigIntError> {
    match isize::from_str_radix(s, 16) {
        Ok(fx) => Ok(Object::Fixnum(fx)),
        Err(_) => match BigInt::from_str_radix(s, 16) {
            Ok(b) => Ok(Object::Bignum(gc.alloc(Bignum::new(b)))),
            Err(e) => Err(e),
        },
    }
}

pub fn read_number(
    gc: &mut Box<Gc>,
    s: &str,
) -> Result<Object, ParseError<usize, lexer::Token, ReadError>> {
    let mut chars: Vec<char> = s.chars().collect();
    chars.push('\0');
    let mut is_inexact_context = false;
    let parsed = NumberParser::new().parse(gc, &mut is_inexact_context, NumberLexer::new(&chars));
    // To distinguish UnrecognizedToken thrown by reader.larlpop and number_reader.larlpop.
    // We catch UnrecognizedToken thrown by number_reader here.
    match parsed {
        // Convert this error here, so that we can refer content of s here in context.
        Err(ParseError::UnrecognizedToken { token, expected }) => {
            let context_start = max(0, (token.0 as isize) - 10) as usize;
            // Show what is causing this error.
            let context = format!("number_reader: {}", &s[context_start..token.2]);
            Err(ParseError::User {
                error: ReadError::UnrecognizedToken {
                    token: token.1,
                    expected,
                    context,
                },
            })
        }
        _ => parsed,
    }
}

pub fn read_string(s: &str) -> String {
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
                    } else if hex_ch.is_ascii_digit() {
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
                i -= 1;
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

pub fn read_symbol(s: &str) -> error::Result<String> {
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
            if *ch2 == 'x' {
                let mut current_ch = 0 as char;
                loop {
                    let hex_ch = match chars.get(i) {
                        Some(c) => c,
                        None => {
                            eprintln!("invalid \\x in symbol end");
                            break;
                        }
                    };
                    i += 1;
                    if *hex_ch == ';' {
                        ret.push(current_ch);
                        break;
                    } else if hex_ch.is_ascii_digit() {
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
                        eprintln!("invalid \\x in symbol {}", hex_ch);
                    }
                }
            } else if *ch2 == '"' {
                ret.push('"');
            } else {
                ret.push(*ch);
                ret.push(*ch2);
            }
        } else {
            ret.push(*ch);
        }
    }
    let is_bar_symbol = ret.len() > 2
        && match (ret.chars().next(), ret.chars().last()) {
            (Some('|'), Some('|')) => true,
            _ => false,
        };
    if is_bar_symbol {
        let raw_symbol = &ret[1..ret.len() - 1];
        if has_only_alphabets(raw_symbol) {
            Ok(raw_symbol.to_string())
        } else {
            Ok(ret)
        }
    } else {
        Ok(ret)
    }
}

fn has_only_alphabets(s: &str) -> bool {
    for c in s.chars() {
        if !c.is_alphabetic() {
            return false;
        }
    }
    true
}

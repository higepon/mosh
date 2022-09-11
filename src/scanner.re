/*
 * scanner.re
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: Reader.y 183 2008-07-04 06:19:28Z higepon $
 */
#include <stdio.h>
#include "Object.h"
#include "Pair.h"
#include "StringProcedures.h"
#include "TextualInputPort.h"
#include "TextualOutputPort.h"
#include "EqHashTable.h"
#include "ucs4string.h"
#include "ScannerHelper.h"
#include "Scanner.h"
#include "OSCompatThread.h"
#include "VM.h"
#include "MultiVMProcedures.h"

#define YYCTYPE ucs4char
#define YYCURSOR cursor_
#define YYMARKER marker_
#define YYLIMIT limit_
#define YYTOKEN token_
//#define YYDEBUG(state, ch)  yydebug(state, ch)
#define YYFILL(n) fill(n)

using namespace scheme;

Scanner::Scanner() : eofP_(false),
                     buffer_(nullptr),
                     cursor_(buffer_),
                     token_(buffer_),
                     limit_(buffer_),
                     marker_(buffer_),
                     bufferSize_(32)
{
}

Scanner::~Scanner()
{
}

// Since re2c greedy look-aheads,
// we need to give back read buffer to input-port.
void Scanner::emptyBuffer()
{
    TextualInputPort* const inputPort = currentVM()->readerContext()->port();

    for (ucs4char* p = limit_ - 1; p >= cursor_; p--) {
        // special case
        if (eofP_ && p == limit_ - 1 && *p == '\0') {
            continue;
        } else {
            inputPort->unGetChar(*p);
        }
    }

    // even if port reaches EOF, we turns off this flag.
    // fill() will detect EOF.
    eofP_ = false;
    cursor_ = buffer_;
    limit_ = buffer_;
    token_ = buffer_;
    marker_ = buffer_;
}

void Scanner::fill(int n)
{
    if (eofP_) return;
    TextualInputPort* const inputPort = currentVM()->readerContext()->port();
    const int restCharCount = limit_ - token_;
    const int tokenOffset = token_ - buffer_;

    if (buffer_ == nullptr) {
        buffer_ = new(GC) ucs4char[bufferSize_];
        cursor_ = buffer_;
        limit_ = buffer_;
        token_ = buffer_;
        marker_ = buffer_;
    }

    if ((restCharCount + n) > bufferSize_) {
        ucs4char* newBuffer = new(GC) ucs4char[restCharCount + n + 1];
        bufferSize_ = restCharCount + n + 1;
        memmove(newBuffer, token_, restCharCount * sizeof(ucs4char));
        cursor_ = &newBuffer[cursor_ - buffer_];
        token_ = &newBuffer[token_ - buffer_];
        limit_ = &newBuffer[limit_ - buffer_];
        marker_ = &newBuffer[marker_ - buffer_];
        buffer_ = newBuffer;
    } else if (restCharCount > 0) {
        memmove(buffer_, token_, restCharCount * sizeof(ucs4char));
    }

    int i;
    for (i = 0; i < n; i++) {
        const ucs4char ch = inputPort->getChar();
        if (ch == EOF) {
            eofP_ = true;
            buffer_[i + restCharCount] = '\0';
            i++;
            break;
        } else {
            buffer_[i + restCharCount] = ch;
        }
//         if (!inputPort->isDataReady()) {
//             i++;
//
//             break;
//         }

    }
    const int readSize = i;
    cursor_ = cursor_ - tokenOffset;
    token_ = buffer_;
    marker_ = marker_ - tokenOffset;
    limit_ = limit_ - tokenOffset + readSize;
}

int yylex(YYSTYPE* yylval)
{
    return currentVM()->readerContext()->port()->scanner()->scan(yylval);
}

int Scanner::scan(YYSTYPE* yylval)
{
/*!re2c
  EOS                    = "\X0000";
  LINE_FEED              = "\n";
  CHARACTER_TABULATION   = "\X0009";
  LINE_TABULATION        = "\X000B";
  LINE_SEPARATOR         = "\X2028";
  FORM_FEED              = "\X000C";
  CARRIGE_RETURN         = "\r";
  NEXT_LINE              = "\X0085";
  UNICODE_ZL_ZP          = [\X2028-\X2029];
  UNICODE_ZS             = "\X0020" | "\X00A0" | "\X1680" | "\X180E" | [\X2000-\X200A] | "\X202F" | "\X205F" | "\X3000";
  LINE_ENDING            = LINE_FEED | CARRIGE_RETURN | (CARRIGE_RETURN LINE_FEED) | NEXT_LINE | (CARRIGE_RETURN NEXT_LINE) | LINE_SEPARATOR;
  WHITE_SPACE            = CHARACTER_TABULATION | LINE_FEED | LINE_TABULATION | FORM_FEED | CARRIGE_RETURN | NEXT_LINE | UNICODE_ZL_ZP| UNICODE_ZS;
  DELMITER               = [\(\)\[\]\";#]|EOS|WHITE_SPACE;
  ANY_CHARACTER          = [^];
  DIGIT                  = [0-9];
  HEX_DIGIT              = DIGIT | [a-f] | [A-F];
  HEX_SCALAR_VALUE       = HEX_DIGIT +;
  INTRA_LINE_WHITE_SPACE = "\t" | UNICODE_ZS;
  INLINE_HEX_ESCAPE      = "\\x" HEX_SCALAR_VALUE ";";
  /* We use "INTRA_LINE_WHITE_SPACE *" instead of "INTRA_LINE_WHITE_SPACE" defined in R6RS */
  STRING_ELEMENT         = [^\"\\] | ('\\' [abtnvfr\"\\\|]) | ("\\" INTRA_LINE_WHITE_SPACE * LINE_ENDING INTRA_LINE_WHITE_SPACE *) | INLINE_HEX_ESCAPE;
  REGEXP_ELEMENT         = "\\\/" | [^/];
  DIGIT_2                = [0-1];
  DIGIT_8                = [0-7];
  DIGIT_10               = DIGIT;
  DIGIT_16               = HEX_DIGIT;
  UINTEGER_2             = DIGIT_2 +;
  UINTEGER_8             = DIGIT_8 +;
  UINTEGER_10            = DIGIT_10 +;
  UINTEGER_16            = DIGIT_16 +;
  NAN_INF                = "nan.0" | "inf.0";
  SIGN                   = [\+\-]?;
  EXPONENT_MARKER        = [eEsSfFdDlL];
  MANTISSA_WIDTH         = ("|" (DIGIT_10)+)?;
  SUFFIX                 = (EXPONENT_MARKER SIGN (DIGIT_10)+)?;
  DECIMAL_10             = (UINTEGER_10 SUFFIX) | ((DIGIT_10)+ "." (DIGIT_10)* SUFFIX) | ("." (DIGIT_10)+ SUFFIX) | ((DIGIT_10)+ "." SUFFIX);
  UREAL_2                = UINTEGER_2 | (UINTEGER_2 "/" UINTEGER_2);
  UREAL_8                = UINTEGER_8 | (UINTEGER_8 "/" UINTEGER_8);
  UREAL_10               = UINTEGER_10 | (UINTEGER_10 "/" UINTEGER_10) | DECIMAL_10 MANTISSA_WIDTH;
  UREAL_16               = UINTEGER_16 | (UINTEGER_16 "/" UINTEGER_16) | DECIMAL_10;
  REAL_10                = (SIGN UREAL_10) | ([\+\-] NAN_INF);
  REAL_16                = (SIGN UREAL_16) | ([\+\-] NAN_INF);
  REAL_2                 = (SIGN UREAL_2) | ([\+\-] NAN_INF);
  REAL_8                 = (SIGN UREAL_8) | ([\+\-] NAN_INF);
  COMPLEX_2             = REAL_2 | (REAL_2 "@" REAL_2) | (REAL_2 [\+\-] UREAL_2 "i") | (REAL_2 [\+\-] NAN_INF "i") | (REAL_2 [\+\-] "i") | ([\+\-] UREAL_2 "i") | ([\+\-] NAN_INF "i") | ([\+\-] "i");
  COMPLEX_8             = REAL_8 | (REAL_8 "@" REAL_8) | (REAL_8 [\+\-] UREAL_8 "i") | (REAL_8 [\+\-] NAN_INF "i") | (REAL_8 [\+\-] "i") | ([\+\-] UREAL_8 "i") | ([\+\-] NAN_INF "i") | ([\+\-] "i");
  COMPLEX_10             = REAL_10 | (REAL_10 "@" REAL_10) | (REAL_10 [\+\-] UREAL_10 "i") | (REAL_10 [\+\-] NAN_INF "i") | (REAL_10 [\+\-] "i") | ([\+\-] UREAL_10 "i") | ([\+\-] NAN_INF "i") | ([\+\-] "i");
  COMPLEX_16             = REAL_16 | (REAL_16 "@" REAL_16) | (REAL_16 [\+\-] UREAL_16 "i") | (REAL_16 [\+\-] NAN_INF "i") | (REAL_16 [\+\-] "i") | ([\+\-] UREAL_16 "i") | ([\+\-] NAN_INF "i") | ([\+\-] "i");
  RADIX_2                = ("#"[bB]);
  RADIX_8                = "#"[oO];
  RADIX_10               = ("#"[dD])?;
  RADIX_16               = "#"[xX];
  EXACTNESS              = ("#"[iIeE])?;
  EXACT                  = "#"[eE];
  INEXACT                = "#"[iI];
  PREFIX_2               = (RADIX_2 EXACTNESS) | (EXACTNESS RADIX_2);
  PREFIX_8               = (RADIX_8 EXACTNESS) | (EXACTNESS RADIX_8);
  PREFIX_10              = (RADIX_10 EXACTNESS) | (EXACTNESS RADIX_10);
  PREFIX_16              = (RADIX_16 EXACTNESS) | (EXACTNESS RADIX_16);
  NUM_2                  = PREFIX_2 COMPLEX_2;
  NUM_10                 = PREFIX_10 COMPLEX_10;
  NUM_8                  = PREFIX_8 COMPLEX_8;
  NUM_16                 = PREFIX_16 COMPLEX_16;
  SPECIAL_INITIAL        = [!\$%&\*\/\:\<=\>\?\^\_~];
  LETTER                 = [a-z] | [A-Z];
  CONSTITUENT            = LETTER | [\X0080-\XFFFF]; /* todo: replace \X0080-\XFFFF to Unicode category */
  INITIAL                = CONSTITUENT | SPECIAL_INITIAL | INLINE_HEX_ESCAPE | "-"; /* we allow '-' identifier for shell mode */
  SUBSEQUENT             = INITIAL | DIGIT | [\+\-\.@]; /* todo: Add Unicode category Nd, Mc and Me */
  PECULIAR_IDENTIFIER    = [\+\-] | "..." | ("->" (SUBSEQUENT)*) | "@"; /* "@" is not R6RS match.scm required it. */
  SHEBANG                = "#!" [^\n\X0000]* (LINE_ENDING | EOS);
  COMMENT                = (";"[^\n\X0000]* (LINE_ENDING | EOS)) | ("#!" [a-zA-Z0-9/\_\.\-]+);
  DEFINING_SHARED        = "#" DIGIT+ "=";
  DEFINED_SHARED         = "#" DIGIT+ "#";
  /* Per R7RS Small Errata, we allow \\\\ and \\\" here */
  MNEMONIC_ESCAPE        = ('\\' [abtnr\\\"]);
  SYMBOL_ELEMENT         = [^\|\\] | "\\|" | INLINE_HEX_ESCAPE | MNEMONIC_ESCAPE;
  IDENTIFIER             = (INITIAL (SUBSEQUENT)*) | PECULIAR_IDENTIFIER | "|" SYMBOL_ELEMENT * "|";
*/

    int comment_count = 0;

    for(;;)
    {
/*!re2c
       SHEBANG DELMITER {
            YYCURSOR--;
            ucs4string shebang(YYTOKEN, YYCURSOR - YYTOKEN - 1);
            if (shebang == ucs4string(UC("#!r6rs"))) {
                currentVM()->readerContext()->port()->setStrictR6RsReaderMode();
            } else {
                 // just ignore the shebang.
            }
            YYTOKEN = YYCURSOR;
            continue;
       }
       "#"[tT] DELMITER {
            yylval->boolValue = true;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return SCHEME_BOOLEAN;
        }
        DEFINING_SHARED {
            ucs4string n =  ucs4string(YYTOKEN + 1, (YYCURSOR - YYTOKEN - 1));
            yylval->intValue = atoi(n.ascii_c_str());
            YYTOKEN = YYCURSOR;
            return DEFINING_SHARED;
        }
        DEFINED_SHARED {
            ucs4string n =  ucs4string(YYTOKEN + 1, (YYCURSOR - YYTOKEN - 1));
            yylval->intValue = atoi(n.ascii_c_str());
            YYTOKEN = YYCURSOR;
            return DEFINED_SHARED;
        }
        "#"[fF] DELMITER {
            yylval->boolValue = false;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return SCHEME_BOOLEAN;
        }
        "#\\space" DELMITER {
            yylval->charValue = ' ';
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\newline" DELMITER {
            yylval->charValue = '\n';
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\nul" DELMITER {
            yylval->charValue = 0x00;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\alarm" DELMITER {
            yylval->charValue = 0x07;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\backspace" DELMITER {
            yylval->charValue = 0x08;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\tab" DELMITER {
            yylval->charValue = 0x09;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\linefeed" DELMITER {
            yylval->charValue = 0x0A;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\vtab" DELMITER {
            yylval->charValue = 0x0B;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\page" DELMITER {
            yylval->charValue = 0x0C;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\return" DELMITER {
            yylval->charValue = 0x0D;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\delete" DELMITER {
            yylval->charValue = 0x7F;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\esc" DELMITER {
            yylval->charValue = 0x1B;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\" ANY_CHARACTER DELMITER {
            yylval->charValue = YYTOKEN[2];
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\x" HEX_SCALAR_VALUE DELMITER {
            YYCURSOR--;
            yylval->charValue = ScannerHelper::hexStringToUCS4Char(YYTOKEN + 3, YYCURSOR );
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        /* we now omit "(DECIMAL_10 MANTISSA_WIDTH)" for UREAL_10. */
        /* it causes infinite loop. */
       NUM_2  DELMITER {
           YYCURSOR--;
           yylval->stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN));
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       NUM_10 DELMITER {
           YYCURSOR--;
           yylval->stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN));
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       NUM_8 DELMITER {
           YYCURSOR--;
           yylval->stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN));
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       NUM_16 DELMITER {
           YYCURSOR--;
           yylval->stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN));
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       IDENTIFIER DELMITER {
            YYCURSOR--;
            yylval->stringValue = ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN));
            YYTOKEN = YYCURSOR;
            return IDENTIFIER;
            }
        "#/" REGEXP_ELEMENT* "/" DELMITER {
            YYCURSOR--;
            yylval->stringValue = ucs4string(YYTOKEN + 2, (YYCURSOR - YYTOKEN) - 3);
            YYTOKEN = YYCURSOR;
            return REGEXP;
        }
        "\"" STRING_ELEMENT* "\"" {
            yylval->stringValue = ucs4string(YYTOKEN + 1, (YYCURSOR - YYTOKEN) - 2);
            YYTOKEN = YYCURSOR;
            return STRING;
        }
        "." DELMITER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return DOT;
        }
        "`" ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_QUASIQUOTE;
        }
        "'" ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_QUOTE;
        }
        ",@" ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_UNQUOTESPLICING;
        }
        "," ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_UNQUOTE; }
        "#'" ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_SYNTAX; }
        "#`" ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_QUASISYNTAX; }
        "#," ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_UNSYNTAX;
        }
        "#,@" ANY_CHARACTER {
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return ABBV_UNSYNTAXSPLICING;
        }
        COMMENT {
            YYTOKEN = YYCURSOR;
            continue;
        }
        [\]\)] {
            yylval->charValue = YYTOKEN[0];
            YYTOKEN = YYCURSOR;
            return RIGHT_PAREN;
        }
        [\(\[] {
            yylval->charValue = YYTOKEN[0];
            YYTOKEN = YYCURSOR;
            return LEFT_PAREN;
        }
        "#(" {
            YYTOKEN = YYCURSOR;
            return VECTOR_START;
        }
        "#u8(" {
            YYTOKEN = YYCURSOR;
            return BYTE_VECTOR_START;
        }
        "#vu8(" {
            YYTOKEN = YYCURSOR;
            return BYTE_VECTOR_START;
        }
        WHITE_SPACE {
            YYTOKEN = YYCURSOR;
            continue;
        }
        EOS {
            YYTOKEN = YYCURSOR;
            return END_OF_FILE;
        }
        "#;" {
            YYTOKEN = YYCURSOR;
            return DATUM_COMMENT;
        }
        "#|" {
            comment_count = 1;
            goto comment;
        }
        [^] {
            // syntax error
            return 0;
        }

*/

comment:
        YYTOKEN = YYCURSOR;
/*!re2c
        "|#" {
            YYTOKEN = YYCURSOR;
            if (--comment_count == 0) {
                continue;
            } else {
                goto comment;
            }
        }
        "#|" {
            comment_count++;
            goto comment;
        }
        EOS {
            YYTOKEN = YYCURSOR;
            return 0;
        }
        ANY_CHARACTER
        {
            goto comment;
        }
*/
    }
}

ucs4string Scanner::currentToken() const
{
    if (limit_ >= token_) {
        return ucs4string(token_, limit_ - token_);
    } else {
        return ucs4string(UC(""));
    }
}

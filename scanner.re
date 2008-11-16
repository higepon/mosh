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
#include "ucs4string.h"
#include "ScannerHelper.h"
#include "Scanner.h"

#include "Reader.h"
#include "Reader.tab.hpp"
#include "VM.h"

#define YYCTYPE ucs4char
#define YYCURSOR cursor_
#define YYMARKER marker_
#define YYLIMIT limit_
#define YYTOKEN token_
#define YYDEBUG(state, ch)  yydebug(state, ch)
#define YYFILL(n) fill(n)

using namespace scheme;
extern VM* theVM;
extern TextualInputPort* parser_port();
extern YYSTYPE yylval;

Scanner::Scanner() : dummy_('Z'),  // for YYDEBUG
                     buffer_(NULL),
                     cursor_(&dummy_),
                     token_(buffer_),
                     limit_(buffer_),
                     marker_(buffer_),
                     bufferSize_(32)
{
}

Scanner::~Scanner()
{
}


static void yydebug(int state, ucs4char ch)
{
//    printf("state=%d ch=[%c] ch=%x\n", state, ch, ch);
}


void Scanner::fill(int n)
{
    TextualInputPort* const inputPort = Reader::port();
    const int restCharCount = limit_ - token_;
    const int tokenOffset = token_ - buffer_;

    if (buffer_ == NULL) {
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
            buffer_[i + restCharCount] = '\0';
            i++;
            break;
        } else {
            buffer_[i + restCharCount] = ch;
        }
//         if (!inputPort->isDataReady()) {
//             i++;
//             printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//             break;
//         }

    }
    const int readSize = i;
    cursor_ = cursor_ - tokenOffset;
    token_ = buffer_;
    marker_ = marker_ - tokenOffset;
    limit_ = limit_ - tokenOffset + readSize;
}

int yylex()
{
    return Reader::port()->scanner()->scan();
}

int Scanner::scan()
{
/*!re2c
  LINE_FEED              = "\n";
  CHARACTER_TABULATION   = "\X0009";
  LINE_TABULATION        = "\X000B";
  LINE_SEPARATOR         = "\X2028";
  FORM_FEED              = "\X000C";
  CARRIGE_RETURN         = "\r";
  NEXT_LINE              = "\X0085";
  UNICODE_ZL_ZP          = [\X2028-\x2029];
  UNICODE_ZS             = "\X0020" | "\X00A0" | "\X1680" | "\X180E" | [\X2000-\X200A] | "\X202F" | "\X205F" | "\X3000";
  LINE_ENDING            = LINE_FEED | CARRIGE_RETURN | (CARRIGE_RETURN LINE_FEED) | NEXT_LINE | (CARRIGE_RETURN NEXT_LINE) | LINE_SEPARATOR;
  WHITE_SPACE            = CHARACTER_TABULATION | LINE_FEED | LINE_TABULATION | FORM_FEED | CARRIGE_RETURN | NEXT_LINE | UNICODE_ZL_ZP | UNICODE_ZS;
  DELMITER               = [\(\)\[\]\";#]|WHITE_SPACE;
  ANY_CHARACTER          = [^];
  DIGIT                  = [0-9];
  HEX_DIGIT              = DIGIT | [a-f] | [A-F];
  HEX_SCALAR_VALUE       = HEX_DIGIT +;
  INTRA_LINE_WHITE_SPACE = "\t" | UNICODE_ZS;
  INLINE_HEX_ESCAPE      = "\\x" HEX_SCALAR_VALUE ";";
  /* We use "INTRA_LINE_WHITE_SPACE *" instead of "INTRA_LINE_WHITE_SPACE" defined in R6RS */
  STRING_ELEMENT         = [^\"\\] | ("\\" [abtnvfr\"\\]) | ("\\" INTRA_LINE_WHITE_SPACE * LINE_ENDING INTRA_LINE_WHITE_SPACE) | INLINE_HEX_ESCAPE;
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
  UREAL_10               = UINTEGER_10 | (UINTEGER_10 "/" UINTEGER_10) | DECIMAL_10;
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
  INITIAL                = CONSTITUENT | SPECIAL_INITIAL | INLINE_HEX_ESCAPE;
  SUBSEQUENT             = INITIAL | DIGIT | [\+\-\.@]; /* todo: Add Unicode category Nd, Mc and Me */
  PECULIAR_IDENTIFIER    = [\+\-] | "..." | ("->" (SUBSEQUENT)*) | "@"; /* "@" is not R6RS match.scm required it. */
  IDENTIFIER             = (INITIAL (SUBSEQUENT)*) | PECULIAR_IDENTIFIER;
  COMMENT                = (";"[^\n\X0000]* (LINE_ENDING | "\X0000")) | ("#!"[^\n]*);
*/

    for(;;)
    {
/*!re2c
        "#"[tT] DELMITER {
            yylval.boolValue = true;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return BOOLEAN;
        }
        "#"[fF] DELMITER {
            yylval.boolValue = false;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return BOOLEAN;
        }
        "#\\space" DELMITER {
            yylval.charValue = ' ';
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\newline" DELMITER {
            yylval.charValue = '\n';
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\nul" DELMITER {
            yylval.charValue = 0x00;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\alerm" DELMITER {
            yylval.charValue = 0x07;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\backspace" DELMITER {
            yylval.charValue = 0x08;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\tab" DELMITER {
            yylval.charValue = 0x09;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\linefeed" DELMITER {
            yylval.charValue = 0x0A;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\vtab" DELMITER {
            yylval.charValue = 0x0B;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\page" DELMITER {
            yylval.charValue = 0x0C;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\return" DELMITER {
            yylval.charValue = 0x0D;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\esc" DELMITER {
            yylval.charValue = 0x1B;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\delete" DELMITER {
            yylval.charValue = 0x7F;
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\" ANY_CHARACTER DELMITER {
            yylval.charValue = YYTOKEN[2];
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        "#\\x" HEX_SCALAR_VALUE DELMITER {
            yylval.charValue = ScannerHelper::hexStringToUCS4Char(YYTOKEN + 3, YYCURSOR - 1);
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return CHARACTER;
        }
        /* we now omit "(DECIMAL_10 MANTISSA_WIDTH)" for UREAL_10. */
        /* it causes infinite loop. */
       NUM_2 DELMITER {
           yylval.stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN) - 1);
           YYCURSOR--;
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       NUM_10 DELMITER {
           yylval.stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN) - 1);
           YYCURSOR--;
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       NUM_8 DELMITER {
           yylval.stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN) - 1);
           YYCURSOR--;
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       NUM_16 DELMITER {
           yylval.stringValue =  ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN) - 1);
           YYCURSOR--;
           YYTOKEN = YYCURSOR;
           return NUMBER;
       }
       IDENTIFIER DELMITER {
            yylval.stringValue = ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN) - 1);
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return IDENTIFIER;
            }
        "#/" REGEXP_ELEMENT* "/" DELMITER {
            yylval.stringValue = ucs4string(YYTOKEN + 2, (YYCURSOR - YYTOKEN) - 4);
            YYCURSOR--;
            YYTOKEN = YYCURSOR;

            return REGEXP;
        }
        "\"" STRING_ELEMENT* "\"" DELMITER {
            yylval.stringValue = ucs4string(YYTOKEN + 1, (YYCURSOR - YYTOKEN) - 3);
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return STRING;
        }
        "." {
            return DOT;
        }
        "`"   {
            YYTOKEN = YYCURSOR;
            return ABBV_QUASIQUOTE;
        }
        "'"   {
            YYTOKEN = YYCURSOR;
            return ABBV_QUOTE;
        }
        ",@"  {
            YYTOKEN = YYCURSOR;
            return ABBV_UNQUOTESPLICING;
        }
        ","   {
            YYTOKEN = YYCURSOR;
            return ABBV_UNQUOTE; }
        "#'"  {
            YYTOKEN = YYCURSOR;
            return ABBV_SYNTAX; }
        "#`"  {
            YYTOKEN = YYCURSOR;
            return ABBV_QUASISYNTAX; }
        "#,"  {
            YYTOKEN = YYCURSOR;
            return ABBV_UNSYNTAX;
        }
        "#,@" {
            YYTOKEN = YYCURSOR;
            return ABBV_UNSYNTAXSPLICING;
        }
        COMMENT {
            YYTOKEN = YYCURSOR;
            continue;
        }
        [\]\)] {
            YYTOKEN = YYCURSOR;
            return RIGHT_PAREN;
        }
        [\(\[] {
            YYTOKEN = YYCURSOR;
            return LEFT_PAREN;
        }
        "#(" {
            YYTOKEN = YYCURSOR;
            return VECTOR_START;
        }
        "#vu8(" {
            YYTOKEN = YYCURSOR;
            return BYTE_VECTOR_START;
        }
        WHITE_SPACE {
            YYTOKEN = YYCURSOR;
            continue;
        }
        "\X0000" {
            YYTOKEN = YYCURSOR;
            return END_OF_FILE;
        }
        "#|" {
            goto comment;
        }
*/

comment:
        YYTOKEN = YYCURSOR;
/*!re2c
        "|#" {
            YYTOKEN = YYCURSOR;
            continue;
        }
        ANY_CHARACTER
        {
            goto comment;
        }
*/
    }
}

ucs4char* Scanner::currentToken() const
{
    return token_;
}

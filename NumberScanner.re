/*
 * NumberScanner.re
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
#include "NumberScanner.h"

#include "Reader.h"
#include "NumberReader.h"
#include "NumberReader.tab.hpp"
#include "VM.h"

#define YYCTYPE ucs4char
#define YYCURSOR cursor_
#define YYMARKER marker_
#define YYLIMIT limit_
#define YYTOKEN token_
#define YYDEBUG(state, ch)  yydebug(state, ch)
#define YYFILL(n) fill(n)
extern YYSTYPE number_yylval;
#define yylval number_yylval
using namespace scheme;
extern VM* theVM;

NumberScanner::NumberScanner() : dummy_('Z'),  // for YYDEBUG
                     buffer_(NULL),
                     cursor_(&dummy_),
                     token_(buffer_),
                     limit_(buffer_),
                     marker_(buffer_),
                     bufferSize_(32)
{
}

NumberScanner::~NumberScanner()
{
}

void NumberScanner::fill(int n)
{
    TextualInputPort* const inputPort = NumberReader::port();
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

    }
    const int readSize = i;
    cursor_ = cursor_ - tokenOffset;
    token_ = buffer_;
    marker_ = marker_ - tokenOffset;
    limit_ = limit_ - tokenOffset + readSize;

}

int number_yylex()
{
    return NumberReader::port()->numberScanner()->scan();
}

int NumberScanner::scan()
{
/*!re2c
    DIGIT_10        = [0-9];
    EXPONENT_MARKER = [eEsSfFdDlL] [\+\-] DIGIT_10+;
    EXACT           = "#"[eE];
    INEXACT         = "#"[iI];
    RADIX_2         = "#" [bB];
    RADIX_8         = "#" [oO];
    RADIX_10        = "#" [dD];
    RADIX_16        = "#" [xX];
    DIGIT_2         = [01];
    DIGIT_8         = [0-7];
    DIGIT_16_1      = [A-F];
    DIGIT_16_2      = [a-f];
    MY_NAN          = "nan.0";
    MY_INF          = "inf.0";
*/

    for(;;)
    {
/*!re2c
        EXACT {
            YYTOKEN = YYCURSOR;
            return EXACT;
        }
        EXPONENT_MARKER {
            yylval.stringValue = ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN));
            YYTOKEN = YYCURSOR;
            return EXPONENT_MARKER;
        }
        INEXACT {
            YYTOKEN = YYCURSOR;
            return INEXACT;
        }
        MY_NAN {
            YYTOKEN = YYCURSOR;
            return MY_NAN;
        }
        MY_INF {
            YYTOKEN = YYCURSOR;
            return MY_INF;
        }
        "+" {
            YYTOKEN = YYCURSOR;
            return PLUS;
        }
        "-" {
            YYTOKEN = YYCURSOR;
            return MINUS;
        }
        "/" {
            YYTOKEN = YYCURSOR;
            return SLASH;
        }
        "i" {
            YYTOKEN = YYCURSOR;
            return IMAG;
        }
        "@" {
            YYTOKEN = YYCURSOR;
            return AT;
        }
        "." {
            YYTOKEN = YYCURSOR;
            return DOT;
        }
        RADIX_2 {
            YYTOKEN = YYCURSOR;
            return RADIX_2;
        }
        RADIX_8 {
            YYTOKEN = YYCURSOR;
            return RADIX_8;
        }
        RADIX_10 {
            YYTOKEN = YYCURSOR;
            return RADIX_10;
        }
        RADIX_16 {
            YYTOKEN = YYCURSOR;
            return RADIX_16;
        }
        DIGIT_2 {
            yylval.intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_2;
        }
        DIGIT_8 {
            yylval.intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_8;
        }
        DIGIT_10 {
            yylval.intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_10;
        }
        DIGIT_16_1 {
            const ucs4char ch = YYTOKEN[0];
            yylval.intValue = ch - 'A' + 10;
            YYTOKEN = YYCURSOR;
            return DIGIT_16;
        }
        DIGIT_16_2 {
            const ucs4char ch = YYTOKEN[0];
            yylval.intValue = ch - 'a' + 10;
            YYTOKEN = YYCURSOR;
            return DIGIT_16;
        }
        "\X0000" {
            YYTOKEN = YYCURSOR;
            return END_OF_FILE;
        }
*/

    }
}

ucs4char* NumberScanner::currentToken() const
{
    return token_;
}

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
#include "EqHashTable.h"
#include "ScannerHelper.h"
#include "NumberScanner.h"

#include "Reader.h"
#include "VM.h"
#include "MultiVMProcedures.h"

#define YYCTYPE ucs4char
#define YYCURSOR cursor_
#define YYMARKER marker_
#define YYLIMIT limit_
#define YYTOKEN token_
#define YYGETCONDITION()  condition_
#define YYSETCONDITION(s) condition_ = s
#define YYCONDTYPE int
//#define YYDEBUG(state, ch)  yydebug(state, ch)
#define YYFILL(n) fill(n)
extern YYSTYPE number_yylval;
#define yylval number_yylval
using namespace scheme;
extern VM* theVM;

NumberScanner::NumberScanner() :
                     buffer_(nullptr),
                     cursor_(buffer_),
                     token_(buffer_),
                     limit_(buffer_),
                     marker_(buffer_),
                     bufferSize_(32),
                    condition_(INITIAL)
{
}

NumberScanner::~NumberScanner()
{
}

void NumberScanner::fill(int n)
{
    TextualInputPort* const inputPort = currentVM()->numberReaderContext()->port();
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

int number_yylex(YYSTYPE* yylval)
{
    return currentVM()->numberReaderContext()->port()->numberScanner()->scan(yylval);
}

int NumberScanner::scan(YYSTYPE* yylval)
{
/*!re2c
    DIGIT_10        = [0-9];
    EXPONENT_MARKER = [eEsSfFdDlL] [\+\-]? DIGIT_10+;
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
        <INITIAL,IN_HEX>EXACT {
            YYTOKEN = YYCURSOR;
            return EXACT;
        }
        <INITIAL>EXPONENT_MARKER {
            yylval->stringValue = ucs4string(YYTOKEN, (YYCURSOR  - YYTOKEN));
            YYTOKEN = YYCURSOR;
            return EXPONENT_MARKER;
        }
        <INITIAL,IN_HEX>INEXACT {
            YYTOKEN = YYCURSOR;
            return INEXACT;
        }
        <INITIAL,IN_HEX>MY_NAN {
            YYTOKEN = YYCURSOR;
            return MY_NAN;
        }
        <INITIAL,IN_HEX>MY_INF {
            YYTOKEN = YYCURSOR;
            return MY_INF;
        }
        <INITIAL,IN_HEX>"+" {
            YYTOKEN = YYCURSOR;
            return PLUS;
        }
        <INITIAL,IN_HEX>"-" {
            YYTOKEN = YYCURSOR;
            return MINUS;
        }
        <INITIAL,IN_HEX>"/" {
            YYTOKEN = YYCURSOR;
            return SLASH;
        }
        <INITIAL,IN_HEX>"i" {
            YYTOKEN = YYCURSOR;
            return IMAG;
        }
        <INITIAL,IN_HEX>"@" {
            YYTOKEN = YYCURSOR;
            return AT;
        }
        <INITIAL,IN_HEX>"." {
            YYTOKEN = YYCURSOR;
            return DOT;
        }
        <INITIAL,IN_HEX>RADIX_2 {
            YYTOKEN = YYCURSOR;
            return RADIX_2;
        }
        <INITIAL,IN_HEX>RADIX_8 {
            YYTOKEN = YYCURSOR;
            return RADIX_8;
        }
        <INITIAL,IN_HEX>RADIX_10 {
            YYTOKEN = YYCURSOR;
            return RADIX_10;
        }
        <INITIAL>RADIX_16 {
            YYSETCONDITION(IN_HEX);
            YYTOKEN = YYCURSOR;
            return RADIX_16;
        }
        <INITIAL,IN_HEX>DIGIT_2 {
            yylval->intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_2;
        }
        <INITIAL,IN_HEX>DIGIT_8 {
            yylval->intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_8;
        }
        <INITIAL,IN_HEX>DIGIT_10 {
            yylval->intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_10;
        }
        <INITIAL,IN_HEX>DIGIT_16_1 {
            const ucs4char ch = YYTOKEN[0];
            yylval->intValue = ch - 'A' + 10;
            YYTOKEN = YYCURSOR;
            return DIGIT_16;
        }
        <INITIAL,IN_HEX>DIGIT_16_2 {
            const ucs4char ch = YYTOKEN[0];
            yylval->intValue = ch - 'a' + 10;
            YYTOKEN = YYCURSOR;
            return DIGIT_16;
        }
        <*>"\X0000" {
            YYSETCONDITION(INITIAL);
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

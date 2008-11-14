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


static void yydebug(int state, ucs4char ch)
{
//    printf("state=%d ch=[%c] ch=%x\n", state, ch, ch);
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
    EXACT    = "#"[eE];
    INEXACT  = "#"[iI];
    RADIX_2  = "#" [bB];
    RADIX_10 = "#" [dD];
    DIGIT_2  = [01];
    DIGIT_10 = [0-9];
    MY_NAN   = "nan.0";
    MY_INF   = "inf.0";
*/

    for(;;)
    {
/*!re2c
        EXACT {
            YYTOKEN = YYCURSOR;
            return EXACT;
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
        RADIX_2 {
            YYTOKEN = YYCURSOR;
            return RADIX_2;
        }
        RADIX_10 {
            YYTOKEN = YYCURSOR;
            return RADIX_10;
        }
        DIGIT_2 {
            yylval.intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_2;
        }
        DIGIT_10 {
            yylval.intValue = YYTOKEN[0] - '0';
            YYTOKEN = YYCURSOR;
            return DIGIT_10;
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

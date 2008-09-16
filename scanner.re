#include <stdio.h>
#include <errno.h>
#include "Object.h"
#include "Pair.h"
#include "StringProcedures.h"
#include "TextualInputPort.h"
#include "TextualOutputPort.h"
#include "ucs4string.h"

#include "reader.h"
#include "reader.tab.hpp"
#include "VM.h"
// re2c definitions
#define YYCTYPE ucs4char
#define YYCURSOR cursor
#define YYMARKER marker
#define YYLIMIT limit
#define YYTOKEN token
#define YYDEBUG(state, ch)  yydebug(state, ch)
#define YYFILL(n) fill(n)

using namespace scheme;
extern VM* theVM;
extern TextualInputPort* parser_port();
static ucs4char buffer[32];
static ucs4char* cursor = buffer;
static ucs4char* limit = buffer;
static ucs4char* marker = buffer;
static ucs4char* token = buffer;
extern YYSTYPE yylval;
// N.B. Do not use -g (optimization) option. -u causes YYCURSOR bug.

#define DEBUG_SCANNER 0


class ScannerHelper {
public:
    static ucs4char hexStringToUCS4Char(ucs4char* start, ucs4char* end)
    {
        TextualOutputPort* const port = theVM->getOutputPort().toTextualOutputPort();
        ucs4char ret = 0;
        for (ucs4char* ch = start; ch != end; ch++) {
            const ucs4char hexChar = *ch;
            if (isdigit(hexChar)) {
                ret = (ret << 4) | (hexChar - '0');
            } else if ('a' <= hexChar && hexChar <= 'f') {
                ret = (ret << 4) | (hexChar - 'a' + 10);
            } else if ('A' <= hexChar && hexChar <= 'F') {
                ret = (ret << 4) | (hexChar - 'A' + 10);
            } else {
                MOSH_ASSERT(false); // not reached
            }
        }
        return ret;
    }

    static int num10StringToInt(ucs4char* start, ucs4char* end)
    {
        printf("start = %x, end = %x\n", start, end);
        char* buf = new(GC) char[end - start];
        for (int i = 0; i < end - start; i++) {
            printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
            buf[i] = start[i];
        }
        errno = 0;
        printf("<%s>", buf);fflush(stdout);
        long long ret = strtoll(buf, NULL, 10);
        if ((errno == ERANGE && (ret == LONG_MAX || ret == LONG_MIN))
            || (errno != 0 && ret == 0)) {
            fprintf(stderr, "error num-10");
            exit(-1);
        } else {
            return (int)ret;
        }
    }
};

static void yydebug(int state, ucs4char ch)
{
#if DEBUG_SCANNER
    TextualOutputPort* const port = theVM->getOutputPort().toTextualOutputPort();
    port->format(UC("state=~d ch=[~a]\n"), Pair::list2(Object::makeInt(state), Object::makeChar(ch)));
#endif
}


static void fill(int n)
{
#if DEBUG_SCANNER
    printf("[before] token = %x cursor=%x limit = %x : ", token, cursor, limit);
    for (ucs4char* p = token; p != limit; p++) {
        if (*p == '\0') {
            printf("[EOS]");
        } else if (*p == EOF) {
            printf("[EOF]");
        } else {
            printf("[%c]", *p);
        }
    }
    printf("\n");
#endif

    // 自動拡張をバグらせたくない。
    TextualInputPort* const inputPort = parser_port();
    const int restCharCount = limit - token;
    const int tokenOffset = token - buffer;
#if DEBUG_SCANNER
    printf("restCharCount = %d\n", restCharCount);
#endif
    if (restCharCount > 0) {
        memmove(buffer, token, restCharCount * sizeof(ucs4char));
    }

    int i;
    for (i = 0; i < n; i++) {
        const ucs4char ch = inputPort->getChar();
        if (ch == EOF) {
            buffer[i + restCharCount] = '\0';
            i++;
            break;
        } else {
#if DEBUG_SCANNER
            printf("[%d]=[%c] ", i + restCharCount, ch);
#endif
            buffer[i + restCharCount] = ch;
        }
    }
    const int readSize = i;
#if DEBUG_SCANNER
    printf("\nreadSize = %d\n", readSize);
#endif
    cursor = cursor - tokenOffset;
    token = buffer;
    marker = marker - tokenOffset;
    limit = limit - tokenOffset + readSize;

#if DEBUG_SCANNER
    printf("[after] token = %x cursor=%x limit = %x marker = %x \n", token, cursor, limit, marker);

    for (ucs4char* p = token; p != limit; p++) {
        if (*p == '\0') {
            printf("[EOS]");
        } else if (*p == EOF) {
            printf("[EOF]");
        } else {
            printf("[%c]", *p);
        }
    }
#endif
//     int i = 0;
//     printf("n= %d, YYMARKER = %x, YYTOKEN = %x YYCURSOR = %x YYLIMIT = %x\n", n, YYMARKER, YYTOKEN, YYCURSOR, YYLIMIT);
//     MOSH_ASSERT(YYMARKER <= YYCURSOR);
// //     for (ucs4char* p = YYCURSOR; p != YYLIMIT; ++p, ++i) {
// //         buffer[i] = *p;
// //         if (p == YYMARKER) {
// //            YYMARKER = &(buffer[i]);
// //            printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
// //         }
// //     }

//     const int diff = YYLIMIT - YYTOKEN;
//     const int notDoneByteSize = (YYLIMIT - YYTOKEN) * sizeof(ucs4char);
//     if (notDoneByteSize != 0) {
//         memmove(buffer, YYCURSOR, notDoneByteSize);
//         i = (YYLIMIT - YYTOKEN);
//     }
// //     const int notDoneByteSize = (YYLIMIT - YYCURSOR) * sizeof(ucs4char);
// //     if (notDoneByteSize != 0) {
// //         memmove(buffer, YYCURSOR, notDoneByteSize);
// //         i = (YYLIMIT - YYCURSOR);
// //     }


//     TextualInputPort* const port = parser_port();

// //    TextualOutputPort* const outputPort = theVM->getOutputPort().toTextualOutputPort();
//     int realReadSize = 0;
//     for (realReadSize = 0; realReadSize < n; realReadSize++) {
//         const ucs4char ch = port->getChar();
//         if (ch == EOF) {
//             buffer[diff + realReadSize] = '\0';
//             realReadSize++;
//             break;
//         }
//         buffer[diff + realReadSize] = ch;
//     }
//     cursor -= diff;
//     token = buffer;
//     marker -= diff;
//     printf("diff = %d = %d, %x \n", diff, n,buffer);
//     YYLIMIT  = buffer + diff + realReadSize;
//     printf("<2>YYMARKER = %x, YYTOKEN = %x YYCURSOR = %x YYLIMIT = %x\n", YYMARKER, YYTOKEN, YYCURSOR, YYLIMIT);
}

int yylex()
{
/*!re2c
  LINE_FEED              = "\n";
  LINE_TABULATION        = "\X000B";
  LINE_SEPARATOR         = "\X2028";
  FORM_FEED              = "\X000C";
  CARRIGE_RETURN         = "\r";
  NEXT_LINE              = "\X0085";
  UNICODE_ZL_ZP          = [\X2028-\x2029];
  UNICODE_ZS             = "\X0020" | "\X00A0" | "\X1680" | "\X180E" | [\X2000-\X200A] | "\X202F" | "\X205F" | "\X3000";
  LINE_ENDING            = LINE_FEED | CARRIGE_RETURN | (CARRIGE_RETURN LINE_FEED) | NEXT_LINE | (CARRIGE_RETURN NEXT_LINE) | LINE_SEPARATOR;
  WHITE_SPACE            = LINE_FEED | LINE_TABULATION | FORM_FEED | CARRIGE_RETURN | NEXT_LINE | UNICODE_ZL_ZP | UNICODE_ZS;
  DELMITER               = [\(\)\[\]\";#]|WHITE_SPACE;
  ANY_CHARACTER          = [^];
  DIGIT                  = [0-9];
  HEX_DIGIT              = DIGIT | [a-f] | [A-F];
  HEX_SCALAR_VALUE       = HEX_DIGIT +;
  INTRA_LINE_WHITE_SPACE = "\t" | UNICODE_ZS;
  INLINE_HEX_ESCAPE      = "\\x" HEX_SCALAR_VALUE ";";
  STRING_ELEMENT         = [^\"\\] | [\a\b\t\n\v\f\r] | "\\\"" | "\\\\" | ("\\" INTRA_LINE_WHITE_SPACE LINE_ENDING INTRA_LINE_WHITE_SPACE) | INLINE_HEX_ESCAPE;
  REGEXP_ELEMENT         = "\\\/" | [^/];
  DIGIT_10               = DIGIT;
  UINTEGER_10            = DIGIT_10 +;
  NAN_INF                = "nan.0" | "inf.0";
  SIGN                   = [\+\-]?;
  EXPONENT_MARKER        = [eEsSfFdDlL];
  MANTISSA_WIDTH         = ("|" (DIGIT_10)+)?;
  SUFFIX                 = (EXPONENT_MARKER SIGN (DIGIT_10)+)?;
  DECIMAL_10             = (UINTEGER_10 SUFFIX) | ("." (DIGIT_10)+ SUFFIX) | ((DIGIT_10)+ "." (DIGIT_10)* SUFFIX) | ((DIGIT_10)+ "." SUFFIX);
  UREAL_10               = UINTEGER_10 | (UINTEGER_10 "/" UINTEGER_10);
  REAL_10                = (SIGN UREAL_10) | ([\+\-] NAN_INF);
  COMPLEX_10             = REAL_10 | (REAL_10 "@" REAL_10) | (REAL_10 [\+\-] UREAL_10 "i") | (REAL_10 [\+\-] NAN_INF "i") | (REAL_10 [\+\-] "i") | ([\+-\] UREAL_10 "i") | ([\+\-] NAN_INF "i") | ([\+\-] "i");
  RADIX_10               = ("#"[dD])?;
  EXACTNESS              = ("#"[iIeE])?;
  PREFIX_10              = (RADIX_10 EXACTNESS) | (EXACTNESS RADIX_10);
  NUM_10                 = PREFIX_10 COMPLEX_10;
  SPECIAL_INITIAL        = [!\$%&\*\/\:\<=\>\?\^\_~];
  LETTER                 = [a-z] | [A-Z];
  CONSTITUENT            = LETTER | [\X0080-\XFFFF]; /* todo: replace \X0080-\XFFFF to Unicode category */
  INITIAL                = CONSTITUENT | SPECIAL_INITIAL | INLINE_HEX_ESCAPE;
  SUBSEQUENT             = INITIAL | DIGIT | [\+\-\.@]; /* todo: Add Unicode category Nd, Mc and Me */
  PECULIAR_IDENTIFIER    = [\+\-] | "..." | ("->" (SUBSEQUENT)*);
  IDENTIFIER             = (INITIAL (SUBSEQUENT)*) | PECULIAR_IDENTIFIER;

*/

scan:
    for(;;)
    {
#if DEBUG_SCANNER

    printf("[1]YYMARKER = %x, YYCURSOR = %x YYLIMIT = %x\n", YYMARKER, YYCURSOR, YYLIMIT);
#endif
/*!re2c
        "#"[tT] DELMITER {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
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
        NUM_10 DELMITER {
            yylval.intValue = ScannerHelper::num10StringToInt(YYTOKEN, YYCURSOR - 1);
            YYCURSOR--;
            YYTOKEN = YYCURSOR;
            return NUMBER;
        }
        IDENTIFIER DELMITER {
            yylval.stringValue = ucs4string(YYTOKEN, (YYCURSOR - YYTOKEN) - 1);
            YYTOKEN = YYCURSOR;
            return IDENTIFIER;
        }
        "#/" REGEXP_ELEMENT* "/" DELMITER {
            printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
            yylval.stringValue = ucs4string(YYTOKEN + 2, (YYCURSOR - YYTOKEN) - 4);
            YYCURSOR--;
            YYTOKEN = YYCURSOR;

            return REGEXP;
        }
        "\"" STRING_ELEMENT* "\"" DELMITER {
#if DEBUG_SCANNER
    for (ucs4char* p = token; p != limit; p++) {
        if (*p == '\0') {
            printf("[EOS]");
        } else if (*p == EOF) {
            printf("[EOF]");
        } else {
            printf("[%c]", *p);
        }
    }

    printf("YYCURSOR = %x YYTOKEN=%x <%c>\n", YYCURSOR, YYTOKEN, *YYCURSOR);
#endif
            yylval.stringValue = ucs4string(YYTOKEN + 1, (YYCURSOR - YYTOKEN) - 3);
            YYTOKEN = YYCURSOR;
            return STRING;
        }
        "." DELMITER ? { return DOT; }
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
        [\]\)] {

            printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
            YYTOKEN = YYCURSOR;
            return RIGHT_PAREN;
        }
        [\(\[] {
            printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
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
            goto scan;
        }
        "\X0000" {
            YYTOKEN = YYCURSOR;
            return END_OF_FILE;
        }

*/
    }
}

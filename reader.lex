%{
#include <stdio.h>
#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Symbol.h"
#include "reader.h" // include before reader.tab.hpp is included.
#include "reader.tab.hpp"
extern bool parser_input(char* buf, int max_size);
using namespace scheme;
#undef YY_INPUT
#define YY_INPUT(buffer, result, max_size) { const bool isEOF = parser_input(buffer, max_size); if (isEOF) {result= YY_NULL; } else { result = 1;} }
%}

character-tabulation \t
line-tabulation \x0B
form-feed \x0C
paragraph-separator \x2029

/* Unicode categories */
Zs \x20|\xA0|\x1680|\x180E|\x2000|\x2001|\x2002|\x2003|\x2004|\x2005|\x2006|\x2007|\x2008|\x2009|\x200A|\x202F|\x205F|\x3000
Zl \x2028
Zp \x2029

lexme  {identifier}|{boolean}|{number}|{character}|{string}|[()\[\]’‘,\.]|#\(|,@|#vu8\(|#’|#‘|#,|#,@
delimiter  [()\[\]\";#]|{whitespace}
whitespace {character-tabulation}|{linefeed}|{line-tabulation}|{form-feed}|{carriage-return}|{next-line}|{Zs}|{Zl}|{Zp}
line-ending  {linefeed}|{carriage-return}|{carriage-return}{linefeed}|{next-line}|{carriage-return}{next-line}|{line-separator}
linefeed \n
carriage-return \r
next-line \x85

line-separator \x2028
comment  (;.*{line-ending})|(;.*{paragraph-separator}|#;{interlexeme-space}{datum}|(#!r6rs)

identifier  ({initial}{subsequent}*)|{peculiar-identifier}
initial  {constituent}|{special-initial}|{inline-hex-escape}
letter  [a-z]|[A-Z]

/* not enough */
constituent {letter}|[\x80-\xffff]

special-initial  [!\$%&\*\/\:\{=\}\?\^\_~]

dot "."{delimiter}?

/* not enough */
subsequent  {initial}|{digit}|[\x80-\xffff]|{special-subsequent}
digit  [0-9]
hex-digit  {digit}|[aAbBcCdDeEfF]
special-subsequent [+\-.@]
inline-hex-escape  (\\x{hex-scalar-value};)
hex-scalar-value {hex-digit}+
peculiar-identifier (([\+\-]|\.\.\.){subsequent}*)
boolean {true}{false}
true #[tT]
false #[fF]
character  (#\\.+|#\\{character-name}|#\\x{hex-scalar-value})
character-name  (nul|alarm|backspace|tab|linefeed|newline|vtab|page|return|esc|space|delete)
string  \"{string-element}*\"

/* todo 最初の要素の一文字が怪しい */
string-element  [^\\\"]|\\[abtnvfr\"\\\\]|\\{intraline-whitespace}{line-ending}{intraline-whitespace}|{inline-hex-escape}
intraline-whitespace  \t

number {num-2}|{num-8}|{num-10}|{num-16}
num-2 {prefix-2}{complex-2}
num-8 {prefix-8}{complex-8}
num-10 {prefix-10}{complex-10}
num-16 {prefix-16}{complex-16}

complex-2 {real-2}|{real-2}@{real-2}|{real-2}\+{ureal-2}i|{real-2}-{ureal-2}i|{real-2}+{naninf}i|{real-2}-{naninf}i|{real-2}\+i|{real-2}-i|\+{ureal-2}i|-{ureal-2}-i|\+{naninf}i|-{naninf}i|\+i|-i
complex-8 {real-8}|{real-8}@{real-8}|{real-8}\+{ureal-8}i|{real-8}-{ureal-8}i|{real-8}+{naninf}i|{real-8}-{naninf}i|{real-8}\+i|{real-8}-i|\+{ureal-8}i|-{ureal-8}-i|\+{naninf}i|-{naninf}i|\+i|-i
complex-10 {real-10}|{real-10}@{real-10}|{real-10}\+{ureal-10}i|{real-10}-{ureal-10}i|{real-10}+{naninf}i|{real-10}-{naninf}i|{real-10}\+i|{real-10}-i|\+{ureal-10}i|-{ureal-10}-i|\+{naninf}i|-{naninf}i|\+i|-i
complex-16 {real-16}|{real-16}@{real-16}|{real-16}\+{ureal-16}i|{real-16}-{ureal-16}i|{real-16}+{naninf}i|{real-16}-{naninf}i|{real-16}\+i|{real-16}-i|\+{ureal-16}i|-{ureal-16}-i|\+{naninf}i|-{naninf}i|\+i|-i

real-2 {sign}{ureal-2}|\+{naninf}|-{naninf}
real-8 {sign}{ureal-8}|\+{naninf}|-{naninf}
real-10 {sign}{ureal-10}|\+{naninf}|-{naninf}
real-16 {sign}{ureal-16}|\+{naninf}|-{naninf}
naninf nan\.0|inf\.0

ureal-2 {uinteger-2}|{uinteger-2}\/{uinteger-2}
ureal-8 {uinteger-8}|{uinteger-8}\/{uinteger-8}
ureal-10 {uinteger-10}|{uinteger-10}\/{uinteger-10}|{decimal-10}{mantissa-width}
ureal-16 {uinteger-16}|{uinteger-16}\/{uinteger-16}

decimal-10 {uinteger-10}{suffix}|\.{digit-10}+{suffix}|{digit-10}+\.{digit-10}\*{suffix}|{digit-10}+\.{suffix}
uinteger-2 {digit-2}+
prefix-2 {radix-2}{exactness}|{exactness}{radix-2}
uinteger-8 {digit-8}+
prefix-8 {radix-8}{exactness}|{exactness}{radix-8}
uinteger-10 {digit-10}+
prefix-10 {radix-10}{exactness}|{exactness}{radix-10}
uinteger-16 {digit-16}+
prefix-16 {radix-16}{exactness}|{exactness}{radix-16}
suffix ({exponent-marker}{sign}{digit-10}+)?
exponent-marker [eEsSfFdDlL]
mantissa-width (\|{digit-10}+)?
sign [+\-]?
exactness (#[iIeE])?
radix-2 #[bB]
radix-8 #[oO]
radix-10 (#[dD])?
radix-16 #[xX]
digit-2 [01]
digit-8 [0-7]
digit-10 {digit}
digit-16 {hex-digit}
%x COMMENT
%%
[\])] { return RIGHT_PAREN; }

[(\[] { return LEFT_PAREN; }
"#(" { return VECTOR_START; }
"#vu8(" { return BYTE_VECTOR_START; }

"#|"                     BEGIN(COMMENT);
<COMMENT>[^|\n]*
<COMMENT>[^|\n]*\n
<COMMENT>"|"+[^|/\n]*
<COMMENT>"|"+[^|/\n]*\n
<COMMENT><<EOF>>
<COMMENT>"|"+"#"         BEGIN(INITIAL);
{identifier} {
  //  yylval.string_value = yytext;
  return IDENTIFIER; }
{string} {
  // remove dobule quotation.
  yytext[yyleng - 1] = '\0';
  yylval.stringValue = reinterpret_cast<ucs4char*>(yytext + 1);
  return STRING;
 }

{true} {
    yylval.boolValue = true;
    return BOOLEAN;
  }
{false} {
    yylval.boolValue = false;
    return BOOLEAN;
 }

{dot} { return DOT; }

<<EOF>> { return END_OF_FILE; }



{num-10} {
    errno = 0;
    long long ret = strtoll(yytext, NULL, 10);
    if ((errno == ERANGE && (ret == LONG_MAX || ret == LONG_MIN))
        || (errno != 0 && ret == 0)) {
      printf("error num-10");
    } else {
      yylval.intValue = ret;
    }
    return NUMBER;
 }
  {delimiter}
"`"                   { return ABBV_QUASIQUOTE; }
"'"                   { return ABBV_QUOTE; }
",@"                  { return ABBV_UNQUOTESPLICING; }
","                   { return ABBV_UNQUOTE; }
"#'"                  { return ABBV_SYNTAX; }
"#`"                  { return ABBV_QUASISYNTAX; }
"#,"                  { return ABBV_UNSYNTAX; }
"#,@"                 { return ABBV_UNSYNTAXSPLICING; }

{character} { printf("character, %s", yytext); }
{lexme} { printf("lexme, %s", yytext); }


%%

int yywrap()
{
  return 1;
}

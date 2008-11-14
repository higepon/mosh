%{
#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "StringProcedures.h"
#include "NumberScanner.h"
#include "TextualInputPort.h"
#include "Arithmetic.h"
#include "Reader.h"
#include "NumberReader.h"
#include "ScannerHelper.h"
#include "Scanner.h"
#include "Ratnum.h"
#include "Flonum.h"

using namespace scheme;
extern int number_yylex();
extern int number_yyerror(const char *);
%}

%token END_OF_FILE PLUS MINUS SLASH AT MY_NAN MY_INF IMAG
%token RADIX_2

%token <intValue> EXACT INEXACT
%token <intValue> DIGIT_2

%type <exactValue> exactness prefix2
%type <intValue> digit2
%type <object> top_level datum num2 complex2 real2 ureal2 sreal2 uinteger2 naninf

%start top_level

%%
top_level : datum { NumberReader::parsed = $$; YYACCEPT; }
          | END_OF_FILE { NumberReader::parsed = Object::Eof; YYACCEPT; }
datum : num2
      ;
num2  : prefix2 complex2 { $$ = ScannerHelper::applyExactness($1, $2); }
      ;

complex2  : real2
          | real2 AT real2           { $$ = Arithmetic::makePolar($1, $3); }
          | sreal2 IMAG             { $$ = Object::makeCompnum(Object::makeFixnum(0), $1); }
          | real2 PLUS ureal2 IMAG  { $$ = Object::makeCompnum($1, $3); }
          | real2 MINUS ureal2 IMAG { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | real2 PLUS IMAG         { $$ = Object::makeCompnum($1, Object::makeFixnum(1)); }
          | real2 MINUS IMAG        { $$ = Object::makeCompnum($1, Object::makeFixnum(-1)); }
          | PLUS IMAG               { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(1)); }
          | MINUS IMAG              { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(-1)); }
          | real2 PLUS naninf IMAG  { $$ = Object::makeCompnum($1, $3); }
          | real2 MINUS naninf IMAG { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | PLUS naninf IMAG        { $$ = Object::makeCompnum(Object::makeFixnum(0), $2); }
          | MINUS naninf IMAG       { $$ = Object::makeCompnum(Object::makeFixnum(0), Arithmetic::mul(-1, $2)); }



real2     : ureal2
          | sreal2
          | PLUS naninf   { $$ = Arithmetic::mul(1, $2); }
          | MINUS naninf  { $$ = Arithmetic::mul(-1, $2); }


ureal2 : uinteger2
       | uinteger2 SLASH uinteger2     { $$ = Arithmetic::div($1, $3); }
sreal2 : PLUS ureal2                   { $$ = $2; }
       | MINUS ureal2                  { $$ = Arithmetic::mul(-1, $2); }
       ;


uinteger2 : digit2  { $$ = Object::makeFixnum($1); }
          | uinteger2 digit2  {
                $$ = Arithmetic::add(Arithmetic::mul(2, $1), Object::makeFixnum($2));
            }
          ;

digit2 : DIGIT_2;
exactness : /* empty */     { $$ = 0; }
          | EXACT           { $$ = 1; }
          | INEXACT         { $$ = -1; }
          ;
prefix2 : RADIX_2 exactness { $$ = $2;}
        | exactness RADIX_2 { $$ = $1;}
        ;

naninf : MY_NAN { $$ = Flonum::NOT_A_NUMBER; }
       | MY_INF { $$ = Flonum::POSITIVE_INF; }
%%

extern ucs4char* token;
int number_yyerror(char const *str)
{
  TextualInputPort* const port = Reader::port();
    port->setError(format(UC("~a near [~a] at ~a:~d. "),
                          Pair::list4(str, Object::makeString(port->scanner()->currentToken()), port->toString(), Object::makeFixnum(port->getLineNo()))));
    return 0;
}

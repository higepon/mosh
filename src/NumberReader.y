/*
 * NumberReader.y
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
#include "EqHashTable.h"
#include "NumberScanner.h"
#include "TextualInputPort.h"
#include "TextualOutputPort.h"
#include "Arithmetic.h"
#include "Reader.h"
#include "NumberReader.h"
#include "ScannerHelper.h"
#include "Scanner.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "ProcedureMacro.h"
#include "MultiVMProcedures.h"

using namespace scheme;
extern int number_yylex(YYSTYPE* yylval);
extern int number_yyerror(const char *);
//#define YYDEBUG 1
// yydebug = 1

/* static int ucs4stringToInt(const ucs4string& text) */
/* { */
/*     int ret = strtol(text.ascii_c_str(), NULL, 10); */
/*     if (errno == ERANGE) { */
/*         MOSH_FATAL("reader suffix"); */
/*     } */
/*     return ret; */
/* } */

// e1000 => 1000 e+100 => 100, e-100 =>-100
/* static int suffix(const ucs4string& text) */
/* { */
/*     MOSH_ASSERT(text.size() > 0); */
/*     const char* p = text.ascii_c_str(); */
/*     int ret = strtol(p + 1, NULL, 10); */
/*     if (errno == ERANGE) { */
/*         MOSH_FATAL("reader suffix"); */
/*     } */
/*     return ret; */
/* } */

// text => "e100", "e+100" or "e-100" style
static Object suffixToNumberOld(const ucs4string& text)
{
    int sign = 1;
    ucs4string decimal10(UC(""));
    if (text[1] == '-') {
        sign = -1;
        decimal10 = text.substr(2, text.size() - 2);
    } else if (text[1] == '+') {
        decimal10 = text.substr(2, text.size() - 2);
    } else {
        decimal10 = text.substr(1, text.size() - 1);
    }
    Object exponent = Bignum::makeInteger(decimal10);
    if (sign == -1) {
        exponent = Arithmetic::negate(exponent);
    }
    MOSH_ASSERT(!exponent.isBignum());
    return Arithmetic::expt(Object::makeFixnum(10), exponent);
}

static Object suffixToNumber(const ucs4string& text)
{
    int sign = 1;
    ucs4string decimal10(UC(""));
    if (text[1] == '-') {
        sign = -1;
        decimal10 = text.substr(2, text.size() - 2);
    } else if (text[1] == '+') {
        decimal10 = text.substr(2, text.size() - 2);
    } else {
        decimal10 = text.substr(1, text.size() - 1);
    }
    Object exponent = Bignum::makeInteger(decimal10);
    if (sign == -1) {
        exponent = Arithmetic::negate(exponent);
    }
    return exponent;
}

%}
%code
{
  bool hasExplicitExact;
}
%initial-action
{
  hasExplicitExact = false;
}
%define api.pure
%token END_OF_FILE PLUS MINUS SLASH DOT AT MY_NAN MY_INF IMAG
%token RADIX_2 RADIX_8 RADIX_10 RADIX_16

%token <intValue> EXACT INEXACT
%token <intValue> DIGIT_2 DIGIT_8 DIGIT_10 DIGIT_16
%token <stringValue> EXPONENT_MARKER

%type <exactValue> exactness prefix2 prefix8 prefix10 prefix16
%type <intValue> digit2 digit8 digit10 digit16
%type <stringValue> uinteger10String suffix
%type <object> top_level datum  naninf
%type <object> num2  complex2   real2  ureal2   sreal2  uinteger2
%type <object> num8  complex8   real8  ureal8   sreal8  uinteger8
%type <object> num10 complex10  inexact_complex10 real10  ureal10 sreal10 uinteger10 decimal10
%type <object> num16 complex16  real16 ureal16  sreal16 uinteger16
%start top_level

%%
top_level : datum { currentVM()->numberReaderContext()->setParsed($$); YYACCEPT; }
| END_OF_FILE { currentVM()->numberReaderContext()->setParsed(Object::Eof); YYACCEPT; }

datum     : num2 | num8| num10 | num16;

num2      : prefix2 complex2 { $$ = ScannerHelper::applyExactness($1, $2); }
          ;

complex2  : real2
          | real2 AT real2          { $$ = Arithmetic::makePolar($1, $3); }
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
          | PLUS naninf  { $$ = $2; }
          | MINUS naninf { $$ = Arithmetic::mul(-1, $2); }
          ;

ureal2    : uinteger2
          | uinteger2 SLASH uinteger2 {
               bool isDiv0Error = false;
               $$ = Arithmetic::div($1, $3, isDiv0Error);
               if (isDiv0Error) {
                   yyerror("division by zero");
                   YYERROR;
               }
          }
          ;

sreal2    : PLUS ureal2  { $$ = $2; }
          | MINUS ureal2 { $$ = Arithmetic::mul(-1, $2); }
          ;

uinteger2 : digit2 { $$ = Object::makeFixnum($1); }
          | uinteger2 digit2  {
               $$ = Arithmetic::add(Arithmetic::mul(2, $1), Object::makeFixnum($2));
          }
          ;

digit2    : DIGIT_2;

num8     : prefix8 complex8 { $$ = ScannerHelper::applyExactness($1, $2); }

complex8 : real8
          | real8 AT    real8       { $$ = Arithmetic::makePolar($1, $3); }
          | real8 PLUS  ureal8 IMAG { $$ = Object::makeCompnum($1, $3); }
          | real8 MINUS ureal8 IMAG { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | real8 PLUS  IMAG         { $$ = Object::makeCompnum($1, Object::makeFixnum(1)); }
          | real8 MINUS IMAG         { $$ = Object::makeCompnum($1, Object::makeFixnum(-1)); }
          | PLUS  ureal8 IMAG        { $$ = Object::makeCompnum(Object::makeFixnum(0), $2); }
          | MINUS ureal8 IMAG        { $$ = Object::makeCompnum(Object::makeFixnum(0), Arithmetic::mul(-1, $2)); }
          | PLUS  IMAG                { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(1)); }
          | MINUS IMAG                { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(-1)); }
          | real8 PLUS  naninf IMAG  { $$ = Object::makeCompnum($1, $3); }
          | real8 MINUS naninf IMAG  { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | PLUS  naninf IMAG         { $$ = Object::makeCompnum(Object::makeFixnum(0), $2); }
          | MINUS naninf IMAG         { $$ = Object::makeCompnum(Object::makeFixnum(0), Arithmetic::mul(-1, $2)); }
          ;

real8    : ureal8
          | sreal8
          | PLUS naninf  { $$ = $2; }
          | MINUS naninf { $$ = Arithmetic::mul(-1, $2); }
          ;

ureal8   : uinteger8
          | uinteger8 SLASH uinteger8 {
               bool isDiv0Error = false;
               $$ = Arithmetic::div($1, $3, isDiv0Error);
               if (isDiv0Error) {
                   yyerror("division by zero");
                   YYERROR;
               }
          }
          ;

sreal8    : PLUS  ureal8 { $$ = $2; }
          | MINUS ureal8 { $$ = Arithmetic::mul(-1, $2); }
          ;

uinteger8 : digit8 { $$ = Object::makeFixnum($1); }
          | uinteger8 digit8  {
                $$ = Arithmetic::add(Arithmetic::mul(8, $1), Object::makeFixnum($2));
          }
          ;

digit8    : digit2
          | DIGIT_8 { $$ = $1; }
          ;

num16     : prefix16 complex16 { $$ = ScannerHelper::applyExactness($1, $2); }

complex16 : real16
          | real16 AT    real16       { $$ = Arithmetic::makePolar($1, $3); }
          | real16 PLUS  ureal16 IMAG { $$ = Object::makeCompnum($1, $3); }
          | real16 MINUS ureal16 IMAG { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | real16 PLUS  IMAG         { $$ = Object::makeCompnum($1, Object::makeFixnum(1)); }
          | real16 MINUS IMAG         { $$ = Object::makeCompnum($1, Object::makeFixnum(-1)); }
          | PLUS  ureal16 IMAG        { $$ = Object::makeCompnum(Object::makeFixnum(0), $2); }
          | MINUS ureal16 IMAG        { $$ = Object::makeCompnum(Object::makeFixnum(0), Arithmetic::mul(-1, $2)); }
          | PLUS  IMAG                { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(1)); }
          | MINUS IMAG                { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(-1)); }
          | real16 PLUS  naninf IMAG  { $$ = Object::makeCompnum($1, $3); }
          | real16 MINUS naninf IMAG  { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | PLUS  naninf IMAG         { $$ = Object::makeCompnum(Object::makeFixnum(0), $2); }
          | MINUS naninf IMAG         { $$ = Object::makeCompnum(Object::makeFixnum(0), Arithmetic::mul(-1, $2)); }
          ;

real16    : ureal16
          | sreal16
          | PLUS naninf  { $$ = $2; }
          | MINUS naninf { $$ = Arithmetic::mul(-1, $2); }
          ;

ureal16   : uinteger16
          | uinteger16 SLASH uinteger16 {
               bool isDiv0Error = false;
               $$ = Arithmetic::div($1, $3, isDiv0Error);
               if (isDiv0Error) {
                   yyerror("division by zero");
                   YYERROR;
               }
          }
          ;

sreal16   : PLUS  ureal16 { $$ = $2; }
          | MINUS ureal16 { $$ = Arithmetic::mul(-1, $2); }
          ;

uinteger16 : digit16 { $$ = Object::makeFixnum($1); }
          | uinteger16 digit16  {
                $$ = Arithmetic::add(Arithmetic::mul(16, $1), Object::makeFixnum($2));
          }
          ;

digit16   : digit10
          | DIGIT_16 { $$ = $1; }
          ;

num10     : inexact_complex10
          | prefix10 complex10 { $$ = ScannerHelper::applyExactness($1, $2); }

/*
   See test/number.scm
*/
inexact_complex10 : INEXACT real10 {
            $$ = ScannerHelper::applyExactness(-1, $2);
          }
          | INEXACT real10 PLUS ureal10 IMAG {
            if (Arithmetic::isExactZero($4)) {
              $$ = Object::makeCompnum(ScannerHelper::applyExactness(-1, $2), Object::makeFlonum(0.0));
            } else {
              $$ = ScannerHelper::applyExactness(-1, Object::makeCompnum($2, $4));
            }
          }
          | INEXACT real10 MINUS ureal10 IMAG {
            if (Arithmetic::isExactZero($4)) {
              $$ = Object::makeCompnum(ScannerHelper::applyExactness(-1, $2), Object::makeFlonum(0.0));
            } else {
              $$ = ScannerHelper::applyExactness(-1, Arithmetic::mul(-1, Object::makeCompnum($2, $4)));
            }
          }
          | INEXACT PLUS  ureal10 IMAG {
            if (Arithmetic::isExactZero($3)) {
              $$ = Object::makeCompnum(Object::makeFlonum(0.0), Object::makeFlonum(0.0));
            } else {
              $$ = Object::makeCompnum(Object::makeFlonum(0.0), ScannerHelper::applyExactness(-1, $3));
            }
          }
          | INEXACT MINUS ureal10 IMAG {
            if (Arithmetic::isExactZero($3)) {
              $$ = Object::makeCompnum(Object::makeFlonum(0.0), Object::makeFlonum(0.0));
            } else {
              $$ = Object::makeCompnum(Object::makeFlonum(0.0), ScannerHelper::applyExactness(-1, Arithmetic::mul(-1, $3)));
            }
          }
          ;

complex10 : real10
          | real10 AT    real10       { $$ = Arithmetic::makePolar($1, $3); }
          | real10 PLUS  ureal10 IMAG { $$ = Object::makeCompnum($1, $3); }
          | real10 MINUS ureal10 IMAG { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | real10 PLUS  IMAG         { $$ = Object::makeCompnum($1, Object::makeFixnum(1)); }
          | real10 MINUS IMAG         { $$ = Object::makeCompnum($1, Object::makeFixnum(-1)); }
          | PLUS  ureal10 IMAG        { $$ = Object::makeCompnum(Object::makeFixnum(0), $2); }
          | MINUS ureal10 IMAG        { $$ = Object::makeCompnum(Object::makeFixnum(0), Arithmetic::mul(-1, $2)); }
          | PLUS  IMAG                { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(1)); }
          | MINUS IMAG                { $$ = Object::makeCompnum(Object::makeFixnum(0), Object::makeFixnum(-1)); }
          | real10 PLUS  naninf IMAG  { $$ = Object::makeCompnum($1, $3); }
          | real10 MINUS naninf IMAG  { $$ = Object::makeCompnum($1, Arithmetic::mul(-1, $3)); }
          | PLUS  naninf IMAG         { $$ = Object::makeCompnum(Object::makeFixnum(0), $2); }
          | MINUS naninf IMAG         { $$ = Object::makeCompnum(Object::makeFixnum(0), Arithmetic::mul(-1, $2)); }
          ;

real10    : ureal10
          | sreal10
          | PLUS naninf  { $$ = $2; }
          | MINUS naninf { $$ = Arithmetic::mul(-1, $2); }
          ;

ureal10   : decimal10
          | uinteger10 SLASH uinteger10 {
               bool isDiv0Error = false;
               $$ = Arithmetic::div($1, $3, isDiv0Error);
               if (isDiv0Error) {
                   yyerror("division by zero");
                   YYERROR;
               }
          }
          ;

sreal10   : PLUS  ureal10 { $$ = $2; }
          | MINUS ureal10 { $$ = Arithmetic::mul(-1, $2); }
          ;

decimal10 : uinteger10String suffix {
              // This should be exact because suffix is not present.
              if ($2.empty()) {
                  $$ = Bignum::makeInteger($1);
              } else {
                  if (hasExplicitExact) {
                    // #e-1e1000
                    $$ = Arithmetic::mul(Bignum::makeInteger($1), suffixToNumberOld($2));
                  } else {
                    // 1e10000
                    $$ = ScannerHelper::applyExactness(-1, Arithmetic::mul(Bignum::makeInteger($1), suffixToNumberOld($2)));
                  }
// todo ("#e-1e-1000" (- (expt 10 -1000)))
//                   int suffixNum = suffix($2);
//                   Object z0 = Arithmetic::mul(Bignum::makeInteger($1),
//                                               Arithmetic::expt(Object::makeFixnum(10), Object::makeFixnum(suffixNum)));
//                   z0 = Arithmetic::inexact(z0);
//                   $$ = Object::makeFlonum(FlonumUtil::algorithmR(Bignum::makeInteger($1), suffixNum, z0.toFlonum()->value()));
              }
          }
          | DOT uinteger10String suffix {
              ucs4string ret(UC("."));
              ret += $2;
              if (!$3.empty()) {
                  $$ = Arithmetic::mul(Flonum::fromString(ret), suffixToNumberOld($3));
              } else {

                  $$ = Flonum::fromString(ret);
              }

          }
          | uinteger10String DOT uinteger10String suffix {
              ucs4string uinteger10 = $1;
              uinteger10 += $3;
              Object f = Bignum::makeInteger(uinteger10);
              if (!$4.empty()) {
                Object e = suffixToNumber($4);
                ucs4string fstring = $1;
                fstring += UC(".");
                fstring += $3;
                double z0 = Arithmetic::mul(Flonum::fromString(fstring), suffixToNumberOld($4)).toFlonum()->value();
                if (!e.isFixnum()) {
                  yyerror("invalid flonum expression: suffix");
                }
                if (!f.isFixnum()) {
                  yyerror("invalid flonum expression: too large significand");
                }
                const int digit = static_cast<int>($3.size());
                $$ = Object::makeFlonum(FlonumUtil::algorithmR(f, e.toFixnum() - digit, z0));
              } else {
                ucs4string ret = $1;
                ret += UC(".") + $3;
                $$ = Flonum::fromString(ret);
              }
          }
          | uinteger10String DOT suffix {
              ucs4string ret = $1;
              ret += UC(".0");
              if (!$3.empty()) {
//                  printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
                $$ = Arithmetic::mul(Flonum::fromString(ret), suffixToNumberOld($3));
              } else {
//                  printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
                  $$ = Flonum::fromString(ret);
              }
          }

          ;

uinteger10 : uinteger10String { $$ = Bignum::makeInteger($1); }

uinteger10String : digit10  {
                const ucs4char ch = '0' + $1;
                $$ = ucs4string(UC(""));
                $$ += ch;
           }
           | uinteger10String digit10  {
               const ucs4char ch = '0' + $2;
               $$ = $1;
               $$ += ch;
          }
          ;

digit10   : digit8
          | DIGIT_10         { $$ = $1; }
          ;

exactness : /* empty */     { $$ = 0; }
          | EXACT           { $$ = 1; hasExplicitExact = true; }
          | INEXACT         { $$ = -1; }
          ;

suffix    : /* empty */     { $$ = ucs4string(UC("")); }
          | EXPONENT_MARKER {
              ucs4string ret(UC("e"));
              ret += $1.substr(1, $1.size() - 1);
              $$ = ret;
          }
          ;

prefix2   : RADIX_2 exactness { $$ = $2; }
          | exactness RADIX_2 { $$ = $1; }
          ;

prefix10  : RADIX_10 exactness  { $$ = $2;}
          | exactness RADIX_10  { $$ = $1;}
          | exactness /* {yydebug = 1; }*/
          ;

prefix8   : RADIX_8 exactness  { $$ = $2;}
          | exactness RADIX_8  { $$ = $1;}
          ;

prefix16  : RADIX_16 exactness  { $$ = $2;}
          | exactness RADIX_16  { $$ = $1;}
          ;

naninf    : MY_NAN { $$ = Flonum::NOT_A_NUMBER; }
          | MY_INF { $$ = Flonum::POSITIVE_INF; }
%%

extern ucs4char* token;
int number_yyerror(char const *str)
{
  TextualInputPort* const port = currentVM()->numberReaderContext()->port();
  port->setError(format(nullptr, UC("~a near [~a] at ~a:~d. "),
                        Pair::list4(Object(str), Object::makeString(port->scanner()->currentToken()), Object(port->toString()), Object::makeFixnum(port->getLineNo()))));
  return 0;
}

%{
#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Symbol.h"
#include "Vector.h"
#include "SString.h"
#include "ByteVector.h"
#include "ByteArrayBinaryInputPort.h"
#include "StringProcedures.h"
#include "TextualInputPort.h"
#include "ByteVectorProcedures.h"
#include "Arithmetic.h"
#include "Codec.h"
#include "reader.h"
#include "Scanner.h"
using namespace scheme;
extern ucs4string readString(const ucs4string& s);
extern Object applyExactness(int exactness, Object num);
extern int yylex();
extern int yyerror(const char *);
extern TextualInputPort* parser_port();
Object parsed;
%}

%token <stringValue> IDENTIFIER
%token <boolValue> BOOLEAN
%token <stringValue> STRING
%token <intValue> CHARACTER
%token <intValue> CHARACTER_NAME
%token <stringValue> REGEXP
%token <intValue> NUMBER
%token <stringValue> NUMBER2
 //%token <intValue> DIGIT_2
 //%token RADIX_2 EXACT INEXACT MINUS PLUS
 //%type <exactValue> exactness prefix2
%token LEFT_PAREN RIGHT_PAREN END_OF_FILE VECTOR_START BYTE_VECTOR_START DOT
%token ABBV_QUASIQUOTE ABBV_QUOTE ABBV_UNQUOTESPLICING ABBV_UNQUOTE
%token ABBV_SYNTAX ABBV_QUASISYNTAX ABBV_UNSYNTAXSPLICING ABBV_UNSYNTAX
%type <object> datum lexme_datum top_level compound_datum list datum_list
%type <object> /* symbol*/ vector bytevector abbreviation //num2 uinteger2 complex2 real2 ureal2 sreal2
 //%type <intValue> digit2
%start top_level

%%
top_level : datum { parsed = $$; YYACCEPT; }
          | END_OF_FILE { parsed = Object::Eof; YYACCEPT; }
datum : lexme_datum    { $$ = $1;}
      | compound_datum
      {
          $$ = $1;
      }
      ;

lexme_datum : BOOLEAN { $$ = $1 ? Object::True : Object::False; }
            | STRING
            {
                $$ = readString($1);
            }
            | REGEXP
            {
                $$ = Object::makeRegexp($1);
            }
            | NUMBER { $$ = Object::makeFixnum($1); }
            | NUMBER2 { $$ = Object::makeString($1); }
            | IDENTIFIER
            {
                $$ = Symbol::intern($1.strdup());
            }
            | CHARACTER
            {
                $$ = Object::makeChar($1);
            }
            ;
compound_datum : list
               {
                   $$ = $1;
               }
               | vector { $$ = $1; }
               | bytevector { $$ = $1; }
               ;

list : LEFT_PAREN datum_list RIGHT_PAREN
       {
           // TODO: not to use reverse.
           $2 = Pair::reverse($2);
           if ($2.isPair()) {
                $2.toPair()->sourceInfo = Pair::list2(Object::makeString(parser_port()->toString()),
                                                      Object::makeFixnum(parser_port()->getLineNo()));
           }
           $$ = $2;
       }
     | LEFT_PAREN datum_list datum DOT datum RIGHT_PAREN
       {
           $2 = Pair::reverse($2);
           $$ = Pair::appendD2($2, Object::cons($3, $5));
       }
     | abbreviation datum { $$ = Object::cons($1, Object::cons($2, Object::Nil)); }
     ;
vector : VECTOR_START datum_list RIGHT_PAREN { $$ = Object::makeVector(Pair::reverse($2)); }
     ;
bytevector : BYTE_VECTOR_START datum_list RIGHT_PAREN
            {
              const Object bytevector = u8ListToByteVector(Pair::reverse($2));
              if (bytevector.isNil()) {
                yyerror("malformed bytevector literal #vu8(...)");
                YYERROR;
              } else {
                $$ = bytevector;
              }
            }
     ;
datum_list : datum_list datum
           {
                 $$ = Object::cons($2, $1);
           }
           | {$$ = Object::Nil; }
           ;
abbreviation : ABBV_QUOTE                          { $$ = Symbol::QUOTE; }
| ABBV_UNQUOTESPLICING                             { $$ = Symbol::UNQUOTE_SPLICING; }
| ABBV_QUASIQUOTE                                  { $$ = Symbol::QUASIQUOTE; }
| ABBV_UNQUOTE                                     { $$ = Symbol::UNQUOTE; }
| ABBV_SYNTAX                                      { $$ = Symbol::SYNTAX;}
| ABBV_UNSYNTAXSPLICING                            { $$ = Symbol::UNSYNTAX_SPLICING; }
| ABBV_QUASISYNTAX                                 { $$ = Symbol::QUASISYNTAX; }
| ABBV_UNSYNTAX                                    { $$ = Symbol::UNSYNTAX; }

// /*exactness : /* empty */     { printf("<empty>");$$ = 0; }
//           | EXACT           { printf("<exact>");$$ = 1; }
//           | INEXACT         { printf("<inexact>");$$ = -1; }
//           ;

// num2      : prefix2 complex2    { printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);$$ = applyExactness($1, $2); }//Object::Nil;/*ApplyExactness($1, $2);*/ }
// complex2  : real2 { $$ = $1; }
// real2     : ureal2 { $$ = $1; }
//           | sreal2 { $$ = $1; }
// ureal2    : uinteger2 { printf("<ureal>");$$ = $1; }
// sreal2    : PLUS ureal2                   { $$ = $2; }
//           | MINUS ureal2                  { $$ = Arithmetic::mul(Object::makeFixnum(-1), $2); }
//           ;

// digit2    : DIGIT_2 { printf("<digit2>");$$ = $1; }
// uinteger2 : digit2  { printf("<uinteger2>");$$ = Object::makeFixnum($1); }
//           | uinteger2 digit2  { printf("<uinteger2>2");$$ = Arithmetic::add(Arithmetic::mul(Object::makeFixnum(2), $1), Object::makeFixnum($2)); }
//           ;

// prefix2   : RADIX_2 exactness            { printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout); $$ = $2;}
//           | exactness RADIX_2            { printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);$$ = $1;}
//           ;

// */
%%

extern ucs4char* token;
int yyerror(char const *str)
{
    TextualInputPort* const port = parser_port();
    port->setError(format(UC("~a near [~a] at ~a:~d. "),
                          Pair::list4(str, Object::makeString(port->scanner()->currentToken()), port->toString(), Object::makeFixnum(port->getLineNo()))));
    return 0;
}

%{
#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Symbol.h"
#include "SString.h"
#include "ByteVector.h"
#include "ByteArrayBinaryInputPort.h"
#include "StringProcedures.h"
#include "TextualInputPort.h"
#include "ByteVectorProcedures.h"
#include "Codec.h"
#include "reader.h"
#include "Scanner.h"
using namespace scheme;
extern ucs4string readString(const ucs4string& s);
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
%token LEFT_PAREN RIGHT_PAREN END_OF_FILE VECTOR_START BYTE_VECTOR_START DOT
%token ABBV_QUASIQUOTE ABBV_QUOTE ABBV_UNQUOTESPLICING ABBV_UNQUOTE
%token ABBV_SYNTAX ABBV_QUASISYNTAX ABBV_UNSYNTAXSPLICING ABBV_UNSYNTAX
%type <object> datum lexme_datum top_level compound_datum list datum_list
%type <object> /* symbol*/ vector bytevector abbreviation
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
            | NUMBER { $$ = Object::makeInt($1); }
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
           if ($2.isPair()) {
                $2.toPair()->sourceInfo = Pair::list2(Object::makeString(parser_port()->toString()),
                                                      Object::makeInt(parser_port()->getLine()));
           }
           $$ = $2;
       }
     | LEFT_PAREN datum_list datum DOT datum RIGHT_PAREN
       {
           $$ = Pair::appendD2($2, Object::cons($3, $5));
       }
     | abbreviation datum { $$ = Object::cons($1, Object::cons($2, Object::Nil)); }
     ;
vector : VECTOR_START datum_list RIGHT_PAREN { $$ = Object::makeVector($2); }
     ;
bytevector : BYTE_VECTOR_START datum_list RIGHT_PAREN
            {
              const Object bytevector = u8ListToByteVector($2);
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
               $$ = Pair::appendD2($1, Pair::list1($2));
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

%%

extern ucs4char* token;
int yyerror(char const *str)
{
    TextualInputPort* const port = parser_port();
    port->setError(format(UC("~a near [~a] at ~a:~d. "),
                          Pair::list4(str, Object::makeString(port->scanner()->currentToken()), port->toString(), Object::makeInt(port->getLine()))));
    return 0;
}

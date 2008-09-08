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
#include "Codec.h"
#include "reader.h"
#define YYDEBUG 1
using namespace scheme;
extern Codec* parser_codec();
extern ucs4string readString(const ucs4string& s);
extern int yylex();
extern int yyerror(const char *);
extern char* yytext;
extern int yyleng;
Object parsed;
%}

%token <stringValue> IDENTIFIER
%token <boolValue> BOOLEAN
%token <stringValue> STRING
%token <stringValue> CHARACTER
%token <intValue> NUMBER
%token LEFT_PAREN RIGHT_PAREN END_OF_FILE VECTOR_START BYTE_VECTOR_START DOT
%token ABBV_QUASIQUOTE ABBV_QUOTE ABBV_UNQUOTESPLICING ABBV_UNQUOTE
%token ABBV_SYNTAX ABBV_QUASISYNTAX ABBV_UNSYNTAXSPLICING ABBV_UNSYNTAX
%type <object> datum lexme_datum compound_datum list datum_list top_level
%type <object> symbol vector bytevector abbreviation
%start top_level

%%
top_level: datum { parsed = $$; YYACCEPT; }
datum : lexme_datum    { $$ = $1;}
      | compound_datum { $$ = $1;}
      ;

lexme_datum : BOOLEAN { $$ = $1 ? Object::True : Object::False; }
            | STRING
            {
                ucs4string text = parser_codec()->readWholeString(new ByteArrayBinaryInputPort((uint8_t*)$1, yyleng));
                $$ = readString(text);
            }
            | NUMBER { $$ = Object::makeInt($1); }
            | symbol
            | CHARACTER
            {
                $$ = Object::makeChar(parser_codec()->in(new ByteArrayBinaryInputPort((uint8_t*)$1, yyleng)));
            }
            | END_OF_FILE { $$ = Object::Eof; }
            ;
symbol : IDENTIFIER
         {
             ucs4string text = parser_codec()->readWholeString(new ByteArrayBinaryInputPort((uint8_t*)$1,
                                                                                            yyleng));
             $$ = Symbol::intern(text.c_str());
         }
       ;
compound_datum : list { $$ = $1; }
               | vector { $$ = $1; }
               | bytevector { $$ = $1; }
               ;

list : LEFT_PAREN datum_list RIGHT_PAREN { $$ = $2; }
     | LEFT_PAREN datum_list datum DOT datum RIGHT_PAREN
       {
         $$ = Pair::appendD2($2, Object::cons($3, $5));
       }
     | abbreviation datum { $$ = Object::cons($1, Object::cons($2, Object::Nil)); }
     ;
vector : VECTOR_START datum_list RIGHT_PAREN { $$ = Object::makeVector($2); }
     ;
bytevector : BYTE_VECTOR_START datum_list RIGHT_PAREN { $$ = Object::makeByteVector($2); }
     ;
datum_list : /* empty */ { $$ = Object::Nil; }
           | datum_list datum { $$ = Pair::appendD2($1, Pair::list1($2)); }
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
int
yyerror(char const *str)
{
/*     extern char *yytext; */
     fprintf(stderr, "parser error near %s\n", yytext); 
    return 0;
}

/* int main(void) */
/* { */
/*     extern int yyparse(void); */
/*     extern FILE *yyin; */

/*     yyin = stdin; */
/*     if (yyparse()) { */
/*         fprintf(stderr, "Error ! Error ! Error !\n"); */
/*         exit(1); */
/*     } */
/*     parsed.print(); */
/* } */

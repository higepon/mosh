%{
#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "reader.h"
#define YYDEBUG 1
using namespace scheme;
extern int yylex();
extern int yyerror(const char *);
extern char* yytext;
Object parsed;
%}

%token <Object> IDENTIFIER
%token <Object> STRING
%token <boolValue> BOOLEAN
%token LEFT_PAREN RIGHT_PAREN END_OF_FILE
%type <object> datum lexme_datum compound_datum list datum_list top_level
%start top_level

%%
top_level: datum { parsed = $$; YYACCEPT; }
datum : lexme_datum    { $$ = $1;}
      | compound_datum { $$ = $1;}
      ;

lexme_datum : BOOLEAN { $$ = $1 ? Object::True : Object::False; }
            | END_OF_FILE { $$ = Object::Eof; }
            ;

compound_datum : list { $$ = $1; }
               ;

list : LEFT_PAREN datum_list RIGHT_PAREN { $$ = $2; }
     ;
datum_list : /* empty */ { $$ = Object::Nil; }
           | datum_list datum { $$ = Pair::appendD2($1, Pair::list1($2)); }
           ;

%%
int
yyerror(char const *str)
{
/*     extern char *yytext; */
/*     fprintf(stderr, "parser error near %s\n", yytext); */
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

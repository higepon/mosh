/*
 * Reader.y
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
#include "Symbol.h"
#include "Vector.h"
#include "SString.h"
#include "ByteVector.h"
#include "ByteArrayBinaryInputPort.h"
#include "StringProcedures.h"
#include "TextualInputPort.h"
#include "ByteVectorProcedures.h"
#include "Arithmetic.h"
#include "EqHashTable.h"
#include "Reader.h"
#include "NumberReader.h"
#include "Scanner.h"
#include "VM.h"
#include "MultiVMProcedures.h"
using namespace scheme;
extern int yylex(YYSTYPE *lvalp);
extern int yyerror(const char *);
%}
%pure_parser
%token <stringValue> IDENTIFIER
%token <boolValue> SCHEME_BOOLEAN
%token <stringValue> STRING
%token <intValue> CHARACTER
%token <intValue> CHARACTER_NAME
%token <intValue> DEFINING_SHARED
%token <intValue> DEFINED_SHARED
%token <stringValue> REGEXP
%token <stringValue> NUMBER NUMBER2 NUMBER8 NUMBER10 NUMBER16
%token <charValue> LEFT_PAREN RIGHT_PAREN 
%token END_OF_FILE VECTOR_START BYTE_VECTOR_START DOT DATUM_COMMENT
%token ABBV_QUASIQUOTE ABBV_QUOTE ABBV_UNQUOTESPLICING ABBV_UNQUOTE
%token ABBV_SYNTAX ABBV_QUASISYNTAX ABBV_UNSYNTAXSPLICING ABBV_UNSYNTAX

%type <object> datum lexme_datum top_level compound_datum list datum_list defining_datum defined_datum
%type <object> vector bytevector abbreviation

%start top_level

%%

top_level      : END_OF_FILE { currentVM()->readerContext()->setParsed(Object::Eof); YYACCEPT; }
               | datum { currentVM()->readerContext()->setParsed($$); YYACCEPT; }
               ;

datum          : lexme_datum
               | compound_datum
               | defining_datum
               | defined_datum
               | DATUM_COMMENT datum datum { $$ = $3; }
               | DATUM_COMMENT datum END_OF_FILE { currentVM()->readerContext()->setParsed(Object::Eof); YYACCEPT; }
               | DATUM_COMMENT lexme_datum { $$ = Object::Ignore; }
               | DATUM_COMMENT compound_datum { $$ = Object::Ignore; }
               ;

defined_datum : DEFINED_SHARED {
                 if (currentVM()->readerContext()->port()->isStrictR6RsReaderMode()) {
                     yyerror("#1# style is not allowed on #!r6rs mode.");
                     YYERROR;
                 } else {
                     currentVM()->readerContext()->setIsSharedStructureFound();
                     $$ = Object::makeSharedReference($1);
                 }
               };

defining_datum : DEFINING_SHARED datum {
                 if (currentVM()->readerContext()->port()->isStrictR6RsReaderMode()) {
                     yyerror("#1= style is not allowed on #!r6rs mode.");
                     YYERROR;
                 } else {
                     currentVM()->readerContext()->setIsSharedStructureFound();
                     currentVM()->readerContext()->addShared($1, $2);
                     $$ = $2;
                 }
               };

lexme_datum    : SCHEME_BOOLEAN { $$ = $1 ? Object::True : Object::False; }
               | STRING {
                 ucs4string s = ReaderHelper::readString($1);
                 $$ = Object::makeString(s);
               }
               | REGEXP {
                 if (currentVM()->readerContext()->port()->isStrictR6RsReaderMode()) {
                   yyerror("Regexp literal is not allowed on #!r6rs mode");
                   YYERROR;
                 } else {
                   $$ = Object::makeRegexp($1, false, false);
                 }
               }
               | NUMBER {
                   bool isErrorOccured = false;
                   $$ = currentVM()->numberReaderContext()->read($1, isErrorOccured);
                   if (isErrorOccured) {
                       yyerror("invalid binary number sequence");
                       YYERROR;
                   }
               }
               | IDENTIFIER {
                   $$ = Symbol::intern(ReaderHelper::readSymbol($1).strdup());
               }
               | CHARACTER {
                    $$ = Object::makeChar($1);
               }
               ;

compound_datum : list
               | vector
               | bytevector
               ;

list           : LEFT_PAREN datum_list RIGHT_PAREN {
                   if (!(($1 == '(' && $3 == ')') ||
                         ($1 == '[' && $3 == ']'))) {
                       yyerror("mismatched parentheses");
                       YYERROR;
                   }
                   // TODO: not to use reverse.
                   $2 = Pair::reverse($2);
                   if ($2.isPair()) {
                       $2 = Object::makeAnnoatedPair($2.car(),
                                                     $2.cdr(),
                                                     Pair::list2(Object::makeString(currentVM()->readerContext()->port()->toString()),
                                                                 Object::makeFixnum(currentVM()->readerContext()->port()->getLineNo())));
                   }
                   $$ = $2;
               }
               | LEFT_PAREN datum_list datum DOT datum RIGHT_PAREN {
                   if (!(($1 == '(' && $6 == ')') ||
                         ($1 == '[' && $6 == ']'))) {
                       yyerror("mismatched parentheses");
                       YYERROR;
                   }
                   $2 = Pair::reverse($2);
                   $$ = Pair::appendD2($2, Object::cons($3, $5));
               }
               | abbreviation datum { $$ = Object::cons($1, Object::cons($2, Object::Nil)); }
               ;

vector         : VECTOR_START datum_list RIGHT_PAREN {
                    if ($3 != ')') {
                        yyerror("vector terminated by bracket");
                        YYERROR;
                    }
                    $$ = Object::makeVector(Pair::reverse($2));
               }
               ;

bytevector     : BYTE_VECTOR_START datum_list RIGHT_PAREN {
                    if ($3 != ')') {
                        yyerror("bytevector terminated by bracket");
                        YYERROR;
                    }

                    const Object bytevector = u8ListToByteVector(Pair::reverse($2));
                    if (bytevector.isNil()) {
                        yyerror("malformed bytevector literal #vu8(...)");
                        YYERROR;
                    } else {
                        $$ = bytevector;
                    }
               }
               ;

datum_list     : datum_list datum {
                    if ($2 == Object::Ignore) {
                        $$ = $1;
                    } else {
                        $$ = Object::cons($2, $1);
                    }
               }
               | {$$ = Object::Nil; }
               ;

abbreviation   : ABBV_QUOTE            { $$ = Symbol::QUOTE; }
               | ABBV_UNQUOTESPLICING  { $$ = Symbol::UNQUOTE_SPLICING; }
               | ABBV_QUASIQUOTE       { $$ = Symbol::QUASIQUOTE; }
               | ABBV_UNQUOTE          { $$ = Symbol::UNQUOTE; }
               | ABBV_SYNTAX           { $$ = Symbol::SYNTAX;}
               | ABBV_UNSYNTAXSPLICING { $$ = Symbol::UNSYNTAX_SPLICING; }
               | ABBV_QUASISYNTAX      { $$ = Symbol::QUASISYNTAX; }
               | ABBV_UNSYNTAX         { $$ = Symbol::UNSYNTAX; }

%%

extern ucs4char* token;
int yyerror(char const *str)
{
    TextualInputPort* const port = currentVM()->readerContext()->port();
    const Object prevError = port->error();
    ucs4string currentToken = port->scanner()->currentToken();
    if (currentToken.empty()) {
        currentToken = ucs4string(UC("<end of file>"));
    }
    if (prevError.isNil()) {
        port->setError(format(nullptr, UC("~a near [~a] at ~a:~d. "),
                              Pair::list4(Object(str),
                                          Object::makeString(currentToken),
                                          Object(port->toString()),
                                          Object::makeFixnum(port->getLineNo()))));

    } else {
        port->setError(format(nullptr, UC("~a: ~a near [~a] at ~a:~d. "),
                              Pair::list5(prevError,
                                          Object(str),
                                          Object::makeString(currentToken),
                                          Object(port->toString()),
                                          Object::makeFixnum(port->getLineNo()))));
    }
    return 0;
}

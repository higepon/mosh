/*
 * Symbol.h - <symbol>
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
 *   Copyright (c) 2008  Kokosabu(MIURA Yasuyuki)  <kokosabu@gmail.com>
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
 *  $Id$
 */

#ifndef SCHEME_SYMBOL_H_
#define SCHEME_SYMBOL_H_

#include "scheme.h"

namespace scheme {

typedef gc_map2  Symbols;
class Mutex;

class Symbol EXTEND_GC
{
private:
    const ucs4char* const name_;
    ucs4char* savedName_;

    // N.B.
    // Once this is static Symbols symbols.
    // But this causes crash on multi thread.
    // If main thread exit earlier than sub thread.
    // main thread's exit() calls static destructor and frees symbols.
    static Symbols* symbols;
    static Mutex* mutex;

public:
    Symbol(const ucs4char* name) : name_(name)
    {
#ifdef DEBUG_VERSION
        savedName_ = ucs4string(name).strdup();
        MOSH_ASSERT(savedName_ != NULL);
        ucs4string text2(savedName_);
#endif
    }
    const ucs4char* c_str();
    static Object intern(const ucs4char* name);
    static bool isInterned(Object symbol);
    static Object add(const ucs4char* name);

    static Object QUOTE;
    static Object QUASIQUOTE;
    static Object UNQUOTE;
    static Object UNQUOTE_SPLICING;
    static Object QUOTE_B;
    static Object QUASIQUOTE_B;
    static Object UNQUOTE_B;
    static Object UNQUOTE_SPLICING_B;
    static Object SYNTAX;
    static Object QUASISYNTAX;
    static Object UNSYNTAX;
    static Object UNSYNTAX_SPLICING;
    static Object SYNTAX_B;
    static Object QUASISYNTAX_B;
    static Object UNSYNTAX_B;
    static Object UNSYNTAX_SPLICING_B;


    static Object AFTER;
    static Object BEFORE;
    static Object TOP_LEVEL;
    static Object BIG;
    static Object LITTLE;

    // <file-options symbol>
    static Object NO_CREATE;
    static Object NO_FAIL;
    static Object NO_TRUNCATE;

    // <buffer-mode symbol>
    static Object NONE;
    static Object LINE;
    static Object BLOCK;

    // <eol-style symbol>
    static Object LF;
    static Object CR;
    static Object CRLF;
    static Object NEL;
    static Object CRNEL;
    static Object LS;

    // <error-handling symbol>
    static Object IGNORE_ERROR;
    static Object RAISE_ERROR;
    static Object REPLACE_ERROR;

    // psyntax
    static Object UNINITIALIZED;
    static Object SHIFT;

    static void initBuitinSymbols();
};

} // namespace scheme

#endif // SCHEME_SYMBOL_H_

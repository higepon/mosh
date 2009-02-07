/*
 * Symbol.cpp - Builtin Symbols.
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

#include "Object.h"
#include "Object-inl.h"
#include "Symbol.h"
using namespace scheme;

Symbols Symbol::symbols;
Object Symbol::QUOTE;
Object Symbol::QUASIQUOTE;
Object Symbol::UNQUOTE;
Object Symbol::UNQUOTE_SPLICING;
Object Symbol::AFTER;
Object Symbol::BEFORE;
Object Symbol::TOP_LEVEL;
Object Symbol::SYNTAX;
Object Symbol::QUASISYNTAX;
Object Symbol::UNSYNTAX;
Object Symbol::UNSYNTAX_SPLICING;
Object Symbol::BIG;
Object Symbol::LITTLE;
Object Symbol::LF;
Object Symbol::CR;
Object Symbol::CRLF;
Object Symbol::NEL;
Object Symbol::CRNEL;
Object Symbol::LS;
Object Symbol::IGNORE;
Object Symbol::RAISE;
Object Symbol::REPLACE;
Object Symbol::NONE;
Object Symbol::LINE;
Object Symbol::BLOCK;
Object Symbol::NO_CREATE;
Object Symbol::NO_FAIL;
Object Symbol::NO_TRUNCATE;

void Symbol::initBuitinSymbols()
{
    QUOTE             = Symbol::intern(UC("quote"));
    QUASIQUOTE        = Symbol::intern(UC("quasiquote"));
    UNQUOTE           = Symbol::intern(UC("unquote"));
    UNQUOTE_SPLICING  = Symbol::intern(UC("unquote-splicing"));
    AFTER             = Symbol::intern(UC("after"));
    BEFORE            = Symbol::intern(UC("before"));
    TOP_LEVEL         = Symbol::intern(UC("top level"));
    SYNTAX            = Symbol::intern(UC("syntax"));
    QUASISYNTAX       = Symbol::intern(UC("quasisyntax"));
    UNSYNTAX          = Symbol::intern(UC("unsyntax"));
    UNSYNTAX_SPLICING = Symbol::intern(UC("unsyntax-splicing"));
    BIG               = Symbol::intern(UC("big"));
    LITTLE            = Symbol::intern(UC("little"));

    // <file-options symbol>
    NO_CREATE         = Symbol::intern(UC("no-create"));
    NO_FAIL           = Symbol::intern(UC("no-fail"));
    NO_TRUNCATE       = Symbol::intern(UC("no-truncate"));

    // <buffer-mode symbol>
    NONE              = Symbol::intern(UC("none"));
    LINE              = Symbol::intern(UC("line"));
    BLOCK             = Symbol::intern(UC("block"));

    // <eol-style symbol>
    LF                = Symbol::intern(UC("lf"));
    CR                = Symbol::intern(UC("cr"));
    CRLF              = Symbol::intern(UC("crlf"));
    NEL               = Symbol::intern(UC("nel"));
    CRNEL             = Symbol::intern(UC("crnel"));
    LS                = Symbol::intern(UC("ls"));

    // <error-handling-mode symbol>
    IGNORE            = Symbol::intern(UC("ignore"));
    RAISE             = Symbol::intern(UC("raise"));
    REPLACE           = Symbol::intern(UC("replace"));
}

const ucs4char* Symbol::c_str()
{

// N.B. Don't pass the local pointer to Symbol::intern
#ifdef DEBUG_VERSION
    ucs4string text(name_);
    ucs4string text2(savedName_);
    MOSH_ASSERT(text.size() == text2.size());
    MOSH_ASSERT(memcmp(savedName_, name_, text.size()) == 0);
#endif
    return name_;
}

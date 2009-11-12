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
#include "OSCompatThread.h"
using namespace scheme;

Symbols* Symbol::symbols;
Mutex* Symbol::mutex;
Object Symbol::QUOTE;
Object Symbol::QUASIQUOTE;
Object Symbol::UNQUOTE;
Object Symbol::UNQUOTE_SPLICING;
Object Symbol::QUOTE_B;
Object Symbol::QUASIQUOTE_B;
Object Symbol::UNQUOTE_B;
Object Symbol::UNQUOTE_SPLICING_B;
Object Symbol::SYNTAX;
Object Symbol::QUASISYNTAX;
Object Symbol::UNSYNTAX;
Object Symbol::UNSYNTAX_SPLICING;
Object Symbol::SYNTAX_B;
Object Symbol::QUASISYNTAX_B;
Object Symbol::UNSYNTAX_B;
Object Symbol::UNSYNTAX_SPLICING_B;

Object Symbol::AFTER;
Object Symbol::BEFORE;
Object Symbol::TOP_LEVEL;
Object Symbol::BIG;
Object Symbol::LITTLE;
Object Symbol::LF;
Object Symbol::CR;
Object Symbol::CRLF;
Object Symbol::NEL;
Object Symbol::CRNEL;
Object Symbol::LS;
Object Symbol::IGNORE_ERROR;
Object Symbol::RAISE_ERROR;
Object Symbol::REPLACE_ERROR;
Object Symbol::NONE;
Object Symbol::LINE;
Object Symbol::BLOCK;
Object Symbol::NO_CREATE;
Object Symbol::NO_FAIL;
Object Symbol::NO_TRUNCATE;
Object Symbol::UNINITIALIZED;
Object Symbol::SHIFT;

void Symbol::initBuitinSymbols()
{
    symbols = new Symbols;
    mutex = new Mutex;
    QUOTE             = Symbol::intern(UC("quote"));
    QUASIQUOTE        = Symbol::intern(UC("quasiquote"));
    UNQUOTE           = Symbol::intern(UC("unquote"));
    UNQUOTE_SPLICING  = Symbol::intern(UC("unquote-splicing"));
    QUOTE_B           = Symbol::intern(UC("QUOTE"));
    QUASIQUOTE_B      = Symbol::intern(UC("QUASIQUOTE"));
    UNQUOTE_B         = Symbol::intern(UC("UNQUOTE"));
    UNQUOTE_SPLICING_B= Symbol::intern(UC("UNQUOTE-SPLICING"));
    SYNTAX             = Symbol::intern(UC("syntax"));
    QUASISYNTAX        = Symbol::intern(UC("quasisyntax"));
    UNSYNTAX           = Symbol::intern(UC("unsyntax"));
    UNSYNTAX_SPLICING  = Symbol::intern(UC("unsyntax-splicing"));
    SYNTAX_B           = Symbol::intern(UC("SYNTAX"));
    QUASISYNTAX_B      = Symbol::intern(UC("QUASISYNTAX"));
    UNSYNTAX_B         = Symbol::intern(UC("UNSYNTAX"));
    UNSYNTAX_SPLICING_B= Symbol::intern(UC("UNSYNTAX-SPLICING"));


    AFTER             = Symbol::intern(UC("after"));
    BEFORE            = Symbol::intern(UC("before"));
    TOP_LEVEL         = Symbol::intern(UC("top level"));
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
    IGNORE_ERROR      = Symbol::intern(UC("ignore"));
    RAISE_ERROR       = Symbol::intern(UC("raise"));
    REPLACE_ERROR     = Symbol::intern(UC("replace"));

    UNINITIALIZED = Symbol::intern(UC("uninitialized"));
    SHIFT = Symbol::intern(UC("shift"));
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

Object Symbol::intern(const ucs4char* name)
{
    mutex->lock();
    Symbols::const_iterator it = (*symbols).find(name);
    if (it == (*symbols).end()) {
        const Object o = Object::makeSymbol(name);
//            symbols.insert(std::pair<const ucs4char*, Object>(name, o));
        (*symbols)[name] = o;
        mutex->unlock();
        return o;
    } else {
        mutex->unlock();
        return (*it).second;
    }
}
bool Symbol::isInterned(Object symbol)
{
    MOSH_ASSERT(symbol.isSymbol());
    return symbol == Symbol::intern(symbol.toSymbol()->c_str());
}

Object Symbol::add(const ucs4char* name)
{
    Object sym = Object::makeSymbol(name);
    mutex->lock();
    (*symbols)[name] = sym;
    mutex->unlock();
    return sym;
}

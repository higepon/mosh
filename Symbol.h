/*
 * Symbol.h - <symbol>
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
 *  $Id$
 */

#ifndef __SCHEME_SYMBOL_H__
#define __SCHEME_SYMBOL_H__

#include "scheme.h"

namespace scheme {


//extern int strcmp99(const ucs4char *s1, const ucs4char *s2);
struct ltstr EXTEND_GC
{
  bool operator()(const ucs4char* s1, const ucs4char* s2) const
  {
      while (*s1 == *s2++) {
          if (*s1++=='\0') {
              return false;
          }
      }
    return(*s1 - *--s2) < 0;
  }
};

#ifdef USE_BOEHM_GC
class gc_map2 : public std::map<const ucs4char* const, Object, ltstr, gc_allocator<std::pair<const ucs4char* const, Object> > >, public gc { };
#else
    class gc_map2 : public std::map<const ucs4char* const, Object, ltstr, std::allocator<std::pair<const ucs4char* const, Object> > > {};
#endif


typedef gc_map2  Symbols;

class Symbol EXTEND_GC
{
private:
    const ucs4char* const name_;
    const ucs4char* savedName_;
    static Symbols symbols;

public:
    Symbol(const ucs4char* name) : name_(name)
    {
#ifdef DEBUG_VERSION
        savedName_ = ucs4string(name).strdup();
#endif
    }
    const ucs4char* c_str()
    {

// N.B. Don't pass the local pointer to Symbol::intern
#ifdef DEBUG_VERSION
        ucs4string text(name_);
        MOSH_ASSERT(memcmp(savedName_, name_, text.size() * sizeof(ucs4char)) == 0);
#endif
        return name_;

    }
    static Object intern(const ucs4char* name)
    {
        Symbols::const_iterator it = symbols.find(name);
        if (it == symbols.end()) {
            const Object o = Object::makeSymbol(name);
//            symbols.insert(std::pair<const ucs4char*, Object>(name, o));
            symbols[name] = o;
            return o;
        } else {
            return (*it).second;
        }
    }
    static Object add(const ucs4char* name)
    {
        return (symbols[name] = Object::makeSymbol(name));
    }

    static Object QUOTE;
    static Object QUASIQUOTE;
    static Object UNQUOTE;
    static Object UNQUOTE_SPLICING;
    static Object AFTER;
    static Object BEFORE;
    static Object TOP_LEVEL;
    static Object SYNTAX;
    static Object QUASISYNTAX;
    static Object UNSYNTAX;
    static Object UNSYNTAX_SPLICING;
    static void initBuitinSymbols();
};

}; // namespace scheme

#endif // __SCHEME_SYMBOL_H__

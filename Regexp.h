/*
 * Regexp.h - <regexp>
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

#ifndef __SCHEME_REGEXP_H__
#define __SCHEME_REGEXP_H__

#include "scheme.h"
#include "oniguruma.h"

namespace scheme {

// depend on theVM instance for error reporting.
class Regexp EXTEND_GC
{
public:
    Regexp(const ucs4string& pattern, bool caseFold);
    ~Regexp() {}

    Object match(const ucs4string& text);
    const ucs4string& pattern() { return pattern_; }

    Object replace(Object t, Object subst);
    ucs4string replace(ucs4string& t, ucs4string& subst, bool& matched);
    Object replaceAll(Object t, Object subst);

private:
    OnigRegion* matchInternal(const ucs4string& text);

    ucs4string pattern_;
    regex_t* regexp_;
    OnigErrorInfo einfo_;
};

class RegMatch EXTEND_GC
{
public:
    RegMatch(OnigRegion* region, const ucs4string& text) : region_(region), text_(text)
    {
    }

    int matchStart(int index);
    int matchEnd(int index);
    Object matchAfter(int index);
    Object matchBefore(int index);
    Object matchSubString(int index);

private:
    OnigRegion* region_;
    const ucs4string& text_;
};

}; // namespace scheme

#endif // __SCHEME_REGEXP_H__

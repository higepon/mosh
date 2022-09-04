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

#ifndef SCHEME_REGEXP_H_
#define SCHEME_REGEXP_H_

#include "scheme.h"
#include "oniguruma_compat.h"

namespace scheme {

class Regexp EXTEND_GC
{
public:
    Regexp(const ucs4string& pattern, bool caseFold, bool isSingleLine);
    ~Regexp() = default;

    Object match(const ucs4string& text);
    const ucs4string& pattern() { return pattern_; }

    Object replace(Object t, Object subst);
    ucs4string replace(ucs4string& t, ucs4string& subst, bool& matched);
    Object replaceAll(Object t, Object subst);
    bool isErrorOccured() const;
    Object errorMessage() const;
    Object irritants() const;

private:
    OnigRegion* matchInternal(const ucs4string& text);

    ucs4string pattern_;
    OnigRegexType* regexp_; // do not use regex_t..
    OnigErrorInfo einfo_;
    bool isErrorOccured_{false};
    Object errorMessage_;
    Object irritants_;
};

class RegMatch EXTEND_GC
{
public:
    RegMatch(OnigRegion* region, const ucs4string& text);
    ~RegMatch();

    int matchStart(int index);
    int matchEnd(int index);
    Object matchAfter(int index);
    Object matchBefore(int index);
    Object matchSubString(int index);
    bool isErrorOccured() const;
    Object errorMessage() const;
    Object irritants() const;

private:
    OnigRegion* region_;
    const ucs4string& text_;
    bool isErrorOccured_{false};
    Object errorMessage_;
    Object irritants_;
};

} // namespace scheme

#endif // SCHEME_REGEXP_H_

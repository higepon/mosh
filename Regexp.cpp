/*
 * Regexp.cpp - Regexp.
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

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Regexp.h"
#include "SString.h"
#include "VM.h"

using namespace scheme;

extern VM* theVM;

#if WORDS_BIGENDIAN
#define ONIG_ENCODING ONIG_ENCODING_UTF32_BE
#else
#define ONIG_ENCODING ONIG_ENCODING_UTF32_LE
#endif


Regexp::Regexp(const ucs4string& pattern, bool caseFold) : pattern_(pattern),
                                                           isErrorOccured_(false),
                                                           errorMessage_(Object::Nil),
                                                           irritants_(Object::Nil)
{
    const ucs4char* p = pattern_.data();
    int r = onig_new(&regexp_,
                     (const uint8_t*)p,
                     (const uint8_t*)(p + pattern_.size()),
                     (ONIG_OPTION_DEFAULT | caseFold ? ONIG_OPTION_IGNORECASE : 0),
                     ONIG_ENCODING,
                     ONIG_SYNTAX_RUBY,
                     &einfo_);
    if (r != ONIG_NORMAL)
    {
        char errorMessageBuffer[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((uint8_t*)errorMessageBuffer, r, &einfo_);
        isErrorOccured_ = true;
        errorMessage_ = errorMessageBuffer;
        irritants_ = L1(Object::makeString(pattern.data()));
    }
}

bool Regexp::isErrorOccured() const
{
    return isErrorOccured_;
}

Object Regexp::errorMessage() const
{
    return errorMessage_;
}

Object Regexp::irritants() const
{
    return irritants_;
}

Object Regexp::match(const ucs4string& text)
{
    OnigRegion* region= matchInternal(text);
    if (NULL == region) {
        return Object::False;
    } else {
        return Object::makeRegMatch(region, text);
    }
}

OnigRegion* Regexp::matchInternal(const ucs4string& text)
{
    OnigRegion* region = onig_region_new();
    const uint8_t* start = (const uint8_t*)text.data();
    const uint8_t* end   = start + text.size() * sizeof(ucs4char);
    const uint8_t* range = end;
    const int r = onig_search(regexp_, start, end, start, range, region, ONIG_OPTION_NONE);
    if (r >= 0) {
        return region;
    } else if (r == ONIG_MISMATCH) {
        return NULL;
    } else {
        char errorMessageBuffer[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((uint8_t*)errorMessageBuffer, r);
        isErrorOccured_ = true;
        errorMessage_ = errorMessageBuffer;
        irritants_ = L2(Object::makeString(text.data()),
                        Object::makeString(pattern_.data()));
        return NULL;
    }
}

ucs4string Regexp::replace(ucs4string& text, ucs4string& subst, bool& matched)
{
    OnigRegion* const region = matchInternal(text);
    if (NULL == region) {
        matched = false;
        return text;
    }
    const ucs4string beg = text.substr(0, region->beg[0] / sizeof(ucs4char));
    const ucs4string end = text.substr(region->end[0] / sizeof(ucs4char), text.size() - region->end[0] / sizeof(ucs4char));
    matched = true;
    return (beg + subst + end).data();
}

Object Regexp::replace(Object t, Object subst)
{
    bool matched;
    const ucs4string ret = replace(t.toString()->data(), subst.toString()->data(), matched);
    if (matched) {
        return Object::makeString(ret);
    } else {
        return t;
    }
}

Object Regexp::replaceAll(Object t, Object subst)
{
    ucs4string ret;
    ucs4string targetString = t.toString()->data();
    ucs4string substitute = subst.toString()->data();

    for (;;) {
        OnigRegion* const region = matchInternal(targetString);
        if (NULL == region) {
            ret += targetString;
            break;
        }

        const ucs4string preString  = targetString.substr(0, region->beg[0] / sizeof(ucs4char));
        const ucs4string postString = targetString.substr(region->end[0] / sizeof(ucs4char), targetString.size() - region->end[0] / sizeof(ucs4char));
        ret += preString + substitute;
        targetString = postString;
    }
    return Object::makeString(ret);
}

RegMatch::RegMatch(OnigRegion* region, const ucs4string& text) : region_(region),
                                                                 text_(text),
                                                                 isErrorOccured_(false),
                                                                 errorMessage_(Object::Nil),
                                                                 irritants_(Object::Nil)
{
}

Object RegMatch::errorMessage() const
{
    return errorMessage_;
}

Object RegMatch::irritants() const
{
    return irritants_;
}

bool RegMatch::isErrorOccured() const
{
    return isErrorOccured_;
}

int RegMatch::matchStart(int index)
{
    if (index < 0 || index >= region_->num_regs) {
        isErrorOccured_ = true;
        errorMessage_ = "submatch index out of range";
        irritants_ = L1(Object::makeFixnum(index));
        return -1;
    }
    return region_->beg[index] / sizeof(ucs4char);
}

int RegMatch::matchEnd(int index)
{
    if (index < 0 || index >= region_->num_regs) {
        isErrorOccured_ = true;
        errorMessage_ = "submatch index out of range";
        irritants_ = L1(Object::makeFixnum(index));
        return -1;
    }
    return region_->end[index] / sizeof(ucs4char);
}

Object RegMatch::matchAfter(int index)
{
    if (index < 0 || index >= region_->num_regs) {
        isErrorOccured_ = true;
        errorMessage_ = "submatch index out of range";
        irritants_ = L1(Object::makeFixnum(index));
        return Object::Undef;
    }
    return Object::makeString(text_.substr(region_->end[index] / sizeof(ucs4char),
                                          text_.size() - region_->end[index] / sizeof(ucs4char)).data());
}

Object RegMatch::matchBefore(int index)
{
    if (index < 0 || index >= region_->num_regs)
    {
        isErrorOccured_ = true;
        errorMessage_ = "submatch index out of range";
        irritants_ = L1(Object::makeFixnum(index));
        return Object::Undef;
    }
    return Object::makeString(text_.substr(0, region_->beg[index] / sizeof(ucs4char)).data());
}

Object RegMatch::matchSubString(int index)
{
    if (index< 0 || index >= region_->num_regs)
    {
        isErrorOccured_ = true;
        errorMessage_ = "submatch index out of range";
        irritants_ = L1(Object::makeFixnum(index));
        return Object::Undef;
    }

    // really?
    if (region_->beg[index] == region_->end[index]) {
        return Object::False;
    }
    return Object::makeString(text_.substr(region_->beg[index] / sizeof(ucs4char),
                                           region_->end[index] / sizeof(ucs4char) - region_->beg[index] / sizeof(ucs4char)).data());

}

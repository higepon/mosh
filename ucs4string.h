/*
 * ucs4string.h - ucs4string
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

#ifndef SCHEME_UCING_H_
#define SCHEME_UCING_H_

#include <string>
#include <vector>

namespace scheme {

#ifdef USE_BOEHM_GC
typedef std::basic_string< ucs4char, std::char_traits<ucs4char>, gc_allocator<ucs4char> > ucs4string_base;
//template <class T1>
//class gc_vector : public std::vector<T1, gc_allocator<T1> >, public gc { };

#else
typedef std::basic_string< ucs4char, std::char_traits<ucs4char>, std::allocator<ucs4char> > ucs4string_base;
//template <class T1>
//class gc_vector : public std::vector<T1, std::allocator<T1> > {};
#endif


class ucs4string : public ucs4string_base
{
public:
    ucs4string() {}
    ucs4string(int n, ucs4char c = ' ') : ucs4string_base(n, c) {}
    ucs4string(const ucs4char* s) : ucs4string_base(s) {}
    ucs4string(const ucs4char* s, int n) : ucs4string_base(s, n) {}
    ucs4string(ucs4string::const_iterator a, ucs4string::const_iterator b) : ucs4string_base(a, b) {}
    char* ascii_c_str() const;
    ucs4string substr(int x, int size) const;
    void split(ucs4char ch, gc_vector<ucs4string>& v) const;
    ucs4char* strdup();
    bool is_ascii() const;
    static ucs4string from_c_str(const char* s, int size);
    // see R6RS 11.11 Characters
    static bool isValidScalar(int ch)
    {
        return (0 <= ch && ch <= 0xD7FF) || (0xE000 <= ch && ch <= 0x10FFFF);
    }
};

} // namespace scheme

#endif // SCHEME_UCING_H_

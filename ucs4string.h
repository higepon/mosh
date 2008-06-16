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
 *  $Id: ucs4string.h 5321 2008-05-09 05:54:00Z higepon $
 */

#ifndef __SCHEME_UCING_H__
#define __SCHEME_UCING_H__

#include <string>
#include <vector>

namespace scheme {

#ifdef USE_BOEHM_GC
typedef std::basic_string< ucs4char, std::char_traits<ucs4char>, gc_allocator<ucs4char> > ucs4string_base;
template <class T1>
class gc_vector : public std::vector<T1, gc_allocator<T1> >, public gc { };

#else
typedef std::basic_string< ucs4char, std::char_traits<ucs4char>, std::allocator<ucs4char> > ucs4string_base;
template <class T1>
class gc_vector : public std::vector<T1, std::allocator<T1> > {};
#endif


class ucs4string : public ucs4string_base
{
public:
    ucs4string() {}
    ucs4string(int n, ucs4char c = ' ') : ucs4string_base(n, c) {}
    ucs4string(const ucs4char* s) : ucs4string_base(s) {}
    ucs4string(ucs4string::iterator a, ucs4string::iterator b) : ucs4string_base(a, b) {}
    const char* ascii_c_str() const
    {
        const int sz = size();
#ifdef USE_BOEHM_GC
        char* ret = new(PointerFreeGC) char[sz + 1];
#else
        char* ret = new char[sz];
#endif

        for (int i = 0; i < sz; i++) {
            ret[i] = (*this)[i] & 0xff;
        }
        ret[sz] = '\0';
        return ret;
    }

    static ucs4string from_c_str(const char* s, int size)
    {
#ifdef USE_BOEHM_GC
        ucs4char* ret = new(PointerFreeGC) ucs4char[size + 1];
#else
        ucs4char* ret = new ucs4char[sz];
#endif
        for (int i = 0; i < size; i++) {
            ret[i] = s[i];
        }
        ret[size] = '\0';
        return ucs4string(ret);
    }

    ucs4string substr(int x, int size) const { return ucs4string(ucs4string_base::substr(x, size).data()); }

    void split(ucs4char ch, gc_vector<ucs4string>& v)
    {
        size_type index = 0;
        size_type next = 0;
        while ((index = find_first_of(ch, next)) != ucs4string::npos)
        {
            v.push_back(ucs4string(begin() + next, begin() + index));
            next = index + 1;
        }
        v.push_back(ucs4string(begin() + next, end()));
    }
};

}; // namespace scheme

#endif // __SCHEME_UCING_H__

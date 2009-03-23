/*
 * ucs4string.cpp - 
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
 *  $Id: ucs4string.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "scheme.h"
#include "ucs4string.h"

#ifdef _WIN32
#include <map>
#include <list>
#include <vector>
const ucs4char* UC(const char *str)
{
        typedef std::map<const char *, ucs4char*> Hash;
        typedef std::list<std::vector<ucs4char> > Data;
        static Hash hash;
        static Data data;
        Hash::iterator i = hash.find(str);
        if (i == hash.end()) {
                size_t len = strlen(str);
                data.push_back(std::vector<ucs4char>());
                std::vector<ucs4char>& d = data.back();
                d.resize(len + 1);
                for (size_t i = 0; i < len; i++) {
                        d[i] = str[i];
                }
                d[len] = 0;
                hash[str] = &d[0];
                return &d[0];
        } else {
                return i->second;
        }
}
#endif

using namespace scheme;

ucs4char* ucs4string::strdup()
{
    const int length = size();
#ifdef USE_BOEHM_GC
    ucs4char* ret = new (PointerFreeGC) ucs4char[length + 1];
#else
    ucs4char* ret = new ucs4char[length + 1];
#endif

    for (int i = 0; i < length; i++) {
        ret[i] = (*this)[i];
    }
    ret[length] = '\0';
    return ret;
}

void ucs4string::split(ucs4char ch, gc_vector<ucs4string>& v)
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

ucs4string ucs4string::substr(int x, int size) const
{
    return ucs4string(ucs4string_base::substr(x, size).c_str());
}

char* ucs4string::ascii_c_str() const
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

ucs4string ucs4string::from_c_str(const char* s, int size)
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

bool ucs4string::is_ascii() const
{
    const int length = size();
    for (int i = 0; i < length; i++) {
        const ucs4char ch = (*this)[i];
        if (ch < 0 && ch > 255) {
            return false;
        }
    }
    return true;
}

/*
 * SString.h - <string>
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

#ifndef SCHEME_STRING_H_
#define SCHEME_STRING_H_

namespace scheme {

class String EXTEND_GC
{
public:
    String(const ucs4char* s) : data_(s) {}
    String(int n, ucs4char c = ' ') : data_(ucs4string(n, c)) {}
    String(const char* s)
    {
        const int len = strlen(s);
#ifdef USE_BOEHM_GC
        ucs4char* p = new(PointerFreeGC) ucs4char[len + 1];
#else
        ucs4char* p = new ucs4char[len + 1];
#endif
        for (int i = 0; i < len + 1; i++) {
            p[i] = s[i];
        }
        data_ = p;
    }
    ucs4char charAt(int n);
    ucs4string& data() { return data_; }

    int length() const { return data_.length(); }

    bool operator==(String& s)
    {
        return data() == s.data();
    }

private:
    ucs4string data_;
};

inline Object::Object(const ucs4char* str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str)))))
{
}


inline Object::Object(const char* str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str)))))
{
}

inline Object Object::makeString(int n, ucs4char c /* = ' ' */)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(n, c)))));
}

inline Object Object::makeString(const ucs4char* str)
{
    return Object(str);
}

inline Object Object::makeString(const ucs4string& str)
{
    return Object(str.c_str());
}

inline Object Object::makeString(const char* str)
{
    return Object(str);
}

inline Object::Object(const ucs4string& str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str.data())))))
{

}


// inline Object::Object(const char* str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str)))))
// {
// }


} // namespace scheme

#endif // SCHEME_STRING_H_

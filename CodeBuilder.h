/*
 * CodeBuilder.h - CodeBuilder for compilation.
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
 *  $Id: Regexp.h 5249 2008-04-21 06:28:24Z higepon $
 */

#ifndef __SCHEME_CODE_BUILDER_H__
#define __SCHEME_CODE_BUILDER_H__

#include "scheme.h"

namespace scheme {
//template <class T1>
// class gc_vector2 : public std::vector<T1, gc_allocator<T1> >, public gc {
// public:
//     gc_vector2(int size) : std::vector<T1, gc_allocator<T1> >(size) {}
// };
class CodeBuilder EXTEND_GC
{
public:
    CodeBuilder() {}

    void put(Object a) { code.push_back(a); }
    void put(Object a, Object b) { code.push_back(a); code.push_back(b); }
    void put(Object a, Object b, Object c) { code.push_back(a); code.push_back(b); code.push_back(c); }
    void put(Object a, Object b, Object c, Object d) { code.push_back(a); code.push_back(b); code.push_back(c); code.push_back(d); }
    void put(Object a, Object b, Object c, Object d, Object e) { code.push_back(a); code.push_back(b); code.push_back(c); code.push_back(d); code.push_back(e); }

    void append(CodeBuilder* cb)
    {
        code.insert(code.end(), cb->code.begin(), cb->code.end());
    }

    Object emit()
    {
        Object ret = Object::Nil;
        for (int i = code.size() - 1; i >= 0; i--) {
            ret = Object::cons(code[i], ret);
        }
        return ret;
    }

    ObjectVector code;
};

}; // namespace scheme

#endif // __SCHEME_CODE_BUILDER_H__

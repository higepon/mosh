/*
 * Code.h - 
 *
 *   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: Code.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_CODE_
#define SCHEME_CODE_

#include "scheme.h"

namespace scheme {

class Code EXTEND_GC
{
public:
    explicit Code(int size) : size_(size),  code_(Object::makeObjectArrayLocal(size))
    {
    }

    ~Code() = default;

    void push(Object o)
    {
        MOSH_ASSERT(index_ < size_);
        code_[index_++] = o;
    }

    void set(int index, Object o)
    {
        MOSH_ASSERT(index < size_);
        code_[index] = o;
    }

    int size() const
    {
        return size_;
    }

    Object* code() const
    {
        return code_;
    }

private:
    int size_;
    int index_{0};
    Object* code_;
};

}; // namespace scheme

#endif // SCHEME_CODE_

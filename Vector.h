/*
 * Vector.h - <vector>
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

#ifndef __SCHEME_VECTOR_H__
#define __SCHEME_VECTOR_H__

namespace scheme {

class Vector EXTEND_GC
{
public:
    Vector(int num) : num_(num)
    {
        MOSH_ASSERT(num < 1000000); // if n is too big, you may forget some cast?
        objects_ = Object::makeObjectArray(num);
    }

    Vector(int num, Object obj) : num_(num)
    {
        MOSH_ASSERT(num < 1000000); // if n is too big, you may forget some cast?
        objects_ = Object::makeObjectArray(num);
        for (int i = 0; i < num; i++) {
            objects_[i] = obj;
        }
    }

    Vector(int num, Object* objects) : num_(num), objects_(objects)
    {
    }

    Vector(Object pair) : num_(Pair::length(pair))
    {
        MOSH_ASSERT(pair.isPair() || pair.isNil());
        objects_ = Object::makeObjectArray(num_);
        int i = 0;
        for (Object o = pair; !o.isNil(); o = o.cdr()) {
            objects_[i] = o.car();
            i++;
        }
    }

    ~Vector() {}

    Object ref(int index) const
    {
        return objects_[index];
    }

    void set(int index, Object obj)
    {
        objects_[index] = obj;
    }

    int length() const
    {
        return num_;
    }

    Object* data()
    {
        return objects_;
    }

private:
    const int num_;
    Object* objects_;

};

}; // namespace scheme

#endif // __SCHEME_VECTOR_H__

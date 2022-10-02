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

#ifndef SCHEME_VECTOR_H_
#define SCHEME_VECTOR_H_

namespace scheme {

class Vector EXTEND_GC
{
public:
    explicit Vector(size_t num);
    Vector(size_t num, Object obj);
    Vector(size_t num, Object* objects);
    explicit Vector(Object pair);
    ~Vector();
    void fill(Object obj);
    Object ref(size_t index) const;
    void set(size_t index, Object obj);
    size_t length() const;
    bool isValidIndex(size_t index) const;
    Object* data();

private:
    size_t num_;
    Object* objects_;
};

inline Vector::Vector(size_t num, Object obj) : num_(num)
{
    MOSH_ASSERT(num < 1000000); // if n is too big, you may forget some cast?
    objects_ = Object::makeObjectArrayLocal(num);
    for (size_t i = 0; i < num; i++) {
        objects_[i] = obj;
    }
}

inline Vector::Vector(size_t num) : num_(num)
{
    MOSH_ASSERT(num < 1000000); // if n is too big, you may forget some cast?
    objects_ = Object::makeObjectArrayLocal(num);
}



inline Vector::Vector(size_t num, Object* objects) : num_(num), objects_(objects)
{
}

inline void Vector::fill(Object obj)
{
    {
        for (size_t i = 0; i < num_; i++) {
            objects_[i] = obj;
        }
    }
}

inline Object Vector::ref(size_t index) const
{
    return objects_[index];
}

inline void Vector::set(size_t index, Object obj)
{
    objects_[index] = obj;
}

inline size_t Vector::length() const
{
    return num_;
}

inline bool Vector::isValidIndex(size_t index) const
{
    return index < num_;
}

inline Object::Object(size_t n, Object o)
  : val(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Vector, reinterpret_cast<intptr_t>(new Vector(n, o)))))
{
}


inline Object Object::makeVector(size_t n, Object* objects)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Vector, reinterpret_cast<intptr_t>
                                                        (new Vector(n, objects)))));
}

inline Object Object::makeVector(Object pair)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Vector, reinterpret_cast<intptr_t>
                                                        (new Vector(pair)))));
}



} // namespace scheme

#endif // SCHEME_VECTOR_H_

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
    Vector(int num);
    Vector(int num, Object obj);
    Vector(int num, Object* objects);
    Vector(Object pair);
    ~Vector();

    Object ref(int index) const;
    void set(int index, Object obj);
    void fill(Object obj);
    int length() const;
    Object* data();

private:
    int num_;
    Object* objects_;
};

inline Object Vector::ref(int index) const
{
    return objects_[index];
}

inline void Vector::set(int index, Object obj)
{
    objects_[index] = obj;
}

inline int Vector::length() const
{
    return num_;
}

inline Object::Object(int n, Object o)
  : val(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>(new Vector(n, o)))))
{
}


inline Object Object::makeVector(int n, Object* objects)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>
                                                        (new Vector(n, objects)))));
}

inline Object Object::makeVector(Object pair)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>
                                                        (new Vector(pair)))));
}


}; // namespace scheme

#endif // __SCHEME_VECTOR_H__

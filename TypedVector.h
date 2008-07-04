/*
 * TypedVector.h - <typed-vector>
 *   Used for R6RS Records.
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

#ifndef __SCHEME_TYPED_VECTOR_H__
#define __SCHEME_TYPED_VECTOR_H__

#include "scheme.h"

namespace scheme {

class TypedVectorDesc EXTEND_GC
{
public:
    TypedVectorDesc(Object name, Object supertype, Object data, Object fieldMutability) :
        name(name),
        supertype(supertype),
        data(data),
        fieldNumber(Pair::length(fieldMutability)) {}

    const Object name;
    const Object supertype;
    const Object data;
    const int fieldNumber;
};

class TypedVector EXTEND_GC
{
public:
    TypedVector(Object desc, Object fieldsList);

    Object instanceOf(Object d)
    {
        if (desc == d) {
            return Object::True;
        }
        Object target = desc;
        while (!(target = target.toTypedVectorDesc()->supertype).isFalse()) {
            if (d == target) return Object::True;
        }
        return Object::False;
    }

    Object getNth(Object index)
    {
        return fields[index.toInt()];
    }

    void setNth(Object index, Object val)
    {
        fields[index.toInt()] = val;
    }


    const Object desc;
    Object* fields;
    const int fieldNumber;
};

}; // namespace scheme

#endif // __SCHEME_TYPED_VECTOR_H__

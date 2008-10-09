/*
 * Vector.cpp - 
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
 *  $Id: Vector.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Vector.h"
#include "ProcedureMacro.h" // temporary
#include "TextualOutputPort.h"

using namespace scheme;

Vector::Vector(int num) : num_(num)
{
    MOSH_ASSERT(num < 1000000); // if n is too big, you may forget some cast?
    printf("num=%d\n", num);fflush(stdout);
    objects_ = Object::makeObjectArray(num);
}

Vector::Vector(int num, Object obj) : num_(num)
{
    MOSH_ASSERT(num < 1000000); // if n is too big, you may forget some cast?
    objects_ = Object::makeObjectArray(num);
    for (int i = 0; i < num; i++) {
        objects_[i] = obj;
    }
}

Vector::Vector(int num, Object* objects) : num_(num), objects_(objects)
{
}

Vector::Vector(Object pair)
{
    MOSH_ASSERT(pair.isPair() || pair.isNil());
    num_ = Pair::length(pair);
    MOSH_ASSERT(num_ >= 0);
    objects_ = Object::makeObjectArray(num_);
    MOSH_ASSERT(objects_ != NULL);
    int i = 0;
    for (Object o = pair; !o.isNil(); o = o.cdr()) {
        objects_[i] = o.car();
        i++;
    }
}

Vector::~Vector()
{
}

Object* Vector::data()
{
    return objects_;
}

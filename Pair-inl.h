/*
 * Pair-inl.h - 
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
 *  $Id: Pair-inl.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_PAIR_INL_
#define SCHEME_PAIR_INL_

#include "scheme.h"

namespace scheme {

inline Object& Object::car() const
{
    return toPair()->car;
}

inline Object Object::cons(Object car, Object cdr, Object sourceInfo /* = Object::False */)
{
// this makes Mosh very slow.
// don't use this.
//     if (sourceInfo.isFalse() && car.isPair()) {
//         sourceInfo = car.sourceInfo();
//     }
    return Object(reinterpret_cast<intptr_t>(new Pair(car, cdr, sourceInfo)));
}

inline Object& Object::cdr() const
{
    return toPair()->cdr;
}

inline bool Object::isPair() const
{
    return isPointer() && ((toPair()->car.val & 0x03) != 0x03);
}

inline Object& Object::sourceInfo() const
{
    return toPair()->sourceInfo;
}

inline Object& Object::first() const
{
    return car();
}

inline Object& Object::second() const
{
    return cdr().car();
}

inline Object& Object::third() const
{
    return cdr().cdr().car();
}

inline Object& Object::fourth() const
{
    return cdr().cdr().cdr().car();
}

inline Object& Object::fifth() const
{
    return cdr().cdr().cdr().cdr().car();
}

} // namespace scheme

#endif // SCHEME_PAIR_INL_

/*
 * Fixnum.h - 
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
 *  $Id: Fixnum.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_FIXNUM__
#define __SCHEME_FIXNUM__

#include "scheme.h"

namespace scheme {

class Fixnum EXTEND_GC
{
public:

    static bool canFit(long n)
    {
        const int FIXNUM_BITS = 29;
        const long MAX = (1L << FIXNUM_BITS) - 1;
        const long MIN = -MAX - 1;
        return MIN <= n && n <= MAX;
    }

#define MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(compare, symbol) \
    static bool compare(int n1, int n2)\
    {\
        return n1 symbol n2;\
    }\

    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(gt, >)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(ge, >=)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(lt, <)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(le, <=)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(eq, ==)

    static Object add(int n1, int n2)
    {
        const long ret = n1 + n2;
        if (canFit(ret)) {
            return Object::makeFixnum(ret);
        } else {
            return Object::makeBignum(ret);
        }
    }
    static Object sub(int n1, int n2)
    {
        const long ret = n1 - n2;
        if (canFit(ret)) {
            return Object::makeFixnum(ret);
        } else {
            return Object::makeBignum(ret);
        }
    }
    static Object mul(int n1, int n2)
    {
        const long ret = n1 * n2;

        /* Overflow check from Gauche */
        if ((n1 != 0 && ret / n2 != n1) || !canFit(ret)) {
            return Bignum::mul(n1, n2);
        } else {
            return Object::makeFixnum(ret);
        }
    }
};

}; // namespace scheme

#endif // __SCHEME_FIXNUM__

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
    enum
    {
        BITS = 29,
        MAX = (1L << BITS) - 1,
        MIN = -MAX - 1,
    };

    static bool canFit(long long n)
    {
        return Fixnum::MIN <= n && n <= Fixnum::MAX;
    }

    static Object sqrt(Object n);

    static bool isEven(int n)
    {
        return (n & 1) == 0;
    }

    static bool isOdd(int n)
    {
        return !isEven(n);
    }

    static Object exp(int n);
    static Object log(int n);
    static Object sin(int n);
    static Object cos(int n);
    static Object tan(int n);

    static Object abs(int n)
    {
        return Object::makeFixnum(::abs(n));
    }

    static Object integerDiv(int x, int y)
    {
        int div;
        if (x == 0) {
            div = 0;
        } else if (x > 0) {
            div = x / y;
        } else if (y > 0) {
            div = (x - y + 1) / y;
        } else {
            div = (x + y + 1) / y;
        }
        return Object::makeFixnum(div);
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

};

}; // namespace scheme

#endif // __SCHEME_FIXNUM__

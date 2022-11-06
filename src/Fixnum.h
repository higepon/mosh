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

#ifndef SCHEME_FIXNUM_
#define SCHEME_FIXNUM_

#include "scheme.h"
#include "Arithmetic.h"

namespace scheme {

class Fixnum EXTEND_GC
{
public:
    //enum
    //{
        static const fixedint BITS = sizeof(fixedint) * 8 - 2;
        static const fixedint MAX = (1L << (BITS - 1)) - 1;
        static const fixedint MIN = -MAX - 1;
    //};

    static bool canFit(long long n)
    {
        return Fixnum::MIN <= n && n <= Fixnum::MAX;
    }

// todo
    static bool canFitU(uint64_t n)
    {
        return n <= Fixnum::MAX;
    }


    static Object sqrt(Object n);

    static bool isEven(fixedint n)
    {
        return (n & 1) == 0;
    }

    static bool isOdd(fixedint n)
    {
        return !isEven(n);
    }

    static Object exp(fixedint n);
    static Object log(fixedint n);
    static Object sin(fixedint n);
    static Object cos(fixedint n);
    static Object tan(fixedint n);
    static Object asin(fixedint n);
    static Object acos(fixedint n);
    static Object atan(fixedint n);

    static Object abs(fixedint n);

    static fixedint fxdiv(fixedint x, fixedint y)
    {
        fixedint div;
        if (x == 0) {
            div = 0;
        } else if (x > 0) {
            div = x / y;
        } else if (y > 0) {
            div = (x - y + 1) / y;
        } else {
            div = (x + y + 1) / y;
        }
        return div;
    }

    static fixedint fxmod(fixedint x, fixedint y)
    {
        return x - fxdiv(x, y) * y;
    }

    static fixedint fxdiv0(fixedint x, fixedint y)
    {
        const fixedint d = fxdiv(x, y);
        const fixedint m = fxmod(x, y);
        if (m <= (::abs(y) / 2)) {
            return d;
        } else if (y > 0) {
            return d + 1;
        } else {
            return d - 1;
        }
    }

    static fixedint fxmod0(fixedint x, fixedint y)
    {
        return x - y * fxdiv0(x, y);
    }

    static fixedint fxnot(fixedint fx)
    {
        return ~fx;
    }

    static fixedint fxand(fixedint fx1, fixedint fx2)
    {
        return fx1 & fx2;
    }

    static fixedint fxior(fixedint fx1, fixedint fx2)
    {
        return fx1 | fx2;
    }

    static fixedint fxxor(fixedint fx1, fixedint fx2)
    {
        return fx1 ^ fx2;
    }

    static fixedint fxif(fixedint fx1, fixedint fx2, fixedint fx3)
    {
        return fxior(fxand(fx1, fx2), fxand(fxnot(fx1), fx3));
    }

    static fixedint fxbitCount(fixedint fx)
    {
        if (fx > 0) {
            return Arithmetic::nbits(fx);
        } else {
            return ~Arithmetic::nbits(~fx);
        }
    }

    static fixedint fxlength(fixedint fx)
    {
        if (fx == 0) {
            return 0;
        }
        const unsigned long ufx = (fx < 0) ? ~fx : fx;
        if (sizeof(fixedint) == sizeof(uint32_t)) {
            return (sizeof(fixedint) * 8) - Arithmetic::nlz((uint32_t)ufx);
        } else {
            return (sizeof(fixedint) * 8) - Arithmetic::nlz((uint64_t)ufx);
        }
    }

    static fixedint fxfirstBitSet(fixedint fx)
    {
        if (fx == 0) {
            return -1;
        }
        if (sizeof(fixedint) == sizeof(uint32_t)) {
            return Arithmetic::ntz((uint32_t)fx);
        } else {
            return Arithmetic::ntz((uint32_t)fx);
        }
    }

    static bool fxbitSetP(fixedint fx1, unsigned long fx2)
    {
        return ((unsigned long)fx1 >> fx2) & 1;
    }

    static long long fxarithmeticShiftLeft(fixedint fx1, fixedint fx2)
    {
        return static_cast<long long>(fx1) << fx2;
    }

    static long long fxarithmeticShiftRight(fixedint fx1, fixedint fx2)
    {
        return fx1 >> fx2;
    }

    static fixedint fxcopyBit(fixedint fx1, fixedint fx2, fixedint fx3)
    {
        const long long mask = fxarithmeticShiftLeft(1, fx2);
        return fxif(mask, fxarithmeticShiftLeft(fx3, fx2), fx1);
    }

    static long long fxbitField(fixedint fx1, fixedint fx2, fixedint fx3)
    {
        const fixedint mask = fxnot(fxarithmeticShiftLeft(-1, fx3));
        return fxarithmeticShiftRight(fxand(fx1, mask), fx2);
    }

    static fixedint fxcopyBitField(fixedint fx1, fixedint fx2, fixedint fx3, fixedint fx4)
    {
        const fixedint mask1 = fxarithmeticShiftLeft(-1, fx2);
        const fixedint mask2 = fxnot(fxarithmeticShiftLeft(-1, fx3));
        const fixedint mask  = fxand(mask1, mask2);
        return fxif(mask, fxarithmeticShiftLeft(fx4, fx2), fx1);
    }

    static fixedint fxrotateBitField(fixedint fx1, unsigned int fx2, unsigned int fx3, unsigned int fx4)
    {
        const fixedint width = fx3 - fx2;
        if (width > 0) {
            const fixedint count  = fxmod(fx4, width);
            const long long field0 = fxbitField(fx1, fx2, fx3);
            const long long field1 = fxarithmeticShiftLeft(field0, fx4);
            const long long field2 = fxarithmeticShiftRight(field0, width - count);
            const fixedint field  = fxior(field1, field2);

            return fxcopyBitField(fx1, fx2, fx3, field);
        } else {
            return fx1;
        }
    }

    static int fxreverseBitField(fixedint fx1, unsigned fx2, unsigned fx3)
    {
        uint32_t bits  = (unsigned int)fx1;
        int      start = fx2;
        int      end   = fx3 - 1;

        while (start < end) {
            int sbit = (bits >> start) & 1;
            int ebit = (bits >> end  ) & 1;

            bits &= (uint32_t)-1 - (1 << end);
            bits |= sbit << end;
            bits &= (uint32_t)-1 - (1 << start);
            bits |= ebit << start;

            start++;
            end--;
        }

        return bits;
    }

    static Object integerDiv(fixedint x, fixedint y);

#define MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(compare, symbol) \
    static bool compare(fixedint n1, fixedint n2)\
    {\
        return n1 symbol n2;\
    }\

    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(gt, >)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(ge, >=)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(lt, <)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(le, <=)
    MAKE_FIXNUM_FIXNUM_COMPARE_FUNC(eq, ==)

};

} // namespace scheme

#endif // SCHEME_FIXNUM_

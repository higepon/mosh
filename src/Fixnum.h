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
    enum
    {
        BITS = sizeof(fixedint) - 2,
        MAX = (1L << (BITS - 1)) - 1,
        MIN = -MAX - 1
    };

    static bool canFit(fixedint n)
    {
        return Fixnum::MIN <= n && n <= Fixnum::MAX;
    }

// todo
    static bool canFitU(uint64_t n)
    {
        return n <= Fixnum::MAX;
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
        const uint32_t ufx = (fx < 0) ? ~fx : fx;
        return 32 - Arithmetic::nlz(ufx);
    }

    static fixedint fxfirstBitSet(fixedint fx)
    {
        if (fx == 0) {
            return -1;
        }
        return Arithmetic::ntz(fx);
    }

    static bool fxbitSetP(int fx1, unsigned int fx2)
    {
        return ((unsigned int)fx1 >> fx2) & 1;
    }

    static int fxarithmeticShiftLeft(int fx1, unsigned int fx2)
    {
        return fx1 << fx2;
    }

    static int fxarithmeticShiftRight(int fx1, unsigned int fx2)
    {
        return fx1 >> fx2;
    }

    static int fxcopyBit(int fx1, unsigned int fx2, int fx3)
    {
        const int mask = fxarithmeticShiftLeft(1, fx2);
        return fxif(mask, fxarithmeticShiftLeft(fx3, fx2), fx1);
    }

    static int fxbitField(int fx1, unsigned int fx2, unsigned int fx3)
    {
        const int mask = fxnot(fxarithmeticShiftLeft(-1, fx3));
        return fxarithmeticShiftRight(fxand(fx1, mask), fx2);
    }

    static int fxcopyBitField(int fx1, unsigned int fx2, unsigned int fx3, int fx4)
    {
        const int mask1 = fxarithmeticShiftLeft(-1, fx2);
        const int mask2 = fxnot(fxarithmeticShiftLeft(-1, fx3));
        const int mask  = fxand(mask1, mask2);
        return fxif(mask, fxarithmeticShiftLeft(fx4, fx2), fx1);
    }

    static int fxrotateBitField(int fx1, unsigned int fx2, unsigned int fx3, unsigned int fx4)
    {
        const int width = fx3 - fx2;
        if (width > 0) {
            const int count  = fxmod(fx4, width);
            const int field0 = fxbitField(fx1, fx2, fx3);
            const int field1 = fxarithmeticShiftLeft(field0, fx4);
            const int field2 = fxarithmeticShiftRight(field0, width - count);
            const int field  = fxior(field1, field2);

            return fxcopyBitField(fx1, fx2, fx3, field);
        } else {
            return fx1;
        }
    }

    static int fxreverseBitField(int fx1, unsigned fx2, unsigned fx3)
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

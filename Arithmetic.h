/*
 * Arithmetic.h - 
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
 *  $Id: Arithmetic.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_ARITHMETIC__
#define __SCHEME_ARITHMETIC__

#include "scheme.h"

namespace scheme {

class Arithmetic EXTEND_GC
{
public:
    Arithmetic();
    ~Arithmetic();

    static Object asin(Object n);
    static Object sin(Object n);
    static Object cos(Object n);
    static Object tan(Object n);
    static Object integerDiv(Object n1, Object n2);
    static Object integerDiv0(Object n1, Object n2);
    static Object exp(Object n);
    static Object log(Object n);
    static Object log(Object n1, Object n2);
    static Object floor(Object n);
    static Object ceiling(Object n);
    static Object truncate(Object n);
    static Object round(Object n);
    static Object abs(Object n);
    static Object sqrt(Object n);
    static double realToDouble(Object n);
    static Object maginude(Object n);
    static Object makePolar(Object n1, Object n2);
    static bool isEven(Object n);
    static bool isInteger(Object n);
    static bool isRealValued(Object n);
    static bool isRationalValued(Object n);
    static bool isIntegerValued(Object n);
    static bool isNegative(Object n);
    static Object negate(Object n);
    static Object bitwiseNot(Object e);
    static Object bitwiseAnd(Object e1, Object e2);
    static Object bitwiseIor(Object e1, Object e2);
    static Object bitwiseXor(Object e1, Object e2);
    static Object bitwiseBitCount(Object e);
    static Object bitwiseLength(Object e);
    static Object bitwiseFirstBitSet(Object e);
    static Object bitwiseShiftLeft(Object e1, unsigned long e2);
    static Object bitwiseShiftRight(Object e1, unsigned long e2);
    static Object numerator(Object n);
    static Object denominator(Object n);
    static Object exact(Object n);
    static Object inexact(Object n);
    static bool isExact(Object n);
    static bool isExactZero(Object n1);
    static bool isZero(Object n1);
    static Object toFlonum(Object real);
    static Object add(Object number1, Object number2);
    static Object sub(Object number1, Object number2);
    static Object mul(Object number1, Object number2);
    static Object div(Object number1, Object number2, bool noRaise = false);
    static Object mul(int number1, Object number2);
    static bool gt(Object number1, Object number2);
    static bool ge(Object number1, Object number2);
    static bool lt(Object number1, Object number2);
    static bool le(Object number1, Object number2);
    static bool eq(Object number1, Object number2);
    static bool fitsU64(Object n);
    static uint64_t toU64(Object n);
    static bool fitsS64(Object n);
    static int64_t toS64(Object n);

    // Originally from Ypsilon Scheme
    static uint32_t nbits(uint32_t x)
    {
        uint32_t t;
        x = x - ((x >> 1) & 0x55555555);
        t = ((x >> 2) & 0x33333333);
        x = (x & 0x33333333) + t;
        x = (x + (x >> 4)) & 0x0F0F0F0F;
        x = x + (x << 8);
        x = x + (x << 16);
        return(x >> 24);
    }

    // Originally from Ypsilon Scheme
    static uint32_t nlz(uint32_t x)
    {
        uint32_t t;
        int n = 32;
        t = x >> 16; if (t) { n -= 16 ; x = t; }
        t = x >>  8; if (t) { n -=  8 ; x = t; }
        t = x >>  4; if (t) { n -=  4 ; x = t; }
        t = x >>  2; if (t) { n -=  2 ; x = t; }
        t = x >>  1; if (t) { return n - 2; }
        return n - x;
    }

    // Originally from Ypsilon Scheme
    static uint32_t ntz(uint32_t x)
    {
        return nbits(~x & (x - 1));
    }

};

}; // namespace scheme

#endif // __SCHEME_ARITHMETIC__

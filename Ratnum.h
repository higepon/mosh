/*
 * Ratnum.h -
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
 *  $Id: Ratnum.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_RATIONAL_
#define SCHEME_RATIONAL_

#include "scheme.h"
#include "Bignum.h"

namespace scheme {

class Ratnum EXTEND_GC
{
public:
    Ratnum(int numerator, int denominator)
    {
        mpq_init(value);
        if (numerator >= 0 && denominator < 0) {
            mpq_set_si(value, -numerator, ::abs(denominator));
        } else if (numerator < 0 && denominator< 0) {
            mpq_set_si(value, ::abs(numerator), ::abs(denominator));
        } else {
            mpq_set_si(value, numerator, denominator);
        }
        mpq_canonicalize(value);
    }

    Ratnum(mpq_t rational)
    {
        mpq_set(value, rational);
    }

    Object sqrt() const;
    Object floor() const;
    Object ceiling() const;
    Object round() const;
    Object truncate() const;

    Object abs() const
    {
        mpq_t ret;
        mpq_init(ret);
        mpq_abs(ret, value);
        return makeNumber(ret);
    }

    Object numerator() const
    {
        return Bignum::makeInteger(mpq_numref(value));
    }

    Object denominator() const
    {
        return Bignum::makeInteger(mpq_denref(value));
    }

    char* toString(int radix = 10) const
    {
        return mpq_get_str(NULL, radix, value);
    }

    double toDouble() const
    {
        return mpq_get_d(value);
    }

    bool equal(int number)
    {
        return mpq_cmp_si(value, number, 1) == 0;
    }

    bool equal(Ratnum* number)
    {
        return mpq_cmp(value, number->value) == 0;
    }

    static Object div(int n1, Bignum* n2)
    {
        mpq_t ret;
        mpq_init(ret);
        mpq_set_si(ret, n1, 1);

        mpq_t num2;
        mpq_init(num2);
        mpq_set_z(num2, n2->value);
        mpq_div(ret, ret, num2);
        return makeNumber(ret);
    }

    static Object div(Bignum* n1, int n2)
    {
        mpq_t ret;
        mpq_init(ret);
        mpq_set_z(ret, n1->value);

        mpq_t num2;
        mpq_init(num2);
        mpq_set_si(num2, n2, 1);
        mpq_div(ret, ret, num2);
        return makeNumber(ret);
    }

    static Object div(Bignum* n1, Bignum* n2)
    {
        mpq_t ret;
        mpq_init(ret);
        mpq_set_z(ret, n1->value);

        mpq_t num2;
        mpq_init(num2);
        mpq_set_z(num2, n2->value);
        mpq_div(ret, ret, num2);
        return makeNumber(ret);
    }


#define MAKE_RATNUM_OP(op)\
    static Object op(Ratnum* number1, Ratnum* number2)\
    {\
        mpq_t ret;\
        mpq_init(ret);\
        mpq_##op(ret, number1->value, number2->value);\
        return makeNumber(ret);\
    }\
    static Object op(Ratnum* number1, Bignum* number2)\
    {\
        mpq_t ret;\
        mpq_init(ret);\
        mpq_set_z(ret, number2->value);\
        mpq_##op(ret, number1->value, ret);\
        return makeNumber(ret);\
    }\
    static Object op(Bignum* number1, Ratnum* number2)\
    {\
        mpq_t ret;\
        mpq_init(ret);\
        mpq_set_z(ret, number1->value);\
        mpq_##op(ret, ret, number2->value);\
        return makeNumber(ret);\
    }\
    static Object op(Ratnum* number1, int number2)\
    {\
        mpq_t ret;\
        mpq_init(ret);\
        mpq_set_si(ret, number2, 1);\
        mpq_canonicalize(ret);\
        mpq_##op(ret, number1->value, ret);\
        return makeNumber(ret);\
    }\
    static Object op(int number1, Ratnum* number2)\
    {\
        mpq_t ret;\
        mpq_init(ret);\
        mpq_set_si(ret, number1, 1);\
        mpq_canonicalize(ret);\
        mpq_##op(ret, ret, number2->value);\
        return makeNumber(ret);\
    }

    MAKE_RATNUM_OP(add)
    MAKE_RATNUM_OP(sub)
    MAKE_RATNUM_OP(mul)
    MAKE_RATNUM_OP(div)

#define MAKE_RATNUM_COMPARE(compare, symbol)\
    static bool compare(Ratnum* number1, Ratnum* number2)\
    {\
        return mpq_cmp(number1->value, number2->value) symbol;\
    }\
    static bool compare(Ratnum* number1, Bignum* number2)\
    {\
        mpq_t n2;\
        mpq_init(n2);\
        mpq_set_z(n2, number2->value);\
        return mpq_cmp(number1->value, n2) symbol;\
    }\
    static bool compare(Bignum* number1, Ratnum* number2)\
    {\
        mpq_t n1;\
        mpq_init(n1);                            \
        mpq_set_z(n1, number1->value);\
        return mpq_cmp(n1, number2->value) symbol;\
    }\
    static bool compare(Ratnum* number1, int number2)\
    {\
        mpq_t n2;\
        mpq_init(n2);\
        mpq_set_si(n2, number2, 1);\
        mpq_canonicalize(n2);\
        return mpq_cmp(number1->value, n2) symbol;\
    }\
    static bool compare(int number1, Ratnum* number2)\
    {\
        mpq_t n1;\
        mpq_init(n1);\
        mpq_set_si(n1, number1, 1);\
        mpq_canonicalize(n1);\
        return mpq_cmp(n1, number2->value) symbol;\
    }

    MAKE_RATNUM_COMPARE(gt, >0);
    MAKE_RATNUM_COMPARE(ge, >=0);
    MAKE_RATNUM_COMPARE(lt, <0);
    MAKE_RATNUM_COMPARE(le, <=0);
    MAKE_RATNUM_COMPARE(eq, ==0);

    static Object makeNumber(mpq_t r)
    {
        if (mpz_cmp_si(mpq_denref(r), 1) == 0) {
            if (mpz_cmp_si(mpq_numref(r), Fixnum::MIN) >= 0 &&
                mpz_cmp_si(mpq_numref(r), Fixnum::MAX) <= 0) {
                return Object::makeFixnum(mpz_get_si(mpq_numref(r)));
            } else {
                return Object::makeBignum(mpq_numref(r));
            }
        } else {
            return Object::makeRatnum(r);
        }
    }

    mpq_t value;

private:

    Object sqrtUnsigned(const mpq_t r) const;

};

} // namespace scheme

#endif // SCHEME_RATIONAL_

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

class Ratnum : public gc_cleanup
{
private:
    mpq_t value;

    Ratnum(mpq_t rational)
    {
        value[0] = rational[0];
    }

    Object sqrtUnsigned(const mpq_t r) const;

public:
    Ratnum()
    {
        mpq_init(value);
    }

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

    virtual ~Ratnum()
    {
        mpq_clear(value);
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
        // Since mpq_numref returns reference, copy it.
        mpz_t ret;
        mpz_init_set(ret, mpq_numref(value));
        return Bignum::makeInteger(ret);
    }

    Object denominator() const
    {
        // Since mpq_denref returns reference, copy it.
        mpz_t ret;
        mpz_init_set(ret, mpq_denref(value));
        return Bignum::makeInteger(ret);
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

        mpq_t temp;
        mpq_init(temp);
        mpq_set_z(temp, n2->value_);
        mpq_div(ret, ret, temp);
        mpq_clear(temp);
        return makeNumber(ret);
    }

    static Object div(Bignum* n1, int n2)
    {
        mpq_t ret;
        mpq_init(ret);
        // mpq_set_z do copy mpz_t, so this is safe
        mpq_set_z(ret, n1->value_);

        mpq_t temp;
        mpq_init(temp);
        mpq_set_si(temp, n2, 1);
        mpq_div(ret, ret, temp);
        mpq_clear(temp);
        return makeNumber(ret);
    }

    static Object div(Bignum* n1, Bignum* n2)
    {
        mpq_t ret;
        mpq_init(ret);
        mpq_set_z(ret, n1->value_);

        mpq_t temp;
        mpq_init(temp);
        mpq_set_z(temp, n2->value_);
        mpq_div(ret, ret, temp);
        mpq_clear(temp);
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
        mpq_set_z(ret, number2->value_);\
        mpq_##op(ret, number1->value, ret);\
        return makeNumber(ret);\
    }\
    static Object op(Bignum* number1, Ratnum* number2)\
    {\
        mpq_t ret;\
        mpq_init(ret);\
        mpq_set_z(ret, number1->value_);\
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
    static bool compare(const Ratnum* number1, const Ratnum* number2)\
    {\
        return mpq_cmp(number1->value, number2->value) symbol;\
    }\
    static bool compare(const Ratnum* number1, const Bignum* number2)\
    {\
        mpq_t temp;\
        mpq_init(temp);                             \
        mpq_set_z(temp, number2->value_);\
        bool ret = mpq_cmp(number1->value, temp) symbol;\
        mpq_clear(temp);\
        return ret;\
    }\
    static bool compare(Bignum* number1, Ratnum* number2)\
    {\
        mpq_t temp;\
        mpq_init(temp);\
        mpq_set_z(temp, number1->value_);\
        bool ret = mpq_cmp(temp, number2->value) symbol;\
        mpq_clear(temp);\
        return ret;\
    }\
    static bool compare(Ratnum* number1, int number2)\
    {\
        mpq_t temp;\
        mpq_init(temp);\
        mpq_set_si(temp, number2, 1);\
        mpq_canonicalize(temp);\
        bool ret = mpq_cmp(number1->value, temp) symbol;\
        mpq_clear(temp);\
        return ret;\
    }\
    static bool compare(int number1, Ratnum* number2)\
    {\
        mpq_t temp;\
        mpq_init(temp);\
        mpq_set_si(temp, number1, 1);\
        mpq_canonicalize(temp);\
        bool ret = mpq_cmp(temp, number2->value) symbol;\
        mpq_clear(temp);\
        return ret;\
    }

    MAKE_RATNUM_COMPARE(gt, >0);
    MAKE_RATNUM_COMPARE(ge, >=0);
    MAKE_RATNUM_COMPARE(lt, <0);
    MAKE_RATNUM_COMPARE(le, <=0);
    MAKE_RATNUM_COMPARE(eq, ==0);

    static Object makeNumber(double v)
    {
        mpq_t ret;
        mpq_init(ret);
        mpq_set_d(ret, v);
        return makeNumber(ret);
    }

    static Object makeNumber(mpq_t r)
    {
        if (mpz_cmp_si(mpq_denref(r), 1) == 0) {
            if (mpz_cmp_si(mpq_numref(r), Fixnum::MIN) >= 0 &&
                mpz_cmp_si(mpq_numref(r), Fixnum::MAX) <= 0) {
                const Object ret = Object::makeFixnum(mpz_get_si(mpq_numref(r)));
                mpq_clear(r);
                return ret;
            } else {
                mpz_t copy;
                mpz_init_set(copy, mpq_numref(r));
                mpq_clear(r);
                return Object::makeBignum(new Bignum(copy));
            }
        } else {
            return Object::makeRatnum(new Ratnum(r));
        }
    }
};

} // namespace scheme

#endif // SCHEME_RATIONAL_

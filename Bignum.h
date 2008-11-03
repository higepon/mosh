/*
 * Bignum.h -
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
 *  $Id: Bignum.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_BIGNUM__
#define __SCHEME_BIGNUM__

#include "scheme.h"
#include "Fixnum.h"

namespace scheme {

class Bignum EXTEND_GC
{
public:
    Bignum(long value);
    Bignum(mpz_t value);
    ~Bignum();

    char* toString() const;
    double toDouble() const;

    bool isZero() const
    {
        return mpz_cmp_si(value, 0) == 0;
    }

#define MAKE_BIGNUM_OP(op)\
    static Object op(int n1, Bignum* n2)\
    {\
        Bignum* b = new Bignum(n1);\
        mpz_##op(b->value, b->value, n2->value);\
        return makeNumber(b);\
    }\
    static Object op(Bignum* n1, int n2)\
    {\
        Bignum* b = new Bignum(n2);\
        mpz_##op(b->value, n1->value, b->value);\
        return makeNumber(b);\
    }\
    static Object op(Bignum* n1, Bignum* n2)\
    {\
        Bignum* ret = new Bignum(1);\
        mpz_##op(ret->value, n1->value, n2->value);\
        return makeNumber(ret);\
    }

    MAKE_BIGNUM_OP(add)
    MAKE_BIGNUM_OP(sub)
    MAKE_BIGNUM_OP(mul)

#define MAKE_BIGNUM_COMPARE(compare, symbol)\
    static bool compare(Bignum* n1, int n2)\
    {\
        return mpz_cmp_si(n1->value, n2) symbol;\
    }\
    static bool compare(int n1, Bignum* n2)\
    {\
        return (- mpz_cmp_si(n2->value, n1)) symbol;\
    }\
    static bool compare(Bignum* n1, Bignum* n2)\
    {\
        return mpz_cmp(n1->value, n2->value) symbol;\
    }

    MAKE_BIGNUM_COMPARE(gt, >0)
    MAKE_BIGNUM_COMPARE(ge, >=0)
    MAKE_BIGNUM_COMPARE(lt, <0)
    MAKE_BIGNUM_COMPARE(le, <=0)
    MAKE_BIGNUM_COMPARE(eq, ==0)

    static Object add(int n1, int n2)
    {
        const long ret = n1 + n2;
        if (Fixnum::canFit(ret)) {
            return Object::makeFixnum(ret);
        } else {
            return Object::makeBignum(ret);
        }
    }
    static Object sub(int n1, int n2)
    {
        const long ret = n1 - n2;
        if (Fixnum::canFit(ret)) {
            return Object::makeFixnum(ret);
        } else {
            return Object::makeBignum(ret);
        }
    }

    static Object mul(int n1, int n2)
    {
        const long ret = n1 * n2;

        /* Overflow check from Gauche */
        if ((n1 != 0 && ret / n2 != n1) || !Fixnum::canFit(ret)) {
            return Bignum::mul(n1, n2);
        } else {
            return Object::makeFixnum(ret);
        }
    }


    mpz_t value;

private:
    static Object makeNumber(Bignum* b)
    {
        if (mpz_cmp_si(b->value, Fixnum::MIN) >= 0 &&
            mpz_cmp_si(b->value, Fixnum::MAX) <= 0) {
            return Object::makeFixnum(mpz_get_si(b->value));
        } else {
            return Object::makeBignum(b);
        }
    }
};

}; // namespace scheme

#endif // __SCHEME_BIGNUM__

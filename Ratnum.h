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

class Fraction : public gc_cleanup
{
public:
    Fraction()
    {
        mpq_init(value_);
    }

    Fraction(int numerator, int denominator)
    {
        mpq_init(value_);
        if (numerator >= 0 && denominator < 0) {
            mpq_set_si(value_, -numerator, ::abs(denominator));
        } else if (numerator < 0 && denominator< 0) {
            mpq_set_si(value_, ::abs(numerator), ::abs(denominator));
        } else {
            mpq_set_si(value_, numerator, denominator);
        }
        mpq_canonicalize(value_);
    }

    Fraction(double value)
    {
        mpq_init(value_);
        mpq_set_d(value_, value);
    }

    Fraction(const Integer* integer)
    {
        mpq_init(value_);
        mpq_set_z(value_, integer->value_);
    }

    ~Fraction()
    {
        mpq_clear(value_);
    }

    Fraction* abs() const
    {
        Fraction* ret = new Fraction;
        mpq_abs(ret->value_, value_);
        return ret;
    }

    void setNumerator(const Integer* integer)
    {
        mpq_set_num(value_, integer->value_);
    }

    Integer* numerator() const
    {
        Integer* ret = new Integer;
        mpz_set(ret->value_, mpq_numref(value_));
        return ret;
    }

    void setDenominator(const Integer* integer)
    {
        mpq_set_den(value_, integer->value_);
    }

    Integer* denominator() const
    {
        Integer* ret = new Integer;
        mpz_set(ret->value_, mpq_denref(value_));
        return ret;
    }

    char* toString(int radix = 10) const
    {
        char* buf = new(PointerFreeGC)char[mpz_sizeinbase(mpq_numref(value_), radix)
                                       + mpz_sizeinbase(mpq_denref(value_), radix) + 3];
        return mpq_get_str(buf, radix, value_);
    }

    double toDouble() const
    {
        return mpq_get_d(value_);
    }

    bool eq(int number) const
    {
        return mpq_cmp_si(value_, number, 1) == 0;
    }

    bool eq(const Fraction* number) const
    {
        return mpq_cmp(value_, number->value_) == 0;
    }

    Fraction* sqrtAbs() const
    {
        Fraction* ret = abs();
        ret->setDenominator(ret->denominator()->sqrt());
        ret->setNumerator(ret->numerator()->sqrt());
        return ret;
    }

    Integer* floor() const
    {
        Integer* ret = new Integer;
        mpz_fdiv_q(ret->value_,  mpq_numref(value_), mpq_denref(value_));
        return ret;
    }

    Integer* ceiling() const
    {
        Integer* ret = new Integer;
        mpz_cdiv_q(ret->value_,  mpq_numref(value_), mpq_denref(value_));
        return ret;
    }

    Integer* truncate() const
    {
        Integer* ret = new Integer;
        mpz_tdiv_q(ret->value_,  mpq_numref(value_), mpq_denref(value_));
        return ret;
    }


// Object Ratnum::truncate() const
// {
//     mpz_t ret;
//     mpz_init(ret);
//     mpz_tdiv_q(ret, mpq_numref(value), mpq_denref(value));
//     return Bignum::makeInteger(ret);
// }


    static Fraction* div(int n1, const Integer* n2)
    {
        Fraction* ret = new Fraction(n1, 1);
        mpq_t temp;
        mpq_init(temp);
        mpq_set_z(temp, n2->value_);
        mpq_div(ret->value_, ret->value_, temp);
        mpq_clear(temp);
        return ret;
    }

    static Fraction* div(const Integer* n1, int n2)
    {
        Fraction* ret = new Fraction(n1);
        mpq_t temp;
        mpq_init(temp);
        mpq_set_si(temp, n2, 1);
        mpq_div(ret->value_, ret->value_, temp);
        mpq_clear(temp);
        return ret;
    }

    static Fraction* div(const Integer* n1, const Integer* n2)
    {
        Fraction* ret = new Fraction(n1);
        Fraction temp(n2);
        mpq_div(ret->value_, ret->value_, temp.value_);
        return ret;
    }

    static Fraction* add(const Fraction* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_add(ret->value_, number1->value_, number2->value_);
        return ret;
    }

    static Fraction* add(const Fraction* number1, const Integer* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number2->value_);
        mpq_add(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* add(const Integer* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number1->value_);
        mpq_add(ret->value_, ret->value_, number2->value_);
        return ret;
    }

    static Fraction* add(const Fraction* number1, int number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number2, 1);
        mpq_canonicalize(ret->value_);
        mpq_add(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* add(int number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number1, 1);
        mpq_canonicalize(ret->value_);
        mpq_add(ret->value_, ret->value_, number2->value_);
        return ret;
    }

    static Fraction* sub(const Fraction* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_sub(ret->value_, number1->value_, number2->value_);
        return ret;
    }

    static Fraction* sub(const Fraction* number1, const Integer* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number2->value_);
        mpq_sub(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* sub(const Integer* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number1->value_);
        mpq_sub(ret->value_, ret->value_, number2->value_);
        return ret;
    }

    static Fraction* sub(const Fraction* number1, int number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number2, 1);
        mpq_canonicalize(ret->value_);
        mpq_sub(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* sub(int number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number1, 1);
        mpq_canonicalize(ret->value_);
        mpq_sub(ret->value_, ret->value_, number2->value_);
        return ret;
    }

    static Fraction* mul(const Fraction* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_mul(ret->value_, number1->value_, number2->value_);
        return ret;
    }

    static Fraction* mul(const Fraction* number1, const Integer* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number2->value_);
        mpq_mul(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* mul(const Integer* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number1->value_);
        mpq_mul(ret->value_, ret->value_, number2->value_);
        return ret;
    }

    static Fraction* mul(const Fraction* number1, int number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number2, 1);
        mpq_canonicalize(ret->value_);
        mpq_mul(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* mul(int number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number1, 1);
        mpq_canonicalize(ret->value_);
        mpq_mul(ret->value_, ret->value_, number2->value_);
        return ret;
    }


    static Fraction* div(const Fraction* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_div(ret->value_, number1->value_, number2->value_);
        return ret;
    }

    static Fraction* div(const Fraction* number1, const Integer* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number2->value_);
        mpq_div(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* div(const Integer* number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_z(ret->value_, number1->value_);
        mpq_div(ret->value_, ret->value_, number2->value_);
        return ret;
    }

    static Fraction* div(const Fraction* number1, int number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number2, 1);
        mpq_canonicalize(ret->value_);
        mpq_div(ret->value_, number1->value_, ret->value_);
        return ret;
    }

    static Fraction* div(int number1, const Fraction* number2)
    {
        Fraction* ret = new Fraction;
        mpq_set_si(ret->value_, number1, 1);
        mpq_canonicalize(ret->value_);
        mpq_div(ret->value_, ret->value_, number2->value_);
        return ret;
    }

    static bool gt(const Fraction* number1, const Fraction* number2)
    {
        return mpq_cmp(number1->value_, number2->value_) > 0;
    }

    static bool gt(const Fraction* number1, const Integer* number2)
    {
        Fraction temp(number2);
        return gt(number1, &temp);
    }

    static bool gt(const Integer* number1, const Fraction* number2)
    {
        Fraction temp(number1);
        return gt(&temp, number2);
    }

    static bool gt(const Fraction* number1, int number2)
    {
        Fraction temp(number2, 1);
        return gt(number1, &temp);
    }

    static bool gt(int number1, Fraction* number2)
    {
        Fraction temp(number1, 1);
        return gt(&temp, number2);
    }



    static bool ge(const Fraction* number1, const Fraction* number2)
    {
        return mpq_cmp(number1->value_, number2->value_) >= 0;
    }

    static bool ge(const Fraction* number1, const Integer* number2)
    {
        Fraction temp(number2);
        return ge(number1, &temp);
    }

    static bool ge(const Integer* number1, const Fraction* number2)
    {
        Fraction temp(number1);
        return ge(&temp, number2);
    }

    static bool ge(const Fraction* number1, int number2)
    {
        Fraction temp(number2, 1);
        return ge(number1, &temp);
    }

    static bool ge(int number1, Fraction* number2)
    {
        Fraction temp(number1, 1);
        return ge(&temp, number2);
    }


    static bool lt(const Fraction* number1, const Fraction* number2)
    {
        return mpq_cmp(number1->value_, number2->value_) < 0;
    }

    static bool lt(const Fraction* number1, const Integer* number2)
    {
        Fraction temp(number2);
        return lt(number1, &temp);
    }

    static bool lt(const Integer* number1, const Fraction* number2)
    {
        Fraction temp(number1);
        return lt(&temp, number2);
    }

    static bool lt(const Fraction* number1, int number2)
    {
        Fraction temp(number2, 1);
        return lt(number1, &temp);
    }

    static bool lt(int number1, Fraction* number2)
    {
        Fraction temp(number1, 1);
        return lt(&temp, number2);
    }

    static bool le(const Fraction* number1, const Fraction* number2)
    {
        return mpq_cmp(number1->value_, number2->value_) <= 0;
    }

    static bool le(const Fraction* number1, const Integer* number2)
    {
        Fraction temp(number2);
        return le(number1, &temp);
    }

    static bool le(const Integer* number1, const Fraction* number2)
    {
        Fraction temp(number1);
        return le(&temp, number2);
    }

    static bool le(const Fraction* number1, int number2)
    {
        Fraction temp(number2, 1);
        return le(number1, &temp);
    }

    static bool le(int number1, Fraction* number2)
    {
        Fraction temp(number1, 1);
        return le(&temp, number2);
    }


    static bool eq(const Fraction* number1, const Fraction* number2)
    {
        return mpq_cmp(number1->value_, number2->value_) == 0;
    }

    static bool eq(const Fraction* number1, const Integer* number2)
    {
        Fraction temp(number2);
        return eq(number1, &temp);
    }

    static bool eq(const Integer* number1, const Fraction* number2)
    {
        Fraction temp(number1);
        return eq(&temp, number2);
    }

    static bool eq(const Fraction* number1, int number2)
    {
        Fraction temp(number2, 1);
        return eq(number1, &temp);
    }

    static bool eq(int number1, Fraction* number2)
    {
        Fraction temp(number1, 1);
        return eq(&temp, number2);
    }

private:
    mpq_t value_;
};


class Ratnum EXTEND_GC
{
public:
    Ratnum(int numerator, int denominator) : fraction_(new Fraction(numerator, denominator))
    {
    }

    Ratnum(Fraction* fraction) : fraction_(fraction)
    {
    }

//     Ratnum(const Fraction* fraction)
//     {
//         mpq_set(value, rational);
//     }

    Object sqrt() const;
    Object floor() const;
    Object ceiling() const;
    Object round() const;
    Object truncate() const;

    Object abs() const
    {
        return makeNumber(fraction_->abs());
    }

    Object numerator() const
    {
        return Bignum::makeInteger(fraction_->numerator());
    }

    Object denominator() const
    {
        return Bignum::makeInteger(fraction_->denominator());
    }

    char* toString(int radix = 10) const
    {
        return fraction_->toString(radix);
    }

    double toDouble() const
    {
        return fraction_->toDouble();
    }

    bool equal(int number)
    {
        return fraction_->eq(number);
    }

    bool equal(Ratnum* number)
    {
        return fraction_->eq(number->fraction_);
    }

    static Object div(int n1, Bignum* n2)
    {
        return makeNumber(Fraction::div(n1, n2->integer()));
    }

    static Object div(Bignum* n1, int n2)
    {
        return makeNumber(Fraction::div(n1->integer(), n2));
    }

    static Object div(Bignum* n1, Bignum* n2)
    {
        return makeNumber(Fraction::div(n1->integer(), n2->integer()));
    }


#define MAKE_RATNUM_OP(op)\
    static Object op(Ratnum* number1, Ratnum* number2)\
    {\
        return makeNumber(Fraction::op(number1->fraction_, number2->fraction_)); \
    }\
    static Object op(Ratnum* number1, Bignum* number2)\
    {\
        return makeNumber(Fraction::op(number1->fraction_, number2->integer())); \
    }\
    static Object op(Bignum* number1, Ratnum* number2)\
    {\
        return makeNumber(Fraction::op(number1->integer(), number2->fraction_)); \
    }\
    static Object op(Ratnum* number1, int number2)\
    {\
        return makeNumber(Fraction::op(number1->fraction_, number2)); \
    }\
    static Object op(int number1, Ratnum* number2)\
    {\
        return makeNumber(Fraction::op(number1, number2->fraction_)); \
    }

    MAKE_RATNUM_OP(add)
    MAKE_RATNUM_OP(sub)
    MAKE_RATNUM_OP(mul)
    MAKE_RATNUM_OP(div)

#define MAKE_RATNUM_COMPARE(compare, symbol)\
    static bool compare(Ratnum* number1, Ratnum* number2)\
    {\
        return Fraction::compare(number1->fraction_, number2->fraction_);\
    }\
    static bool compare(Ratnum* number1, Bignum* number2)\
    {\
        return Fraction::compare(number1->fraction_, number2->integer());\
    }\
    static bool compare(Bignum* number1, Ratnum* number2)\
    {\
        return Fraction::compare(number1->integer(), number2->fraction_);\
    }\
    static bool compare(Ratnum* number1, int number2)\
    {\
        return Fraction::compare(number1->fraction_, number2);\
    }\
    static bool compare(int number1, Ratnum* number2)\
    {\
        return Fraction::compare(number1, number2->fraction_);\
    }

    MAKE_RATNUM_COMPARE(gt, >0);
    MAKE_RATNUM_COMPARE(ge, >=0);
    MAKE_RATNUM_COMPARE(lt, <0);
    MAKE_RATNUM_COMPARE(le, <=0);

    MAKE_RATNUM_COMPARE(eq, ==0);

    static Object makeNumber(Fraction* fraction)
    {
        if (Integer::eq(1, fraction->denominator())) {
            if (Integer::ge(fraction->numerator(), Fixnum::MIN) &&
                Integer::le(fraction->numerator(), Fixnum::MAX)) {
                return Object::makeFixnum(fraction->numerator()->toSlong());
            } else {
                return Object::makeBignum(fraction->numerator());
            }
        } else {
            return Object::makeRatnum(fraction);
        }
    }

private:
    Fraction* fraction_;

//    Object sqrtUnsigned(const mpq_t r) const;

};

} // namespace scheme

#endif // SCHEME_RATIONAL_

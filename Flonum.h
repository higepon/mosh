/*
 * Flonum.h -
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
 *  $Id: Flonum.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_FLONUM__
#define __SCHEME_FLONUM__

#include <stdlib.h>
#include <math.h> // isnan
#include "scheme.h"
#include "Bignum.h"

namespace scheme {

class Flonum EXTEND_GC
{
public:
    Flonum(double value) : value_(value)
    {
    }

    ~Flonum()
    {
    }

    static Object fromString(const ucs4string& text)
    {
        const double ret = strtod(text.ascii_c_str(), NULL);
        // should check errno?
        return Object::makeFlonum(ret);
    }

    double value() const { return value_; }
    Object toExact() const;
    Object toRatnum() const;
    bool isNan() const { return isnan(value_); }
    bool isInfinite() const { return isinf(value_); }

    Object numerator() const;
    Object denominator() const;

    bool isRational() const
    {
        return !isinf(value_) && !isnan(value_);
    }

    bool isNegativeZero() const
    {
        union {
            double   dvalue;
            uint64_t uvalue;
        } v;
        v.dvalue = value_;
        return value_ == 0.0 && (v.uvalue >> 63);
    }


    bool isInteger() const
    {
        if (isinf(value_) || isnan(value_)) {
            return false;
        }
        return value_ == ::round(value_);
    }

    static Object expt(Flonum* f1, Flonum* f2)
    {
        return Object::makeFlonum(pow(f1->value(), f2->value()));
    }

    Object abs() const
    {
        return Object::makeFlonum(fabs(value_));
    }

    Object exp() const
    {
        return Object::makeFlonum(::exp(value_));
    }

    Object log() const
    {
        return Object::makeFlonum(::log(value_));
    }

    Object cos() const
    {
        return Object::makeFlonum(::cos(value_));
    }

    Object sin() const
    {
        return Object::makeFlonum(::sin(value_));
    }

    Object tan() const
    {
        return Object::makeFlonum(::tan(value_));
    }

    Object acos() const
    {
        return Object::makeFlonum(::acos(value_));
    }

    Object asin() const
    {
        return Object::makeFlonum(::asin(value_));
    }

    Object atan() const
    {
        return Object::makeFlonum(::atan(value_));
    }

    static Object atan(Flonum* f1, Flonum* f2)
    {
        return Object::makeFlonum(::atan2(f1->value(), f2->value()));
    }

    static Object log(Flonum* f1, Flonum* f2)
    {
        return Object::makeFlonum(::log(f1->value()) / ::log(f2->value()));
    }

    Object floor() const
    {
        return Object::makeFlonum(::floor(value_));
    }

    Object ceiling() const
    {
        return Object::makeFlonum(ceil(value_));
    }

    Object truncate() const
    {
        return Object::makeFlonum(trunc(value_));
    }

    Object round() const
    {
        return Object::makeFlonum(::round(value_));
    }


    static Object integerDiv(Flonum* n1, Flonum* n2)
    {
        const double f1 = n1->value();
        const double f2 = n2->value();
        return Object::makeFlonum(iDiv(f1, f2));
    }

    static Object integerMod(Flonum* n1, Flonum* n2)
    {
        const double f1 = n1->value();
        const double f2 = n2->value();
        return Object::makeFlonum(iMod(f1, f2));
    }

    static Object integerDiv0(Flonum* n1, Flonum* n2)
    {
        const double f1 = n1->value();
        const double f2 = n2->value();
        return Object::makeFlonum(iDiv0(f1, f2));
    }

    static Object integerMod0(Flonum* n1, Flonum* n2)
    {
        const double f1 = n1->value();
        const double f2 = n2->value();
        return Object::makeFlonum(iMod0(f1, f2));
    }

    bool isEven() const
    {
        return (value_ * 0.5 == ::floor(value_ * 0.5));
    }

    bool isOdd() const
    {
        return !isEven();
    }

#define MAKE_FLONUM_COMPARE_FUNC(compare, symbol) \
    static bool compare(const Flonum* n1, const Flonum* n2)\
    {\
        return n1->value() symbol n2->value();\
    }\

    MAKE_FLONUM_COMPARE_FUNC(gt, >)
    MAKE_FLONUM_COMPARE_FUNC(ge, >=)
    MAKE_FLONUM_COMPARE_FUNC(lt, <)
    MAKE_FLONUM_COMPARE_FUNC(le, <=)
    MAKE_FLONUM_COMPARE_FUNC(eq, ==)


#define MAKE_RATIONAL_COMPARE_FUNC(compare, symbol) \
    static bool compare(Flonum* n1, Ratnum* n2)\
    {\
        return n1->value() symbol n2->toDouble();\
    }\
    static bool compare(Ratnum* n1, Flonum* n2)\
    {\
        return n1->toDouble() symbol n2->value();\
    }\
    static bool compare(Flonum* n1, Bignum* n2)\
    {\
        return n1->value() symbol n2->toDouble();\
    }\
    static bool compare(Bignum* n1, Flonum* n2)\
    {\
        return n1->toDouble() symbol n2->value();\
    }


    MAKE_RATIONAL_COMPARE_FUNC(gt, >)
    MAKE_RATIONAL_COMPARE_FUNC(ge, >=)
    MAKE_RATIONAL_COMPARE_FUNC(lt, <)
    MAKE_RATIONAL_COMPARE_FUNC(le, <=)
    MAKE_RATIONAL_COMPARE_FUNC(eq, ==)

#define MAKE_FIXNUM_COMPARE_FUNC(compare, symbol) \
    static bool compare(Flonum* n1, int n2)\
    {\
        return n1->value() symbol static_cast<double>(n2);\
    }\
    static bool compare(int n1, Flonum* n2)\
    {\
        return static_cast<double>(n1) symbol n2->value();\
    }

    MAKE_FIXNUM_COMPARE_FUNC(gt, >)
    MAKE_FIXNUM_COMPARE_FUNC(ge, >=)
    MAKE_FIXNUM_COMPARE_FUNC(lt, <)
    MAKE_FIXNUM_COMPARE_FUNC(le, <=)
    MAKE_FIXNUM_COMPARE_FUNC(eq, ==)

#define MAKE_FLONUM_OP_FUNC(op, symbol) \
    static Object op(Flonum* n1, int n2)\
    {\
        return Object::makeFlonum(n1->value() symbol static_cast<double>(n2));\
    }\
    static Object op(int n1, Flonum* n2)\
    {\
        return Object::makeFlonum(static_cast<double>(n1) symbol n2->value());\
    }\
    static Object op(Flonum* n1, Flonum* n2)\
    {\
        return Object::makeFlonum(n1->value() symbol n2->value());\
    }\
    static Object op(Ratnum* n1, Flonum* n2)\
    {\
        return Object::makeFlonum(n1->toDouble() symbol n2->value());\
    }\
    static Object op(Flonum* n1, Ratnum* n2)\
    {\
        return Object::makeFlonum(n1->value() symbol n2->toDouble());\
    }

    MAKE_FLONUM_OP_FUNC(add, +);
    MAKE_FLONUM_OP_FUNC(sub, -);
    MAKE_FLONUM_OP_FUNC(mul, *);
    MAKE_FLONUM_OP_FUNC(div, /);

#define MAKE_LOCAL_OP_F(op, symbol)\
static Object op(Bignum* n1, Flonum* n2)\
{\
    return Object::makeFlonum(n1->toDouble() symbol n2->value());\
}\
static Object op(Flonum* n1, Bignum* n2)\
{\
    return Object::makeFlonum(n1->value() symbol n2->toDouble());\
}

MAKE_LOCAL_OP_F(add, +)
MAKE_LOCAL_OP_F(sub, -)
MAKE_LOCAL_OP_F(div, /)
MAKE_LOCAL_OP_F(mul, *)



    static void initialize();
    static Object NEGATIVE_INF;
    static Object POSITIVE_INF;
    static Object NOT_A_NUMBER;

//     static Object add(Bignum* n1, Flonum* n2);
//     static Object add(Flonum* n1, Bignum* n2);
//     static Object sub(Bignum* n1, Flonum* n2);
//     static Object sub(Flonum* n1, Bignum* n2);
//     static Object mul(Bignum* n1, Flonum* n2);
//     static Object mul(Flonum* n1, Bignum* n2);
//     static Object div(Bignum* n1, Flonum* n2);
//     static Object div(Flonum* n1, Bignum* n2);

    Object sqrt() const
    {
        if (value_ < 0.0) {
            return Object::makeCompnum(Object::makeFlonum(0.0),
                                       Object::makeFlonum(::sqrt(fabs(value_))));
        } else {
            return Object::makeFlonum(::sqrt(value_));
        }
    }

private:
    static double iDiv(double f1, double f2)
    {
        return f2 > 0.0 ? ::floor(f1 / f2) : - ::floor(f1 / (-f2));
    }

    static double iMod(double f1, double f2)
    {
        return f1 - f2 * iDiv(f1, f2);
    }

    static double iDiv0(double f1, double f2)
    {
        const double div = iDiv(f1, f2);
        const double mod = iMod(f1, f2);
        if (mod < (fabs(f2) / 2.0)) {
            return div;
        } else if (f2 > 0) {
            return div + 1.0;
        } else {
            return div - 1.0;
        }
    }

    static double iMod0(double f1, double f2)
    {
        return f1 - f2 * iDiv0(f1, f2);
    }

    double value_;
};

// for better performance
inline Object Object::makeFlonum(double value)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Flonum,
                                                        reinterpret_cast<word>(new Flonum(value)))));
}


}; // namespace scheme

#endif // __SCHEME_FLONUM__

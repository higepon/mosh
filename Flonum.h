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

    double value() const { return value_; }

#define MAKE_FLONUM_COMPARE_FUNC(compare, symbol) \
    static bool compare(Flonum* n1, Flonum* n2)\
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

    static Object add(Bignum* n1, Flonum* n2);
    static Object add(Flonum* n1, Bignum* n2);
    static Object sub(Bignum* n1, Flonum* n2);
    static Object sub(Flonum* n1, Bignum* n2);
    static Object mul(Bignum* n1, Flonum* n2);
    static Object mul(Flonum* n1, Bignum* n2);
    static Object div(Bignum* n1, Flonum* n2);
    static Object div(Flonum* n1, Bignum* n2);

//     static bool gt(Flonum* n1, Bignum* n2);
//     static bool ge(Flonum* n1, Bignum* n2);
//     static bool lt(Flonum* n1, Bignum* n2);
//     static bool le(Flonum* n1, Bignum* n2);
//     static bool eq(Flonum* n1, Bignum* n2);
//     static bool gt(Bignum* n1, Flonum* n2);
//     static bool ge(Bignum* n1, Flonum* n2);
//     static bool lt(Bignum* n1, Flonum* n2);
//     static bool le(Bignum* n1, Flonum* n2);
//     static bool eq(Bignum* n1, Flonum* n2);

private:
    double value_;
};

}; // namespace scheme

#endif // __SCHEME_FLONUM__

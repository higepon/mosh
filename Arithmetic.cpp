/*
 * Arithmetic.cpp -
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
 *  $Id: Arithmetic.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Arithmetic.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Bignum.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Fixnum.h"
#include "ErrorProcedures.h"

using namespace scheme;

#define MAKE_COMPARE_FUNC(compare, symbol)          \
    bool Arithmetic::compare(Object number1, Object number2) \
    { \
        if (number1.isFixnum()) {\
            if (number2.isFixnum()) {\
                return Fixnum::compare(number1.toFixnum(), number2.toFixnum()); \
            } else if (number2.isRatnum()) {\
                return Ratnum::compare(number1.toFixnum(), number2.toRatnum());\
            } else if (number2.isFlonum()) {\
                return Flonum::compare(number1.toFixnum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Bignum::compare(number1.toFixnum(), number2.toBignum());\
            }\
        } else if (number1.isBignum()) {\
            if (number2.isFixnum()) {\
                return Bignum::compare(number1.toBignum(), number2.toFixnum()); \
            } else if (number2.isRatnum()) {\
                return Ratnum::compare(number1.toBignum(), number2.toRatnum()); \
            } else if (number2.isFlonum()) {\
                return Flonum::compare(number1.toBignum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Bignum::compare(number1.toBignum(), number2.toBignum());\
            }\
        } else if (number1.isRatnum()) {\
            if (number2.isFixnum()) {\
                return Ratnum::compare(number1.toRatnum(), number2.toFixnum());\
            } else if (number2.isRatnum()) {\
                return Ratnum::compare(number1.toRatnum(), number2.toRatnum());\
            } else if (number2.isFlonum()) {\
                return Flonum::compare(number1.toRatnum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Ratnum::compare(number1.toRatnum(), number2.toBignum());\
            }\
        } else if (number1.isFlonum()) {\
            if (number2.isFixnum()) {\
                return Flonum::compare(number1.toFlonum(), number2.toFixnum()); \
            } else if (number2.isRatnum()) {\
                return Flonum::compare(number1.toFlonum(), number2.toRatnum()); \
            } else if (number2.isFlonum()) {\
                return Flonum::compare(number1.toFlonum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Flonum::compare(number1.toFlonum(), number2.toBignum());\
            }\
        }\
        callWrongTypeOfArgumentViolationAfter(#symbol, "number", Pair::list2(number1, number2), Pair::list2(number1, number2));\
        return false;\
    }

MAKE_COMPARE_FUNC(lt, <)
MAKE_COMPARE_FUNC(le, <=)
MAKE_COMPARE_FUNC(gt, >)
MAKE_COMPARE_FUNC(ge, >=)
MAKE_COMPARE_FUNC(eq, =)

#define MAKE_OP_FUNC(op, symbol)\
    Object Arithmetic::op(Object number1, Object number2)\
    {\
        if (number1.isFixnum()) {\
            if (number2.isFixnum()) {\
                return Fixnum::op(number1.toFixnum(), number2.toFixnum());\
            } else if (number2.isRatnum()) {\
                return Ratnum::op(number1.toFixnum(), number2.toRatnum());\
            } else if (number2.isFlonum()) {\
                return Flonum::op(number1.toFixnum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Bignum::op(number1.toFixnum(), number2.toBignum());\
            }\
        } else if (number1.isBignum()) {\
            if (number2.isFixnum()) {\
                return Bignum::op(number1.toBignum(), number2.toFixnum()); \
            } else if (number2.isRatnum()) {\
                return Ratnum::op(number1.toBignum(), number2.toRatnum()); \
            } else if (number2.isFlonum()) {\
                return Flonum::op(number1.toBignum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Bignum::op(number1.toBignum(), number2.toBignum());\
            }\
        } else if (number1.isBignum()) {\
            if (number2.isFixnum()) {\
                return Bignum::op(number1.toBignum(), number2.toFixnum()); \
            } else if (number2.isRatnum()) {\
                return Ratnum::op(number1.toBignum(), number2.toRatnum()); \
            } else if (number2.isFlonum()) {\
                return Flonum::op(number1.toBignum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Bignum::op(number1.toBignum(), number2.toBignum());\
            }\
        } else if (number1.isRatnum()) {\
            if (number2.isFixnum()) {\
                return Ratnum::op(number1.toRatnum(), new Ratnum(number2.toFixnum(), 1));\
            } else if (number2.isRatnum()) {\
                return Ratnum::op(number1.toRatnum(), number2.toRatnum());\
            } else if (number2.isFlonum()) {\
                return Flonum::op(number1.toRatnum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Ratnum::op(number1.toRatnum(), number2.toBignum());\
            }\
        } else if (number1.isFlonum()) {\
            if (number2.isFixnum()) {\
                return Flonum::op(number1.toFlonum(), number2.toFixnum()); \
            } else if (number2.isRatnum()) {\
                return Flonum::op(number1.toFlonum(), number2.toRatnum()); \
            } else if (number2.isFlonum()) {\
                return Flonum::op(number1.toFlonum(), number2.toFlonum());\
            } else if (number2.isBignum()) {\
                return Flonum::op(number1.toFlonum(), number2.toBignum());\
            }\
        }\
        callWrongTypeOfArgumentViolationAfter(#symbol, "number", Pair::list2(number1, number2), Pair::list2(number1, number2));\
        return Object::False;\
    }

MAKE_OP_FUNC(add, +)
MAKE_OP_FUNC(sub, -)
MAKE_OP_FUNC(mul, *)

Object Arithmetic::div(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            if (number2.toFixnum() == 0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Object::makeRatnum(number1.toFixnum(), number2.toFixnum());
            }
        } else if (number2.isRatnum()) {
            if (number2.toRatnum()->equal(0)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Ratnum::div(number1.toFixnum(), number2.toRatnum());
            }
        } else if (number2.isFlonum()) {
            if (number2.toFlonum()->value() == 0.0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Flonum::div(number1.toFixnum(), number2.toFlonum());
            }
        }
    } else if (number1.isRatnum()) {
        if (number2.isFixnum()) {
            if (number2.toFixnum() == 0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Ratnum::div(number1.toRatnum(), number2.toFixnum());
            }
        } else if (number2.isRatnum()) {
            return Ratnum::div(number1.toRatnum(), number2.toRatnum());
        } else if (number2.isFlonum()) {
            if (number2.toFlonum()->value() == 0.0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Flonum::div(number1.toRatnum(), number2.toFlonum());
            }
        }
    } else if (number1.isFlonum()) {
        if (number2.isFixnum()) {
            if (number2.toFixnum() == 0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Flonum::div(number1.toFlonum(), number2.toFixnum());
            }
        } else if (number2.isRatnum()) {
            return Flonum::div(number1.toFlonum(), number2.toRatnum());
        } else if (number2.isFlonum()) {
            if (number2.toFlonum()->value() == 0.0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Flonum::div(number1.toFlonum(), number2.toFlonum());
            }
        }
    }

    callWrongTypeOfArgumentViolationAfter("/", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return Object::False;
}

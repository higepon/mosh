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
#include "Compnum.h"

using namespace scheme;

double Arithmetic::realToDouble(Object n)
{
    MOSH_ASSERT(n.isReal());
    if (n.isFixnum()) {
        return n.toFixnum();
    } else if (n.isBignum()) {
        return n.toBignum()->toDouble();
    } else if (n.isFlonum()) {
        return n.toFlonum()->value();
    } else {
        MOSH_ASSERT(false);
        return 0.0;
    }
}

Object Arithmetic::makePolar(Object n1, Object n2)
{
    MOSH_ASSERT(isRealValued(n1));
    MOSH_ASSERT(isRealValued(n2));
    const Object real = n1.isCompnum() ? n1.toCompnum()->real() : n1;
    const Object imag = n2.isCompnum() ? n2.toCompnum()->real() : n2;
    if (eq(imag, Object::makeFixnum(0))) {
            return real;
    }
    const double r = realToDouble(real);
    const double a = realToDouble(imag);
    return Object::makeCompnum(Object::makeFixnum(r * cos(a)), Object::makeFlonum(r * sin(a)));
}

Object Arithmetic::bitwiseNot(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        return Bignum::makeInteger(~(e.toFixnum()));
    } else if (e.isBignum()) {
        return e.toBignum()->bitwiseNot();
    }
    MOSH_ASSERT(false);
}

Object Arithmetic::bitwiseAnd(Object e1, Object e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    MOSH_ASSERT(e2.isExactInteger());
    if (e1.isFixnum()) {
        if (e2.isFixnum()) {
            return Object::makeFixnum(e1.toFixnum() & e2.toFixnum());
        } else if (e2.isBignum()) {
            return e2.toBignum()->bitwiseAnd(e1.toFixnum());
        }
    } else if (e1.isBignum()) {
        if (e2.isFixnum()) {
            return e1.toBignum()->bitwiseAnd(e2.toFixnum());
        } else if (e2.isBignum()) {
            return e1.toBignum()->bitwiseAnd(e2.toBignum());
        }
    }
    MOSH_ASSERT(false);
}

Object Arithmetic::bitwiseIor(Object e1, Object e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    MOSH_ASSERT(e2.isExactInteger());
    if (e1.isFixnum()) {
        if (e2.isFixnum()) {
            return Object::makeFixnum(e1.toFixnum() | e2.toFixnum());
        } else if (e2.isBignum()) {
            return e2.toBignum()->bitwiseIor(e1.toFixnum());
        }
    } else if (e1.isBignum()) {
        if (e2.isFixnum()) {
            return e1.toBignum()->bitwiseIor(e2.toFixnum());
        } else if (e2.isBignum()) {
            return e1.toBignum()->bitwiseIor(e2.toBignum());
        }
    }
    MOSH_ASSERT(false);
}

Object Arithmetic::bitwiseXor(Object e1, Object e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    MOSH_ASSERT(e2.isExactInteger());
    if (e1.isFixnum()) {
        if (e2.isFixnum()) {
            return Object::makeFixnum(e1.toFixnum() ^ e2.toFixnum());
        } else if (e2.isBignum()) {
            return e2.toBignum()->bitwiseXor(e1.toFixnum());
        }
    } else if (e1.isBignum()) {
        if (e2.isFixnum()) {
            return e1.toBignum()->bitwiseXor(e2.toFixnum());
        } else if (e2.isBignum()) {
            return e1.toBignum()->bitwiseXor(e2.toBignum());
        }
    }
    MOSH_ASSERT(false);
}

Object Arithmetic::bitwiseBitCount(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        const int n = e.toFixnum();
        if (n > 0) {
            return Object::makeFixnum(nbits(n));
        } else {
            return Object::makeFixnum(~nbits(~n));
        }
    } else if (e.isBignum()){
        return e.toBignum()->bitwiseBitCount();
    }
    MOSH_ASSERT(false);
}

Object Arithmetic::bitwiseLength(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        const int n = e.toFixnum();
        if (n == 0) {
            return Object::makeFixnum(0);
        }
        const uint32_t un = (n < 0) ? ~n : n;
        return Object::makeFixnum(32 - nlz(un));
    } else if (e.isBignum()){
        return e.toBignum()->bitwiseLength();
    }
    MOSH_ASSERT(false);
}

Object Arithmetic::bitwiseFirstBitSet(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        const int n = e.toFixnum();
        if (n == 0) {
            return Object::makeFixnum(-1);
        }
        int bit = 0;
        bit += ntz(n);
        return Object::makeFixnum(bit);
    } else if (e.isBignum()){
        return e.toBignum()->bitwiseFirstBitSet();
    }
    MOSH_ASSERT(false);
    return Object::False;
}

Object Arithmetic::bitwiseShiftLeft(Object e1, unsigned long e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    if (e1.isFixnum()) {
        return Bignum::bitwiseShiftLeft(e1.toFixnum(), e2);
    } else if (e1.isBignum()) {
        return Bignum::bitwiseShiftLeft(e1.toBignum(), e2);
    }
    MOSH_ASSERT(false);
    return Object::False;
}

Object Arithmetic::bitwiseShiftRight(Object e1, unsigned long e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    if (e1.isFixnum()) {
        return Bignum::bitwiseShiftRight(e1.toFixnum(), e2);
    } else if (e1.isBignum()) {
        return Bignum::bitwiseShiftRight(e1.toBignum(), e2);
    }
    MOSH_ASSERT(false);
    return Object::False;
}

Object Arithmetic::toFlonum(Object real)
{
    MOSH_ASSERT(real.isReal());
    if (real.isFlonum()) {
        return real;
    } else if (real.isFixnum()) {
        return Object::makeFlonum(real.toFixnum());
    } else if (real.isBignum()) {
        return Object::makeFlonum(real.toBignum()->toDouble());
    } else if (real.isRatnum()) {
        return Object::makeFlonum(real.toRatnum()->toDouble());
    } else {
        MOSH_ASSERT(false);
        return Object::Nil;
    }
}

Object Arithmetic::numerator(Object n)
{
    MOSH_ASSERT(n.isRational());
    if (n.isRatnum()) {
        return n.toRatnum()->numerator();
    } else {
        return n;
    }
}

#include "ProcedureMacro.h"
#include "TextualOutputPort.h"

Object Arithmetic::denominator(Object n)
{
//    VM_LOG1("n=~a", n);
    MOSH_ASSERT(n.isRational());
    if (n.isRatnum()) {
        return n.toRatnum()->denominator();
    } else if (n.isFlonum()) {
        Object ratnum = n.toFlonum()->toRatnum();
        return inexact(ratnum.toRatnum()->denominator());
    } else {
        return Object::makeFixnum(1);
    }
}

bool Arithmetic::isInteger(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFlonum()) {
        if (n.toFlonum()->isNan() ||
            n.toFlonum()->isInfinite()) {
            return false;
        }
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return isZero(c->imag()) && isInteger(c->real());
    }
    return Arithmetic::eq(denominator(n), Object::makeFixnum(1));
}

bool Arithmetic::isRealValued(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isReal()) {
        return true;
    } else if (n.isCompnum()) {
        if (isZero(n.toCompnum()->imag())) {
            return true;
        } else {
            return false;
        }
    }
    MOSH_ASSERT(false);
    return false;
}

bool Arithmetic::isRationalValued(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isRational()) {
        return true;
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return isZero(c->imag()) && isRationalValued(c->real());
    } else {
        // +nan.0 +inf.0
        return false;
    }
}

bool Arithmetic::isIntegerValued(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isInteger()) {
        return true;
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return isZero(c->imag()) && isIntegerValued(c->real());
    } else {
        return false;
    }
}

bool Arithmetic::isExactZero(Object n)
{
    return isZero(n) && isExact(n);
}

bool Arithmetic::isZero(Object n)
{
    return Arithmetic::eq(Object::makeFixnum(0), n);
}

Object Arithmetic::exact(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum() || n.isBignum() || n.isRatnum()) {
        return n;
    } else if (n.isFlonum()) {
        return n.toFlonum()->toRatnum();
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return Object::makeCompnum(exact(c->real()), exact(c->imag()));
    } else {
        MOSH_ASSERT(false);
    }
}

Object Arithmetic::inexact(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Object::makeFlonum(n.toFixnum());
    } else if (n.isBignum()) {
        return Object::makeFlonum(n.toBignum()->toDouble());
    } else if (n.isFlonum()) {
        return n;
    } else if (n.isRatnum()) {
        return Object::makeFlonum(n.toRatnum()->toDouble());
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return Object::makeCompnum(inexact(c->real()), inexact(c->imag()));
    } else {
        MOSH_ASSERT(false);
    }
}

bool Arithmetic::isExact(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum() || n.isBignum() || n.isRatnum()) {
        return true;
    } else if (n.isFlonum()) {
        return false;
    } else if (n.isCompnum()) {
        return
            isExact(n.toCompnum()->real()) &&
            isExact(n.toCompnum()->imag());
    } else {
        MOSH_ASSERT(false);
        return false;
    }
}

#define MAKE_REAL_COMPARE_FUNC(compare, symbol)          \
    bool Arithmetic::compare(Object n1, Object n2) \
    { \
        if (n1.isFixnum()) {\
            if (n2.isFixnum()) {\
                return Fixnum::compare(n1.toFixnum(), n2.toFixnum()); \
            } else if (n2.isRatnum()) {\
                return Ratnum::compare(n1.toFixnum(), n2.toRatnum());\
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toFixnum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Bignum::compare(n1.toFixnum(), n2.toBignum());\
            }\
        } else if (n1.isBignum()) {\
            if (n2.isFixnum()) {\
                return Bignum::compare(n1.toBignum(), n2.toFixnum()); \
            } else if (n2.isRatnum()) {\
                return Ratnum::compare(n1.toBignum(), n2.toRatnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toBignum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Bignum::compare(n1.toBignum(), n2.toBignum());\
            }\
        } else if (n1.isRatnum()) {\
            if (n2.isFixnum()) {\
                return Ratnum::compare(n1.toRatnum(), n2.toFixnum());\
            } else if (n2.isRatnum()) {\
                return Ratnum::compare(n1.toRatnum(), n2.toRatnum());\
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toRatnum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Ratnum::compare(n1.toRatnum(), n2.toBignum());\
            }\
        } else if (n1.isFlonum()) {\
            if (n2.isFixnum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toFixnum()); \
            } else if (n2.isRatnum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toRatnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toBignum());\
            }\
        }\
        callWrongTypeOfArgumentViolationAfter(#symbol, "number", Pair::list2(n1, n2), Pair::list2(n1, n2));\
        return false;\
    }

MAKE_REAL_COMPARE_FUNC(lt, <)
MAKE_REAL_COMPARE_FUNC(le, <=)
MAKE_REAL_COMPARE_FUNC(gt, >)
MAKE_REAL_COMPARE_FUNC(ge, >=)

bool Arithmetic::eq(Object n1, Object n2)
{
    if (n1.isFixnum()) {
        if (n2.isFixnum()) {
            return Fixnum::eq(n1.toFixnum(), n2.toFixnum());
        } else if (n2.isRatnum()) {
            return Ratnum::eq(n1.toFixnum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toFixnum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            return Bignum::eq(n1.toFixnum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isBignum()) {
        if (n2.isFixnum()) {
            return Bignum::eq(n1.toBignum(), n2.toFixnum());
        } else if (n2.isRatnum()) {
            return Ratnum::eq(n1.toBignum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toBignum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            return Bignum::eq(n1.toBignum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isRatnum()) {
        if (n2.isFixnum()) {
            return Ratnum::eq(n1.toRatnum(), n2.toFixnum());
        } else if (n2.isRatnum()) {
            return Ratnum::eq(n1.toRatnum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toRatnum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            return Ratnum::eq(n1.toRatnum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isFlonum()) {
        if (n2.isFixnum()) {
            return Flonum::eq(n1.toFlonum(), n2.toFixnum());
        } else if (n2.isRatnum()) {
            return Flonum::eq(n1.toFlonum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toFlonum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            return Flonum::eq(n1.toFlonum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isCompnum()) {
        if (n2.isFixnum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isRatnum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isFlonum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isBignum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1.toCompnum(), n2.toCompnum());
        }
    }
    callWrongTypeOfArgumentViolationAfter("=", "number", Pair::list2(n1, n2), Pair::list2(n1, n2));
    return false;
}


#define MAKE_OP_FUNC(op, symbol)\
    Object Arithmetic::op(Object n1, Object n2)\
    {\
        MOSH_ASSERT(n1.isNumber()); \
        MOSH_ASSERT(n2.isNumber()); \
        if (n1.isFixnum()) {\
            if (n2.isFixnum()) {\
                return Bignum::op(n1.toFixnum(), n2.toFixnum());\
            } else if (n2.isRatnum()) {\
                return Ratnum::op(n1.toFixnum(), n2.toRatnum());\
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toFixnum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Bignum::op(n1.toFixnum(), n2.toBignum());\
            } else if (n2.isCompnum()) {\
                return Compnum::op(n1, n2.toCompnum());\
            }\
        } else if (n1.isBignum()) {\
            if (n2.isFixnum()) {\
                return Bignum::op(n1.toBignum(), n2.toFixnum()); \
            } else if (n2.isRatnum()) {\
                return Ratnum::op(n1.toBignum(), n2.toRatnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toBignum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Bignum::op(n1.toBignum(), n2.toBignum());\
            } else if (n2.isCompnum()) {\
                return Compnum::op(n1, n2.toCompnum());\
            }\
        } else if (n1.isRatnum()) {\
            if (n2.isFixnum()) {\
                return Ratnum::op(n1.toRatnum(), new Ratnum(n2.toFixnum(), 1));\
            } else if (n2.isRatnum()) {\
                return Ratnum::op(n1.toRatnum(), n2.toRatnum());\
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toRatnum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Ratnum::op(n1.toRatnum(), n2.toBignum());\
            } else if (n2.isCompnum()) { \
                return Compnum::op(n1, n2.toCompnum());\
            }\
        } else if (n1.isFlonum()) {\
            if (n2.isFixnum()) {\
                return Flonum::op(n1.toFlonum(), n2.toFixnum()); \
            } else if (n2.isRatnum()) {\
                return Flonum::op(n1.toFlonum(), n2.toRatnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toFlonum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Flonum::op(n1.toFlonum(), n2.toBignum());\
            } else if (n2.isCompnum()) { \
                return Compnum::op(n1, n2.toCompnum());\
            }\
        } else if (n1.isCompnum()) {\
            if (n2.isFixnum() || n2.isBignum() || n2.isRatnum() || n2.isFlonum()) { \
                 return Compnum::op(n1.toCompnum(), n2);\
            } else if (n2.isCompnum()) {\
                return Compnum::op(n1.toCompnum(), n2.toCompnum());\
            }\
        }\
        callWrongTypeOfArgumentViolationAfter(#symbol, "number", Pair::list2(n1, n2), Pair::list2(n1, n2));\
        return Object::False;\
    }

MAKE_OP_FUNC(add, +)
MAKE_OP_FUNC(sub, -)
MAKE_OP_FUNC(mul, *)

Object Arithmetic::mul(int number1, Object number2)
{
    return mul(Object::makeFixnum(number1), number2);
}

#include "ProcedureMacro.h"
#include "TextualOutputPort.h"
Object Arithmetic::div(Object n1, Object n2)
{
    if (n1.isFixnum()) {
        if (n2.isFixnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Object::makeRatnum(n1.toFixnum(), n2.toFixnum());
            }
        } else if (n2.isRatnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Ratnum::div(n1.toFixnum(), n2.toRatnum());
            }
        } else if (n2.isFlonum()) {
            if (n2.toFlonum()->value() == 0.0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Flonum::div(n1.toFixnum(), n2.toFlonum());
            }
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Ratnum::div(n1.toFixnum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Compnum::div(n1, n2.toCompnum());
            }
        }
    } else if (n1.isRatnum()) {
        if (n2.isFixnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Ratnum::div(n1.toRatnum(), n2.toFixnum());
            }
        } else if (n2.isRatnum()) {
            return Ratnum::div(n1.toRatnum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            if (n2.toFlonum()->value() == 0.0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Flonum::div(n1.toRatnum(), n2.toFlonum());
            }
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Ratnum::div(n1.toRatnum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Compnum::div(n1, n2.toCompnum());
            }
        }
    } else if (n1.isFlonum()) {
        if (n2.isFixnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Flonum::div(n1.toFlonum(), n2.toFixnum());
            }
        } else if (n2.isRatnum()) {
            return Flonum::div(n1.toFlonum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            // we don't check division by zero.
            return Flonum::div(n1.toFlonum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Flonum::div(n1.toFlonum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Compnum::div(n1, n2.toCompnum());
            }
        }
    } else if (n1.isBignum()) {
        if (n2.isFixnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Ratnum::div(n1.toBignum(), n2.toFixnum());
            }
        } else if (n2.isRatnum()) {
            return Ratnum::div(n1.toBignum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            if (n2.toFlonum()->value() == 0.0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Flonum::div(n1.toBignum(), n2.toFlonum());
            }
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Ratnum::div(n1.toBignum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Compnum::div(n1, n2.toCompnum());
            }
        }
    } else if (n1.isCompnum()) {
        if (n2.isFixnum() || n2.isBignum() || n2.isRatnum() || n2.isFlonum()) {
            if (isExactZero(n2)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(n1, n2));
                return Object::False;
            } else {
                return Compnum::div(n1.toCompnum(), n2);
            }
        } else if (n2.isCompnum()) {
            return Compnum::div(n1.toCompnum(), n2.toCompnum());
        }
    }

    callWrongTypeOfArgumentViolationAfter("/", "number", Pair::list2(n1, n2), Pair::list2(n1, n2));
    return Object::False;
}

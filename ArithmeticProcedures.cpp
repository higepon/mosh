/*
 * ArithmeticProcedures.cpp - arithmetic procedures.
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
 *   Copyright (c) 2008  Kokosabu(MIURA Yasuyuki)  <kokosabu@gmail.com>
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
 *  $Id: ArithmeticProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ArithmeticProcedures.h"
#include "ProcedureMacro.h"
#include "Arithmetic.h"
#include "Ratnum.h"
#include "Fixnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "ProcedureMacro.h"
#include "Compnum.h"
#include "TextualOutputPort.h"

using namespace scheme;

Object scheme::makePolarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-polar");
    checkArgumentLength(2);
    argumentCheckNumber(0, n1);
    argumentCheckNumber(1, n2);
    return Arithmetic::makePolar(n1, n2);
}

Object scheme::exptEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("expt");
    checkArgumentLength(2);
    argumentCheckNumber(0, n1);
    argumentCheckNumber(1, n2);
    if (n2.isBignum()) {
        callImplementationRestrictionAfter(theVM, procedureName, "too big", Pair::list2(n1, n2));
        return Object::Undef;
    } else {
        return Arithmetic::expt(n1, n2);
    }
}

Object scheme::sqrtEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("sqrt");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::sqrt(n);
}

Object scheme::acosEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("acos");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::acos(n);
}

Object scheme::atanEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("atan");
    checkArgumentLengthBetween(1, 2);
    if (argc == 1) {
        argumentCheckNumber(0, n);
        bool isDiv0Error = false;
        const Object ret = Arithmetic::atan(n, isDiv0Error);
        if (isDiv0Error) {
            callAssertionViolationAfter(theVM, procedureName, "division by zero", L1(n));
            return Object::Undef;
        } else {
            return ret;
        }
    } else {
        argumentCheckReal(0, n1);
        argumentCheckReal(0, n2);
        return Arithmetic::atan2(n1, n2);
    }
}

Object scheme::asinEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("asin");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::asin(n);
}

Object scheme::tanEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("tan");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    bool isDiv0Error = false;
    const Object ret = Arithmetic::tan(n, isDiv0Error);
    if (isDiv0Error) {
        callAssertionViolationAfter(theVM, procedureName, "division by zero", L1(n));
        return Object::Undef;
    } else {
        return ret;
    }
}

Object scheme::sinEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("sin");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::sin(n);
}

Object scheme::cosEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cos");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::cos(n);
}

Object scheme::logEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("log");
    checkArgumentLengthBetween(1, 2);
    if (argc == 1) {
        argumentCheckNumber(0, n);
        if (Arithmetic::isExactZero(n)) {
            callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "nonzero", L1(n));
            return Object::Undef;
        }
        return Arithmetic::log(n);
    } else {
        argumentCheckNumber(0, n1);

        argumentCheckNumber(1, n2);
        if (Arithmetic::isExactZero(n1) || Arithmetic::isExactZero(n2)) {
            callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "nonzero", L2(n1, n2));
            return Object::Undef;
        }
        bool isDiv0Error = false;
        const Object ret = Arithmetic::log(n1, n2, isDiv0Error);
        if (isDiv0Error) {
            callAssertionViolationAfter(theVM, procedureName, "division by zero", L2(n1, n2));
            return Object::Undef;
        } else {
            return ret;
        }
    }
}

Object scheme::expEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("exp");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::exp(n);
}

Object scheme::floorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("floor");
    checkArgumentLength(1);
    argumentCheckReal(0, n);
    return Arithmetic::floor(n);
}

Object scheme::ceilingEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("ceiling");
    checkArgumentLength(1);
    argumentCheckReal(0, n);
    return Arithmetic::ceiling(n);
}

Object scheme::truncateEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("truncate");
    checkArgumentLength(1);
    argumentCheckReal(0, n);
    return Arithmetic::truncate(n);
}

Object scheme::roundEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("round");
    checkArgumentLength(1);
    argumentCheckReal(0, n);
    return Arithmetic::round(n);
}

Object scheme::integerDivEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("div");
    checkArgumentLength(2);
    argumentCheckReal(0, n1);
    argumentCheckReal(1, n2);

    if (n1.isFlonum()) {
        Flonum* const flonum = n1.toFlonum();
        if (flonum->isInfinite() || flonum->isNan()) {
            callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "neither infinite nor a NaN", n1);
            return Object::Undef;
        }
    }

    if (n2.isFixnum()) {
        const int fn2 = n2.toFixnum();
        if (0 == fn2) {
            callAssertionViolationAfter(theVM, procedureName, "div by 0 is not defined", Pair::list2(n1, n2));
            return Object::Undef;
        }
    }
    if (n2.isFlonum()) {
        const double fn2 = n2.toFlonum()->value();
        if (0.0 == fn2) {
            callAssertionViolationAfter(theVM, procedureName, "div by 0.0 is not defined", Pair::list2(n1, n2));
            return Object::Undef;
        }
    }

    if (n1.isFixnum() && n2.isFixnum()) {
        return Fixnum::integerDiv(n1.toFixnum(), n2.toFixnum());
    } else if (n1.isFlonum() && n2.isFlonum()) {
        return Flonum::integerDiv(n1.toFlonum(), n2.toFlonum());
    } else {
            bool isDiv0Error = false;
            Object ret = Object::Undef;
        if (Arithmetic::isNegative(n2)) {
            ret = Arithmetic::negate(Arithmetic::floor(Arithmetic::div(n1, Arithmetic::negate(n2), isDiv0Error)));
        } else {
            ret = Arithmetic::floor(Arithmetic::div(n1, n2, isDiv0Error));
        }
        if (isDiv0Error) {
            callAssertionViolationAfter(theVM, procedureName, "division by zero", Pair::list2(n1, n2));
            return Object::Undef;
        } else {
            return ret;
        }

    }
}

Object scheme::integerDiv0Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("div0");
    checkArgumentLength(2);
    argumentCheckReal(0, n1);
    argumentCheckReal(1, n2);
    Object div = integerDivEx(theVM, argc, argv);
    if (div.isUndef()) {
        return Object::Undef;
    }
    Object mod = Arithmetic::sub(n1, Arithmetic::mul(div, n2));
    // we can ignore isDiv0Error parameter of Arithmetic::div.
    // Because we know division by zero never occur.
    bool isDiv0Error = false;
    if (Arithmetic::lt(mod, Arithmetic::abs(Arithmetic::div(n2, Object::makeFixnum(2), isDiv0Error)))) {
        return div;
    } else {
        if (Arithmetic::isNegative(n2)) {
            return Arithmetic::sub(div, Object::makeFixnum(1));
        } else {
            return Arithmetic::add(div, Object::makeFixnum(1));
        }
    }
}

Object scheme::absEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("abs");
    checkArgumentLength(1);
    argumentCheckReal(0, n);
    return Arithmetic::abs(n);
}

Object scheme::evenPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("even?");
    checkArgumentLength(1);
    argumentCheckIntegerValued(0, n);
    return Object::makeBool(Arithmetic::isEven(n));
}

Object scheme::oddPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("odd?");
    checkArgumentLength(1);
    argumentCheckIntegerValued(0, n);
    return Object::makeBool(!Arithmetic::isEven(n));
}

Object scheme::magnitudeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("magnitude");
    checkArgumentLength(1);
    argumentCheckNumber(0, number);
    return Arithmetic::magnitude(number);
}

Object scheme::angleEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("angle");
    checkArgumentLength(1);
    argumentCheckNumber(0, number);
    return Arithmetic::angle(number);
}

Object scheme::complexPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("complex?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isComplex());
}

Object scheme::realPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("real?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isReal());
}

Object scheme::integerPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("integer?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isInteger());
}

Object scheme::realValuedPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("real-valued?");
    checkArgumentLength(1);
    const Object n = argv[0];
    if (n.isNumber()) {
        return Object::makeBool(Arithmetic::isRealValued(n));
    } else {
        return Object::False;
    }
}

Object scheme::rationalValuedPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("rational-valued?");
    checkArgumentLength(1);
    const Object n = argv[0];
    if (n.isNumber()) {
        return Object::makeBool(Arithmetic::isRationalValued(n));
    } else {
        return Object::False;
    }
}

Object scheme::integerValuedPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("integer-valued?");
    checkArgumentLength(1);
    const Object n = argv[0];
    if (n.isNumber()) {
        return Object::makeBool(Arithmetic::isIntegerValued(n));
    } else {
        return Object::False;
    }
}

Object scheme::numeratorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("numerator");
    checkArgumentLength(1);
    argumentCheckRational(0, rational);
    return Arithmetic::numerator(rational);
}

Object scheme::denominatorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("denominator");
    checkArgumentLength(1);
    argumentCheckRational(0, rational);
    return Arithmetic::denominator(rational);
}

Object scheme::infinitePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("infinite?");
    checkArgumentLength(1);
    argumentCheckReal(0, real);
    if (real.isFlonum()) {
        return Object::makeBool(real.toFlonum()->isInfinite());
    } else {
        return Object::False;
    }
}

Object scheme::finitePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("finite?");
    checkArgumentLength(1);
    argumentCheckReal(0, real);
    if (real.isFlonum()) {
        return Object::makeBool(!real.toFlonum()->isInfinite());
    } else {
        return Object::True;
    }
}

Object scheme::nanPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("nan?");
    checkArgumentLength(1);
    argumentCheckReal(0, real);
    if (real.isFlonum()) {
        return Object::makeBool(real.toFlonum()->isNan());
    } else {
        return Object::False;
    }
}

Object scheme::exactPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("exact?");
    checkArgumentLength(1);
    argumentCheckNumber(0, number);
    return Object::makeBool(Arithmetic::isExact(number));
}

Object scheme::inexactPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("inexact?");
    checkArgumentLength(1);
    argumentCheckNumber(0, number);
    return Object::makeBool(!Arithmetic::isExact(number));
}

Object scheme::realPartEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("real-part");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::real(n);
}

Object scheme::imagPartEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("imag-part");
    checkArgumentLength(1);
    argumentCheckNumber(0, n);
    return Arithmetic::imag(n);
}

Object scheme::numberPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("number?");
    checkArgumentLength(1);
    const Object obj = argv[0];
    return Object::makeBool(obj.isNumber());
}

Object scheme::rationalPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("rational?");
    checkArgumentLength(1);
    const Object obj = argv[0];
    return Object::makeBool(obj.isFixnum() || obj.isBignum() || obj.isRatnum());
}

Object scheme::bignumPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bignum?");
    checkArgumentLength(1);
    const Object obj = argv[0];
    return Object::makeBool(obj.isBignum());
}

Object scheme::flonumPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("flonum?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isFlonum());
}

Object scheme::fixnumPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("fixnum?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isFixnum());
}

Object scheme::makeRectangularEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-rectangular");
    checkArgumentLength(2);
    argumentCheckReal(0, real);
    argumentCheckReal(1, imag);
    return Object::makeCompnum(real, imag);
}

Object scheme::fixnumWidthEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("fixnum-width");
    checkArgumentLength(0);
    return Object::makeFixnum(Fixnum::BITS);
}

Object scheme::leastFixnumEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("least-fixnum");
    checkArgumentLength(0);
    return Object::makeFixnum(Fixnum::MIN);
}

Object scheme::greatestFixnumEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("greatest-fixnum");
    checkArgumentLength(0);
    return Object::makeFixnum(Fixnum::MAX);
}

Object scheme::inexactEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("inexact");
    checkArgumentLength(1);
    argumentCheckNumber(0, number);
    return Arithmetic::inexact(number);
}

Object scheme::exactEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("exact");
    checkArgumentLength(1);
    argumentCheckNumber(0, number);
    return Arithmetic::exact(number);
}

Object scheme::maxEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("max");
    checkArgumentLengthAtLeast(1);
    Object maxNumber = Flonum::NEGATIVE_INF;
    for (int i = 0; i < argc; i++) {
        argumentCheckReal(i, number);
        if (number.isFlonum() && (number.toFlonum())->isNan()) {
            return number;
        }
        if (Arithmetic::gt(number, maxNumber)) {
            maxNumber = number;
        }
    }
    return maxNumber;
}

Object scheme::minEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("min");
    checkArgumentLengthAtLeast(1);
    Object minNumber = Flonum::POSITIVE_INF;
    for (int i = 0; i < argc; i++) {
        argumentCheckReal(i, number);
        if (number.isFlonum() && (number.toFlonum())->isNan()) {
            return number;
        }
        if (Arithmetic::lt(number, minNumber)) {
            minNumber = number;
        }
    }
    return minNumber;
}

Object scheme::addEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("+");
    if (0 == argc) {
        return Object::makeFixnum(0);
    } else if (1 == argc) {
        argumentCheckNumber(0, number);
        return number;
    }

    Object ret = Object::makeFixnum(0);
    for (int i = 0; i < argc; i++) {
        // We don't check whether n is number or not here.
        ret = Arithmetic::add(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "number", L2(ret, argv[i]));
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::subEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("-");
    checkArgumentLengthAtLeast(1);

    if (1 == argc) {
        argumentCheckNumber(0, number);
        return Arithmetic::mul(-1, number);
    }

    Object ret = argv[0];
    for (int i = 1; i < argc; i++) {
        ret = Arithmetic::sub(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            callWrongTypeOfArgumentViolationAfter(theVM, "-", "number", L2(ret, argv[i]));
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::mulEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("*");

    if (0 == argc) {
        return Object::makeFixnum(1);
    } else if (1 == argc) {
        argumentCheckNumber(0, number);
        return number;
    }

    Object ret = Object::makeFixnum(1);
    for (int i = 0; i < argc; i++) {
        ret = Arithmetic::mul(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            callWrongTypeOfArgumentViolationAfter(theVM, "*", "number", L2(ret, argv[i]));
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::divideEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("/");
    checkArgumentLengthAtLeast(1);

    bool isDiv0Error = false;
    if (1 == argc) {
        argumentCheckNumber(0, number);
        const Object ret = Arithmetic::div(Object::makeFixnum(1), number, isDiv0Error);
        if (isDiv0Error) {
            callAssertionViolationAfter(theVM, procedureName, "division by zero", L2(Object::makeFixnum(1), number));
            return Object::Undef;
        } else {
            return ret;
        }
    } else {
        Object ret = argv[0];
        for (int i = 1; i < argc; i++) {
            ret = Arithmetic::div(ret, argv[i], isDiv0Error);
            if (isDiv0Error) {
                callAssertionViolationAfter(theVM, procedureName, "division by zero", L2(ret, argv[i]));
                return Object::Undef;
            } else if (ret.isFalse()) {
                callWrongTypeOfArgumentViolationAfter(theVM, "/", "number", L2(ret, argv[i]));
                return Object::Undef;
            }
        }
        return ret;
    }
}

Object scheme::eqEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("=");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentCheckNumber(i, number1);
        argumentCheckNumber(i + 1, number2);
        if (Arithmetic::eq(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::gtEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName(">");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentCheckReal(i, number1);
        argumentCheckReal(i + 1, number2);
        if (Arithmetic::gt(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::geEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName(">=");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentCheckReal(i, number1);
        argumentCheckReal(i + 1, number2);
        if (Arithmetic::ge(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::ltEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("<");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentCheckReal(i, number1);
        argumentCheckReal(i + 1, number2);
        if (Arithmetic::lt(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::leEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("<=");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentCheckReal(i, number1);
        argumentCheckReal(i + 1, number2);
        if (Arithmetic::le(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

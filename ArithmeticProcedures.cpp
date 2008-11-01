/*
 * ArithmeticProcedures.cpp - arithmetic procedures.
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
#include "Rational.h"

using namespace scheme;

Object scheme::numberPEx(int argc, const Object* argv)
{
    DeclareProcedureName("number?");
    checkArgumentLength(1);
    const Object obj = argv[0];
    return Object::makeBool(obj.isFixnum() || obj.isRational());
}

Object scheme::rationalPEx(int argc, const Object* argv)
{
    DeclareProcedureName("rational?");
    checkArgumentLength(1);
    const Object obj = argv[0];
    return Object::makeBool(obj.isFixnum() || obj.isRational());
}

Object scheme::flonumPEx(int argc, const Object* argv)
{
    DeclareProcedureName("flonum?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isFlonum());
}

Object scheme::inexactEx(int argc, const Object* argv)
{
    DeclareProcedureName("inexact");
    const Object obj = argv[0];
    if (obj.isFixnum()) {
        return Object::makeFlonum(obj.toFixnum());
    } else if (obj.isFlonum()) {
        return obj;
    } else if (obj.isRational()) {
        return Object::makeFlonum(obj.toRational()->toDouble());
    }
    MOSH_ASSERT(false);
}


Object scheme::maxEx(int argc, const Object* argv)
{
    DeclareProcedureName("max");
    checkArgumentLengthAtLeast(1);
    Object maxNumber = argv[0];
    for (int i = 1; i < argc; i++) {
        const Object number = argv[i];
        if (Arithmetic::gt(number, maxNumber)) {
            maxNumber = number;
        }
    }
    return maxNumber;
}

Object scheme::minEx(int argc, const Object* argv)
{
    DeclareProcedureName("min");
    checkArgumentLengthAtLeast(1);
    Object minNumber = argv[0];
    for (int i = 1; i < argc; i++) {
        const Object number = argv[i];
        if (Arithmetic::lt(number, minNumber)) {
            minNumber = number;
        }
    }
    return minNumber;
}

Object scheme::addEx(int argc, const Object* argv)
{
    DeclareProcedureName("+");
    if (0 == argc) {
        return Object::makeFixnum(0);
    } else if (1 == argc) {
        argumentCheckFixnum(0, number);
        return number;
    }

    Object ret = Object::makeFixnum(0);
    for (int i = 0; i < argc; i++) {
        ret = Arithmetic::add(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::subEx(int argc, const Object* argv)
{
    DeclareProcedureName("-");
    checkArgumentLengthAtLeast(1);

    if (1 == argc) {
        argumentAsFixnum(0, number);
        return Object::makeFixnum(-1 * number);
    }

    Object ret = argv[0];
    for (int i = 1; i < argc; i++) {
        ret = Arithmetic::sub(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::mulEx(int argc, const Object* argv)
{
    DeclareProcedureName("*");

    if (0 == argc) {
        return Object::makeFixnum(1);
    } else if (1 == argc) {
        argumentCheckFixnum(0, number);
        return number;
    }

    Object ret = Object::makeFixnum(1);
    for (int i = 0; i < argc; i++) {
        ret = Arithmetic::mul(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::divideEx(int argc, const Object* argv)
{
    DeclareProcedureName("/");
    checkArgumentLengthAtLeast(1);

    if (1 == argc) {
        argumentCheckFixnum(0, number);
        return Arithmetic::div(Object::makeFixnum(1), number);
    }

    Object ret = argv[0];
    for (int i = 1; i < argc; i++) {
        ret = Arithmetic::div(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::quotientEx(int argc, const Object* argv)
{
    DeclareProcedureName("quotient");
    checkArgumentLength(2);
    return divideEx(argc, argv);
}

Object scheme::remainderEx(int argc, const Object* argv)
{
    DeclareProcedureName("remainder");
    checkArgumentLength(2);
    argumentAsFixnum(0, a);
    argumentAsFixnum(1, b);
    return Object::makeFixnum(a % b);
}


Object scheme::eqEx(int argc, const Object* argv)
{
    DeclareProcedureName("=");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        Object number1 = argv[i];
        Object number2 = argv[i + 1];
        if (Arithmetic::eq(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::gtEx(int argc, const Object* argv)
{
    DeclareProcedureName(">");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        Object number1 = argv[i];
        Object number2 = argv[i + 1];
        if (Arithmetic::gt(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::geEx(int argc, const Object* argv)
{
    DeclareProcedureName(">=");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        Object number1 = argv[i];
        Object number2 = argv[i + 1];
        if (Arithmetic::ge(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::ltEx(int argc, const Object* argv)
{
    DeclareProcedureName("<");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        Object number1 = argv[i];
        Object number2 = argv[i + 1];
        if (Arithmetic::lt(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::leEx(int argc, const Object* argv)
{
    DeclareProcedureName("<=");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        Object number1 = argv[i];
        Object number2 = argv[i + 1];
        if (Arithmetic::le(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

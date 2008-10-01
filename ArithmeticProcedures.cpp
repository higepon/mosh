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

using namespace scheme;

Object scheme::maxEx(int argc, const Object* argv)
{
    DeclareProcedureName("max");
    checkArgumentLengthAtLeast(1);
    Object maxNumber = argv[0];
    for (int i = 1; i < argc; i++) {
        const Object number = argv[i];
        if (numberGreater(number, maxNumber)) {
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
        if (numberLess(number, minNumber)) {
            minNumber = number;
        }
    }
    return minNumber;
}

Object scheme::addEx(int argc, const Object* argv)
{
    DeclareProcedureName("+");
    if (0 == argc) {
        return Object::makeInt(0);
    } else if (1 == argc) {
        argumentCheckInt(0, number);
        return number;
    }

    Object ret = Object::makeInt(0);
    for (int i = 0; i < argc; i++) {
        ret = numberAdd(ret, argv[i]);

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
        argumentAsInt(0, number);
        return Object::makeInt(-1 * number);
    }

    Object ret = argv[0];
    for (int i = 1; i < argc; i++) {
        ret = numberSub(ret, argv[i]);

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
        return Object::makeInt(1);
    } else if (1 == argc) {
        argumentCheckInt(0, number);
        return number;
    }

    Object ret = Object::makeInt(1);
    for (int i = 0; i < argc; i++) {
        ret = numberMul(ret, argv[i]);

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
        argumentCheckInt(0, number);
        return numberDiv(Object::makeInt(1), number);
    }

    Object ret = argv[0];
    for (int i = 1; i < argc; i++) {
        ret = numberDiv(ret, argv[i]);

        // error occured
        if (ret.isFalse()) {
            return Object::Undef;
        }
    }
    return ret;
}

Object scheme::eqEx(int argc, const Object* argv)
{
    DeclareProcedureName("=");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        Object number1 = argv[i];
        Object number2 = argv[i + 1];
        if (numberEqual(number1, number2)) {
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
        if (numberGreater(number1, number2)) {
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
        if (numberGreaterEqual(number1, number2)) {
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
        if (numberLess(number1, number2)) {
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
        if (numberLessEqual(number1, number2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

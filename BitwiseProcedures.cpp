/*
 * BitwiseProcedures.cpp -
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
 *  $Id: BitwiseProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "ProcedureMacro.h"
#include "ErrorProcedures.h"
#include "Arithmetic.h"
#include "BitwiseProcedures.h"

using namespace scheme;

Object scheme::bitwiseNotEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-not");
    checkArgumentLength(1);
    argumentCheckExactInteger(0, e);
    return Arithmetic::bitwiseNot(e);
}

Object scheme::bitwiseAndEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-and");
    if (0 == argc) {
        return Object::makeFixnum(-1);
    } else if (1 == argc) {
        return argv[0];
    }
    argumentCheckExactInteger(0, arg0);
    Object accum = arg0;
    for (int i = 1; i < argc; i++) {
        argumentCheckExactInteger(i, e);
        accum = Arithmetic::bitwiseAnd(accum, e);
    }
    return accum;
}

Object scheme::bitwiseIorEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-ior");
    if (0 == argc) {
        return Object::makeFixnum(0);
    } else if (1 == argc) {
        return argv[0];
    }
    argumentCheckExactInteger(0, arg0);
    Object accum = arg0;
    for (int i = 1; i < argc; i++) {
        argumentCheckExactInteger(i, e);
        accum = Arithmetic::bitwiseIor(accum, e);
    }
    return accum;
}

Object scheme::bitwiseXorEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-xor");
    if (0 == argc) {
        return Object::makeFixnum(0);
    } else if (1 == argc) {
        return argv[0];
    }
    argumentCheckExactInteger(0, arg0);
    Object accum = arg0;
    for (int i = 1; i < argc; i++) {
        argumentCheckExactInteger(i, e);
        accum = Arithmetic::bitwiseXor(accum, e);
    }
    return accum;
}

Object scheme::bitwiseBitCountEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-bit-count");
    checkArgumentLength(1);
    argumentCheckExactInteger(0, e);
    return Arithmetic::bitwiseBitCount(e);
}

Object scheme::bitwiseLengthEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-length");
    checkArgumentLength(1);
    argumentCheckExactInteger(0, e);
    return Arithmetic::bitwiseLength(e);
}

Object scheme::bitwiseFirstBitSetEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-first-bit-set");
    checkArgumentLength(1);
    argumentCheckExactInteger(0, e);
    return Arithmetic::bitwiseFirstBitSet(e);
}

Object scheme::bitwiseArithmeticShiftLeftEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-arithmetic-shift-left");
    checkArgumentLength(2);
    argumentCheckExactInteger(0, e1);
    argumentAsFixnum(1, e2);

    if (e2 < 0) {
        callWrongTypeOfArgumentViolationAfter(procedureName, "fixnum greater than zero", argv[1]);
        return Object::Undef;
    } else {
        return Arithmetic::bitwiseShiftLeft(e1, static_cast<unsigned long>(e2));
    }
}

Object scheme::bitwiseArithmeticShiftRightEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-arithmetic-shift-right");
    checkArgumentLength(2);
    argumentCheckExactInteger(0, e1);
    argumentAsFixnum(1, e2);

    if (e2 < 0) {
        callWrongTypeOfArgumentViolationAfter(procedureName, "fixnum greater than zero", argv[1]);
        return Object::Undef;
    } else {
        return Arithmetic::bitwiseShiftRight(e1, static_cast<unsigned long>(e2));
    }
}

Object scheme::bitwiseArithmeticShiftEx(int argc, const Object* argv)
{
    DeclareProcedureName("bitwise-arithmetic-shift");
    checkArgumentLength(2);
    argumentCheckExactInteger(0, e1);
    argumentAsFixnum(1, e2);

    if (e2 >= 0) {
        return Arithmetic::bitwiseShiftLeft(e1, static_cast<unsigned long>(e2));
    } else {
        return Arithmetic::bitwiseShiftRight(e1, static_cast<unsigned long>(abs(e2)));
    }
}

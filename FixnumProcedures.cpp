/*
 * FixnumProcedures.cpp -
 *
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
 *  $Id$
 */

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "FixnumProcedures.h"
#include "ProcedureMacro.h"
#include "ErrorProcedures.h"
#include "Fixnum.h"
#include "Bignum.h"
#include "VM.h"
#include "Arithmetic.h"

using namespace scheme;

Object scheme::fxEqPEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx=?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentAsFixnum(i, fx1);
        argumentAsFixnum(i + 1, fx2);
        if (Fixnum::eq(fx1, fx2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::fxGtPEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx>?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentAsFixnum(i, fx1);
        argumentAsFixnum(i + 1, fx2);
        if (Fixnum::gt(fx1, fx2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::fxLtPEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx<?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentAsFixnum(i, fx1);
        argumentAsFixnum(i + 1, fx2);
        if (Fixnum::lt(fx1, fx2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::fxGePEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx>=?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentAsFixnum(i, fx1);
        argumentAsFixnum(i + 1, fx2);
        if (Fixnum::ge(fx1, fx2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::fxLePEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx<=?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentAsFixnum(i, fx1);
        argumentAsFixnum(i + 1, fx2);
        if (Fixnum::le(fx1, fx2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::fxzeroPEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxzero?");
    checkArgumentLength(1);
    argumentAsFixnum(0, fixnum);
    return Object::makeBool(Fixnum::eq(fixnum, 0));
}

Object scheme::fxpositivePEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxpositive?");
    checkArgumentLength(1);
    argumentAsFixnum(0, fixnum);
    return Object::makeBool(Fixnum::gt(fixnum, 0));
}

Object scheme::fxnegativePEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxnegative?");
    checkArgumentLength(1);
    argumentAsFixnum(0, fixnum);
    return Object::makeBool(Fixnum::lt(fixnum, 0));
}

Object scheme::fxoddPEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxodd?");
    checkArgumentLength(1);
    argumentAsFixnum(0, fixnum);
    return Object::makeBool(Fixnum::isOdd(fixnum));
}

Object scheme::fxevenPEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxeven?");
    checkArgumentLength(1);
    argumentAsFixnum(0, fixnum);
    return Object::makeBool(Fixnum::isEven(fixnum));
}

Object scheme::fxmaxEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxmax");
    checkArgumentLengthAtLeast(1);
    Object maxFixnum = Object::makeFixnum(Fixnum::MIN);
    for (int i = 0; i < argc; i++) {
        argumentAsFixnum(i, fixnum);
        if (Fixnum::gt(fixnum, maxFixnum.toFixnum())) {
            maxFixnum = argv[i];
        }
    }
    return maxFixnum;
}

Object scheme::fxminEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxmin");
    checkArgumentLengthAtLeast(1);
    Object minFixnum = Object::makeFixnum(Fixnum::MAX);
    for (int i = 0; i < argc; i++) {
        argumentAsFixnum(i, fixnum);
        if (Fixnum::lt(fixnum, minFixnum.toFixnum())) {
            minFixnum = argv[i];
        }
    }
    return minFixnum;
}

Object scheme::fxAddEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx+");
    checkArgumentLength(2);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    const int32_t ret = fx1 + fx2;

    if (Fixnum::canFit(ret)) {
        return Object::makeFixnum(ret);
    } else {
        callImplementationRestrictionAfter(procedureName, UC("sum is not a fixnum"), Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }
}

Object scheme::fxMulEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx*");
    checkArgumentLength(2);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    const int64_t ret = fx1 * fx2;

    if (Fixnum::canFit(ret)) {
        return Object::makeFixnum(ret);
    } else {
        callImplementationRestrictionAfter(procedureName, UC("product is not a fixnum"), Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }
}

Object scheme::fxSubEx(int argc, const Object* argv)
{
    DeclareProcedureName("fx-");
    checkArgumentLengthBetween(1, 2);

    if (argc == 1) {
        argumentAsFixnum(0, fixnum);
        if (fixnum != Fixnum::MIN) {
            return Object::makeFixnum(-fixnum);
        } else {
            callImplementationRestrictionAfter(procedureName, UC("result is not a fixnum"), Pair::list1(argv[0]));
            return Object::Undef;
        }
    } else {
        argumentAsFixnum(0, fx1);
        argumentAsFixnum(1, fx2);
        const int32_t ret = fx1 - fx2;

        if (Fixnum::canFit(ret)) {
            return Object::makeFixnum(ret);
        } else {
            callImplementationRestrictionAfter(procedureName, UC("difference is not a fixnum"), Pair::list2(argv[0], argv[1]));
            return Object::Undef;
        }
    }
}

Object scheme::fxdivEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxdiv");
    checkArgumentLength(2);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (0 == fx2) {
        callAssertionViolationAfter(procedureName, "Dividing by zero");
        return Object::Undef;
    }
    return Object::makeFixnum(Fixnum::fxdiv(fx1, fx2));
}

Object scheme::fxmodEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxmod");
    checkArgumentLength(2);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (0 == fx2) {
        callAssertionViolationAfter(procedureName, "Dividing by zero");
        return Object::Undef;
    }
    return Object::makeFixnum(Fixnum::fxmod(fx1, fx2));
}

Object scheme::fxdiv0Ex(int argc, const Object* argv)
{
    DeclareProcedureName("fxdiv0");
    checkArgumentLength(2);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (0 == fx2) {
        callAssertionViolationAfter(procedureName, "Dividing by zero");
        return Object::Undef;
    }
    return Object::makeFixnum(Fixnum::fxdiv0(fx1, fx2));
}

Object scheme::fxmod0Ex(int argc, const Object* argv)
{
    DeclareProcedureName("fxmod0");
    checkArgumentLength(2);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (0 == fx2) {
        callAssertionViolationAfter(procedureName, "Dividing by zero");
        return Object::Undef;
    }
    return Object::makeFixnum(Fixnum::fxmod0(fx1, fx2));
}

Object scheme::fxnotEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxnot");
    checkArgumentLength(1);

    argumentAsFixnum(0, fx);
    return Object::makeFixnum(Fixnum::fxnot(fx));
}

// -1 is identity element
Object scheme::fxandEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxand");

    int ret = -1;
    for (int i = 0; i < argc; i++) {
        argumentAsFixnum(i, fx);
        ret = Fixnum::fxand(ret, fx);
    }
    return Object::makeFixnum(ret);
}

// 0 is identity element
Object scheme::fxiorEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxior");

    int ret = 0;
    for (int i = 0; i < argc; i++) {
        argumentAsFixnum(i, fx);
        ret = Fixnum::fxior(ret, fx);
    }
    return Object::makeFixnum(ret);
}

// 0 is identity element
Object scheme::fxxorEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxxor");

    int ret = 0;
    for (int i = 0; i < argc; i++) {
        argumentAsFixnum(i, fx);
        ret = Fixnum::fxxor(ret, fx);
    }
    return Object::makeFixnum(ret);
}

Object scheme::fxifEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxif");
    checkArgumentLength(3);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    argumentAsFixnum(2, fx3);
    return Object::makeFixnum(Fixnum::fxif(fx1, fx2, fx3));
}

Object scheme::fxbitCountEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxbit-count");
    checkArgumentLength(1);
    argumentAsFixnum(0, fx);
    return Object::makeFixnum(Fixnum::fxbitCount(fx));
}

Object scheme::fxlengthEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxlength");
    checkArgumentLength(1);
    argumentAsFixnum(0, fx);
    return Object::makeFixnum(Fixnum::fxlength(fx));
}

Object scheme::fxfirstBitSetEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxfirst-bit-set");
    checkArgumentLength(1);
    argumentAsFixnum(0, fx);
    return Object::makeFixnum(Fixnum::fxfirstBitSet(fx));
}

Object scheme::fxbitSetPEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxbit-set?");
    checkArgumentLength(2);

    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (fx2 < 0 || fx2 >= Fixnum::BITS) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }

    return Object::makeBool(Fixnum::fxbitSetP(fx1, fx2));
}

Object scheme::fxcopyBitEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxcopy-bit");
    checkArgumentLength(3);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    argumentAsFixnum(2, fx3);

    if (fx2 < 0 || fx2 >= Fixnum::BITS) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list3(argv[0], argv[1], argv[2]));
        return Object::Undef;
    }
    if (fx3 != 0 && fx3 != 1) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list3(argv[0], argv[1], argv[2]));
        return Object::Undef;
    }

    return Object::makeFixnum(Fixnum::fxcopyBit(fx1, fx2, fx3));
}

Object scheme::fxbitFieldEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxbit-field");
    checkArgumentLength(3);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    argumentAsFixnum(2, fx3);

    const bool isFx2OutOfRange = fx2 < 0 || fx2 >= Fixnum::BITS;
    const bool isFx3OutOfRange = fx3 < 0 || fx3 >= Fixnum::BITS;
    if (isFx2OutOfRange || isFx3OutOfRange || fx2 > fx3) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list3(argv[0], argv[1], argv[2]));
        return Object::Undef;
    }

    return Object::makeFixnum(Fixnum::fxbitField(fx1, fx2, fx3));
}

Object scheme::fxcopyBitFieldEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxcopy-bit-field");
    checkArgumentLength(4);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    argumentAsFixnum(2, fx3);
    argumentAsFixnum(3, fx4);

    const bool isFx2OutOfRange = fx2 < 0 || fx2 >= Fixnum::BITS;
    const bool isFx3OutOfRange = fx3 < 0 || fx3 >= Fixnum::BITS;
    if (isFx2OutOfRange || isFx3OutOfRange || fx2 > fx3) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list4(argv[0], argv[1], argv[2], argv[3]));
        return Object::Undef;
    }

    return Object::makeFixnum(Fixnum::fxcopyBitField(fx1, fx2, fx3, fx4));
}

Object scheme::fxarithmeticShiftEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxarithmetic-shift");
    checkArgumentLength(2);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (::abs(fx2) >= Fixnum::BITS) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }

    int32_t ret;
    if (fx2 >= 0) {
        ret = Fixnum::fxarithmeticShiftLeft(fx1, fx2);
    } else {
        ret = Fixnum::fxarithmeticShiftRight(fx1, -fx2);
    }
    if (Fixnum::canFit(ret)) {
        return Object::makeFixnum(ret);
    } else {
        callImplementationRestrictionAfter(procedureName, UC("result is not a fixnum"), Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }
}

Object scheme::fxarithmeticShiftLeftEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxarithmetic-shift-left");
    checkArgumentLength(2);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (fx2 < 0 || fx2 >= Fixnum::BITS) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }

    int32_t ret = Fixnum::fxarithmeticShiftLeft(fx1, fx2);
    if (Fixnum::canFit(ret)) {
        return Object::makeFixnum(ret);
    } else {
        callImplementationRestrictionAfter(procedureName, UC("result is not a fixnum"), Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }
}

Object scheme::fxarithmeticShiftRightEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxarithmetic-shift-right");
    checkArgumentLength(2);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);

    if (fx2 < 0 || fx2 >= Fixnum::BITS) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }

    int32_t ret = Fixnum::fxarithmeticShiftRight(fx1, fx2);
    if (Fixnum::canFit(ret)) {
        return Object::makeFixnum(ret);
    } else {
        callImplementationRestrictionAfter(procedureName, UC("result is not a fixnum"), Pair::list2(argv[0], argv[1]));
        return Object::Undef;
    }
}

Object scheme::fxrotateBitFieldEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxrotate-bit-field");
    checkArgumentLength(4);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    argumentAsFixnum(2, fx3);
    argumentAsFixnum(3, fx4);

    const bool isFx2OutOfRange = fx2 < 0 || fx2 >= Fixnum::BITS;
    const bool isFx3OutOfRange = fx3 < 0 || fx3 >= Fixnum::BITS;
    const bool isFx4OutOfRange = fx4 < 0 || fx4 >= Fixnum::BITS;
    if (isFx2OutOfRange || isFx3OutOfRange || isFx4OutOfRange || fx2 > fx3 || fx4 >= (fx3 - fx2) ) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list4(argv[0], argv[1], argv[2], argv[3]));
        return Object::Undef;
    }

    return Object::makeFixnum(Fixnum::fxrotateBitField(fx1, fx2, fx3, fx4));
}

Object scheme::fxreverseBitFieldEx(int argc, const Object* argv)
{
    DeclareProcedureName("fxreverse-bit-field");
    checkArgumentLength(3);
    argumentAsFixnum(0, fx1);
    argumentAsFixnum(1, fx2);
    argumentAsFixnum(2, fx3);

    const bool isFx2OutOfRange = fx2 < 0 || fx2 >= Fixnum::BITS;
    const bool isFx3OutOfRange = fx3 < 0 || fx3 >= Fixnum::BITS;
    if (isFx2OutOfRange || isFx3OutOfRange || fx2 > fx3) {
        callAssertionViolationAfter(procedureName, "out of range", Pair::list3(argv[0], argv[1], argv[2]));
        return Object::Undef;
    }

    return Object::makeFixnum(Fixnum::fxreverseBitField(fx1, fx2, fx3));
}

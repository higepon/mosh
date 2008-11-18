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
#include "FixnumProcedures.h"
#include "ProcedureMacro.h"
#include "ErrorProcedures.h"
#include "Fixnum.h"

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

//Object scheme::fxAddEx(int argc, const Object* argv);
//Object scheme::fxMulEx(int argc, const Object* argv);
//Object scheme::fxSubEx(int argc, const Object* argv);
//Object scheme::fxdivAndModEx(int argc, const Object* argv);
//Object scheme::fxdivEx(int argc, const Object* argv);
//Object scheme::fxmodEx(int argc, const Object* argv);
//Object scheme::fxdiv0AndMod0Ex(int argc, const Object* argv);
//Object scheme::fxdiv0Ex(int argc, const Object* argv);
//Object scheme::fxmod0Ex(int argc, const Object* argv);
////fx+/carry
////fx-/carry
//Object scheme::fxnotEx(int argc, const Object* argv);
//Object scheme::fxandEx(int argc, const Object* argv);
//Object scheme::fxiorEx(int argc, const Object* argv);
//Object scheme::fxxorEx(int argc, const Object* argv);
//Object scheme::fxifEx(int argc, const Object* argv);
//Object scheme::fxbitCountEx(int argc, const Object* argv);
//Object scheme::fxlengthEx(int argc, const Object* argv);
//Object scheme::fxfirstBitSetEx(int argc, const Object* argv);
//Object scheme::fxcopyBitEx(int argc, const Object* argv);
//Object scheme::fxbitFieldEx(int argc, const Object* argv);
//Object scheme::fxcopyBitFieldEx(int argc, const Object* argv);
//Object scheme::fxarithmeticShiftEx(int argc, const Object* argv);
//Object scheme::fxarithmeticShiftLeftEx(int argc, const Object* argv);
//Object scheme::fxarithmeticShiftRightEx(int argc, const Object* argv);
//Object scheme::fxrotateBitFieldEx(int argc, const Object* argv);
//Object scheme::fxreverseBitFieldEx(int argc, const Object* argv);

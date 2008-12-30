/*
 * ArithmeticProcedures.h - arithmetic procedures.
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
 *  $Id: ArithmeticProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_ARITHMETIC_PROCEDURES__
#define __SCHEME_ARITHMETIC_PROCEDURES__

#include "scheme.h"

namespace scheme {

    Object makePolarEx(VM* theVM, int argc, const Object* argv);
    Object exptEx(VM* theVM, int argc, const Object* argv);
    Object sqrtEx(VM* theVM, int argc, const Object* argv);
    Object asinEx(VM* theVM, int argc, const Object* argv);
    Object acosEx(VM* theVM, int argc, const Object* argv);
    Object atanEx(VM* theVM, int argc, const Object* argv);
    Object tanEx(VM* theVM, int argc, const Object* argv);
    Object sinEx(VM* theVM, int argc, const Object* argv);
    Object cosEx(VM* theVM, int argc, const Object* argv);
    Object logEx(VM* theVM, int argc, const Object* argv);
    Object expEx(VM* theVM, int argc, const Object* argv);
    Object floorEx(VM* theVM, int argc, const Object* argv);
    Object ceilingEx(VM* theVM, int argc, const Object* argv);
    Object truncateEx(VM* theVM, int argc, const Object* argv);
    Object roundEx(VM* theVM, int argc, const Object* argv);
    Object gcdEx(VM* theVM, int argc, const Object* argv);
    Object lcmEx(VM* theVM, int argc, const Object* argv);
    Object integerDiv0Ex(VM* theVM, int argc, const Object* argv);
    Object integerDivEx(VM* theVM, int argc, const Object* argv);
    Object absEx(VM* theVM, int argc, const Object* argv);
    Object evenPEx(VM* theVM, int argc, const Object* argv);
    Object oddPEx(VM* theVM, int argc, const Object* argv);
    Object magnitudeEx(VM* theVM, int argc, const Object* argv);
    Object angleEx(VM* theVM, int argc, const Object* argv);
    Object complexPEx(VM* theVM, int argc, const Object* argv);
    Object realPEx(VM* theVM, int argc, const Object* argv);
    Object integerPEx(VM* theVM, int argc, const Object* argv);
    Object realValuedPEx(VM* theVM, int argc, const Object* argv);
    Object rationalValuedPEx(VM* theVM, int argc, const Object* argv);
    Object integerValuedPEx(VM* theVM, int argc, const Object* argv);
    Object numeratorEx(VM* theVM, int argc, const Object* argv);
    Object denominatorEx(VM* theVM, int argc, const Object* argv);
    Object infinitePEx(VM* theVM, int argc, const Object* argv);
    Object finitePEx(VM* theVM, int argc, const Object* argv);
    Object nanPEx(VM* theVM, int argc, const Object* argv);
    Object inexactEx(VM* theVM, int argc, const Object* argv);
    Object exactEx(VM* theVM, int argc, const Object* argv);
    Object exactPEx(VM* theVM, int argc, const Object* argv);
    Object inexactPEx(VM* theVM, int argc, const Object* argv);
    Object realPartEx(VM* theVM, int argc, const Object* argv);
    Object imagPartEx(VM* theVM, int argc, const Object* argv);
    Object numberPEx(VM* theVM, int argc, const Object* argv);
    Object rationalPEx(VM* theVM, int argc, const Object* argv);
    Object flonumPEx(VM* theVM, int argc, const Object* argv);
    Object bignumPEx(VM* theVM, int argc, const Object* argv);
    Object fixnumPEx(VM* theVM, int argc, const Object* argv);
    Object makeRectangularEx(VM* theVM, int argc, const Object* argv);
    Object fixnumWidthEx(VM* theVM, int argc, const Object* argv);
    Object leastFixnumEx(VM* theVM, int argc, const Object* argv);
    Object greatestFixnumEx(VM* theVM, int argc, const Object* argv);
    Object quotientEx(VM* theVM, int argc, const Object* argv);
    Object remainderEx(VM* theVM, int argc, const Object* argv);
    Object maxEx(VM* theVM, int argc, const Object* argv);
    Object minEx(VM* theVM, int argc, const Object* argv);
    Object eqEx(VM* theVM, int argc, const Object* argv);
    Object gtEx(VM* theVM, int argc, const Object* argv);
    Object geEx(VM* theVM, int argc, const Object* argv);
    Object ltEx(VM* theVM, int argc, const Object* argv);
    Object leEx(VM* theVM, int argc, const Object* argv);
    Object addEx(VM* theVM, int argc, const Object* argv);
    Object subEx(VM* theVM, int argc, const Object* argv);
    Object mulEx(VM* theVM, int argc, const Object* argv);
    Object divideEx(VM* theVM, int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_ARITHMETIC_PROCEDURES__

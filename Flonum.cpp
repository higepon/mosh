/*
 * Flonum.cpp - 
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
 *  $Id: Flonum.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Bignum.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Arithmetic.h"
#include "ProcedureMacro.h"
#include "TextualOutputPort.h"

using namespace scheme;

Object Flonum::POSITIVE_INF;
Object Flonum::NEGATIVE_INF;
Object Flonum::NOT_A_NUMBER;

static double zero(void) { return 0.0; }
static double one (void) { return 1.0; }
static double mone (void) { return -1.0; }

void Flonum::initialize()
{
#ifdef _WIN32
    unsigned int pos_inf[2] = { 0x0, 0x7ff00000 };
    unsigned int neg_inf[2] = { 0x0, 0xfff00000 };
    unsigned int not_num[2] = { 0x0, 0xfff80000 };
    POSITIVE_INF = Object::makeFlonum(*(double*)pos_inf);
    NEGATIVE_INF = Object::makeFlonum(*(double*)neg_inf);
    NOT_A_NUMBER = Object::makeFlonum(*(double*)not_num);
#else
    POSITIVE_INF = Object::makeFlonum(one() / zero());
    NEGATIVE_INF = Object::makeFlonum(mone() / zero());
    NOT_A_NUMBER = Object::makeFlonum(zero() / zero());
#endif
}

Object Flonum::toRatnum() const
{
    mpq_t v;
    mpq_init(v);
    mpq_set_d(v, value_);
    return Object::makeRatnum(v);
}

Object Flonum::toExact() const
{
    mpq_t v;
    mpq_init(v);
    mpq_set_d(v, value_);
    return Ratnum::makeNumber(v);
}

Object Flonum::numerator() const
{
    if (Flonum::eq(this, POSITIVE_INF.toFlonum())) {
        return POSITIVE_INF;
    } else if (Flonum::eq(this, NEGATIVE_INF.toFlonum())) {
        return NEGATIVE_INF;
    } else if (value_ == 0.0) {
        return Object::makeFlonum(0.0);
    }
    return Arithmetic::inexact(toRatnum().toRatnum()->numerator());
}

Object Flonum::denominator() const
{
    if (Flonum::eq(this, POSITIVE_INF.toFlonum())) {
        return Object::makeFlonum(1.0);
    } else if (Flonum::eq(this, NEGATIVE_INF.toFlonum())) {
        return Object::makeFlonum(1.0);
    } else if (value_ == 0.0) {
        return Object::makeFlonum(1.0);
    }
    return Arithmetic::inexact(toRatnum().toRatnum()->denominator());
}


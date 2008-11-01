/*
 * Rational.cpp - 
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
 *  $Id: Rational.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Rational.h"
#include "Object.h"
#include "Object-inl.h"

using namespace scheme;

Rational::Rational(int numerator, int denominator)
{
    mpq_init(r);
    mpq_set_si(r, numerator, denominator);
    mpq_canonicalize(r);
}

Rational::Rational(mpq_t rational)
{
    mpq_set(r, rational);
}

char* Rational::toString()
{
    return mpq_get_str(NULL, 10, r);
}

Rational* Rational::fromFixnum(int num)
{
    return new Rational(num, 1);
}

Object Rational::add(Rational* number1, Rational* number2)
{
    mpq_t ret;
    mpq_init(ret);
    mpq_add(ret, number1->r, number2->r);
    return Object::makeRational(ret);
}

Object Rational::sub(Rational* number1, Rational* number2)
{
    mpq_t ret;
    mpq_init(ret);
    mpq_sub(ret, number1->r, number2->r);
    return Object::makeRational(ret);
}

Object Rational::mul(Rational* number1, Rational* number2)
{
    mpq_t ret;
    mpq_init(ret);
    mpq_mul(ret, number1->r, number2->r);
    return Object::makeRational(ret);
}

Object Rational::div(Rational* number1, Rational* number2)
{
    mpq_t ret;
    mpq_init(ret);
    mpq_div(ret, number1->r, number2->r);
    return Object::makeRational(ret);
}

bool Rational::equal(int number)
{
    return mpq_cmp_si(r, number, 1) == 0;
}

bool Rational::equal(Rational* number)
{
    return mpq_cmp(r, number->r) == 0;
}

bool Rational::gt(Rational* number1, Rational* number2)
{
    return mpq_cmp(number1->r, number2->r) > 0;
}

bool Rational::ge(Rational* number1, Rational* number2)
{
    return mpq_cmp(number1->r, number2->r) >= 0;
}

bool Rational::lt(Rational* number1, Rational* number2)
{
    return mpq_cmp(number1->r, number2->r) < 0;
}

bool Rational::le(Rational* number1, Rational* number2)
{
    return mpq_cmp(number1->r, number2->r) <= 0;
}

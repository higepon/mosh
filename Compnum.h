/*
 * Compnum.h - Complex number
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
 *  $Id: Compnum.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_COMPNUM__
#define __SCHEME_COMPNUM__

#include "scheme.h"
#include "Arithmetic.h"

namespace scheme {

class Compnum EXTEND_GC
{
public:
    Compnum(Object real, Object imag) : real_(real), imag_(imag)
    {

    }

    ~Compnum()
    {
    }

    Object real() const { return real_; }
    Object imag() const { return imag_; }

    static bool eq(Compnum* n1, Compnum* n2)
    {
        return Arithmetic::eq(n1->real(), n2->real()) &&
               Arithmetic::eq(n2->imag(), n2->imag());
    }

    static bool eq(Object n1, Compnum* n2)
    {
        MOSH_ASSERT(n1.isFixnum() || n1.isBignum() || n1.isFlonum() || n1.isRatnum());
        if (Arithmetic::eq(n2->imag(), Object::makeFixnum(0))) {
            return Arithmetic::eq(n1, n2->real());
        } else {
            return false;
        }
    }

    static bool eq(Compnum* n1, Object n2)
    {
        MOSH_ASSERT(n2.isFixnum() || n2.isBignum() || n2.isFlonum() || n2.isRatnum());
        if (Arithmetic::eq(n1->imag(), Object::makeFixnum(0))) {
            return Arithmetic::eq(n1->real(), n2);
        } else {
            return false;
        }
    }

    static Object add(Compnum* n1, Compnum* n2)
    {
        return Object::makeCompnum(Arithmetic::add(n1->real(), n2->real()),
                                   Arithmetic::add(n2->imag(), n2->imag()));
    }

    static Object sub(Compnum* n1, Compnum* n2)
    {
        return Object::makeCompnum(Arithmetic::sub(n1->real(), n2->real()),
                                   Arithmetic::sub(n2->imag(), n2->imag()));
    }

    static Object mul(Compnum* n1, Compnum* n2)
    {
        return Object::makeCompnum(Arithmetic::sub(Arithmetic::mul(n1->real(), n2->real()), Arithmetic::mul(n1->imag(), n2->imag())),
                                   Arithmetic::add(Arithmetic::mul(n1->real(), n2->imag()), Arithmetic::mul(n1->real(), n2->imag())));
    }

    static Object div(Compnum* n1, Compnum* n2)
    {
        const Object denon = Arithmetic::add(Arithmetic::mul(n1->real(), n2->real()),
                                             Arithmetic::mul(n1->imag(), n2->imag()));
        const Object nume = Object::makeCompnum(Arithmetic::add(Arithmetic::mul(n1->real(), n2->real()), Arithmetic::mul(n1->imag(), n2->imag())),
                                                Arithmetic::sub(Arithmetic::mul(n1->imag(), n2->real()), Arithmetic::mul(n1->real(), n2->imag())));
        return Arithmetic::div(nume, denon);
    }


    static Object div(Compnum* n1, Object n2)
    {
        return Object::makeCompnum(Arithmetic::div(n1->real(), n2),
                                   Arithmetic::div(n1->imag(), n2));
    }

private:
    Object real_;
    Object imag_;
};

}; // namespace scheme

#endif // __SCHEME_COMPNUM__

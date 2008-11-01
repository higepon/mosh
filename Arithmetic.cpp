/*
 * Arithmetic.cpp -
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
 *  $Id: Arithmetic.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Arithmetic.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Rational.h"
#include "ErrorProcedures.h"

using namespace scheme;

bool Arithmetic::lt(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return number1.toFixnum() < number2.toFixnum();
        } else if (number2.isRational()) {
            return Rational::lt(Rational::fromFixnum(number1.toFixnum()), number2.toRational());
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            return Rational::lt(number1.toRational(), Rational::fromFixnum(number2.toFixnum()));
        } else if (number2.isRational()) {
            return Rational::lt(number1.toRational(), number2.toRational());
        }
    }
    callWrongTypeOfArgumentViolationAfter("<", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return false;
}

bool Arithmetic::le(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return number1.toFixnum() <= number2.toFixnum();
        } else if (number2.isRational()) {
            return Rational::le(Rational::fromFixnum(number1.toFixnum()), number2.toRational());
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            return Rational::le(number1.toRational(), Rational::fromFixnum(number2.toFixnum()));
        } else if (number2.isRational()) {
            return Rational::le(number1.toRational(), number2.toRational());
        }
    }
    callWrongTypeOfArgumentViolationAfter("<=", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return false;
}

bool Arithmetic::gt(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return number1.toFixnum() > number2.toFixnum();
        } else if (number2.isRational()) {
            return Rational::gt(Rational::fromFixnum(number1.toFixnum()), number2.toRational());
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            return Rational::gt(number1.toRational(), Rational::fromFixnum(number2.toFixnum()));
        } else if (number2.isRational()) {
            return Rational::gt(number1.toRational(), number2.toRational());
        }
    }
    callWrongTypeOfArgumentViolationAfter(">", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return false;
}

bool Arithmetic::ge(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return number1.toFixnum() >= number2.toFixnum();
        } else if (number2.isRational()) {
            return Rational::ge(Rational::fromFixnum(number1.toFixnum()), number2.toRational());
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            return Rational::ge(number1.toRational(), Rational::fromFixnum(number2.toFixnum()));
        } else if (number2.isRational()) {
            return Rational::ge(number1.toRational(), number2.toRational());
        }
    }
    callWrongTypeOfArgumentViolationAfter(">=", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return false;
}

Object Arithmetic::add(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return Object::makeFixnum(number1.toFixnum() + number2.toFixnum());
        } else if (number2.isRational()) {
            return Rational::add(Rational::fromFixnum(number1.toFixnum()), number2.toRational());
        }
    } else if (number1.isRational()) {

        if (number2.isFixnum()) {
            return Rational::add(number1.toRational(), new Rational(number2.toFixnum(), 1));
        } else if (number2.isRational()) {
            return Rational::add(number1.toRational(), number2.toRational());
        }
    }
    callWrongTypeOfArgumentViolationAfter("+", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return Object::False;
}

Object Arithmetic::sub(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return Object::makeFixnum(number1.toFixnum() - number2.toFixnum());
        } else if (number2.isRational()) {
            return Rational::sub(new Rational(number1.toFixnum(), 1), number2.toRational());
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            return Rational::sub(number1.toRational(), Rational::fromFixnum(number2.toFixnum()));
        } else if (number2.isRational()) {
            return Rational::sub(number1.toRational(), number2.toRational());
        }

    }
    callWrongTypeOfArgumentViolationAfter("-", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return Object::False;
}

Object Arithmetic::mul(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return Object::makeFixnum(number1.toFixnum() * number2.toFixnum());
        } else if (number2.isRational()) {
            return Rational::mul(Rational::fromFixnum(number1.toFixnum()), number2.toRational());
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            return Rational::mul(number1.toRational(), Rational::fromFixnum(number2.toFixnum()));
        } else if (number2.isRational()) {
            return Rational::mul(number1.toRational(), number2.toRational());
        }
    }

    callWrongTypeOfArgumentViolationAfter("*", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return Object::False;
}

Object Arithmetic::div(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            if (number2.toFixnum() == 0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Object::makeRational(number1.toFixnum(), number2.toFixnum());
            }
        } else if (number2.isRational()) {
            if (number2.toRational()->equal(0)) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Rational::div(Rational::fromFixnum(number1.toFixnum()),
                                     number2.toRational());
            }
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            if (number2.toFixnum() == 0) {
                callAssertionViolationAfter("/", "Dividing by zero", Pair::list2(number1, number2));
                return Object::False;
            } else {
                return Rational::div(number1.toRational(),
                                     Rational::fromFixnum(number2.toFixnum()));
            }
        } else if (number2.isRational()) {
            return Rational::div(number1.toRational(), number2.toRational());
        }
    }

    callWrongTypeOfArgumentViolationAfter("/", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return Object::False;
}


bool Arithmetic::eq(Object number1, Object number2)
{
    if (number1.isFixnum()) {
        if (number2.isFixnum()) {
            return number1.toFixnum() == number2.toFixnum();
        } else if (number2.isRational()) {
            return number2.toRational()->equal(number1.toFixnum());
        }
    } else if (number1.isRational()) {
        if (number2.isFixnum()) {
            return number1.toRational()->equal(number2.toFixnum());
        } else if (number2.isRational()) {
            return number2.toRational()->equal(number1.toRational());
        }
    }
    callWrongTypeOfArgumentViolationAfter("=", "number", Pair::list2(number1, number2), Pair::list2(number1, number2));
    return false;

}

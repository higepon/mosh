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
#include "Bignum.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Fixnum.h"
#include "Compnum.h"
#include "SString.h"
#include "VM.h"
#include "ErrorProcedures.h"
#include "StringProcedures.h"
#include "ProcedureMacro.h"
#include "TextualOutputPort.h"

#ifdef _WIN32
    #define snprintf _snprintf
#endif
using namespace scheme;

Object Arithmetic::numberToString(Object n, int radix)
{
    MOSH_ASSERT(n.isNumber());
    MOSH_ASSERT(radix == 2 || radix == 8 || radix == 10 || radix == 16);
    if (n.isFixnum()) {
        const int fn = n.toFixnum();
        char buf[64];
        char* start = buf;
        if (fn < 0) {
            *start++ = '-';
        }
        long unsigned ufn = ::abs(fn);
        switch(radix) {
        case 16:
            snprintf(start, 64, "%lx", ufn);
            break;
        case 8:
            snprintf(start, 64, "%lo", ufn);
            break;
        case 2:
        {
            Bignum* const b = new Bignum(ufn);
            snprintf(start, 64, "%s", b->toString(2));
            break;
        }
        case 10: // fallthrough
        default:
            snprintf(start, 64, "%ld", ufn);
            break;
        }
        return Object::makeString(buf);
    } else if (n.isFlonum()) {
        MOSH_ASSERT(radix == 10);
        const double value = n.toFlonum()->value();

        if (n.toFlonum()->isNan()) {
            return Object::makeString("+nan.0");
        } else if (n.toFlonum()->isInfinite()) {
            return Object::makeString((value > 0) ? "+inf.0" : "-inf.0");
        } else {
        return Object::makeString(FlonumUtil::flonumToUcs4String(value, false));
//             char buf[256];
//             snprintf(buf, 256, "%f", value);
//             return Object::makeString(buf);
        }
    } else if (n.isBignum()) {
        return Object::makeString(n.toBignum()->toString(radix));
    } else if (n.isRatnum()) {
        Ratnum* const r = n.toRatnum();
        return Object::makeString(r->toString(radix));
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return format(NULL, UC("~d+~di"), Pair::list2(numberToString(c->real(), radix),
                                                      numberToString(c->imag(), radix)));
    } else {
        MOSH_ASSERT(false);
        return Object::Undef;
    }
}

Object Arithmetic::real(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isCompnum()) {
        return n.toCompnum()->real();
    } else {
        return n;
    }
}

Object Arithmetic::imag(Object n)
{
    if (n.isCompnum()) {
        return n.toCompnum()->imag();
    } else {
        return Object::makeFixnum(0);
    }
}

Object Arithmetic::expt(Object n1, Object n2)
{
    MOSH_ASSERT(n1.isNumber());
    MOSH_ASSERT(n2.isNumber());
    MOSH_ASSERT(!n2.isBignum()); // too big.
    if (n1.isFixnum()) {
        if (n2.isFixnum()) {
            const int fn2 = n2.toFixnum();
            if (fn2 > 0) {
                Object ret = n1;
                for (int i = 0; i < fn2 - 1; i++) {
                    ret = Arithmetic::mul(ret, n1);
                }
                return ret;
            } else if (fn2 == 0) {
                return Object::makeFixnum(1);
            } else {
                const int fn1 = n1.toFixnum();
                if (0 == fn1) {
                    return Object::Undef;
                }
                // inverse
                Object ret = n1;
                for (int i = 0; i < -fn2 - 1; i++) {
                    ret = Arithmetic::mul(ret, n1);
                }
                bool isDiv0Error = false;
                const Object result = Arithmetic::div(Object::makeFixnum(1), ret, isDiv0Error);
                // isDiv0Error never occurs, so ignore.
                MOSH_ASSERT(!isDiv0Error);
                return result;
            }
        } else if (n2.isFlonum()) {
            const double fn1 = static_cast<double>(n1.toFixnum());
            const double fn2 = n2.toFlonum()->value();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isBignum()) {
            MOSH_FATAL("not reached");
        } else if (n2.isRatnum()) {
            const double fn1 = static_cast<double>(n1.toFixnum());
            const double fn2 = n2.toRatnum()->toDouble();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isCompnum()) {
            const int fn1 = n1.toFixnum();
            Compnum* const compnum = n2.toCompnum();
            if (0 == fn1) {
                if (Arithmetic::isNegative(compnum->real())) {
                    return Object::Undef;
                } else {
                    return Object::makeFixnum(0);
                }
            } else {
                return Compnum::expt(n1, n2);
            }
        } else {
            MOSH_ASSERT(false);
            return Object::Undef;
        }
    } else if (n1.isFlonum()) {
        if (n2.isFixnum()) {
            const double fn1 = n1.toFlonum()->value();
            const double fn2 = static_cast<double>(n2.toFixnum());
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isFlonum()) {
            const double fn1 = n1.toFlonum()->value();
            const double fn2 = n2.toFlonum()->value();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isBignum()) {
            MOSH_FATAL("not reached");
        } else if (n2.isRatnum()) {
            const double fn1 = n1.toFlonum()->value();
            const double fn2 = n2.toRatnum()->toDouble();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isCompnum()) {
            const double fn1 = n1.toFlonum()->value();
            Compnum* const compnum = n2.toCompnum();
            if (0.0 == fn1) {
                if (Arithmetic::isNegative(compnum->real())) {
                    return Object::Undef;
                } else {
                    return Object::makeFlonum(0.0);
                }
            } else {
                return Compnum::expt(n1, n2);
            }
        } else {
            MOSH_ASSERT(false);
            return Object::Undef;
        }
    } else if (n1.isBignum()) {
        if (n2.isFixnum()) {
            const int fn2 = n2.toFixnum();
            if (fn2 > 0) {
                // can be much faster using gmp
                Object ret = n1;
                for (int i = 0; i < fn2 - 1; i++) {
                    ret = Arithmetic::mul(ret, n1);
                }
                return ret;
            } else if (fn2 == 0) {
                return Object::makeFixnum(1);
            } else {
                // inverse
                Object ret = n1;
                for (int i = 0; i < -fn2 - 1; i++) {
                    ret = Arithmetic::mul(ret, n1);
                }
                bool isDiv0Error = false;
                const Object result = Arithmetic::div(Object::makeFixnum(1), ret, isDiv0Error);
                MOSH_ASSERT(!isDiv0Error);
                return result;
            }
        } else if (n2.isFlonum()) {
            const double fn1 = n1.toBignum()->toDouble();
            const double fn2 = n2.toFlonum()->value();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isBignum()) {
            MOSH_FATAL("not reached");
        } else if (n2.isRatnum()) {
            const double fn1 = n1.toBignum()->toDouble();
            const double fn2 = n2.toRatnum()->toDouble();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isCompnum()) {
            return Compnum::expt(n1, n2);
        } else {
            MOSH_ASSERT(false);
            return Object::Undef;
        }
    } else if (n1.isRatnum()) {
        if (n2.isFixnum()) {
            const int fn2 = n2.toFixnum();
            if (fn2 > 0) {
                // can be much faster using gmp
                Object ret = n1;
                for (int i = 0; i < fn2 - 1; i++) {
                    ret = Arithmetic::mul(ret, n1);
                }
                return ret;
            } else if (fn2 == 0) {
                return Object::makeFixnum(1);
            } else {
                // inverse
                Object ret = n1;
                for (int i = 0; i < -fn2 - 1; i++) {
                    ret = Arithmetic::mul(ret, n1);
                }
                bool isDiv0Error = false;
                const Object result = Arithmetic::div(Object::makeFixnum(1), ret, isDiv0Error);
                MOSH_ASSERT(!isDiv0Error);
                return result;
            }
        } else if (n2.isFlonum()) {
            const double fn1 = n1.toRatnum()->toDouble();
            const double fn2 = n2.toFlonum()->value();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isBignum()) {
            MOSH_FATAL("not reached");
        } else if (n2.isRatnum()) {
            const double fn1 = n1.toRatnum()->toDouble();
            const double fn2 = n2.toRatnum()->toDouble();
            return Object::makeFlonum(::pow(fn1, fn2));
        } else if (n2.isCompnum()) {
            return Compnum::expt(n1, n2);
        } else {
            MOSH_ASSERT(false);
            return Object::Undef;
        }
    } else if (n1.isCompnum()) {
        return Compnum::expt(n1, n2);
    } else {
        MOSH_ASSERT(false);
        return Object::Undef;
    }
    MOSH_FATAL("not reached");
    return Object::Undef;
}

Object Arithmetic::sqrt(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::sqrt(n);
    } else if (n.isBignum()) {
        return n.toBignum()->sqrt();
    } else if (n.isFlonum()) {
        return n.toFlonum()->sqrt();
    } else if (n.isRatnum()) {
        return n.toRatnum()->sqrt();
    } else if (n.isCompnum()) {
        return n.toCompnum()->sqrt();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::asin(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::asin(n.toFixnum());
    } else if (n.isCompnum()) {
        return Compnum::asin(n);
    } else {
        const double value = realToDouble(n);
        return Object::makeFlonum(::asin(value));
    }
}

Object Arithmetic::acos(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::acos(n.toFixnum());
    } else if (n.isCompnum()) {
        return Compnum::acos(n);
    } else {
        const double value = realToDouble(n);
        return Object::makeFlonum(::acos(value));
    }
}

Object Arithmetic::atan(Object n, bool& isDiv0Error)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::atan(n.toFixnum());
    } else if (n.isCompnum()) {
        return Compnum::atan(n, isDiv0Error);
    } else {
        const double value = realToDouble(n);
        return Object::makeFlonum(::atan(value));
    }
}

Object Arithmetic::atan2(Object n1, Object n2)
{
    MOSH_ASSERT(n1.isReal());
    MOSH_ASSERT(n2.isReal());
    if (Arithmetic::isExactZero(n1)) {
        return Object::makeFixnum(0);
    }
    return Object::makeFlonum(::atan2(realToDouble(n1), realToDouble(n2)));
}

Object Arithmetic::tan(Object n, bool& isDiv0Error)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::tan(n.toFixnum());
    } else if (n.isCompnum()) {
        return n.toCompnum()->tan(isDiv0Error);
    } else {
        const double value = realToDouble(n);
        return Object::makeFlonum(::tan(value));
    }
}

Object Arithmetic::cos(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::cos(n.toFixnum());
    } else if (n.isFlonum()) {
        return Object::makeFlonum(::cos(n.toFlonum()->value()));
    } else if (n.isCompnum()) {
        return n.toCompnum()->cos();
    } else if (n.isReal()) {
        const double value = realToDouble(n);
        return Object::makeFlonum(::cos(value));
    } else {
        return Object::Undef;
    }
}

Object Arithmetic::sin(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::sin(n.toFixnum());
    } else if (n.isFlonum()) {
        return Object::makeFlonum(::sin(n.toFlonum()->value()));
    } else if (n.isCompnum()) {
        return n.toCompnum()->sin();
    } else if (n.isReal()) {
        const double value = realToDouble(n);
        return Object::makeFlonum(::sin(value));
    } else {
        return Object::Undef;
    }
}

Object Arithmetic::log(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::log(n.toFixnum());
    } else if (n.isCompnum()) {
        return n.toCompnum()->log();
    } else {
        const double value = realToDouble(n);
        if (value >= 0) {
            return Object::makeFlonum(::log(realToDouble(n)));
        } else {
            return Object::makeCompnum(Object::makeFlonum(::log(-value)), Object::makeFlonum(::atan2(0.0, value)));
        }
    }
}

Object Arithmetic::log(Object n1, Object n2, bool& isDiv0Error)
{
    return Arithmetic::div(log(n1), log(n2), isDiv0Error);
}


Object Arithmetic::exp(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Fixnum::exp(n.toFixnum());
    } else if (n.isCompnum()) {
        return n.toCompnum()->exp();
    } else {
        return Object::makeFlonum(::exp(realToDouble(n)));
    }
}

Object Arithmetic::floor(Object n)
{
    MOSH_ASSERT(n.isReal());
    if (n.isFixnum() || n.isBignum()) {
        return n;
    } else if (n.isFlonum()) {
        return n.toFlonum()->floor();
    } else if (n.isRatnum()) {
        return n.toRatnum()->floor();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::ceiling(Object n)
{
    MOSH_ASSERT(n.isReal());
    if (n.isFixnum() || n.isBignum()) {
        return n;
    } else if (n.isFlonum()) {
        return n.toFlonum()->ceiling();
    } else if (n.isRatnum()) {
        return n.toRatnum()->ceiling();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::truncate(Object n)
{
    MOSH_ASSERT(n.isReal());
    if (n.isFixnum() || n.isBignum()) {
        return n;
    } else if (n.isFlonum()) {
        return n.toFlonum()->truncate();
    } else if (n.isRatnum()) {
        return n.toRatnum()->truncate();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::round(Object n)
{
    MOSH_ASSERT(n.isReal());
    if (n.isFixnum() || n.isBignum()) {
        return n;
    } else if (n.isFlonum()) {
        return n.toFlonum()->round();
    } else if (n.isRatnum()) {
        return n.toRatnum()->round();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::abs(Object n)
{
    MOSH_ASSERT(n.isReal());
    if (n.isFixnum()) {
        return Fixnum::abs(n.toFixnum());
    } else if (n.isBignum()) {
        return n.toBignum()->abs();
    } else if (n.isFlonum()) {
        return n.toFlonum()->abs();
    } else if (n.isRatnum()) {
        return n.toRatnum()->abs();
    }

    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::negate(Object n)
{
    MOSH_ASSERT(n.isReal());
    return Arithmetic::mul(n, Object::makeFixnum(-1));
}

bool Arithmetic::isNegative(Object n)
{
    MOSH_ASSERT(n.isReal());
    return Arithmetic::lt(n, Object::makeFixnum(0));
}

Object Arithmetic::angle(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isReal()) {
        if (isNegative(n)) {
            // pi
            return Object::makeFlonum(::acos(-1.0));
        } else {
            if (n.isFlonum()) {
                return Object::makeFlonum(0.0);
            } else {
                return Object::makeFixnum(0);
            }
        }
    } else if (n.isCompnum()) {
        return n.toCompnum()->angle();
    } else {
        MOSH_ASSERT(false);
        return Object::Undef;
    }
}

Object Arithmetic::magnitude(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isReal()) {
        if (isNegative(n)) {
            return negate(n);
        } else {
            return n;
        }
    } else if (n.isCompnum()) {
        return n.toCompnum()->magnitude();
    } else {
        MOSH_ASSERT(false);
        return Object::Undef;
    }
}

bool Arithmetic::fitsU32(Object n)
{
    MOSH_ASSERT(n.isExactInteger());
    if (n.isFixnum()) {
        return n.toFixnum() >= 0;
    } else if (n.isBignum()) {
        return n.toBignum()->fitsU32();
    } else {
        MOSH_ASSERT(false);
        return false;
    }
}

uint32_t Arithmetic::toU32(Object n)
{
    MOSH_ASSERT(fitsU32(n));
    if (n.isFixnum()) {
        return n.toFixnum();
    } else if (n.isBignum()) {
        return n.toBignum()->toU32();
    } else {
        MOSH_ASSERT(false);
        return 0;
    }
}

bool Arithmetic::fitsS32(Object n)
{
    MOSH_ASSERT(n.isExactInteger());
    if (n.isFixnum()) {
        return true;
    } else if (n.isBignum()) {
        return n.toBignum()->fitsS32();
    } else {
        MOSH_ASSERT(false);
        return false;
    }
}

int32_t Arithmetic::toS32(Object n)
{
    MOSH_ASSERT(fitsS32(n));
    if (n.isFixnum()) {
        return n.toFixnum();
    } else if (n.isBignum()) {
        return n.toBignum()->toS32();
    } else {
        MOSH_ASSERT(false);
        return 0;
    }
}

bool Arithmetic::fitsU64(Object n)
{
    MOSH_ASSERT(n.isExactInteger());
    if (n.isFixnum()) {
        return n.toFixnum() >= 0;
    } else if (n.isBignum()) {
        return n.toBignum()->fitsU64();
    } else {
        MOSH_ASSERT(false);
        return false;
    }
}

uint64_t Arithmetic::toU64(Object n)
{
    MOSH_ASSERT(fitsU64(n));
    if (n.isFixnum()) {
        return n.toFixnum();
    } else if (n.isBignum()) {
        return n.toBignum()->toU64();
    } else {
        MOSH_ASSERT(false);
        return 0;
    }
}

bool Arithmetic::fitsS64(Object n)
{
    MOSH_ASSERT(n.isExactInteger());
    if (n.isFixnum()) {
        return true;
    } else if (n.isBignum()) {
        return n.toBignum()->fitsS64();
    } else {
        MOSH_ASSERT(false);
        return false;
    }
}

int64_t Arithmetic::toS64(Object n)
{
    MOSH_ASSERT(fitsS64(n));
    if (n.isFixnum()) {
        return n.toFixnum();
    } else if (n.isBignum()) {
        return n.toBignum()->toS64();
    } else {
        MOSH_ASSERT(false);
        return 0;
    }
}

double Arithmetic::realToDouble(Object n)
{
    MOSH_ASSERT(n.isReal());
    if (n.isFixnum()) {
        return n.toFixnum();
    } else if (n.isBignum()) {
        return n.toBignum()->toDouble();
    } else if (n.isFlonum()) {
        return n.toFlonum()->value();
    } else if (n.isRatnum()) {
        return n.toRatnum()->toDouble();
    } else {
        MOSH_ASSERT(false);
        return 0.0;
    }
}

Object Arithmetic::makePolar(Object n1, Object n2)
{
    MOSH_ASSERT(isRealValued(n1));
    MOSH_ASSERT(isRealValued(n2));
    const Object real = n1.isCompnum() ? n1.toCompnum()->real() : n1;
    const Object imag = n2.isCompnum() ? n2.toCompnum()->real() : n2;
    if (eq(imag, Object::makeFixnum(0))) {
        return real;
    }
    const double r = realToDouble(real);
    const double a = realToDouble(imag);
    return Object::makeCompnum(Object::makeFlonum(r * ::cos(a)), Object::makeFlonum(r * ::sin(a)));
}

Object Arithmetic::bitwiseNot(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        return Bignum::makeInteger(Fixnum::fxnot(e.toFixnum()));
    } else if (e.isBignum()) {
        return e.toBignum()->bitwiseNot();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::bitwiseAnd(Object e1, Object e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    MOSH_ASSERT(e2.isExactInteger());
    if (e1.isFixnum()) {
        if (e2.isFixnum()) {
            return Object::makeFixnum(Fixnum::fxand(e1.toFixnum(), e2.toFixnum()));
        } else if (e2.isBignum()) {
            return e2.toBignum()->bitwiseAnd(e1.toFixnum());
        }
    } else if (e1.isBignum()) {
        if (e2.isFixnum()) {
            return e1.toBignum()->bitwiseAnd(e2.toFixnum());
        } else if (e2.isBignum()) {
            return e1.toBignum()->bitwiseAnd(e2.toBignum());
        }
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::bitwiseIor(Object e1, Object e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    MOSH_ASSERT(e2.isExactInteger());
    if (e1.isFixnum()) {
        if (e2.isFixnum()) {
            return Object::makeFixnum(Fixnum::fxior(e1.toFixnum(), e2.toFixnum()));
        } else if (e2.isBignum()) {
            return e2.toBignum()->bitwiseIor(e1.toFixnum());
        }
    } else if (e1.isBignum()) {
        if (e2.isFixnum()) {
            return e1.toBignum()->bitwiseIor(e2.toFixnum());
        } else if (e2.isBignum()) {
            return e1.toBignum()->bitwiseIor(e2.toBignum());
        }
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::bitwiseXor(Object e1, Object e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    MOSH_ASSERT(e2.isExactInteger());
    if (e1.isFixnum()) {
        if (e2.isFixnum()) {
            return Object::makeFixnum(Fixnum::fxxor(e1.toFixnum(), e2.toFixnum()));
        } else if (e2.isBignum()) {
            return e2.toBignum()->bitwiseXor(e1.toFixnum());
        }
    } else if (e1.isBignum()) {
        if (e2.isFixnum()) {
            return e1.toBignum()->bitwiseXor(e2.toFixnum());
        } else if (e2.isBignum()) {
            return e1.toBignum()->bitwiseXor(e2.toBignum());
        }
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::bitwiseBitCount(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        const int n = e.toFixnum();
        return Object::makeFixnum(Fixnum::fxbitCount(n));
    } else if (e.isBignum()){
        return e.toBignum()->bitwiseBitCount();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::bitwiseLength(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        const int n = e.toFixnum();
        return Object::makeFixnum(Fixnum::fxlength(n));
    } else if (e.isBignum()){
        return e.toBignum()->bitwiseLength();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

Object Arithmetic::bitwiseFirstBitSet(Object e)
{
    MOSH_ASSERT(e.isExactInteger());
    if (e.isFixnum()) {
        const int n = e.toFixnum();
        return Object::makeFixnum(Fixnum::fxfirstBitSet(n));
    } else if (e.isBignum()){
        return e.toBignum()->bitwiseFirstBitSet();
    }
    MOSH_ASSERT(false);
    return Object::False;
}

Object Arithmetic::bitwiseShiftLeft(Object e1, unsigned long e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    if (e1.isFixnum()) {
        return Bignum::bitwiseShiftLeft(e1.toFixnum(), e2);
    } else if (e1.isBignum()) {
        return Bignum::bitwiseShiftLeft(e1.toBignum(), e2);
    }
    MOSH_ASSERT(false);
    return Object::False;
}

Object Arithmetic::bitwiseShiftRight(Object e1, unsigned long e2)
{
    MOSH_ASSERT(e1.isExactInteger());
    if (e1.isFixnum()) {
        return Bignum::bitwiseShiftRight(e1.toFixnum(), e2);
    } else if (e1.isBignum()) {
        return Bignum::bitwiseShiftRight(e1.toBignum(), e2);
    }
    MOSH_ASSERT(false);
    return Object::False;
}

Object Arithmetic::toFlonum(Object real)
{
    MOSH_ASSERT(real.isReal());
    if (real.isFlonum()) {
        return real;
    } else if (real.isFixnum()) {
        return Object::makeFlonum(real.toFixnum());
    } else if (real.isBignum()) {
        return Object::makeFlonum(real.toBignum()->toDouble());
    } else if (real.isRatnum()) {
        return Object::makeFlonum(real.toRatnum()->toDouble());
    } else {
        MOSH_ASSERT(false);
        return Object::Nil;
    }
}

Object Arithmetic::numerator(Object n)
{
    MOSH_ASSERT(n.isRational());
    if (n.isRatnum()) {
        return n.toRatnum()->numerator();
    } else {
        return n;
    }
}

#include "ProcedureMacro.h"
#include "TextualOutputPort.h"

Object Arithmetic::denominator(Object n)
{
//    VM_LOG1("n=~a", n);
    MOSH_ASSERT(n.isRational());
    if (n.isRatnum()) {
        return n.toRatnum()->denominator();
    } else if (n.isFlonum()) {
        Object m = n.toFlonum()->toExact();
        return inexact(denominator(m));
    } else {
        return Object::makeFixnum(1);
    }
}

bool Arithmetic::isInteger(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFlonum()) {
        if (n.toFlonum()->isNan() ||
            n.toFlonum()->isInfinite()) {
            return false;
        }
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return isZero(c->imag()) && isInteger(c->real());
    }
    return Arithmetic::eq(denominator(n), Object::makeFixnum(1));
}

bool Arithmetic::isRealValued(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isReal()) {
        return true;
    } else if (n.isCompnum()) {
        if (isZero(n.toCompnum()->imag())) {
            return true;
        } else {
            return false;
        }
    }
    MOSH_ASSERT(false);
    return false;
}

bool Arithmetic::isRationalValued(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isRational()) {
        return true;
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return isZero(c->imag()) && isRationalValued(c->real());
    } else {
        // +nan.0 +inf.0
        return false;
    }
}

bool Arithmetic::isEven(Object n)
{
    MOSH_ASSERT(isIntegerValued(n));
    if (n.isFixnum()) {
        return Fixnum::isEven(n.toFixnum());
    } else if (n.isBignum()) {
        return n.toBignum()->isEven();
    } else if (n.isFlonum()) {
        return n.toFlonum()->isEven();
    } else if (n.isCompnum()) {
        return isEven(n.toCompnum()->real());
    }
    MOSH_ASSERT(false);
    return false;
}

bool Arithmetic::isIntegerValued(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isInteger()) {
        return true;
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return isZero(c->imag()) && isIntegerValued(c->real());
    } else {
        return false;
    }
}

bool Arithmetic::isExactZero(Object n)
{
    return isZero(n) && isExact(n);
}

bool Arithmetic::isZero(Object n)
{
    return Arithmetic::eq(Object::makeFixnum(0), n);
}

Object Arithmetic::exact(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum() || n.isBignum() || n.isRatnum()) {
        return n;
    } else if (n.isFlonum()) {
        return n.toFlonum()->toExact();
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return Object::makeCompnum(exact(c->real()), exact(c->imag()));
    } else {
        MOSH_ASSERT(false);
    }
    return Object::Undef;
}

Object Arithmetic::inexact(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum()) {
        return Object::makeFlonum(n.toFixnum());
    } else if (n.isBignum()) {
        return Object::makeFlonum(n.toBignum()->toDouble());
    } else if (n.isFlonum()) {
        return n;
    } else if (n.isRatnum()) {
        return Object::makeFlonum(n.toRatnum()->toDouble());
    } else if (n.isCompnum()) {
        Compnum* const c = n.toCompnum();
        return Object::makeCompnum(inexact(c->real()), inexact(c->imag()));
    } else {
        MOSH_ASSERT(false);
    }
    return Object::Undef;
}

bool Arithmetic::isExact(Object n)
{
    MOSH_ASSERT(n.isNumber());
    if (n.isFixnum() || n.isBignum() || n.isRatnum()) {
        return true;
    } else if (n.isFlonum()) {
        return false;
    } else if (n.isCompnum()) {
        return
            isExact(n.toCompnum()->real()) &&
            isExact(n.toCompnum()->imag());
    } else {
        MOSH_ASSERT(false);
        return false;
    }
}

#define MAKE_REAL_COMPARE_FUNC(compare, symbol)          \
    bool Arithmetic::compare(Object n1, Object n2)    \
    { \
        if (n1.isFixnum()) {\
            if (n2.isFixnum()) {\
                return Fixnum::compare(n1.toFixnum(), n2.toFixnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toFixnum(), n2.toFlonum());\
            } else if (n2.isRatnum()) {\
                return Ratnum::compare(n1.toFixnum(), n2.toRatnum());\
            } else if (n2.isBignum()) {\
                return Bignum::compare(n1.toFixnum(), n2.toBignum());\
            }\
        } else if (n1.isFlonum()) {\
            if (n2.isFixnum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toFixnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toFlonum());\
            } else if (n2.isRatnum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toRatnum()); \
            } else if (n2.isBignum()) {\
                return Flonum::compare(n1.toFlonum(), n2.toBignum());\
            }\
        } else if (n1.isBignum()) {\
            if (n2.isFixnum()) {\
                return Bignum::compare(n1.toBignum(), n2.toFixnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toBignum(), n2.toFlonum());\
            } else if (n2.isRatnum()) {\
                return Ratnum::compare(n1.toBignum(), n2.toRatnum()); \
            } else if (n2.isBignum()) {\
                return Bignum::compare(n1.toBignum(), n2.toBignum());\
            }\
        } else if (n1.isRatnum()) {\
            if (n2.isFixnum()) {\
                return Ratnum::compare(n1.toRatnum(), n2.toFixnum());\
            } else if (n2.isFlonum()) {\
                return Flonum::compare(n1.toRatnum(), n2.toFlonum());\
            } else if (n2.isRatnum()) {\
                return Ratnum::compare(n1.toRatnum(), n2.toRatnum());\
            } else if (n2.isBignum()) {\
                return Ratnum::compare(n1.toRatnum(), n2.toBignum());\
            }\
        }\
        return false;\
    }

MAKE_REAL_COMPARE_FUNC(lt, <)
MAKE_REAL_COMPARE_FUNC(le, <=)
MAKE_REAL_COMPARE_FUNC(gt, >)
MAKE_REAL_COMPARE_FUNC(ge, >=)

bool Arithmetic::eq(Object n1, Object n2)
{
    MOSH_ASSERT(n1.isNumber());
    MOSH_ASSERT(n2.isNumber());
    if (n1.isFixnum()) {
        if (n2.isFixnum()) {
            return Fixnum::eq(n1.toFixnum(), n2.toFixnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toFixnum(), n2.toFlonum());
        } else if (n2.isRatnum()) {
            return Ratnum::eq(n1.toFixnum(), n2.toRatnum());
        } else if (n2.isBignum()) {
            return Bignum::eq(n1.toFixnum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isFlonum()) {
        if (n2.isFixnum()) {
            return Flonum::eq(n1.toFlonum(), n2.toFixnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toFlonum(), n2.toFlonum());
        } else if (n2.isRatnum()) {
            return Flonum::eq(n1.toFlonum(), n2.toRatnum());
        } else if (n2.isBignum()) {
            return Flonum::eq(n1.toFlonum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isBignum()) {
        if (n2.isFixnum()) {
            return Bignum::eq(n1.toBignum(), n2.toFixnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toBignum(), n2.toFlonum());
        } else if (n2.isRatnum()) {
            return Ratnum::eq(n1.toBignum(), n2.toRatnum());
        } else if (n2.isBignum()) {
            return Bignum::eq(n1.toBignum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isRatnum()) {
        if (n2.isFixnum()) {
            return Ratnum::eq(n1.toRatnum(), n2.toFixnum());
        } else if (n2.isFlonum()) {
            return Flonum::eq(n1.toRatnum(), n2.toFlonum());
        } else if (n2.isRatnum()) {
            return Ratnum::eq(n1.toRatnum(), n2.toRatnum());
        } else if (n2.isBignum()) {
            return Ratnum::eq(n1.toRatnum(), n2.toBignum());
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1, n2.toCompnum());
        }
    } else if (n1.isCompnum()) {
        if (n2.isFixnum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isRatnum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isFlonum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isBignum()) {
            return Compnum::eq(n1.toCompnum(), n2);
        } else if (n2.isCompnum()) {
            return Compnum::eq(n1.toCompnum(), n2.toCompnum());
        }
    }
    return false;
}

#define MAKE_OP_FUNC(op, symbol)\
    Object Arithmetic::op(Object n1, Object n2)   \
    {\
        if (n1.isFixnum()) {\
            if (n2.isFixnum()) {\
                return Bignum::op(n1.toFixnum(), n2.toFixnum());\
            } else if (n2.isRatnum()) {\
                return Ratnum::op(n1.toFixnum(), n2.toRatnum());\
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toFixnum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Bignum::op(n1.toFixnum(), n2.toBignum());\
            } else if (n2.isCompnum()) {\
                return Compnum::op(n1, n2.toCompnum());   \
            }\
        } else if (n1.isBignum()) {\
            if (n2.isFixnum()) {\
                return Bignum::op(n1.toBignum(), n2.toFixnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toBignum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Bignum::op(n1.toBignum(), n2.toBignum());\
            } else if (n2.isRatnum()) {\
                return Ratnum::op(n1.toBignum(), n2.toRatnum()); \
            } else if (n2.isCompnum()) {\
                return Compnum::op(n1, n2.toCompnum());   \
            }\
        } else if (n1.isRatnum()) {\
            if (n2.isFixnum()) {\
                return Ratnum::op(n1.toRatnum(), new Ratnum(n2.toFixnum(), 1));\
            } else if (n2.isRatnum()) {\
                return Ratnum::op(n1.toRatnum(), n2.toRatnum());\
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toRatnum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Ratnum::op(n1.toRatnum(), n2.toBignum());\
            } else if (n2.isCompnum()) { \
                return Compnum::op(n1, n2.toCompnum());   \
            }\
        } else if (n1.isFlonum()) {\
            if (n2.isFixnum()) {\
                return Flonum::op(n1.toFlonum(), n2.toFixnum()); \
            } else if (n2.isRatnum()) {\
                return Flonum::op(n1.toFlonum(), n2.toRatnum()); \
            } else if (n2.isFlonum()) {\
                return Flonum::op(n1.toFlonum(), n2.toFlonum());\
            } else if (n2.isBignum()) {\
                return Flonum::op(n1.toFlonum(), n2.toBignum());\
            } else if (n2.isCompnum()) { \
                return Compnum::op(n1, n2.toCompnum());   \
            }\
        } else if (n1.isCompnum()) {\
            if (n2.isFixnum() || n2.isBignum() || n2.isRatnum() || n2.isFlonum()) { \
                return Compnum::op(n1.toCompnum(), n2);           \
            } else if (n2.isCompnum()) {\
                return Compnum::op(n1.toCompnum(), n2.toCompnum()); \
            }\
        }\
        return Object::False;\
    }

MAKE_OP_FUNC(add, +)
MAKE_OP_FUNC(sub, -)
//MAKE_OP_FUNC(mul, *)

    Object Arithmetic::mul(Object n1, Object n2)
    {
        if (n1.isFixnum()) {
            if (n2.isFixnum()) {
                return Bignum::mul(n1.toFixnum(), n2.toFixnum());
            } else if (n2.isRatnum()) {
                return Ratnum::mul(n1.toFixnum(), n2.toRatnum());
            } else if (n2.isFlonum()) {
                return Flonum::mul(n1.toFixnum(), n2.toFlonum());
            } else if (n2.isBignum()) {
                return Bignum::mul(n1.toFixnum(), n2.toBignum());
            } else if (n2.isCompnum()) {
                return Compnum::mul(n1, n2.toCompnum());
            }
        } else if (n1.isBignum()) {
            if (n2.isFixnum()) {
                return Bignum::mul(n1.toBignum(), n2.toFixnum());
            } else if (n2.isFlonum()) {
                return Flonum::mul(n1.toBignum(), n2.toFlonum());
            } else if (n2.isBignum()) {
                return Bignum::mul(n1.toBignum(), n2.toBignum());
            } else if (n2.isRatnum()) {
                return Ratnum::mul(n1.toBignum(), n2.toRatnum());
            } else if (n2.isCompnum()) {
                return Compnum::mul(n1, n2.toCompnum());
            }
        } else if (n1.isRatnum()) {
            if (n2.isFixnum()) {
                return Ratnum::mul(n1.toRatnum(), new Ratnum(n2.toFixnum(), 1));
            } else if (n2.isRatnum()) {
                return Ratnum::mul(n1.toRatnum(), n2.toRatnum());
            } else if (n2.isFlonum()) {
                return Flonum::mul(n1.toRatnum(), n2.toFlonum());
            } else if (n2.isBignum()) {
                return Ratnum::mul(n1.toRatnum(), n2.toBignum());
            } else if (n2.isCompnum()) {
                return Compnum::mul(n1, n2.toCompnum());
            }
        } else if (n1.isFlonum()) {
            if (n2.isFixnum()) {
                return Flonum::mul(n1.toFlonum(), n2.toFixnum());
            } else if (n2.isRatnum()) {
                return Flonum::mul(n1.toFlonum(), n2.toRatnum());
            } else if (n2.isFlonum()) {
                return Flonum::mul(n1.toFlonum(), n2.toFlonum());
            } else if (n2.isBignum()) {
                return Flonum::mul(n1.toFlonum(), n2.toBignum());
            } else if (n2.isCompnum()) {
                return Compnum::mul(n1, n2.toCompnum());
            }
        } else if (n1.isCompnum()) {
            if (n2.isFixnum() || n2.isBignum() || n2.isRatnum() || n2.isFlonum()) {
                return Compnum::mul(n1.toCompnum(), n2);
            } else if (n2.isCompnum()) {
                return Compnum::mul(n1.toCompnum(), n2.toCompnum());
            }
        }
        return Object::False;
    }


Object Arithmetic::mul(int number1, Object number2)
{
    return mul(Object::makeFixnum(number1), number2);
}

#include "ProcedureMacro.h"
#include "TextualOutputPort.h"
Object Arithmetic::div(Object n1, Object n2, bool& isDiv0Error)
{
    if (n1.isFixnum()) {
        if (n2.isFixnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                if (n2.toFixnum() == 1) {
                    return Object::makeFixnum(n1.toFixnum());
                } else {
                    return Object::makeRatnum(n1.toFixnum(), n2.toFixnum());
                }
            }
        } else if (n2.isRatnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Ratnum::div(n1.toFixnum(), n2.toRatnum());
            }
        } else if (n2.isFlonum()) {
            return Flonum::div(n1.toFixnum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Ratnum::div(n1.toFixnum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Compnum::div(n1, n2.toCompnum(), isDiv0Error);
            }
        }
    } else if (n1.isRatnum()) {
        if (n2.isFixnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Ratnum::div(n1.toRatnum(), n2.toFixnum());
            }
        } else if (n2.isRatnum()) {
            return Ratnum::div(n1.toRatnum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            return Flonum::div(n1.toRatnum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Ratnum::div(n1.toRatnum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Compnum::div(n1, n2.toCompnum(), isDiv0Error);
            }
        }
    } else if (n1.isFlonum()) {
        if (n2.isFixnum()) {
            return Flonum::div(n1.toFlonum(), n2.toFixnum());
        } else if (n2.isRatnum()) {
            return Flonum::div(n1.toFlonum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            // we don't check division by zero.
            return Flonum::div(n1.toFlonum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Flonum::div(n1.toFlonum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Compnum::div(n1, n2.toCompnum(), isDiv0Error);
            }
        }
    } else if (n1.isBignum()) {
        if (n2.isFixnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Ratnum::div(n1.toBignum(), n2.toFixnum());
            }
        } else if (n2.isRatnum()) {
            return Ratnum::div(n1.toBignum(), n2.toRatnum());
        } else if (n2.isFlonum()) {
            return Flonum::div(n1.toBignum(), n2.toFlonum());
        } else if (n2.isBignum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Ratnum::div(n1.toBignum(), n2.toBignum());
            }
        } else if (n2.isCompnum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Compnum::div(n1, n2.toCompnum(), isDiv0Error);
            }
        }
    } else if (n1.isCompnum()) {
        if (n2.isFixnum() || n2.isBignum() || n2.isRatnum() || n2.isFlonum()) {
            if (isExactZero(n2)) {
                isDiv0Error = true;
                return Object::makeFixnum(0);
            } else {
                return Compnum::div(n1.toCompnum(), n2, isDiv0Error);
            }
        } else if (n2.isCompnum()) {
            return Compnum::div(n1.toCompnum(), n2.toCompnum(), isDiv0Error);
        }
    }
    return Object::False;
}

//  Reference:
//  William D. Clinger.
//  How to read floating point numbers accurately
//  Proceedings of the ACM SIGPLAN 1990 conference on Programming language design and implementation, p.92-101, June 1990
double FlonumUtil::algorithmR(Object f, const int e, const double z0)
{
    double z = z0;
    Object x0;
    Object pow10e;
    if (e >= 0) {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        x0 = Arithmetic::mul(f, Arithmetic::expt(Object::makeFixnum(10), Object::makeFixnum(e)));
        pow10e = Object::Undef;
    } else {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        x0 = Object::Undef;
        pow10e = Arithmetic::expt(Object::makeFixnum(10), Object::makeFixnum(-e));
    }
    while (1) {
        if (isinf(z)) return z;
        int k;
        int sign;
        int64_t m = decode_double(z, &k, &sign);
        MOSH_ASSERT(sign >= 0);
        Object x;
        Object y;
        if (e >= 0) {
            if (k >= 0) {
                x = x0;
                y = Bignum::makeIntegerFromS64(m);
                y = Arithmetic::bitwiseShiftLeft(y, k);
            } else {
                x = Arithmetic::bitwiseShiftLeft(x0, -k);
                y = Bignum::makeIntegerFromS64(m);
            }
        } else {
            if (k >= 0) {
                x = f;
                y = Bignum::makeIntegerFromS64(m);
                y = Arithmetic::bitwiseShiftLeft(y, k);
                y = Arithmetic::mul(y, pow10e);
            } else {
                x = Arithmetic::bitwiseShiftLeft(f, -k);

                y = Arithmetic::mul(Bignum::makeIntegerFromS64(m), pow10e);
            }
        }
        Object D = Arithmetic::sub(x, y);
        Object D2 = Arithmetic::mul(Bignum::makeIntegerFromS64(m + m), D);
        bool negD = Arithmetic::isNegative(D);
        if (negD) {
            if (D2.isBignum()) {
                D2 = Arithmetic::mul(D2, Object::makeFixnum(-1));
            } else {
                D2 = Object::makeFixnum(-1 * D2.toFixnum());
            }
        }
        if (Arithmetic::lt(D2, y)) {
            if (negD && m == iexpt_2n52 &&
                Arithmetic::gt(Arithmetic::bitwiseShiftLeft(D2, 1), y)) {
                z = prevfloat(z);
                continue;
            }
            return z;
        }
        if (Arithmetic::eq(D2, y)) {
            if ((m & 1) == 0) {
                if (negD && m == iexpt_2n52) {
                    z = prevfloat(z);
                    continue;
                }
                return z;
            }
            return negD ? prevfloat(z) : nextfloat(z);
        }
        z = negD ? prevfloat(z) : nextfloat(z);
    }
}

double FlonumUtil::nextfloat(double z)
{
    int k;
    int sign;
    int64_t m = decode_double(z, &k, &sign);
    MOSH_ASSERT(sign >= 0);
    if (m == iexpt_2n53 - 1) return ldexp((double)iexpt_2n52, k + 1);
    return ldexp((double)(m + 1), k);
}

double FlonumUtil::prevfloat(double z)
{
    int k;
    int sign;
    int64_t m = decode_double(z, &k, &sign);
    MOSH_ASSERT(sign >= 0);
    if (m == iexpt_2n52) return ldexp((double)(iexpt_2n53 - 1), k - 1);
    return ldexp((double)(m - 1), k);
}

int64_t FlonumUtil::decode_double(double n, int* exp, int* sign)
{
    union { double f64; uint64_t u64; } datum;
    datum.f64 = n;
    uint64_t bits = datum.u64;
    uint64_t mant_bits = bits & (iexpt_2n52 - 1);
    uint32_t sign_bits = bits >> 63;
    uint32_t exp_bits = (bits >> 52) & 0x7ff;
    if (n == 0.0) {
        *exp = 0;
        *sign = sign_bits ? -1 : 1;
        return 0;
    }
    if (isnan(n)) {
        *exp = 972;
        *sign = 1;
        return 0x18000000000000LL; // (uint64_t)0x180000 << 32;
    }
    if (isinf(n)) {
        *exp = 972;
        *sign = sign_bits ? -1 : 1;
        return 0x10000000000000LL; // (uint64_t)0x100000 << 32;
    }
    MOSH_ASSERT(exp_bits != 0x7ff);
    *exp = (exp_bits ? (int)exp_bits - 1023 : -1022) - 52;
    *sign = sign_bits ? -1 : 1;
    if (exp_bits) mant_bits |= iexpt_2n52;
    return mant_bits;
}

//  Robert G. Burger and R. Kent Dybvig.
//  Printing floatingpoint numbers quickly and accurately.
//  In Proceedings of the ACM SIGPLAN '96 Conference on Programming Language Design and Implementation, pages 108--116.
//
//  Originally from Ypsilon Scheme
#define array_sizeof(a) ((int)(sizeof(a)/sizeof(a[0])))
ucs4string FlonumUtil::flonumToUcs4String(double v, bool no_exponential)
{
    char digits[32];
    int digit_count = 0;
    int exponent;
    int e;
    int sign;
    int64_t f = decode_double(v, &e, &sign);
    if (v == 0.0) return (sign > 0) ? UC("0.0") : UC("-0.0");
    if (isnan(v)) return (sign > 0) ? UC("+nan.0") : UC("-nan.0");
    if (isinf(v)) return (sign > 0) ? UC("+inf.0") : UC("-inf.0");
    if (sign == -1) v = -v;
    bool meven = ((f & 1) == 0);
    bool eq_mp_mm = true;
    Object r;
    Object s;
    Object mp;
    Object mm;
    if (e >= 0) {
        Object be = Arithmetic::expt(Object::makeFixnum(2), Object::makeFixnum(e));
        if (f != iexpt_2n52) {
            r = Bignum::makeIntegerFromS64(f);
            r = Arithmetic::bitwiseShiftLeft(r, e + 1);
            s = Object::makeFixnum(2);
            mp = be;
            mm = be;
        } else {
            Object be1 = Arithmetic::expt(Object::makeFixnum(2), Object::makeFixnum(e + 1));
            r = Bignum::makeIntegerFromS64(f);
            r = Arithmetic::bitwiseShiftLeft(r, e + 2);
            s = Object::makeFixnum(4);
            mp = be1;
            mm = be;
            eq_mp_mm = false;
        }
    } else {
        if (e == -1023 || f != iexpt_2n52) {
            r = Bignum::makeIntegerFromS64(f << 1);
            s = Arithmetic::expt(Object::makeFixnum(2), Object::makeFixnum(1 - e));
            mp = Object::makeFixnum(1);
            mm = Object::makeFixnum(1);
        } else {
            r = Bignum::makeIntegerFromS64(f << 2);
            s = Arithmetic::expt(Object::makeFixnum(2), Object::makeFixnum(2 - e));
            mp = Object::makeFixnum(2);
            mm = Object::makeFixnum(1);
            eq_mp_mm = false;
        }
    }
    // scale
    int est = (int)(ceil(log10(v) - 0.1));
    if (est > 0) {
        s = Arithmetic::mul(s, Arithmetic::expt(Object::makeFixnum(10), Object::makeFixnum(est)));
    } else {
        Object scale10 = Arithmetic::expt(Object::makeFixnum(10), Object::makeFixnum(-est));
        r = Arithmetic::mul(r, scale10);
        mp = Arithmetic::mul(mp, scale10);
        mm = eq_mp_mm ? mp : Arithmetic::mul(mm, scale10);
    }
    // fixup
    if (Arithmetic::gt(Arithmetic::add(r, mp), s) || (meven && Arithmetic::eq(Arithmetic::add(r, mp), s))) {
        s = Arithmetic::mul(s, Object::makeFixnum(10));
        exponent = est + 1;
    } else {
        exponent = est;
    }
    // generate
//     BN_TEMPORARY(bn_q);
//     BN_TEMPORARY(bn_s);
//     BN_ALLOC_FIXNUM(bn_q);
//     BN_ALLOC_FIXNUM(bn_s);
loop:
    mp = Arithmetic::mul(mp, Object::makeFixnum(10));
    mm = eq_mp_mm ? mp : Arithmetic::mul(mm, Object::makeFixnum(10));
    r = Arithmetic::mul(r, Object::makeFixnum(10));
    intptr_t dig = '0';
    if (Arithmetic::eq(r, s)) {
        dig += 1;
        r = Object::makeFixnum(0);
    } else if (Arithmetic::gt(r, s)) {

        bool isDiv0Error = false;
        Object nq = Arithmetic::floor(Arithmetic::div(r, s, isDiv0Error)); // floor is correct?
        Object nr = Arithmetic::sub(r, Arithmetic::mul(nq, s));
        MOSH_ASSERT(nq.isFixnum());
        dig += nq.toFixnum();
        r = nr;
//         if (r.isFixnum()) {
//             assert(s.isFixnum());
//             intptr_t nq = r.toFixnum() / s.toFixnum();
//             intptr_t nr = r.toFixnum() - (nq * s.toFixnum());
//             dig += nq;
//             r = Object::makeFixnum(nr);
//         } else {
//             bn_div_ans_t ans;
//             ans.quotient = &bn_q;
//             ans.remainder = r;
//             if (BIGNUMP(s)) {
//                 bn_div(heap, &ans, (scm_bignum_t)r, (scm_bignum_t)s);
//             } else {
//                 bn_let(&bn_s, (scm_fixnum_t)s);
//                 bn_div(heap, &ans, (scm_bignum_t)r, &bn_s);
//             }
//             bn_set_sign((scm_bignum_t)ans.quotient, 1);
//             bn_set_sign((scm_bignum_t)ans.remainder, 1);
//             dig += FIXNUM(bn_demote((scm_bignum_t)ans.quotient));
//             r = bn_demote((scm_bignum_t)ans.remainder);
//         }
    }
    bool tc1 = (Arithmetic::lt(r, mm) || (meven && Arithmetic::eq(r, mm)));
    bool tc2 = (Arithmetic::gt(Arithmetic::add(r, mp), s) > 0 || (meven && Arithmetic::eq(Arithmetic::add(r, mp), s)));
    if (!tc1) {
        if (!tc2) {
            digits[digit_count++] = dig;
            if (digit_count >= array_sizeof(digits)) {
                MOSH_FATAL("something wrong");
            }
            goto loop;
        } else {
            digits[digit_count++] = dig + 1;
        }
    } else {
        if (!tc2) {
            digits[digit_count++] = dig;
        } else {
            if (Arithmetic::lt(Arithmetic::add(r, r), s)) {
                digits[digit_count++] = dig;
            } else {
                digits[digit_count++] = dig + 1;
            }
        }
    }
    // todo: support misc format
    char out[512];
    int out_count = 0;
    digits[digit_count] = 0;
    if (no_exponential || (exponent >= -10 && exponent <= 10)) {
        if (sign == -1) out[out_count++] = '-';
        if (exponent <= 0) {
            out[out_count++] = '0';
            out[out_count++] = '.';
            while (++exponent <= 0) out[out_count++] = '0';
            for (int i = 0; digits[i] != 0; i++) out[out_count++] = digits[i];
        } else {
            for (int i = 0; digits[i] != 0; i++) {
                out[out_count++] = digits[i];
                if (--exponent == 0) out[out_count++] = '.';
            }
            if (exponent >= 0) {
                if (exponent == 0) {
                    out[out_count++] = '0';
                } else {
                    while (exponent > 0) {
                        out[out_count++] = '0';
                        exponent--;
                    }
                    out[out_count++] = '.';
                    out[out_count++] = '0';
                }
            }
        }
        out[out_count] = 0;
    } else {
        if (sign == -1) out[out_count++] = '-';
        out[out_count++] = digits[0];
        if (digits[1]) out[out_count++] = '.';
        for (int i = 1; digits[i] != 0; i++) out[out_count++] = digits[i];
        out[out_count] = 0;
        snprintf(&out[out_count], sizeof(out) - out_count, "e%d", exponent-1);
    }
    return ucs4string::from_c_str(out);
}

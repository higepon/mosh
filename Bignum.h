/*
 * Bignum.h -
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
 *  $Id: Bignum.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_BIGNUM_
#define SCHEME_BIGNUM_

#include "scheme.h"
#include "Fixnum.h"

#ifdef _WIN32
    #ifdef _WIN64
        #define MOSH_BIGNUM_SIZEOF_INTPTR_T 8
    #else
        #define MOSH_BIGNUM_SIZEOF_INTPTR_T 4
    #endif
#elif defined(__GNUC__)
    #if defined(__WORDSIZE) && (__WORDSIZE == 64) // Some FreeBSD have no __WORDSIZE.
        #define MOSH_BIGNUM_SIZEOF_INTPTR_T 8
    #else
        #define MOSH_BIGNUM_SIZEOF_INTPTR_T 4
    #endif
#else
    #error "define MOSH_BIGNUM_SIZEOF_INTPTR_T"
#endif

namespace scheme {

class Bignum : public gc_cleanup
{
public:
    virtual ~Bignum()
    {
        mpz_clear(this->value);
    }
    Bignum()
    {
        mpz_init(this->value);
    }

    Bignum(long value)
    {
        mpz_init(this->value);
        mpz_set_si(this->value, value);
    }

    Bignum(const Bignum& b)
    {
        mpz_init_set(this->value, b.value);
    }

    Bignum(const mpz_t value)
    {
        mpz_init_set(this->value, value);
    }

    char* toString(int radix = 10) const;

    double toDouble() const
    {
        return mpz_get_d(value);
    }

    bool isEven() const
    {
        return mpz_even_p(value) != 0;
    }

    Object sqrt() const;

    Object abs() const
    {
        Bignum temp;
        mpz_abs(temp.value, value);
        return makeInteger(temp.value);
    }

    bool isNegative() const
    {
        return mpz_cmp_si(value, 0) < 0;
    }

    bool fitsU32() const
    {
        return mpz_cmp_si(value, 0)
            && mpz_popcount(value) <= 32;
    }

    // todo fix me
    bool fitsS32() const
    {
        mpz_t temp;
        mpz_init(temp);
        mpz_abs(temp, value);
        bool ret = mpz_popcount(temp) <= 31;
        mpz_clear(temp);
        return ret;
    }

    uint32_t toU32() const
    {
        return (uint32_t)mpz_get_ui(value);
    }

    int32_t toS32() const
    {
        MOSH_ASSERT(fitsS32());
            return (int32_t)mpz_get_si(value);
    }

    bool fitsU64() const
    {
        return mpz_cmp_si(value, 0)
            && mpz_popcount(value) <= 64;
    }

    // todo fix me
    bool fitsS64() const
    {
        Bignum temp;
        mpz_abs(temp.value, value);
        return mpz_popcount(temp.value) <= 63;
    }

    uint64_t toU64() const
    {
        MOSH_ASSERT(fitsU64());
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        uint64_t ret = 0;
        Bignum temp;
        mpz_fdiv_q_2exp(temp.value, value, 32);
        ret = mpz_get_ui(temp.value);
        ret = ret << 32; // upper 32bit
        mpz_set_ui(temp.value, 0xffffffff);
        mpz_and(temp.value, value, temp.value);
        ret += mpz_get_ui(temp.value); // lower 32bit
        return ret;
#else
        return mpz_get_ui(value);
#endif
    }

    int64_t toS64() const
    {
        MOSH_ASSERT(fitsS64());
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        uint64_t ret = 0;
        Bignum temp;
        mpz_fdiv_q_2exp(temp.value, value, 32);
        ret = mpz_get_si(temp.value);
        ret = ret << 32; // upper 32bit
        mpz_set_ui(temp.value, 0xffffffff);
        mpz_and(temp.value, value, temp.value);
        ret += mpz_get_ui(temp.value); // lower 32bit
        return ret;
#else
        return mpz_get_si(value);
#endif
    }

    void setU32(uint32_t value)
    {
        mpz_set_ui(this->value, value);
    }

    void setDouble(double value)
    {
        mpz_set_d(this->value, value);
    }

    Object bitwiseNot() const
    {
        Bignum temp;
        mpz_com(temp.value, value);
        return Object::makeBignum(temp.value);
    }

    Object bitwiseAnd(int n)
    {
        return bitwiseAnd(new Bignum(n));
    }

    Object bitwiseAnd(Bignum* b)
    {
        Bignum temp;
        mpz_and(temp.value, value, b->value);
        return makeInteger(temp.value);
    }

    Object bitwiseIor(int n)
    {
        return bitwiseIor(new Bignum(n));
    }

    Object bitwiseIor(Bignum* b)
    {
        Bignum temp;
        mpz_ior(temp.value, value, b->value);
        return Object::makeBignum(temp.value);
    }

    Object bitwiseXor(int n)
    {
        return bitwiseXor(new Bignum(n));
    }

    Object bitwiseXor(Bignum* b)
    {
        Bignum temp;
        mpz_xor(temp.value, value, b->value);
        return Object::makeBignum(temp.value);
    }

    Object bitwiseBitCount()
    {
        if (gt(this, 0)) {
            return makeInteger(mpz_popcount(value));
        } else {
            Bignum temp;
            mpz_com(temp.value, value);
            const unsigned long ret = mpz_popcount(temp.value);
            return makeInteger(~ret);
        }
    }

    Object bitwiseLength()
    {
        if (mpz_cmp_si(value, 0) < 0) {
            return bitwiseNot().toBignum()->bitwiseLength();
        } else {
            size_t size = mpz_sizeinbase(value, 2);
            return makeInteger(size);
        }
    }

    Object bitwiseFirstBitSet()
    {
        const unsigned long int found = mpz_scan1(value, 0);
        if (found == ULONG_MAX) {
            return Object::makeFixnum(-1);
        } else {
            return makeInteger(found);
        }
    }

    static Object quotient(int n1, const Bignum* n2)
    {
        Bignum temp(n1);
        mpz_tdiv_q(temp.value, temp.value, n2->value);
        return makeInteger(temp.value);
    }

    static Object quotient(const Bignum* n1, int n2)
    {
        Bignum temp(n2);
        mpz_tdiv_q(temp.value, n1->value, temp.value);
        return makeInteger(temp.value);
    }

    static Object quotient(const Bignum* n1, const Bignum* n2)
    {
        Bignum temp;
        mpz_tdiv_q(temp.value, n1->value, n2->value);
        return makeInteger(temp.value);
    }

    static Object remainder(int n1, const Bignum* n2)
    {
        Bignum temp(n1);
        mpz_tdiv_r(temp.value, temp.value, n2->value);
        return makeInteger(temp.value);
    }

    static Object remainder(const Bignum* n1, int n2)
    {
        Bignum temp(n2);
        mpz_tdiv_r(temp.value, n1->value, temp.value);
        return makeInteger(temp.value);
    }

    static Object remainder(const Bignum* n1, const Bignum* n2)
    {
        Bignum temp;
        mpz_tdiv_r(temp.value, n1->value, n2->value);
        return makeInteger(temp.value);
    }


    static Object bitwiseShiftLeft(const Bignum* n1, unsigned long n2)
    {
        Bignum temp;
        mpz_mul_2exp(temp.value, n1->value, n2);
        return makeInteger(temp.value);
    }

    static Object bitwiseShiftLeft(int n1, unsigned long n2)
    {
        Bignum temp(n1);
        mpz_mul_2exp(temp.value, temp.value, n2);
        return makeInteger(temp.value);
    }

    static Object bitwiseShiftRight(const Bignum* n1, unsigned long n2)
    {
        Bignum temp;
        mpz_fdiv_q_2exp(temp.value, n1->value, n2);
        return makeInteger(temp.value);
    }

    static Object bitwiseShiftRight(int n1, unsigned long n2)
    {
        Bignum temp(n1);
        mpz_fdiv_q_2exp(temp.value, temp.value, n2);
        return makeInteger(temp.value);
    }

#define MAKE_BIGNUM_OP(op)\
    static Object op(int n1, Bignum* n2)\
    {\
        Bignum temp(n1);\
        mpz_##op(temp.value, temp.value, n2->value);\
        return makeInteger(temp.value);\
    }\
    static Object op(Bignum* n1, int n2)\
    {\
        Bignum temp(n2);\
        mpz_##op(temp.value, n1->value, temp.value);\
        return makeInteger(temp.value);\
    }\
    static Object op(Bignum* n1, Bignum* n2)\
    {\
        Bignum temp;\
        mpz_##op(temp.value, n1->value, n2->value);\
        return makeInteger(temp.value);\
    }

    MAKE_BIGNUM_OP(add)
    MAKE_BIGNUM_OP(sub)
//    MAKE_BIGNUM_OP(mul)

    static Object mul(int n1, Bignum* n2)
    {
        Bignum temp(n1); // we use stack alloced Bignum instead of using mpz_t directlly for cleanup safety.
        mpz_mul(temp.value, temp.value, n2->value);
        return makeInteger(temp.value);
    }
    static Object mul(Bignum* n1, int n2)
    {
        Bignum temp(n2);
        mpz_mul(temp.value, temp.value, n1->value);
        return makeInteger(temp.value);
    }
    static Object mul(Bignum* n1, Bignum* n2)
    {
        Bignum temp;
        mpz_mul(temp.value, n1->value, n2->value);
        return makeInteger(temp.value);
    }


#define MAKE_BIGNUM_COMPARE(compare, symbol)\
    static bool compare(Bignum* n1, int n2)\
    {\
        return mpz_cmp_si(n1->value, n2) symbol;\
    }\
    static bool compare(int n1, Bignum* n2)\
    {\
        return (- mpz_cmp_si(n2->value, n1)) symbol;\
    }\
    static bool compare(Bignum* n1, Bignum* n2)\
    {\
        return mpz_cmp(n1->value, n2->value) symbol;\
    }

    MAKE_BIGNUM_COMPARE(gt, >0)
    MAKE_BIGNUM_COMPARE(ge, >=0)
    MAKE_BIGNUM_COMPARE(lt, <0)
    MAKE_BIGNUM_COMPARE(le, <=0)
    MAKE_BIGNUM_COMPARE(eq, ==0)

    static Object add(int n1, int n2)
    {
        const fixedint ret = (fixedint)n1 + n2;
        if (Fixnum::canFit(ret)) {
            return Object::makeFixnum(ret);
        } else {
            return Object::makeBignum(ret);
        }
    }
    static Object sub(int n1, int n2)
    {
        const fixedint ret = (fixedint)n1 - n2;
        if (Fixnum::canFit(ret)) {
            return Object::makeFixnum(ret);
        } else {
            return Object::makeBignum(ret);
        }
    }

    static Object mul(int n1, int n2)
    {
        const fixedint ret = (fixedint)n1 * n2;

        /* Overflow check from Gauche */
        if ((n2 != 0 && ret / n2 != n1) || !Fixnum::canFit(ret)) {
            return Bignum::mul(new Bignum(n1), n2);
        } else {
            return Object::makeFixnum(ret);
        }
    }

    static Object makeIntegerFromU32(uint32_t n)
    {
        if (Fixnum::canFit(n)) {
            return Object::makeFixnum(n);
        } else {
            Bignum* const b = new Bignum();
            b->setU32(n);
            return Object::makeBignum(b);
        }
    }

    static Object makeIntegerFromU64(uint64_t n)
    {
        Bignum temp;
        temp.setU32(n >> 32);
        mpz_mul_2exp(temp.value, temp.value, 32);
        mpz_add_ui(temp.value, temp.value, (n & 0xffffffff));
        return makeInteger(temp.value);
    }

    static Object makeIntegerFromS64(int64_t n)
    {
        Bignum temp;
        mpz_set_si(temp.value, n >> 32);
        mpz_mul_2exp(temp.value, temp.value, 32);
        mpz_add_ui(temp.value, temp.value, (n & 0xffffffff));
        return makeInteger(temp.value);
    }

    static Object makeIntegerFromIntprt_t(intptr_t p)
    {
        MOSH_ASSERT(sizeof(uint64_t) >= sizeof(intptr_t));
        const uint64_t val = static_cast<uint64_t>(p);
        if (Fixnum::canFit(val)) {
            return Object::makeFixnum(static_cast<int>(val));
        } else {
            return makeIntegerFromU64(val);
        }
    }

    static Object makeIntegerFromUintprt_t(uintptr_t p)
    {
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        const uint32_t val = static_cast<uint32_t>(p);
        if (Fixnum::canFit(val)) {
            return Object::makeFixnum(val);
        } else {
            return makeIntegerFromU32(val);
        }
#else
        const uint64_t val = static_cast<uint64_t>(p);
        if (Fixnum::canFit(val)) {
            return Object::makeFixnum(static_cast<int>(val));
        } else {
            return makeIntegerFromU64(val);
        }
#endif
    }

    intptr_t toIntptr_t()
    {
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        return toS32();
#else
        return toS64();
#endif
    }

    uintptr_t toUintptr_t()
    {
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        return toU32();
#else
        return toU64();
#endif
    }


    static intptr_t toIntptr_t(Object n)
    {
        MOSH_ASSERT(n.isFixnum() || n.isBignum());
        if (n.isFixnum()) {
            return n.toFixnum();
        } else if (n.isBignum()) {
            return n.toBignum()->toIntptr_t();
        } else {
            // not reached
            return 0;
        }
    }

    static uintptr_t toUintptr_t(Object n)
    {
        MOSH_ASSERT(n.isFixnum() || n.isBignum());
        if (n.isFixnum()) {
            return n.toFixnum();
        } else if (n.isBignum()) {
            return n.toBignum()->toUintptr_t();
        } else {
            // not reached
            return 0;
        }
    }


    static Object makeInteger(long n)
    {
        if (Fixnum::canFit(n)) {
            return Object::makeFixnum(n);
        } else {
            return Object::makeBignum(n);
        }
    }

    static Object makeInteger(const mpz_t n)
    {
        if (mpz_fits_slong_p(n) != 0) {
            const intptr_t val = mpz_get_si(n);
            if (val >= Fixnum::MIN &&
                val <= Fixnum::MAX) {
                return Object::makeFixnum(val);
            }
        }
        return Object::makeBignum(n);
    }

    static Object makeInteger(Bignum* b)
    {
        return makeInteger(b->value);
    }

    static Object makeInteger(const ucs4string& text)
    {
        mpz_t v;
        mpz_init(v);
        mpz_init_set_str(v, text.ascii_c_str(), 10);
        const Object ret =  makeInteger(v);
        mpz_clear(v);
        return ret;
    }


    mpz_t value;

private:
};

inline Object Object::makeBignum(long n)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(new Bignum(n)))));
}

inline Object Object::makeBignum(const mpz_t b)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(new Bignum(b)))));
}

inline Object Object::makeBignum(Bignum* b)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(b))));
}

} // namespace scheme

#endif // SCHEME_BIGNUM_

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
    friend class Ratnum; // Ratnum and Bignum share the mpz_t and mpq_t
    friend Object Object::makeBignum(Bignum b);

// N.B. Don't expose the mpz_t to outside of this class.
private:
    mpz_t value_;

    Bignum(const mpz_t value)
    {
        mpz_init_set(this->value_, value);
    }

public:
    virtual ~Bignum()
    {
        mpz_clear(this->value_);
    }
    Bignum()
    {
        mpz_init(this->value_);
    }

    Bignum(long value)
    {
        mpz_init(this->value_);
        mpz_set_si(this->value_, value);
    }

    Bignum(const Bignum& b)
    {
        mpz_init_set(this->value_, b.value_);
    }

    char* toString(int radix = 10) const;

    double toDouble() const
    {
        return mpz_get_d(value_);
    }

    bool isEven() const
    {
        return mpz_even_p(value_) != 0;
    }

    Object sqrt() const;

    Object abs() const
    {
        Bignum temp;
        mpz_abs(temp.value_, value_);
        return makeInteger(temp);
    }

    bool isNegative() const
    {
        return mpz_cmp_si(value_, 0) < 0;
    }

    bool fitsU32() const
    {
        return mpz_cmp_si(value_, 0)
            && mpz_popcount(value_) <= 32;
    }

    // todo fix me
    bool fitsS32() const
    {
        Bignum temp;
        mpz_abs(temp.value_, value_);
        return mpz_popcount(temp.value_) <= 31;
    }

    uint32_t toU32() const
    {
        return (uint32_t)mpz_get_ui(value_);
    }

    int32_t toS32() const
    {
        MOSH_ASSERT(fitsS32());
            return (int32_t)mpz_get_si(value_);
    }

    bool fitsU64() const
    {
        return mpz_cmp_si(value_, 0)
            && mpz_popcount(value_) <= 64;
    }

    // todo fix me
    bool fitsS64() const
    {
        Bignum temp;
        mpz_abs(temp.value_, value_);
        return mpz_popcount(temp.value_) <= 63;
    }

    uint64_t toU64() const
    {
        MOSH_ASSERT(fitsU64());
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        uint64_t ret = 0;
        Bignum temp;
        mpz_fdiv_q_2exp(temp.value_, value_, 32);
        ret = mpz_get_ui(temp.value_);
        ret = ret << 32; // upper 32bit
        mpz_set_ui(temp.value_, 0xffffffff);
        mpz_and(temp.value_, value_, temp.value_);
        ret += mpz_get_ui(temp.value_); // lower 32bit
        return ret;
#else
        return mpz_get_ui(value_);
#endif
    }

    int64_t toS64() const
    {
        MOSH_ASSERT(fitsS64());
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        uint64_t ret = 0;
        Bignum temp;
        mpz_fdiv_q_2exp(temp.value_, value_, 32);
        ret = mpz_get_si(temp.value_);
        ret = ret << 32; // upper 32bit
        mpz_set_ui(temp.value_, 0xffffffff);
        mpz_and(temp.value_, value_, temp.value_);
        ret += mpz_get_ui(temp.value_); // lower 32bit
        return ret;
#else
        return mpz_get_si(value_);
#endif
    }

    void setU32(uint32_t value_)
    {
        mpz_set_ui(this->value_, value_);
    }

    void setDouble(double value_)
    {
        mpz_set_d(this->value_, value_);
    }

    Object bitwiseNot() const
    {
        Bignum temp;
        mpz_com(temp.value_, value_);
        return makeInteger(temp);
    }

    Object bitwiseAnd(int n)
    {
        Bignum b(n);
        return bitwiseAnd(&b);
    }

    Object bitwiseAnd(Bignum* b)
    {
        Bignum temp;
        mpz_and(temp.value_, value_, b->value_);
        return makeInteger(temp);
    }

    Object bitwiseIor(int n)
    {
        Bignum b(n);
        return bitwiseIor(&b);
    }

    Object bitwiseIor(Bignum* b)
    {
        Bignum temp;
        mpz_ior(temp.value_, value_, b->value_);
        return Object::makeBignum(temp);
    }

    Object bitwiseXor(int n)
    {
        Bignum b(n);
        return bitwiseXor(&b);
    }

    Object bitwiseXor(Bignum* b)
    {
        Bignum temp;
        mpz_xor(temp.value_, value_, b->value_);
        return Object::makeBignum(temp);
    }

    Object bitwiseBitCount()
    {
        if (gt(this, 0)) {
            return makeInteger(mpz_popcount(value_));
        } else {
            Bignum temp;
            mpz_com(temp.value_, value_);
            const unsigned long ret = mpz_popcount(temp.value_);
            return makeInteger(~ret);
        }
    }

    Object bitwiseLength()
    {
        if (mpz_cmp_si(value_, 0) < 0) {
            return bitwiseNot().toBignum()->bitwiseLength();
        } else {
            size_t size = mpz_sizeinbase(value_, 2);
            return makeInteger(size);
        }
    }

    Object bitwiseFirstBitSet()
    {
        const unsigned long int found = mpz_scan1(value_, 0);
        if (found == ULONG_MAX) {
            return Object::makeFixnum(-1);
        } else {
            return makeInteger(found);
        }
    }

    static Object quotient(int n1, const Bignum* n2)
    {
        Bignum temp(n1);
        mpz_tdiv_q(temp.value_, temp.value_, n2->value_);
        return makeInteger(temp);
    }

    static Object quotient(const Bignum* n1, int n2)
    {
        Bignum temp(n2);
        mpz_tdiv_q(temp.value_, n1->value_, temp.value_);
        return makeInteger(temp.value_);
    }

    static Object quotient(const Bignum* n1, const Bignum* n2)
    {
        Bignum temp;
        mpz_tdiv_q(temp.value_, n1->value_, n2->value_);
        return makeInteger(temp);
    }

    static Object remainder(int n1, const Bignum* n2)
    {
        Bignum temp(n1);
        mpz_tdiv_r(temp.value_, temp.value_, n2->value_);
        return makeInteger(temp);
    }

    static Object remainder(const Bignum* n1, int n2)
    {
        Bignum temp(n2);
        mpz_tdiv_r(temp.value_, n1->value_, temp.value_);
        return makeInteger(temp);
    }

    static Object remainder(const Bignum* n1, const Bignum* n2)
    {
        Bignum temp;
        mpz_tdiv_r(temp.value_, n1->value_, n2->value_);
        return makeInteger(temp);
    }


    static Object bitwiseShiftLeft(const Bignum* n1, unsigned long n2)
    {
        Bignum temp;
        mpz_mul_2exp(temp.value_, n1->value_, n2);
        return makeInteger(temp);
    }

    static Object bitwiseShiftLeft(int n1, unsigned long n2)
    {
        Bignum temp(n1);
        mpz_mul_2exp(temp.value_, temp.value_, n2);
        return makeInteger(temp);
    }

    static Object bitwiseShiftRight(const Bignum* n1, unsigned long n2)
    {
        Bignum temp;
        mpz_fdiv_q_2exp(temp.value_, n1->value_, n2);
        return makeInteger(temp);
    }

    static Object bitwiseShiftRight(int n1, unsigned long n2)
    {
        Bignum temp(n1);
        mpz_fdiv_q_2exp(temp.value_, temp.value_, n2);
        return makeInteger(temp);
    }

#define MAKE_BIGNUM_OP(op)\
    static Object op(int n1, Bignum* n2)\
    {\
        Bignum temp(n1);\
        mpz_##op(temp.value_, temp.value_, n2->value_);\
        return makeInteger(temp);\
    }\
    static Object op(Bignum* n1, int n2)\
    {\
        Bignum temp(n2);\
        mpz_##op(temp.value_, n1->value_, temp.value_);\
        return makeInteger(temp);                   \
    }\
    static Object op(Bignum* n1, Bignum* n2)\
    {\
        Bignum temp;\
        mpz_##op(temp.value_, n1->value_, n2->value_);\
        return makeInteger(temp);\
    }

    MAKE_BIGNUM_OP(add)
    MAKE_BIGNUM_OP(sub)
//    MAKE_BIGNUM_OP(mul)

    static Object mul(int n1, Bignum* n2)
    {
        Bignum temp(n1); // we use stack alloced Bignum instead of using mpz_t directlly for cleanup safety.
        mpz_mul(temp.value_, temp.value_, n2->value_);
        return makeInteger(temp);
    }
    static Object mul(Bignum* n1, int n2)
    {
        Bignum temp(n2);
        mpz_mul(temp.value_, temp.value_, n1->value_);
        return makeInteger(temp);
    }
    static Object mul(Bignum* n1, Bignum* n2)
    {
        Bignum temp;
        mpz_mul(temp.value_, n1->value_, n2->value_);
        return makeInteger(temp);
    }


#define MAKE_BIGNUM_COMPARE(compare, symbol)\
    static bool compare(Bignum* n1, int n2)\
    {\
        return mpz_cmp_si(n1->value_, n2) symbol;\
    }\
    static bool compare(int n1, Bignum* n2)\
    {\
        return (- mpz_cmp_si(n2->value_, n1)) symbol;\
    }\
    static bool compare(Bignum* n1, Bignum* n2)\
    {\
        return mpz_cmp(n1->value_, n2->value_) symbol;\
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
            Bignum temp;
            temp.setU32(n);
            return Object::makeBignum(temp);
        }
    }

    static Object makeIntegerFromU64(uint64_t n)
    {
        Bignum temp;
        temp.setU32(n >> 32);
        mpz_mul_2exp(temp.value_, temp.value_, 32);
        mpz_add_ui(temp.value_, temp.value_, (n & 0xffffffff));
        return makeInteger(temp);
    }

    static Object makeIntegerFromS64(int64_t n)
    {
        Bignum temp;
        mpz_set_si(temp.value_, n >> 32);
        mpz_mul_2exp(temp.value_, temp.value_, 32);
        mpz_add_ui(temp.value_, temp.value_, (n & 0xffffffff));
        return makeInteger(temp);
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

    static Object makeInteger(const Bignum& b)
    {
        if (mpz_fits_slong_p(b.value_) != 0) {
            const intptr_t val = mpz_get_si(b.value_);
            if (val >= Fixnum::MIN &&
                val <= Fixnum::MAX) {
                return Object::makeFixnum(val);
            }
        }
        return Object::makeBignum(b.value_);
    }

    static Object makeInteger(const ucs4string& text)
    {
        Bignum temp;
        mpz_init_set_str(temp.value_, text.ascii_c_str(), 10);
        return makeInteger(temp);
    }
};

inline Object Object::makeBignum(long n)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(new Bignum(n)))));
}

// copy constructor
inline Object Object::makeBignum(Bignum b)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(new Bignum(b.value_)))));
}

inline Object Object::makeBignum(Bignum* b)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(b))));
}

} // namespace scheme

#endif // SCHEME_BIGNUM_

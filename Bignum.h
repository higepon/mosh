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

// N.B.
//   Since we want to reduce memory allocation,
//   we pass mpz_t to makeIntger() function.
//   It is the callee responsbility to call mpz_clear mpz_t.
//   If it fits into fixnum, makeIntger immediately call mpz_clear().
//   If not, makeIntger make Bignum instance with mpz_t. ~Bignum will call mpz_clear().
class Bignum : public gc_cleanup
{
    friend class Ratnum; // Ratnum and Bignum share the mpz_t and mpq_t

// N.B. Don't expose the mpz_t to outside of this class and friend class Ratnum.
private:
    mpz_t value_;

    // N.B. mpz_t is shared between caller and this class.
    Bignum(mpz_t value)
    {
        value_[0] = value[0];
    }

    static Object makeInteger(mpz_t v)
    {
        if (mpz_fits_slong_p(v) != 0) {
            const intptr_t val = mpz_get_si(v);
            if (val >= Fixnum::MIN &&
                val <= Fixnum::MAX) {
                mpz_clear(v);
                return Object::makeFixnum(val);
            }
        }
        return Object::makeBignum(new Bignum(v));
    }

public:
    virtual ~Bignum()
    {
        mpz_clear(value_);
    }

    Bignum(long value)
    {
        mpz_init(value_);
        mpz_set_si(value_, value);
    }

    char* toString(int radix = 10) const
    {
        return mpz_get_str(NULL, radix, value_);
    }

    double toDouble() const
    {
        return mpz_get_d(value_);
    }

    bool isEven() const
    {
        return mpz_even_p(value_) != 0;
    }

    Object sqrt() const
    {
        mpz_t ret;
        mpz_init(ret);
        if (isNegative()) {
            mpz_neg(ret, value_);
            mpz_sqrt(ret, ret);
            return makeInteger(ret);
        } else {
            mpz_sqrt(ret, value_);
            return makeInteger(ret);
        }
    }

    Object abs() const
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_abs(ret, value_);
        return makeInteger(ret);
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

    bool fitsS32() const
    {
        mpz_t temp;
        mpz_init(temp);
        mpz_abs(temp, value_);
        bool ret = mpz_popcount(temp) <= 31;
        mpz_clear(temp);
        return ret;
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

    bool fitsS64() const
    {
        mpz_t temp;
        mpz_init(temp);
        mpz_abs(temp, value_);
        bool ret = mpz_popcount(temp) <= 63;
        mpz_clear(temp);
        return ret;
    }

    uint64_t toU64() const
    {
        MOSH_ASSERT(fitsU64());
#if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
        uint64_t ret = 0;
        mpz_t temp;
        mpz_init(temp);
        mpz_fdiv_q_2exp(temp, value_, 32);
        ret = mpz_get_ui(temp);
        ret = ret << 32; // upper 32bit
        mpz_set_ui(temp, 0xffffffff);
        mpz_and(temp, value_, temp);
        ret += mpz_get_ui(temp); // lower 32bit
        mpz_clear(temp);
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
        mpz_t temp;
        mpz_init(temp);
        mpz_fdiv_q_2exp(temp, value_, 32);
        ret = mpz_get_si(temp);
        ret = ret << 32; // upper 32bit
        mpz_set_ui(temp, 0xffffffff);
        mpz_and(temp, value_, temp);
        ret += mpz_get_ui(temp); // lower 32bit
        mpz_clear(temp);
        return ret;
#else
        return mpz_get_si(value_);
#endif
    }

    void setU32(uint32_t value)
    {
        mpz_set_ui(value_, value);
    }

    void setDouble(double value)
    {
        mpz_set_d(value_, value);
    }

    Object bitwiseNot() const
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_com(ret, value_);
        return makeInteger(ret);
    }

    Object bitwiseAnd(int n) const
    {
        mpz_t ret;
        mpz_init_set_si(ret, n);
        mpz_and(ret, ret, value_);
        return makeInteger(ret);
    }

    Object bitwiseAnd(Bignum* b) const
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_and(ret, b->value_, value_);
        return makeInteger(ret);
    }

    Object bitwiseIor(int n) const
    {
        mpz_t ret;
        mpz_init_set_si(ret, n);
        mpz_ior(ret, ret, value_);
        return makeInteger(ret);
    }

    Object bitwiseIor(Bignum* b) const
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_ior(ret, b->value_, value_);
        return makeInteger(ret);
    }

    Object bitwiseXor(int n) const
    {
        mpz_t ret;
        mpz_init_set_si(ret, n);
        mpz_xor(ret, ret, value_);
        return makeInteger(ret);
    }

    Object bitwiseXor(Bignum* b) const
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_xor(ret, b->value_, value_);
        return makeInteger(ret);
    }

    Object bitwiseBitCount() const
    {
        if (gt(this, 0)) {
            return makeInteger(mpz_popcount(value_));
        } else {
            mpz_t temp;
            mpz_init(temp);
            mpz_com(temp, value_);
            const unsigned long ret = mpz_popcount(temp);
            mpz_clear(temp);
            return makeInteger(~ret);
        }
    }

    Object bitwiseLength() const
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
        mpz_t ret;
        mpz_init_set_si(ret, n1);
        mpz_tdiv_q(ret, ret, n2->value_);
        return makeInteger(ret);
    }

    static Object quotient(const Bignum* n1, int n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n2);
        mpz_tdiv_q(ret, n1->value_, ret);
        return makeInteger(ret);
    }

    static Object quotient(const Bignum* n1, const Bignum* n2)
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_tdiv_q(ret, n1->value_, n2->value_);
        return makeInteger(ret);
    }

    static Object remainder(int n1, const Bignum* n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n1);
        mpz_tdiv_r(ret, ret, n2->value_);
        return makeInteger(ret);
    }

    static Object remainder(const Bignum* n1, int n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n2);
        mpz_tdiv_r(ret, n1->value_, ret);
        return makeInteger(ret);
    }

    static Object remainder(const Bignum* n1, const Bignum* n2)
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_tdiv_r(ret, n1->value_, n2->value_);
        return makeInteger(ret);
    }


    static Object bitwiseShiftLeft(const Bignum* n1, unsigned long n2)
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_mul_2exp(ret, n1->value_, n2);
        return makeInteger(ret);
    }

    static Object bitwiseShiftLeft(int n1, unsigned long n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n1);
        mpz_mul_2exp(ret, ret, n2);
        return makeInteger(ret);
    }

    static Object bitwiseShiftRight(const Bignum* n1, unsigned long n2)
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_fdiv_q_2exp(ret, n1->value_, n2);
        return makeInteger(ret);
    }

    static Object bitwiseShiftRight(int n1, unsigned long n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n1);
        mpz_fdiv_q_2exp(ret, ret, n2);
        return makeInteger(ret);
    }

    static Object mul(int n1, Bignum* n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n1);
        mpz_mul(ret, ret, n2->value_);
        return makeInteger(ret);
    }
    static Object mul(Bignum* n1, int n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n2);
        mpz_mul(ret, ret, n1->value_);
        return makeInteger(ret);
    }
    static Object mul(Bignum* n1, Bignum* n2)
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_mul(ret, n1->value_, n2->value_);
        return makeInteger(ret);
    }

    static Object add(int n1, Bignum* n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n1);
        mpz_add(ret, ret, n2->value_);
        return makeInteger(ret);
    }
    static Object add(Bignum* n1, int n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n2);
        mpz_add(ret, ret, n1->value_);
        return makeInteger(ret);
    }
    static Object add(Bignum* n1, Bignum* n2)
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_add(ret, n1->value_, n2->value_);
        return makeInteger(ret);
    }

    static Object sub(int n1, Bignum* n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n1);
        mpz_sub(ret, ret, n2->value_);
        return makeInteger(ret);
    }
    static Object sub(Bignum* n1, int n2)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n2);
        mpz_sub(ret, n1->value_, ret);
        return makeInteger(ret);
    }
    static Object sub(Bignum* n1, Bignum* n2)
    {
        mpz_t ret;
        mpz_init(ret);
        mpz_sub(ret, n1->value_, n2->value_);
        return makeInteger(ret);
    }

#define MAKE_BIGNUM_COMPARE(compare, symbol)\
    static bool compare(const Bignum* n1, int n2)\
    {\
        return mpz_cmp_si(n1->value_, n2) symbol;\
    }\
    static bool compare(int n1, const Bignum* n2)\
    {\
        return (- mpz_cmp_si(n2->value_, n1)) symbol;\
    }\
    static bool compare(const Bignum* n1, const Bignum* n2)\
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
            mpz_t ret;
            mpz_init_set_ui(ret, n);
            return Object::makeBignum(new Bignum(ret));
        }
    }

    static Object makeIntegerFromU64(uint64_t n)
    {
        mpz_t ret;
        mpz_init_set_ui(ret, n >> 32);
        mpz_mul_2exp(ret, ret, 32);
        mpz_add_ui(ret, ret, (n & 0xffffffff));
        return makeInteger(ret);
    }

    static Object makeIntegerFromS64(int64_t n)
    {
        mpz_t ret;
        mpz_init_set_si(ret, n >> 32);
        mpz_mul_2exp(ret, ret, 32);
        mpz_add_ui(ret, ret, (n & 0xffffffff));
        return makeInteger(ret);
    }

//     static Object makeIntegerFromIntprt_t(intptr_t p)
//     {
//         MOSH_ASSERT(sizeof(uint64_t) >= sizeof(intptr_t));
//         const uint64_t val = static_cast<uint64_t>(p);
//         if (Fixnum::canFit(val)) {
//             return Object::makeFixnum(static_cast<int>(val));
//         } else {
//             return makeIntegerFromU64(val);
//         }
//     }

//     static Object makeIntegerFromUintprt_t(uintptr_t p)
//     {
// #if (MOSH_BIGNUM_SIZEOF_INTPTR_T == 4)
//         const uint32_t val = static_cast<uint32_t>(p);
//         if (Fixnum::canFit(val)) {
//             return Object::makeFixnum(val);
//         } else {
//             return makeIntegerFromU32(val);
//         }
// #else
//         const uint64_t val = static_cast<uint64_t>(p);
//         if (Fixnum::canFit(val)) {
//             return Object::makeFixnum(static_cast<int>(val));
//         } else {
//             return makeIntegerFromU64(val);
//         }
// #endif
//     }

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
            MOSH_ASSERT(false);
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

    template <typename T> static Object makeIntegerFromSigned(T val)
    {
        if (sizeof(T) <= 4) {
            return Bignum::makeInteger(static_cast<long>(val)); // todo
        } else if (sizeof(T) == 8) {
            return Bignum::makeIntegerFromS64(static_cast<int64_t>(val));
        } else {
            MOSH_FATAL("unexpected size");
        }
    }

    template <typename T> static Object makeIntegerFromUnsigned(T val)
    {
        if (sizeof(T) <= 4) {
            return Bignum::makeIntegerFromU32(static_cast<uint32_t>(val));
        } else if (sizeof(T) == 8) {
            return Bignum::makeIntegerFromU64(static_cast<uint64_t>(val));
        } else {
            MOSH_FATAL("unexpected size");
        }
    }

    static Object makeInteger(const ucs4string& text)
    {
        mpz_t ret;
        mpz_init_set_str(ret, text.ascii_c_str(), 10);
        return makeInteger(ret);
    }
};

inline Object Object::makeBignum(long n)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(new Bignum(n)))));
}

inline Object Object::makeBignum(Bignum* b)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(b))));
}

} // namespace scheme

#endif // SCHEME_BIGNUM_

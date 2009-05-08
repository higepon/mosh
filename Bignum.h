/*
 * Bignum.h -
 *
 *   Copyright (c) 2008-2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

// Integer Class
// Don't use mpz_xxx outside of this class.
//
// N.B.
// We don't use GC_malloc for GNU MP allocator.
// Because mpz_xxx buffer may have many many "false pointer".
//
// Instead we use normal malloc and free with gc_cleanup
// For each destruction of Integer instance, we free the mpz_xxx buffer.
class Integer : public gc_cleanup
{
friend class Fraction;

public:
    Integer()
    {
        mpz_init(value_);
    }

    Integer(const char* str)
    {
        mpz_init(value_);
        mpz_init_set_str(value_, str, 10);
    }

    Integer(long v)
    {
        mpz_init(value_);
        mpz_set_si(value_, v);
    }

    Integer(const Integer& i)
    {
        mpz_init_set(value_, i.value_);
    }

    // This class should not be inherited.
    ~Integer()
    {
        static int counter = 0;
        printf("\r~Integer %d", counter++);
        mpz_clear(value_);
    }

    double toDouble() const
    {
        return mpz_get_d(value_);
    }

    bool isEven() const
    {
        return mpz_even_p(value_) != 0;
    }

    void setAbsolute()
    {
        mpz_abs(value_, value_);
    }

    Integer* abs() const
    {
        Integer* ret = new Integer(*this);
        ret->setAbsolute();
        return ret;
    }

    Integer* sqrt() const
    {
        Integer* ret = new Integer(*this);
        mpz_sqrt(ret->value_, ret->value_);
        return ret;
    }

    bool isNegative() const
    {
        return mpz_cmp_si(value_, 0) < 0;
    }

    int bitCount() const
    {
        MOSH_ASSERT(!isNegative());
        return mpz_popcount(value_);
    }

    char* toString(int radix = 10) const
    {
        char* buf = new(PointerFreeGC)char[mpz_sizeinbase(value_, radix) + 2];
        return mpz_get_str(buf, radix, value_);
    }


    bool fitsSlong() const
    {
        return mpz_fits_slong_p(value_) != 0;
    }

    long toSlong() const
    {
        return mpz_get_si(value_);
    }

    bool fitsU32() const
    {
        return !isNegative() && bitCount() <= 32;
    }

    bool fitsS32() const
    {
        Integer temp(*this);
        temp.setAbsolute();
        return temp.bitCount() <= 31;
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
        return !isNegative() && bitCount() <= 64;
    }

    bool fitsS64() const
    {
        Integer temp(*this);
        temp.setAbsolute();
        return temp.bitCount() <= 63;
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

    Integer* bitwiseNot() const
    {
        Integer* ret = new Integer(*this);
        mpz_com(ret->value_, value_);
        return ret;
    }

    Integer* bitwiseAnd(int n) const
    {
        Integer temp(n);
        return bitwiseAnd(&temp);
    }

    Integer* bitwiseAnd(const Integer* i) const
    {
        Integer* ret = new Integer(*this);
        mpz_and(ret->value_, value_, i->value_);
        return ret;
    }

    Integer* bitwiseIor(int n) const
    {
        Integer temp(n);
        return bitwiseIor(&temp);
    }

    Integer* bitwiseIor(const Integer* i) const
    {
        Integer* ret = new Integer(*this);
        mpz_ior(ret->value_, value_, i->value_);
        return ret;
    }

    Integer* bitwiseXor(int n) const
    {
        Integer temp(n);
        return bitwiseXor(&temp);
    }

    Integer* bitwiseXor(const Integer* i) const
    {
        Integer* ret = new Integer(*this);
        mpz_xor(ret->value_, value_, i->value_);
        return ret;
    }

    int bitwiseBitCount() const
    {
        if (isNegative()) {
            mpz_t temp;
            mpz_init(temp);
            mpz_com(temp, value_);
            const unsigned long ret = mpz_popcount(temp);
            mpz_clear(temp);
            return ~ret;
        } else {
            return mpz_popcount(value_);
        }
    }

    size_t bitwiseLength() const
    {
        if (isNegative()) {
            return bitwiseNot()->bitwiseLength();
        } else {
            return mpz_sizeinbase(value_, 2);
        }
    }

    int bitwiseFirstBitSet() const
    {
        const unsigned long int found = mpz_scan1(value_, 0);
        if (found == ULONG_MAX) {
            return -1;
        } else {
            return found;
        }
    }

    static Integer* quotient(int n1, const Integer* n2)
    {
        Integer* ret = new Integer(n1);
        mpz_tdiv_q(ret->value_, ret->value_, n2->value_);
        return ret;
    }

    static Integer* quotient(const Integer* n1, int n2)
    {
        Integer* ret = new Integer(n2);
        mpz_tdiv_q(ret->value_, n1->value_, ret->value_);
        return ret;
    }

    static Integer* quotient(const Integer* n1, const Integer* n2)
    {
        Integer* ret = new Integer;
        mpz_tdiv_q(ret->value_, n1->value_, n2->value_);
        return ret;
    }

    static Integer* remainder(int n1, const Integer* n2)
    {
        Integer* ret = new Integer(n1);
        mpz_tdiv_r(ret->value_, ret->value_, n2->value_);
        return ret;
    }

    static Integer* remainder(const Integer* n1, int n2)
    {
        Integer* ret = new Integer(n2);
        mpz_tdiv_r(ret->value_, n1->value_, ret->value_);
        return ret;
    }

    static Integer* remainder(const Integer* n1, const Integer* n2)
    {
        Integer* ret = new Integer;
        mpz_tdiv_r(ret->value_, n1->value_, n2->value_);
        return ret;
    }

    static Integer* bitwiseShiftLeft(const Integer* n1, unsigned long n2)
    {
        Integer* ret = new Integer;
        mpz_mul_2exp(ret->value_, n1->value_, n2);
        return ret;
    }

    static Integer* bitwiseShiftLeft(int n1, unsigned long n2)
    {
        Integer* ret = new Integer(n1);
        mpz_mul_2exp(ret->value_, ret->value_, n2);
        return ret;
    }

    static Integer* bitwiseShiftRight(const Integer* n1, unsigned long n2)
    {
        Integer* ret = new Integer;
        mpz_fdiv_q_2exp(ret->value_, n1->value_, n2);
        return ret;
    }

    static Integer* bitwiseShiftRight(int n1, unsigned long n2)
    {
        Integer* ret = new Integer(n1);
        mpz_fdiv_q_2exp(ret->value_, ret->value_, n2);
        return ret;
    }

    static Integer* add(int n1, const Integer* n2)
    {
        Integer* ret = new Integer(n1);
        mpz_add(ret->value_, ret->value_, n2->value_);
        return ret;
    }

    static Integer* add(const Integer* n1, int n2)
    {
        Integer* ret = new Integer(n2);
        mpz_add(ret->value_, n1->value_, ret->value_);
        return ret;
    }

    static Integer* add(Integer* n1, Integer* n2)
    {
        Integer* ret = new Integer();
        mpz_add(ret->value_, n1->value_, n2->value_);
        return ret;
    }

    static Integer* sub(int n1, const Integer* n2)
    {
        Integer* ret = new Integer(n1);
        mpz_sub(ret->value_, ret->value_, n2->value_);
        return ret;
    }

    static Integer* sub(const Integer* n1, int n2)
    {
        Integer* ret = new Integer(n2);
        mpz_sub(ret->value_, n1->value_, ret->value_);
        return ret;
    }

    static Integer* sub(Integer* n1, Integer* n2)
    {
        Integer* ret = new Integer();
        mpz_sub(ret->value_, n1->value_, n2->value_);
        return ret;
    }

    static Integer* mul(int n1, const Integer* n2)
    {
        Integer* ret = new Integer(n1);
        mpz_mul(ret->value_, ret->value_, n2->value_);
        return ret;
    }

    static Integer* mul(const Integer* n1, int n2)
    {
        Integer* ret = new Integer(n2);
        mpz_mul(ret->value_, n1->value_, ret->value_);
        return ret;
    }

    static Integer* mul(Integer* n1, Integer* n2)
    {
        Integer* ret = new Integer();
        mpz_mul(ret->value_, n1->value_, n2->value_);
        return ret;
    }

    static bool gt(Integer* n1, int n2)
    {
        return mpz_cmp_si(n1->value_, n2) > 0;
    }
    static bool gt(int n1, Integer* n2)
    {
        return (- mpz_cmp_si(n2->value_, n1)) > 0;
    }
    static bool gt(Integer* n1, Integer* n2)
    {
        return mpz_cmp(n1->value_, n2->value_) > 0;
    }

    static bool ge(Integer* n1, int n2)
    {
        return mpz_cmp_si(n1->value_, n2) >= 0;
    }
    static bool ge(int n1, Integer* n2)
    {
        return (- mpz_cmp_si(n2->value_, n1)) >= 0;
    }
    static bool ge(Integer* n1, Integer* n2)
    {
        return mpz_cmp(n1->value_, n2->value_) >= 0;
    }

    static bool lt(Integer* n1, int n2)
    {
        return mpz_cmp_si(n1->value_, n2) < 0;
    }
    static bool lt(int n1, Integer* n2)
    {
        return (- mpz_cmp_si(n2->value_, n1)) < 0;
    }
    static bool lt(Integer* n1, Integer* n2)
    {
        return mpz_cmp(n1->value_, n2->value_) < 0;
    }

    static bool le(Integer* n1, int n2)
    {
        return mpz_cmp_si(n1->value_, n2) <= 0;
    }
    static bool le(int n1, Integer* n2)
    {
        return (- mpz_cmp_si(n2->value_, n1)) <= 0;
    }
    static bool le(Integer* n1, Integer* n2)
    {
        return mpz_cmp(n1->value_, n2->value_) <= 0;
    }

    static bool eq(Integer* n1, int n2)
    {
        return mpz_cmp_si(n1->value_, n2) == 0;
    }
    static bool eq(int n1, Integer* n2)
    {
        return (- mpz_cmp_si(n2->value_, n1)) == 0;
    }
    static bool eq(Integer* n1, Integer* n2)
    {
        return mpz_cmp(n1->value_, n2->value_) == 0;
    }

    static Integer* fromU64(uint64_t n)
    {
        Integer* ret = new Integer;
        ret->setU32(n >> 32);
        mpz_mul_2exp(ret->value_, ret->value_, 32);
        mpz_add_ui(ret->value_, ret->value_, (n & 0xffffffff));
        return ret;
    }

    static Integer* fromS64(int64_t n)
    {
        Integer* ret = new Integer;
        mpz_set_si(ret->value_, n >> 32);
        mpz_mul_2exp(ret->value_, ret->value_, 32);
        mpz_add_ui(ret->value_, ret->value_, (n & 0xffffffff));
        return ret;
    }

private:
   mpz_t value_;
};

class Bignum EXTEND_GC
{
public:
    ~Bignum()
    {
    }
    Bignum() : integer_(new Integer())
    {
    }

    Bignum(Integer* integer) : integer_(integer)
    {
    }

    Bignum(long value) : integer_(new Integer(value))
    {
    }

    Integer* integer() { return integer_; }
    char* toString(int radix = 10) const;

    double toDouble() const
    {
        return integer_->toDouble();
    }


    bool isEven() const
    {
        return integer_->isEven();
    }

    Object sqrt() const;

    Object abs() const
    {
        Integer* ret = integer_->abs();
        return makeInteger(ret);
    }

    bool isNegative() const
    {
        return integer_->isNegative();
    }

    bool fitsU32() const
    {
        return integer_->fitsU32();
    }

    bool fitsS32() const
    {
        return integer_->fitsS32();
    }

    uint32_t toU32() const
    {
        return integer_->toU32();
    }

    int32_t toS32() const
    {
        return integer_->toS32();
    }

    bool fitsU64() const
    {
        return integer_->fitsU64();
    }

    bool fitsS64() const
    {
        return integer_->fitsS64();
    }

    uint64_t toU64() const
    {
        return integer_->toU64();
    }

    int64_t toS64() const
    {
        return integer_->toS64();
    }

    void setU32(uint32_t value)
    {
        integer_->setU32(value);
    }

    void setDouble(double value)
    {
        integer_->setU32(value);
    }

    Object bitwiseNot() const
    {
        return Object::makeBignum(integer_->bitwiseNot());
    }

    Object bitwiseAnd(int n)
    {
        return bitwiseAnd(new Bignum(n));
    }

    Object bitwiseAnd(const Bignum* b)
    {
        return makeInteger(integer_->bitwiseAnd(b->integer_));
    }

    Object bitwiseIor(int n)
    {
        return bitwiseIor(new Bignum(n));
    }

    Object bitwiseIor(const Bignum* b)
    {
        return Object::makeBignum(integer_->bitwiseIor(b->integer_));
    }

    Object bitwiseXor(int n)
    {
        return bitwiseXor(new Bignum(n));
    }

    Object bitwiseXor(const Bignum* b)
    {
        return Object::makeBignum(integer_->bitwiseXor(b->integer_));
    }

    Object bitwiseBitCount()
    {
        return makeInteger(integer_->bitwiseBitCount());
    }

    Object bitwiseLength()
    {
        return makeInteger(integer_->bitwiseLength());
    }

    Object bitwiseFirstBitSet()
    {
        return makeInteger(integer_->bitwiseFirstBitSet());
    }

    static Object quotient(int n1, const Bignum* n2)
    {
        return makeInteger(Integer::quotient(n1, n2->integer_));
    }

    static Object quotient(const Bignum* n1, int n2)
    {
        return makeInteger(Integer::quotient(n1->integer_, n2));
    }

    static Object quotient(const Bignum* n1, const Bignum* n2)
    {
        return makeInteger(Integer::quotient(n1->integer_, n2->integer_));
    }

    static Object remainder(int n1, const Bignum* n2)
    {
        return makeInteger(Integer::remainder(n1, n2->integer_));
    }

    static Object remainder(const Bignum* n1, int n2)
    {
        return makeInteger(Integer::remainder(n1->integer_, n2));
    }

    static Object remainder(const Bignum* n1, const Bignum* n2)
    {
        return makeInteger(Integer::remainder(n1->integer_, n2->integer_));
    }

    static Object bitwiseShiftLeft(const Bignum* n1, unsigned long n2)
    {
        return makeInteger(Integer::bitwiseShiftLeft(n1->integer_, n2));
    }

    static Object bitwiseShiftLeft(int n1, unsigned long n2)
    {
        return makeInteger(Integer::bitwiseShiftLeft(n1, n2));
    }

    static Object bitwiseShiftRight(const Bignum* n1, unsigned long n2)
    {
        return makeInteger(Integer::bitwiseShiftRight(n1->integer_, n2));
    }

    static Object bitwiseShiftRight(int n1, unsigned long n2)
    {
        return makeInteger(Integer::bitwiseShiftRight(n1, n2));
    }


#define MAKE_BIGNUM_OP(op)\
    static Object op(int n1, Bignum* n2)\
    {\
        return makeInteger(Integer::op(n1, n2->integer_)); \
    }\
    static Object op(Bignum* n1, int n2)\
    {\
        return makeInteger(Integer::op(n1->integer_, n2)); \
    }\
    static Object op(Bignum* n1, Bignum* n2)\
    {\
        return makeInteger(Integer::op(n1->integer_, n2->integer_)); \
    }

    MAKE_BIGNUM_OP(add)
    MAKE_BIGNUM_OP(sub)
    MAKE_BIGNUM_OP(mul)


#define MAKE_BIGNUM_COMPARE(compare)\
    static bool compare(Bignum* n1, int n2)\
    {\
        return Integer::compare(n1->integer_, n2);   \
    }\
    static bool compare(int n1, Bignum* n2)\
    {\
        return Integer::compare(n1, n2->integer_);   \
    }\
    static bool compare(Bignum* n1, Bignum* n2)\
    {\
        return Integer::compare(n1->integer_, n2->integer_);   \
    }

    MAKE_BIGNUM_COMPARE(gt)
    MAKE_BIGNUM_COMPARE(ge)
    MAKE_BIGNUM_COMPARE(lt)
    MAKE_BIGNUM_COMPARE(le)
    MAKE_BIGNUM_COMPARE(eq)

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
        Bignum* const b = new Bignum(Integer::fromU64(n));
        return makeInteger(b);
    }

    static Object makeIntegerFromS64(int64_t n)
    {
        Bignum* const b = new Bignum(Integer::fromS64(n));
        return makeInteger(b);
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

    static Object makeInteger(Integer* integer)
    {
        if (integer->fitsSlong()) {
            const intptr_t val = integer->toSlong();
            if (val >= Fixnum::MIN &&
                val <= Fixnum::MAX) {
                return Object::makeFixnum(val);
            }
        }
        return Object::makeBignum(integer);
    }

    static Object makeInteger(Bignum* b)
    {
        return makeInteger(b->integer_);
    }

    static Object makeInteger(const ucs4string& text)
    {
        return makeInteger(new Integer(text.ascii_c_str()));
    }

private:
    Integer* integer_;
};

inline Object Object::makeBignum(long n)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(new Bignum(n)))));
}

inline Object Object::makeBignum(Integer* integer)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(new Bignum(integer)))));
}

inline Object Object::makeBignum(Bignum* b)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::Bignum,
                                                        reinterpret_cast<intptr_t>(b))));
}

} // namespace scheme

#endif // SCHEME_BIGNUM_

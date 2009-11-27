/*
 * Byte Vector.h - <bytevector>
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
 *  $Id$
 */

#ifndef SCHEME_BYTE_VECTOR_H_
#define SCHEME_BYTE_VECTOR_H_

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244) // convert from uint64_t to uint8_t
#else
#include <stdint.h>
#endif

namespace scheme {

class ByteVector EXTEND_GC
{
public:
    explicit ByteVector(size_t num) : length_(num)
    {
#ifdef USE_BOEHM_GC
        data_ = new(PointerFreeGC) uint8_t[num];
#else
        data_ = new uint8_t[num];
#endif
    }

    ByteVector(size_t num, uint8_t v) : length_(num)
    {
#ifdef USE_BOEHM_GC
        data_ = new(PointerFreeGC) uint8_t[num];
#else
        data_ = new uint8_t[num];
#endif
        for (size_t i = 0; i < num; i++) {
            data_[i] = v;
        }
    }

    ByteVector(const gc_vector<uint8_t>& v) : length_(v.size())
    {
#ifdef USE_BOEHM_GC
        data_ = new(PointerFreeGC) uint8_t[length_];
#else
        data_ = new uint8_t[length_];
#endif
        for (size_t i = 0; i < length_; i++) {
            data_[i] = v[i];
        }
    }

    ByteVector(Object pair) : length_(Pair::length(pair))
    {
        MOSH_ASSERT(pair.isPair() || pair.isNil());

#ifdef USE_BOEHM_GC
        data_ = new(PointerFreeGC) uint8_t[length_];
#else
        data_ = new uint8_t[length_];
#endif
        size_t i = 0;
        for (Object p = pair; !p.isNil(); p = p.cdr()) {
            MOSH_ASSERT(p.car().isFixnum());
            MOSH_ASSERT(p.car().toFixnum() >= -128 && p.car().toFixnum() <= 255);
            data_[i] = p.car().toFixnum();
            i++;
        }

    }

    ByteVector(size_t num, uint8_t* data) : data_(data), length_(num)
    {
    }

    ByteVector(const ByteVector& b) : data_(b.data_), length_(b.length_)
    {
    }

    ~ByteVector() {}

    uint8_t u8Ref(size_t index) const
    {
        return data_[index];
    }

    int8_t s8Ref(size_t index) const
    {
        return static_cast<int8_t>(data_[index]);
    }

    void u8Set(size_t index, uint8_t value)
    {
        data_[index] = value;
    }

    void s8Set(size_t index, int8_t value)
    {
        data_[index] = static_cast<uint8_t>(value);
    }

    uint16_t u16RefNative(size_t index) const
    {
        return *(reinterpret_cast<uint16_t*>((&data_[index])));
    }

    uint16_t u16RefLittle(size_t index) const
    {
        return (data_[index + 1] << 8) | data_[index];
    }

    uint32_t u32RefLittle(size_t index) const
    {
        return data_[index + 3] << 24 |
               data_[index + 2] << 16 |
               data_[index + 1] << 8  |
               data_[index];
    }

    int32_t s32RefLittle(size_t index) const
    {
        return data_[index + 3] << 24 |
               data_[index + 2] << 16 |
               data_[index + 1] << 8  |
               data_[index];
    }

    void u64SetLittle(size_t index, uint64_t value)
    {
        data_[index + 7] = value >> 56;
        data_[index + 6] = value >> 48;
        data_[index + 5] = value >> 40;
        data_[index + 4] = value >> 32;
        data_[index + 3] = value >> 24;
        data_[index + 2] = value >> 16;
        data_[index + 1] = value >> 8;
        data_[index + 0] = value & 0xff;
    }

    void s64SetLittle(size_t index, int64_t value)
    {
        data_[index + 7] = value >> 56;
        data_[index + 6] = value >> 48;
        data_[index + 5] = value >> 40;
        data_[index + 4] = value >> 32;
        data_[index + 3] = value >> 24;
        data_[index + 2] = value >> 16;
        data_[index + 1] = value >> 8;
        data_[index + 0] = value & 0xff;
    }

    void u64SetBig(size_t index, uint64_t value)
    {
        data_[index + 0] = value >> 56;
        data_[index + 1] = value >> 48;
        data_[index + 2] = value >> 40;
        data_[index + 3] = value >> 32;
        data_[index + 4] = value >> 24;
        data_[index + 5] = value >> 16;
        data_[index + 6] = value >> 8;
        data_[index + 7] = value & 0xff;
    }

    void s64SetBig(size_t index, int64_t value)
    {
        data_[index + 0] = value >> 56;
        data_[index + 1] = value >> 48;
        data_[index + 2] = value >> 40;
        data_[index + 3] = value >> 32;
        data_[index + 4] = value >> 24;
        data_[index + 5] = value >> 16;
        data_[index + 6] = value >> 8;
        data_[index + 7] = value & 0xff;
    }

    uint64_t u64RefLittle(size_t index) const
    {
        return
            static_cast<uint64_t>(data_[index + 7]) << 56 |
            static_cast<uint64_t>(data_[index + 6]) << 48 |
            static_cast<uint64_t>(data_[index + 5]) << 40 |
            static_cast<uint64_t>(data_[index + 4]) << 32 |
            static_cast<uint64_t>(data_[index + 3]) << 24 |
            static_cast<uint64_t>(data_[index + 2]) << 16 |
            static_cast<uint64_t>(data_[index + 1]) << 8  |
            static_cast<uint64_t>(data_[index + 0]);
    }

    uint64_t u64RefBig(size_t index) const
    {
        return
            static_cast<uint64_t>(data_[index + 0]) << 56 |
            static_cast<uint64_t>(data_[index + 1]) << 48 |
            static_cast<uint64_t>(data_[index + 2]) << 40 |
            static_cast<uint64_t>(data_[index + 3]) << 32 |
            static_cast<uint64_t>(data_[index + 4]) << 24 |
            static_cast<uint64_t>(data_[index + 5]) << 16 |
            static_cast<uint64_t>(data_[index + 6]) << 8  |
            static_cast<uint64_t>(data_[index + 7]);
    }

    int64_t s64RefLittle(size_t index) const
    {
        return
            static_cast<int64_t>(data_[index + 7]) << 56 |
            static_cast<int64_t>(data_[index + 6]) << 48 |
            static_cast<int64_t>(data_[index + 5]) << 40 |
            static_cast<int64_t>(data_[index + 4]) << 32 |
            static_cast<int64_t>(data_[index + 3]) << 24 |
            static_cast<int64_t>(data_[index + 2]) << 16 |
            static_cast<int64_t>(data_[index + 1]) << 8  |
            static_cast<int64_t>(data_[index + 0]);
    }

    int64_t s64RefBig(size_t index) const
    {
        return
            static_cast<int64_t>(data_[index + 0]) << 56 |
            static_cast<int64_t>(data_[index + 1]) << 48 |
            static_cast<int64_t>(data_[index + 2]) << 40 |
            static_cast<int64_t>(data_[index + 3]) << 32 |
            static_cast<int64_t>(data_[index + 4]) << 24 |
            static_cast<int64_t>(data_[index + 5]) << 16 |
            static_cast<int64_t>(data_[index + 6]) << 8  |
            static_cast<int64_t>(data_[index + 7]);
    }


    uint32_t u32RefBig(size_t index) const
    {
        return data_[index + 0] << 24 |
               data_[index + 1] << 16 |
               data_[index + 2] << 8  |
               data_[index + 3];
    }

    int32_t s32RefBig(size_t index) const
    {
        return data_[index + 0] << 24 |
               data_[index + 1] << 16 |
               data_[index + 2] << 8  |
               data_[index + 3];
    }

    int32_t s32RefNative(size_t index) const
    {
        return *(reinterpret_cast<int32_t*>((&data_[index])));
    }

    uint32_t u32RefNative(size_t index) const
    {
        return *(reinterpret_cast<uint32_t*>((&data_[index])));
    }

    int64_t s64RefNative(size_t index) const
    {
        return *(reinterpret_cast<int64_t*>((&data_[index])));
    }

    uint64_t u64RefNative(size_t index) const
    {
        return *(reinterpret_cast<uint64_t*>((&data_[index])));
    }


    void u16SetLittle(size_t index, uint16_t value)
    {
        data_[index] = value & 0xff;
        data_[index + 1] = value >> 8;
    }

    void u16SetBig(size_t index, uint16_t value)
    {
        data_[index] = value >> 8;
        data_[index + 1] = value & 0xff;
    }

    void u32SetLittle(size_t index, uint32_t value)
    {
        data_[index + 3] = value >> 24;
        data_[index + 2] = value >> 16;
        data_[index + 1] = value >> 8;
        data_[index + 0] = value;
    }

    void u32SetBig(size_t index, uint32_t value)
    {
        data_[index + 0] = value >> 24;
        data_[index + 1] = value >> 16;
        data_[index + 2] = value >> 8;
        data_[index + 3] = value;
    }

    void s32SetLittle(size_t index, int32_t value)
    {
        data_[index + 3] = value >> 24;
        data_[index + 2] = value >> 16;
        data_[index + 1] = value >> 8;
        data_[index + 0] = value & 0xff;
    }

    void s32SetBig(size_t index, int32_t value)
    {
        data_[index + 0] = value >> 24;
        data_[index + 1] = value >> 16;
        data_[index + 2] = value >> 8;
        data_[index + 3] = value & 0xff;
    }


    uint16_t u16RefBig(size_t index)
    {
        return (data_[index] << 8) | data_[index + 1];
    }

    int16_t s16RefLittle(size_t index)
    {
        return ((data_[index + 1] << 8) | data_[index]);
    }

    int16_t s16RefBig(size_t index)
    {
        return ((data_[index] << 8) | data_[index + 1]);
    }

    void s16SetLittle(size_t index, int16_t value)
    {
        data_[index] = value & 0xff;
        data_[index + 1] = value >> 8;
    }

    void s16SetBig(size_t index, int16_t value)
    {
        data_[index] = value >> 8;
        data_[index + 1] = value & 0xff;
    }

    int16_t s16RefNative(size_t index)
    {
        return *(reinterpret_cast<int16_t*>((&data_[index])));
    }

    void s16SetNative(size_t index, int16_t value)
    {
        *(reinterpret_cast<int16_t*>(&data_[index])) = value;
    }

    void s32SetNative(size_t index, int32_t value)
    {
        *(reinterpret_cast<int32_t*>(&data_[index])) = value;
    }

    void s64SetNative(size_t index, int64_t value)
    {
        *(reinterpret_cast<int64_t*>(&data_[index])) = value;
    }

    void u16SetNative(size_t index, uint16_t value)
    {
        *(reinterpret_cast<uint16_t*>(&data_[index])) = value;
    }

    void u32SetNative(size_t index, uint32_t value)
    {
        *(reinterpret_cast<uint32_t*>(&data_[index])) = value;
    }

    void u64SetNative(size_t index, uint64_t value)
    {
        *(reinterpret_cast<uint64_t*>(&data_[index])) = value;
    }

    float ieeeSingleNativeRef(size_t index)
    {
        union {
            float   fvalue;
            uint8_t data[sizeof(float)];
        } n;
        memcpy(n.data, data_ + index, sizeof(float));
        return n.fvalue;
    }

    float ieeeSingleRefLittle(size_t index)
    {
#if WORDS_BIGENDIAN
        union {
            float   fvalue;
            uint8_t data[sizeof(float)];
        } n;

        for (size_t i = 0; i < sizeof(float); i++) {
            n.data[i] = data_[index + sizeof(float) - i - 1];
        }
        return n.fvalue;
#else
        return ieeeSingleNativeRef(index);
#endif
    }

    float ieeeSingleRefBig(size_t index)
    {
#if WORDS_BIGENDIAN
        return ieeeSingleNativeRef(index);
#else
        union {
            float   fvalue;
            uint8_t data[sizeof(float)];
        } n;

        for (size_t i = 0; i < sizeof(float); i++) {
            n.data[i] = data_[index + sizeof(float) - i - 1];
        }
        return n.fvalue;
#endif
    }

    void ieeeSingleNativeSet(size_t index, float value)
    {
        union {
            float   fvalue;
            uint8_t data[sizeof(float)];
        } n;
        n.fvalue = value;
        memcpy(data_ + index, n.data, sizeof(float));
    }

    void ieeeSingleSetBig(size_t index, float value)
    {
#if WORDS_BIGENDIAN
        ieeeSingleNativeSet(index, value);
#else
        union {
            float   fvalue;
            uint8_t data[sizeof(float)];
        } n;
        n.fvalue = value;
        for (size_t i = 0; i < sizeof(float); i++) {
            data_[index + sizeof(float) - i - 1] = n.data[i];
        }
#endif
    }

    void ieeeSingleSetLittle(size_t index, float value)
    {
#if WORDS_BIGENDIAN
        union {
            float   fvalue;
            uint8_t data[sizeof(float)];
        } n;
        n.fvalue = value;

        for (size_t i = 0; i < sizeof(float); i++) {
            data_[index + sizeof(float) - i - 1] = n.data[i];
        }
#else
        ieeeSingleNativeSet(index, value);
#endif
    }

    double ieeeDoubleNativeRef(size_t index)
    {
        union {
            double   fvalue;
            uint8_t data[sizeof(double)];
        } n;
        memcpy(n.data, data_ + index, sizeof(double));
        return n.fvalue;
    }

    double ieeeDoubleRefBig(size_t index)
    {
#if WORDS_BIGENDIAN
        return ieeeDoubleNativeRef(index);
#else
        union {
            double   fvalue;
            uint8_t data[sizeof(double)];
        } n;
        for (size_t i = 0; i < sizeof(double); i++) {
            n.data[i] = data_[index + sizeof(double) - i - 1];
        }
        return n.fvalue;
#endif
    }

    double ieeeDoubleRefLittle(size_t index)
    {
#if WORDS_BIGENDIAN
        union {
            double   fvalue;
            uint8_t data[sizeof(double)];
        } n;
        for (size_t i = 0; i < sizeof(double); i++) {
            n.data[i] = data_[index + sizeof(double) - i - 1];
        }
        return n.fvalue;
#else
        return ieeeDoubleNativeRef(index);
#endif
    }

    void ieeeDoubleNativeSet(size_t index, double value)
    {
        union {
            double   fvalue;
            uint8_t data[sizeof(double)];
        } n;
        n.fvalue = value;
        memcpy(data_ + index, n.data, sizeof(double));
    }

    void ieeeDoubleSetBig(size_t index, double value)
    {
#if WORDS_BIGENDIAN
        ieeeDoubleNativeSet(index, value);
#else
        union {
            double   fvalue;
            uint8_t data[sizeof(double)];
        } n;
        n.fvalue = value;
        for (size_t i = 0; i < sizeof(double); i++) {
            data_[index + sizeof(double) - i - 1] = n.data[i];
        }
#endif
    }

    void ieeeDoubleSetLittle(size_t index, double value)
    {
#if WORDS_BIGENDIAN
        union {
            double   fvalue;
            uint8_t data[sizeof(double)];
        } n;
        n.fvalue = value;
        for (size_t i = 0; i < sizeof(double); i++) {
            data_[index + sizeof(double) - i - 1] = n.data[i];
        }
#else
        ieeeDoubleNativeSet(index, value);
#endif
    }


    void fill(uint8_t value)
    {
        memset(data_, value, length_);
    }

    void s8set(size_t index, Object obj)
    {
        MOSH_ASSERT(obj.isFixnum());
        data_[index] = (uint8_t)obj.toFixnum();
    }

    size_t length() const
    {
        return length_;
    }

    const uint8_t* data() const
    {
        return data_;
    }

    uint8_t* data()
    {
        return data_;
    }

    bool equal(const ByteVector* bytevector) const
    {
        if (bytevector->length() == length()) {
            return memcmp(bytevector->data(), data(), length()) == 0;
        } else {
            return false;
        }
    }

    bool isValidIndex(size_t index) const
    {
        return index < length_;
    }

    // 16Ref will access k and k + 1
    bool isValid16RefIndex(size_t index) const
    {
        return (length_ > 1) && (index < length_ - 1);
    }

    // 32Ref will access between k and k + 3
    bool isValid32RefIndex(size_t index) const
    {
        return (length_ > 3) && (index < length_ - 3);
    }

    // 64Ref will access between k and k + 7
    bool isValid64RefIndex(size_t index) const
    {
        return (length_ > 7) && (index < length_ - 7);
    }

    ByteVector* copy()
    {
        ByteVector* bytevector = new ByteVector(length_);
        memcpy(bytevector->data(), data_, length_);
        return bytevector;
    }

    static bool inU16Range(int value)
    {
        return (0 <= value) && (value <= 65535);
    }

    static bool inS16Range(int value)
    {
        return (-32768 <= value) && (value <= 32767);
    }

    // todo
    static bool inU32Range(long long value)
    {
        return (0 <= value) && (value <= 4294967295U);
    }

    // todo
    static bool inS32Range(int64_t value)
    {
        // mm..
        return (-2147483648LL <= value) && (value <= 2147483647LL);
    }

    static bool isByte(int value)
    {
        return (-128 <= value && value <= 127);
    }

    static bool isOctet(int value)
    {
        return (0 <= value && value <= 255);
    }

private:
    uint8_t* data_;
    const size_t length_;
};

inline Object Object::makeByteVector(const gc_vector<uint8_t>& v)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<intptr_t>(new ByteVector(v)))));
}

inline Object Object::makeByteVector(Object pair)
{
    MOSH_ASSERT(pair.isPair() || pair.isNil());
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<intptr_t>(new ByteVector(pair)))));
}


inline Object Object::makeByteVector(ByteVector* b)
{
    return Object(reinterpret_cast<intptr_t>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<intptr_t>(b))));
}

} // namespace scheme

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif // SCHEME_BYTE_VECTOR_H_

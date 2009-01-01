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

#ifndef __SCHEME_BYTE_VECTOR_H__
#define __SCHEME_BYTE_VECTOR_H__

#include <stdint.h>

namespace scheme {

class ByteVector EXTEND_GC
{
public:
    explicit ByteVector(int num) : length_(num)
    {
#ifdef USE_BOEHM_GC
        data_ = new(PointerFreeGC) uint8_t[num];
#else
        data_ = new uint8_t[num];
#endif
    }

    ByteVector(int num, uint8_t v) : length_(num)
    {
#ifdef USE_BOEHM_GC
        data_ = new(PointerFreeGC) uint8_t[num];
#else
        data_ = new uint8_t[num];
#endif
        for (int i = 0; i < num; i++) {
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
        for (int i = 0; i < length_; i++) {
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
        int i = 0;
        for (Object p = pair; !p.isNil(); p = p.cdr()) {
            MOSH_ASSERT(p.car().isFixnum());
            MOSH_ASSERT(p.car().toFixnum() >= -128 && p.car().toFixnum() <= 255);
            data_[i] = p.car().toFixnum();
            i++;
        }

    }

    ByteVector(int num, uint8_t* data) : data_(data), length_(num)
    {
    }

    ByteVector(const ByteVector& b) : data_(b.data_), length_(b.length_)
    {
    }

    ~ByteVector() {}

    uint8_t u8Ref(int index) const
    {
        return data_[index];
    }

    int8_t s8Ref(int index) const
    {
        return static_cast<int8_t>(data_[index]);
    }

    void u8Set(int index, uint8_t value)
    {
        data_[index] = value;
    }

    void s8Set(int index, int8_t value)
    {
        data_[index] = static_cast<uint8_t>(value);
    }

    uint16_t u16RefNative(int index) const
    {
        return *(reinterpret_cast<uint16_t*>((&data_[index])));
    }

    uint16_t u16RefLittle(int index) const
    {
        return (data_[index + 1] << 8) | data_[index];
    }

    uint32_t u32RefLittle(int index) const
    {
        return data_[index + 3] << 24 |
               data_[index + 2] << 16 |
               data_[index + 1] << 8  |
               data_[index];
    }

    int32_t s32RefLittle(int index) const
    {
        return data_[index + 3] << 24 |
               data_[index + 2] << 16 |
               data_[index + 1] << 8  |
               data_[index];
    }

    void u64SetLittle(int index, uint64_t value)
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

    void s64SetLittle(int index, int64_t value)
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

    void u64SetBig(int index, uint64_t value)
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

    void s64SetBig(int index, int64_t value)
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

    uint64_t u64RefLittle(int index) const
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

    uint64_t u64RefBig(int index) const
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

    int64_t s64RefLittle(int index) const
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

    int64_t s64RefBig(int index) const
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


    uint32_t u32RefBig(int index) const
    {
        return data_[index + 0] << 24 |
               data_[index + 1] << 16 |
               data_[index + 2] << 8  |
               data_[index + 3];
    }

    int32_t s32RefBig(int index) const
    {
        return data_[index + 0] << 24 |
               data_[index + 1] << 16 |
               data_[index + 2] << 8  |
               data_[index + 3];
    }

    int32_t s32RefNative(int index) const
    {
        return *(reinterpret_cast<int32_t*>((&data_[index])));
    }

    uint32_t u32RefNative(int index) const
    {
        return *(reinterpret_cast<uint32_t*>((&data_[index])));
    }

    int64_t s64RefNative(int index) const
    {
        return *(reinterpret_cast<int64_t*>((&data_[index])));
    }

    uint64_t u64RefNative(int index) const
    {
        return *(reinterpret_cast<uint64_t*>((&data_[index])));
    }


    void u16SetLittle(int index, uint16_t value)
    {
        data_[index] = value & 0xff;
        data_[index + 1] = value >> 8;
    }

    void u16SetBig(int index, uint16_t value)
    {
        data_[index] = value >> 8;
        data_[index + 1] = value & 0xff;
    }

    void u32SetLittle(int index, uint32_t value)
    {
        data_[index + 3] = value >> 24;
        data_[index + 2] = value >> 16;
        data_[index + 1] = value >> 8;
        data_[index + 0] = value;
    }

    void u32SetBig(int index, uint32_t value)
    {
        data_[index + 0] = value >> 24;
        data_[index + 1] = value >> 16;
        data_[index + 2] = value >> 8;
        data_[index + 3] = value;
    }

    void s32SetLittle(int index, int32_t value)
    {
        data_[index + 3] = value >> 24;
        data_[index + 2] = value >> 16;
        data_[index + 1] = value >> 8;
        data_[index + 0] = value & 0xff;
    }

    void s32SetBig(int index, int32_t value)
    {
        data_[index + 0] = value >> 24;
        data_[index + 1] = value >> 16;
        data_[index + 2] = value >> 8;
        data_[index + 3] = value & 0xff;
    }


    uint16_t u16RefBig(int index)
    {
        return (data_[index] << 8) | data_[index + 1];
    }

    int16_t s16RefLittle(int index)
    {
        return ((data_[index + 1] << 8) | data_[index]);
    }

    int16_t s16RefBig(int index)
    {
        return ((data_[index] << 8) | data_[index + 1]);
    }

    void s16SetLittle(int index, int16_t value)
    {
        data_[index] = value & 0xff;
        data_[index + 1] = value >> 8;
    }

    void s16SetBig(int index, int16_t value)
    {
        data_[index] = value >> 8;
        data_[index + 1] = value & 0xff;
    }

    int16_t s16RefNative(int index)
    {
        return *(reinterpret_cast<int16_t*>((&data_[index])));
    }

    void s16SetNative(int index, int16_t value)
    {
        *(reinterpret_cast<int16_t*>(&data_[index])) = value;
    }

    void s32SetNative(int index, int32_t value)
    {
        *(reinterpret_cast<int32_t*>(&data_[index])) = value;
    }

    void s64SetNative(int index, int64_t value)
    {
        *(reinterpret_cast<int64_t*>(&data_[index])) = value;
    }

    void u16SetNative(int index, uint16_t value)
    {
        *(reinterpret_cast<uint16_t*>(&data_[index])) = value;
    }

    void u32SetNative(int index, uint32_t value)
    {
        *(reinterpret_cast<uint32_t*>(&data_[index])) = value;
    }

    void u64SetNative(int index, uint64_t value)
    {
        *(reinterpret_cast<uint64_t*>(&data_[index])) = value;
    }

    float ieeeSingleNativeRef(int index)
    {
        return *(reinterpret_cast<float*>(&data_[index]));
    }

    float ieeeSingleRefLittle(int index)
    {
#if WORDS_BIGENDIAN
        const int size = sizeof(float);
        uint8_t data[size];
        for (int i = 0; i < size; i++) {
            data[i] = data_[index + size - i - 1];
        }
        return *(reinterpret_cast<float*>(data));
#else
        return *(reinterpret_cast<float*>(&data_[index]));
#endif
    }

    float ieeeSingleRefBig(int index)
    {
#if WORDS_BIGENDIAN
        return *(reinterpret_cast<float*>(&data_[index]));
#else
        const int size = sizeof(float);
        uint8_t data[size];
        for (int i = 0; i < size; i++) {
            data[i] = data_[index + size - i - 1];
        }
        return *(reinterpret_cast<float*>(data));
#endif
    }

    void ieeeSingleNativeSet(int index, float value)
    {
        *(reinterpret_cast<float*>((&data_[index]))) = value;
    }

    void ieeeSingleSetBig(int index, float value)
    {
#if WORDS_BIGENDIAN
        *(reinterpret_cast<float*>((&data_[index]))) = value;
#else
        const int size = sizeof(float);
        uint8_t data[size];
        *(reinterpret_cast<float*>(data)) = value;
        for (int i = 0; i < size; i++) {
            data_[index + size - i - 1] = data[i];
        }
#endif
    }

    void ieeeSingleSetLittle(int index, float value)
    {
#if WORDS_BIGENDIAN
        const int size = sizeof(float);
        uint8_t data[size];
        *(reinterpret_cast<float*>(data)) = value;
        for (int i = 0; i < size; i++) {
            data_[index + size - i - 1] = data[i];
        }
#else
        *(reinterpret_cast<float*>((&data_[index]))) = value;
#endif
    }

    double ieeeDoubleNativeRef(int index)
    {
        return *(reinterpret_cast<double*>(&data_[index]));
    }

    double ieeeDoubleRefBig(int index)
    {
#if WORDS_BIGENDIAN
        return *(reinterpret_cast<double*>(&data_[index]));
#else
        const int size = sizeof(double);
        uint8_t data[size];
        for (int i = 0; i < size; i++) {
            data[i] = data_[index + size - i - 1];
        }
        return *(reinterpret_cast<double*>(data));
#endif
    }

    double ieeeDoubleRefLittle(int index)
    {
#if WORDS_BIGENDIAN
        const int size = sizeof(double);
        uint8_t data[size];
        for (int i = 0; i < size; i++) {
            data[i] = data_[index + size - i - 1];
        }
        return *(reinterpret_cast<double*>(data));
#else
        return *(reinterpret_cast<double*>(&data_[index]));
#endif
    }

    void ieeeDoubleNativeSet(int index, double value)
    {
        *(reinterpret_cast<double*>((&data_[index]))) = value;
    }

    void ieeeDoubleSetBig(int index, double value)
    {
#if WORDS_BIGENDIAN
        *(reinterpret_cast<double*>((&data_[index]))) = value;
#else
        const int size = sizeof(double);
        uint8_t data[size];
        *(reinterpret_cast<double*>(data)) = value;
        for (int i = 0; i < size; i++) {
            data_[index + size - i - 1] = data[i];
        }
#endif
    }

    void ieeeDoubleSetLittle(int index, double value)
    {
#if WORDS_BIGENDIAN
        const int size = sizeof(double);
        uint8_t data[size];
        *(reinterpret_cast<double*>(data)) = value;
        for (int i = 0; i < size; i++) {
            data_[index + size - i - 1] = data[i];
        }
#else
        *(reinterpret_cast<double*>((&data_[index]))) = value;
#endif
    }


    void fill(uint8_t value)
    {
        memset(data_, value, length_);
    }

    void s8set(int index, Object obj)
    {
        MOSH_ASSERT(obj.isFixnum());
        data_[index] = (uint8_t)obj.toFixnum();
    }

    int length() const
    {
        return length_;
    }

    uint8_t* data() const
    {
        return data_;
    }

    bool equal(ByteVector* bytevector) const
    {
        if (bytevector->length() == length()) {
            return memcmp(bytevector->data(), data(), length()) == 0;
        } else {
            return false;
        }
    }

    bool isValidIndex(int index) const
    {
        return (index >= 0 && index < length_);
    }

    // 16Ref will access k and k + 1
    bool isValid16RefIndex(int index) const
    {
        return (index >= 0 && index < length_ - 1);
    }

    // 32Ref will access between k and k + 3
    bool isValid32RefIndex(int index) const
    {
        return (index >= 0 && index < length_ - 3);
    }

    // 64Ref will access between k and k + 7
    bool isValid64RefIndex(int index) const
    {
        return (index >= 0 && index < length_ - 7);
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
    static bool inS32Range(long long value)
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
    const int length_;
};

inline Object Object::makeByteVector(const gc_vector<uint8_t>& v)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(new ByteVector(v)))));
}

inline Object Object::makeByteVector(Object pair)
{
    MOSH_ASSERT(pair.isPair() || pair.isNil());
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(new ByteVector(pair)))));
}


inline Object Object::makeByteVector(ByteVector* b)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(b))));
}

}; // namespace scheme

#endif // __SCHEME_BYTE_VECTOR_H__

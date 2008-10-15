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
            MOSH_ASSERT(p.car().isInt());
            MOSH_ASSERT(p.car().toInt() >= -128 && p.car().toInt() <= 255);
            data_[i] = p.car().toInt();
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

    uint8_t u8Ref(int index)
    {
        return data_[index];
    }

    int8_t s8Ref(int index)
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

    void fill(uint8_t value)
    {
        memset(data_, value, length_);
    }

    void s8set(int index, Object obj)
    {
        MOSH_ASSERT(obj.isInt());
        data_[index] = (uint8_t)obj.toInt();
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

    ByteVector* copy()
    {
        ByteVector* bytevector = new ByteVector(length_);
        memcpy(bytevector->data(), data_, length_);
        return bytevector;
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

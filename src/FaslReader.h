/*
 * FaslReader.h - 
 *
 *   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: FaslReader.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_FASL_READER_
#define SCHEME_FASL_READER_

#include "scheme.h"
#include "Fasl.h"

namespace scheme {

class FaslReader EXTEND_GC
{
public:
    FaslReader(VM* theVM, BinaryInputPort* inputPort);
    Object get();

private:
    void getSymbolsAndStrings();
    void linkShared(Object obj, EqHashTable* seen);
    Object getShared(int index);

    // profiler tells that this should be inlined
    uint32_t fetchU32()
    {
        // TODO: We should check if getU8 returns EOF.
        const uint8_t a = static_cast<uint8_t>(inputPort_->getU8());
        const uint8_t b = static_cast<uint8_t>(inputPort_->getU8());
        const uint8_t c = static_cast<uint8_t>(inputPort_->getU8());
        const uint8_t d = static_cast<uint8_t>(inputPort_->getU8());
        return a | (b << 8) | (c << 16) | (d << 24);
    }

    // profiler tells that this should be inlined
    uint16_t fetchU16()
    {
        const uint8_t a = static_cast<uint8_t>(inputPort_->getU8());
        const uint8_t b = static_cast<uint8_t>(inputPort_->getU8());
        return a | (b << 8);
    }

    uint64_t fetchU64()
    {
        const uint32_t a = fetchU32();
        const uint32_t b = fetchU32();
        return a | (((uint64_t)b) << 32);
    }

    // profiler tells that this should be inlined
    int fetchU8()
    {
        return inputPort_->getU8();
    }

    // profiler tells that this should be inlined
    Object getDatum()
    {
        const int octet = fetchU8();
        switch (octet) {
        case EOF:
            return Object::Eof;
        case Fasl::TAG_LOOKUP: {
            const uint32_t uid = fetchU32();
            Object obj = sharedObjects_->ref(Object::makeFixnum(uid), Object::Ignore);
            if (obj == Object::Ignore) {
                isLinkNeeded_ = true;
                return Object::makeSharedReference(uid);
            } else {
                return obj;
            }
        }
        case Fasl::TAG_DEFINING_SHARED:
        {
            const uint32_t uid = fetchU32();
            Object obj = getDatum();
            sharedObjects_->set(Object::makeFixnum(uid), obj);
            return obj;
        }
        case Fasl::TAG_SYMBOL:
        {
            uint32_t len = fetchU32();
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += (ucs4char)fetchU32();
            }
            return Symbol::intern(text.strdup());
        }
        case Fasl::TAG_ASCII_SYMBOL:
        {
            uint32_t len = fetchU32();
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU8();
            }
            return Symbol::intern(text.strdup());
        }
        case Fasl::TAG_SHORT_ASCII_SYMBOL:
        {
            uint8_t len = static_cast<uint8_t>(fetchU8());
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU8();
            }
            return Symbol::intern(text.strdup());
        }
        case Fasl::TAG_ASCII_UNINTERNED_SYMBOL:
        {
            uint32_t len = fetchU32();
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU8();
            }
            return Object::makeString(text.strdup());
        }
        case Fasl::TAG_SHORT_ASCII_UNINTERNED_SYMBOL:
        {
            uint8_t len = static_cast<uint8_t>(fetchU8());
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU8();
            }
            return Object::makeString(text.strdup());
        }
        case Fasl::TAG_UNINTERNED_SYMBOL:
        {
            uint32_t len = fetchU32();
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += (ucs4char)fetchU32();
            }
            return Object::makeString(text.strdup());
        }
        case Fasl::TAG_STRING:
        {
            uint32_t len = fetchU32();
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += (ucs4char)fetchU32();
            }
            return Object::makeString(text);
        }
        case Fasl::TAG_ASCII_STRING:
        {
            uint32_t len = fetchU32();
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU8();
            }
            return Object::makeString(text);
        }
        case Fasl::TAG_SHORT_ASCII_STRING:
        {
            uint8_t len = static_cast<uint8_t>(fetchU8());
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU8();
            }
            return Object::makeString(text);
        }
        case Fasl::TAG_RTD:
        {
            uint32_t len = fetchU32();
            ucs4string text;
            text.reserve(len);
            for (uint32_t i = 0; i < len; i++) {
                text += (ucs4char)fetchU32();
            }
            ucs4string nameString = text;
            nameString += UC("-rtd$");
            const Object rtd = theVM_->getGlobalValueOrFalse(Symbol::intern(nameString.strdup()));
            MOSH_ASSERT(!rtd.isFalse());
            return rtd;
        }
        case Fasl::TAG_SMALL_FIXNUM: {
            const int value = fetchU8();
            return Object::makeFixnum(value);
        }
        case Fasl::TAG_MEDIUM_FIXNUM: {
            const int value = fetchU16();
            return Object::makeFixnum(value);
        }
        case Fasl::TAG_FIXNUM_0: {
            return Object::makeFixnum(0);
        }
        case Fasl::TAG_FIXNUM_1: {
            return Object::makeFixnum(1);
        }
        case Fasl::TAG_FIXNUM: {
            const int value = fetchU32();
            return Object::makeFixnum(value);
        }
        case Fasl::TAG_FLONUM: {
            union {
                double   f64;
                uint64_t u64;
            } n;
            n.u64 = fetchU64();
            return Object::makeFlonum(n.f64);
        }
        case Fasl::TAG_INSTRUCTION: {
            const int value = fetchU8();
            return Object::makeInstruction(value);
        }
        case Fasl::TAG_COMPILER_INSTRUCTION: {
            const int value = fetchU32();
            return Object::makeCompilerInstruction(value);
        }
        case Fasl::TAG_PAIR: {
            Object kar = getDatum();
            Object kdr = getDatum();
            return Object::cons(kar, kdr);
        }
        case Fasl::TAG_BIGNUM: {
            uint16_t size = fetchU16();
            uint8_t* data = allocatePointerFreeU8Array(size);
            for (uint16_t i = 0; i < size; i++) {
                data[i] = static_cast<uint8_t>(fetchU8());
            }
            return Bignum::deserialize(data, size);
        }
        case Fasl::TAG_RATNUM: {
            const Object numerator = getDatum();
            const Object denominator = getDatum();
            bool isDiv0Error = false;
            return Arithmetic::div(numerator, denominator, isDiv0Error);
        }
        case Fasl::TAG_COMPNUM: {
            const Object real = getDatum();
            const Object imag = getDatum();
            return Object::makeCompnum(real, imag);
        }
        case Fasl::TAG_REGEXP: {
            uint32_t len = fetchU32();
            ucs4string text;
            for (uint32_t i = 0; i < len; i++) {
                text += (ucs4char)fetchU32();
            }
            return Object::makeRegexp(text, false, false);

        }
        case Fasl::TAG_PLIST: {
            const int count = fetchU32();
            Object list = Object::Nil;
            for (int i = 0; i < count; i++) {
                const Object datum = getDatum();
                list = Object::cons(datum, list);
            }
            return list;
        }
        case Fasl::TAG_DLIST: {
            const int count = fetchU32();
            Object list = getDatum();
            for (int i = 0; i < count; i++) {
                list = Object::cons(getDatum(), list);
            }
            return list;
        }
        case Fasl::TAG_VECTOR: {
            const int count = fetchU32();
            Object vector = Object::makeVector(count);
            Vector* const v = vector.toVector();
            for (int i = 0; i < count; i++) {
                v->set(i, getDatum());
            }
            return vector;
        }
        case Fasl::TAG_BVECTOR: {
            const int count = fetchU32();
            Object bv = Object::makeByteVector(count);
            for (int i = 0; i < count; i++) {
                bv.toByteVector()->u8Set(i, static_cast<uint8_t>(fetchU8()));
            }
            return bv;
        }
        case Fasl::TAG_CHAR:
            return Object::makeChar(fetchU32());
        case Fasl::TAG_NIL:
            return Object::Nil;
        case Fasl::TAG_T:
            return Object::True;
        case Fasl::TAG_F:
            return Object::False;
        case Fasl::TAG_SIMPLE_STRUCT:
        {
            Object name = getDatum();
            MOSH_ASSERT(name.isSymbol());

            Object length = getDatum();
            MOSH_ASSERT(length.isFixnum());
            const int len = length.toFixnum();
            Object st = Object::makeSimpleStruct(name, len);
            for (int i = 0; i < len; i++) {
                st.toSimpleStruct()->set(i, getDatum());
            }
            return st;
        }
        case Fasl::TAG_EQ_HASH_TABLE:
        {
            Object length = getDatum();
            MOSH_ASSERT(length.isFixnum());
            const int len = length.toFixnum();
            const Object ret = Object::makeEqHashTable();
            EqHashTable* const ht = ret.toEqHashTable();
            for (int i = 0; i < len; i++) {
                const Object key = getDatum();
                const Object value = getDatum();
                ht->set(key, value);
            }
            return ret;
        }
        default:
            fprintf(stderr, "tag = %d", octet);
            MOSH_ASSERT(false);
        }

        MOSH_ASSERT(false);
        return Object::Undef;
    }

    BinaryInputPort* inputPort_;
    VM* theVM_;
    EqHashTable* sharedObjects_;
    bool isLinkNeeded_{false};
};


}; // namespace scheme

#endif // SCHEME_FASL_READER_

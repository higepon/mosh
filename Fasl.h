/*
 * Fasl.h - Fast loading.
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
 *  $Id: Fasl.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_FASL_
#define SCHEME_FASL_

#include "scheme.h"
#include "Vector.h"
#include "ByteVector.h"

namespace scheme {

class EqHashTable;
class BinaryInputPort;
class BinaryOutputPort;

class Fasl EXTEND_GC
{
public:
    enum {
        TAG_LOOKUP = 1,
        TAG_FIXNUM,
        TAG_PLIST,
        TAG_DLIST,
        TAG_VECTOR,
        TAG_BVECTOR,
        TAG_REGEXP,
        TAG_CHAR,
        TAG_NIL,
        TAG_T,
        TAG_F,
        TAG_SYMBOL,
        TAG_STRING,
        TAG_INSTRUCTION,
        TAG_COMPILER_INSTRUCTION,
        TAG_FLONUM,
        TAG_SMALL_FIXNUM,
        TAG_ASCII_STRING,
        TAG_ASCII_SYMBOL,
        TAG_MEDIUM_FIXNUM,
        TAG_RTD,
        TAG_RECORD,
        TAG_EQ_HASH_TABLE,
        TAG_BIGNUM,
        TAG_FIXNUM_0,
        TAG_FIXNUM_1,
        TAG_ASCII_UNINTERNED_SYMBOL,
        TAG_UNINTERNED_SYMBOL,
        forbidden_comma
    };
};

class FaslReader EXTEND_GC
{
public:
    FaslReader(VM* theVM, BinaryInputPort* inputPort);
    Object get();

private:
    void getSymbolsAndStrings();

    // profiler tells that this should be inlined
    uint32_t fetchU32()
    {
        const uint8_t a = inputPort_->getU8();
        const uint8_t b = inputPort_->getU8();
        const uint8_t c = inputPort_->getU8();
        const uint8_t d = inputPort_->getU8();
        return a | (b << 8) | (c << 16) | (d << 24);
    }

    // profiler tells that this should be inlined
    uint16_t fetchU16()
    {
        const uint8_t a = inputPort_->getU8();
        const uint8_t b = inputPort_->getU8();
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
            return symbolsAndStringsArray_[uid];
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
        case Fasl::TAG_PLIST: {
            const int count = fetchU32();
            Object list = Object::Nil;
            for (int i = 0; i < count; i++) {
                const Object datum = getDatum();
                list = Object::cons(datum, list);
            }
            return list;
        }
        case Fasl::TAG_BIGNUM: {
            int64_t value = fetchU64();
            return Bignum::makeIntegerFromS64(value);
        }
        case Fasl::TAG_DLIST: {
            const int count = fetchU32();
            Object list = getDatum();
            for (int i = 0; i < count; i++) {
                list = Object::cons(getDatum(), list);
            }
            return list;
        }
        case Fasl::TAG_REGEXP: {
            uint32_t len = fetchU32();
            ucs4string text;
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU32();
            }
            return Object::makeRegexp(text);

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
                bv.toByteVector()->u8Set(i, fetchU8());
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
        case Fasl::TAG_SYMBOL:
        case Fasl::TAG_STRING:
            break;
        case Fasl::TAG_RTD:
        {
            const Object name = getDatum();
            MOSH_ASSERT(name.isSymbol());
            ucs4string nameString = name.toSymbol()->c_str();
            nameString += UC("-rtd$");
            const Object rtd = theVM_->getTopLevelGlobalValueOrFalse(Symbol::intern(nameString.strdup()));
            MOSH_ASSERT(!rtd.isFalse());
            return rtd;
        }
        case Fasl::TAG_RECORD:
        {
            Object rtd = getDatum();
            MOSH_ASSERT(rtd.isRecordTypeDescriptor());
            Object length = getDatum();
            MOSH_ASSERT(length.isFixnum());
            const int len = length.toFixnum();
            Object* fields = Object::makeObjectArray(len);
            for (int i = 0; i < len; i++) {
                fields[i] = getDatum();
            }
            return Object::makeRecord(rtd, fields, len);
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
            MOSH_ASSERT(false);
        }
        MOSH_ASSERT(false);
        return Object::Undef;
    }

    Object* symbolsAndStringsArray_;
    BinaryInputPort* inputPort_;
    VM* theVM_;
};

class FaslWriter EXTEND_GC
{
public:
    FaslWriter(BinaryOutputPort* outputPort);

    void put(Object obj);
private:
    void collectSymbolsAndStrings(Object obj);
    void putSymbolsAndStrings();
    void putList(Object list);
    void emitU8(uint8_t value);
    void emitU16(uint16_t value);
    void emitU32(uint32_t value);
    void emitU64(uint64_t value);
    void emitString(const ucs4string& string);
    void emitAsciiString(const ucs4string& string);
    void putDatum(Object obj);

    EqHashTable* symbolsAndStringsTable_;
    BinaryOutputPort* outputPort_;
};

}; // namespace scheme

#endif // SCHEME_FASL_

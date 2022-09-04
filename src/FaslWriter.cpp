/*
 * Fasl.cpp - Fast loading. Based on Fasl of ypsilon scheme by Yoshikatsu Fujita.
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
 *  $Id: Fasl.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "EqHashTable.h"
#include "Symbol.h"
#include "Regexp.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "BinaryInputPort.h"
#include "BinaryOutputPort.h"
#include "TextualOutputPort.h"
#include "ProcedureMacro.h"
#include "Bignum.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Compnum.h"
#include "EqHashTable.h"
#include "SimpleStruct.h"
#include "FaslWriter.h"


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244) // convert from uint64_t to uint8_t
#endif

using namespace scheme;

FaslWriter::FaslWriter(BinaryOutputPort* outputPort) :
                                                       sharedObjects_(new EqHashTable),
                                                       outputPort_(outputPort)
                                                       
{
}

bool FaslWriter::isInteresting(Object obj)
{
    return obj.isString() || obj.isSymbol() || obj.isPair() || obj.isVector()
        || obj.isSimpleStruct() || obj.isEqHashTable();
}

void FaslWriter::scanSharedObjects(Object obj)
{
loop:
    if (!isInteresting(obj)) {
        return;
    }
    const Object val = sharedObjects_->ref(obj, Object::Ignore);
    if (val.isFalse()) {
        sharedObjects_->set(obj, Object::True);
        sharedObjectVector_.push_back(obj);
        return;
    } else if (val.isTrue()) {
        return;
    } else {
        sharedObjects_->set(obj, Object::False);
        if (obj.isPair()) {
            scanSharedObjects(obj.car());
            obj = obj.cdr();
            goto loop;
        } else if (obj.isVector()) {
            Vector* const v = obj.toVector();
            for (int i = 0; i < v->length(); i++) {
                scanSharedObjects(v->ref(i));
            }
        } else if (obj.isEqHashTable()) {
            EqHashTable* const ht = obj.toEqHashTable();
            Vector* const keys = ht->keys().toVector();
            const int length = keys->length();
            for (int i = 0; i < length; i++) {
                const Object key = keys->ref(i);
                MOSH_ASSERT(key.isSymbol());
                scanSharedObjects(key);
                scanSharedObjects(ht->ref(key, Object::False));
            }
        } else if (obj.isSimpleStruct()) {
            SimpleStruct* const record = obj.toSimpleStruct();
            scanSharedObjects(record->name());
            const int length = record->fieldCount();
            for (int i = 0; i < length; i++) {
                scanSharedObjects(record->ref(i));
            }
        }
    }
}

void FaslWriter::emitString(const ucs4string& string)
{
    emitU32(string.size());
    for (uint32_t i = 0; i < string.size(); i++) {
        emitU32(string[i]);
    }
}

void FaslWriter::emitShortAsciiString(const ucs4string& string)
{
    emitU8(string.size());
    for (uint32_t i = 0; i < string.size(); i++) {
        emitU8(string[i]);
    }
}

void FaslWriter::putList(Object obj)
{
    ObjectVector v;
    bool first = true;
    while (obj.isPair()) {
        if (!first && sharedObjects_->ref(obj, Object::False).isTrue()) {
            sharedObjects_->set(obj, Object::cons(Object::makeFixnum(uid_++), Object::Nil));
            break;
        }
        if (!first && sharedObjects_->ref(obj, Object::False).isFixnum()) {
            break;
        } else {
            v.push_back(obj.car());
            obj = obj.cdr();
        }
        first = false;
    }
    if (obj.isNil()) {
        emitU8(Fasl::TAG_PLIST);
        emitU32(v.size());
        for (ObjectVector::reverse_iterator it = v.rbegin(); it != v.rend(); ++it) {
            putDatum(*it);
        }
    } else {
        emitU8(Fasl::TAG_DLIST);
        emitU32(v.size());
        if (sharedObjects_->ref(obj, Object::False).isPair()) {
            Object p = sharedObjects_->ref(obj, Object::False);
            emitU8(Fasl::TAG_DEFINING_SHARED);
            emitU32(p.car().toFixnum());
        }
        putDatum(obj);
        if (sharedObjects_->ref(obj, Object::False).isPair()) {
            Object p = sharedObjects_->ref(obj, Object::False);
        sharedObjects_->set(obj, p.car());

        }

        for (ObjectVector::reverse_iterator it = v.rbegin(); it != v.rend(); ++it) {
            putDatum(*it);
        }
    }
}

void FaslWriter::putDatum(Object obj)
{
    const Object sharedState = sharedObjects_->ref(obj, Object::False);
    if (sharedState.isFixnum()) {
//        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        emitU8(Fasl::TAG_LOOKUP);
        emitU32(sharedState.toFixnum());
        return;
    } else if (sharedState.isTrue()) {
//        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        int uid = uid_++;
        emitU8(Fasl::TAG_DEFINING_SHARED);
        emitU32(uid);
        sharedObjects_->set(obj, Object::makeFixnum(uid));
    }

    if (obj.isNil()) {
        emitU8(Fasl::TAG_NIL);
        return;
    }
    if (obj.isTrue()) {
        emitU8(Fasl::TAG_T);
        return;
    }
    if (obj.isFalse()) {
        emitU8(Fasl::TAG_F);
        return;
    }
    if (obj.isEqHashTable()) {
        emitU8(Fasl::TAG_EQ_HASH_TABLE);
        EqHashTable* const ht = obj.toEqHashTable();
        Vector* const keys = ht->keys().toVector();
        const int length = keys->length();
        putDatum(Object::makeFixnum(length));
        for (int i = 0; i < length; i++) {
            putDatum(keys->ref(i));
            putDatum(ht->ref(keys->ref(i), Object::False));
        }
        return;
    }

    if (obj.isSimpleStruct()) {
        emitU8(Fasl::TAG_SIMPLE_STRUCT);
        SimpleStruct* const simpleStruct = obj.toSimpleStruct();
        putDatum(simpleStruct->name());
        const int length = simpleStruct->fieldCount();
        putDatum(Object::makeFixnum(length));
        for (int i = 0; i < length; i++) {
            // We don't support this pattern?
            MOSH_ASSERT(!simpleStruct->ref(i).isSimpleStruct());
            putDatum(simpleStruct->ref(i));
        }
        return;
    }
    if (obj.isRegexp()) {
        emitU8(Fasl::TAG_REGEXP);
        emitString(obj.toRegexp()->pattern());
        return;
    }
    if (obj.isFlonum()) {
        union {
            double   dvalue;
            uint64_t uvalue;
        } n;
        n.dvalue = obj.toFlonum()->value();
        emitU8(Fasl::TAG_FLONUM);
        emitU64(n.uvalue);
        return;
    }
    if (obj.isBignum()) {
        emitU8(Fasl::TAG_BIGNUM);
        Bignum* b = obj.toBignum();
        size_t size = 0;
        uint8_t* data = b->serialize(&size);
        MOSH_ASSERT(size < 65535);
        emitU16(size);
        for (size_t i = 0; i < size; i++) {
            emitU8(data[i]);
        }
        return;
    }
    if (obj.isFixnum()) {
        const int n = obj.toFixnum();
        if (n == 0) {
            emitU8(Fasl::TAG_FIXNUM_0);
        } else if (n == 1) {
            emitU8(Fasl::TAG_FIXNUM_1);
        } else if (n >= 2 && n <= 255) {
            emitU8(Fasl::TAG_SMALL_FIXNUM);
            emitU8(n);
        } else if (n >= 256 && n <= 65535) {
            emitU8(Fasl::TAG_MEDIUM_FIXNUM);
            emitU16(n);
        } else {
            emitU8(Fasl::TAG_FIXNUM);
            emitU32(obj.toFixnum());
        }
        return;
    }
    if (obj.isInstruction()) {
        emitU8(Fasl::TAG_INSTRUCTION);
        const int val = obj.toInstruction();
        if (0 <= val && val <= 255) {
            emitU8((uint8_t)val);
        } else {
            fprintf(stderr, "instruction out of range");
            exit(-1);
        }
        return;
    }
    if (obj.isCompilerInstruction()) {
        emitU8(Fasl::TAG_COMPILER_INSTRUCTION);
        emitU32(obj.toCompilerInstruction());
        return;
    }
    if (obj.isPair()) {
        putList(obj);
        return;
    }
    if (obj.isVector()) {
        Vector* const vector = obj.toVector();
        const int length = vector->length();
        emitU8(Fasl::TAG_VECTOR);
        emitU32(length);
        for (int i = 0; i < length; i++) {
            putDatum(vector->ref(i));
        }
        return;
    }
    if (obj.isByteVector()) {
        ByteVector* const bv = obj.toByteVector();
        const int length = bv->length();
        emitU8(Fasl::TAG_BVECTOR);
        emitU32(length);
        for (int i = 0; i < length; i++) {
            emitU8(bv->u8Ref(i));
        }
        return;
    }
    if (obj.isChar()) {
        const ucs4char ch = obj.toChar();
        emitU8(Fasl::TAG_CHAR);
        emitU32(ch);
        return;
    }
    if (obj.isRatnum()) {
        emitU8(Fasl::TAG_RATNUM);
        Ratnum* r = obj.toRatnum();
        putDatum(r->numerator());
        putDatum(r->denominator());
        return;
    }
    if (obj.isCompnum()) {
        emitU8(Fasl::TAG_COMPNUM);
        Compnum* c = obj.toCompnum();
        putDatum(c->real());
        putDatum(c->imag());
        return;
    }
    if (obj.isSymbol()) {
        Symbol* const symbol = obj.toSymbol();
        ucs4string text(symbol->c_str());
        if (Symbol::isInterned(obj)) {
            if (text.is_ascii() && text.size() <= 255) {
                emitU8(Fasl::TAG_SHORT_ASCII_SYMBOL);
                emitShortAsciiString(text);
            } else {
                emitU8(Fasl::TAG_SYMBOL);
                emitString(text);
            }
        } else {
            if (text.is_ascii() && text.size() <= 255) {
                emitU8(Fasl::TAG_SHORT_ASCII_UNINTERNED_SYMBOL);
                emitShortAsciiString(text);
            } else {
                emitU8(Fasl::TAG_UNINTERNED_SYMBOL);
                emitString(text);
            }
        }
        return;
    }
    if (obj.isString()) {
        String* const string = obj.toString();
        ucs4string text = string->data();
        if (text.is_ascii() && text.size() < 255) {
            emitU8(Fasl::TAG_SHORT_ASCII_STRING);
            emitShortAsciiString(text);
        } else {
            emitU8(Fasl::TAG_STRING);
            emitString(text);
        }
        return;
    }
    MOSH_ASSERT(false);
}

void FaslWriter::emitU8(uint8_t value)
{
    outputPort_->putU8(value);
}

void FaslWriter::emitU16(uint16_t value)
{
    outputPort_->putU8(value);
    outputPort_->putU8(value >> 8);
}

void FaslWriter::emitU32(uint32_t value)
{
    outputPort_->putU8(value);
    outputPort_->putU8(value >> 8);
    outputPort_->putU8(value >> 16);
    outputPort_->putU8(value >> 24);
}

void FaslWriter::emitU64(uint64_t value)
{
    emitU32(value);
    emitU32(value >> 32);
}

void FaslWriter::put(Object obj)
{
    scanSharedObjects(obj);
    putDatum(obj);
}

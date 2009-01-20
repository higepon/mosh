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
#include "Record.h"
#include "RecordTypeDescriptor.h"
#include "EqHashTable.h"
#include "Fasl.h"


using namespace scheme;

FaslReader::FaslReader(VM* theVM, BinaryInputPort* inputPort) : inputPort_(inputPort), theVM_(theVM)
{
}

Object FaslReader::get()
{
    getSymbolsAndStrings();
    return getDatum();
}

void FaslWriter::collectSymbolsAndStrings(Object obj)
{
loop:
    if (obj.isNil()) {
        return;
    }
    if (obj.isRecordTypeDescriptor()) {
        RecordTypeDescriptor* const rtd = obj.toRecordTypeDescriptor();
        const Object name = rtd->name();
        MOSH_ASSERT(name.isSymbol());
        collectSymbolsAndStrings(name);
        return;
    } else if (obj.isSymbol() || obj.isString()) {
        if (symbolsAndStringsTable_->containsP(obj)) {
            return;
        } else {
            symbolsAndStringsTable_->set(obj, Object::makeFixnum(symbolsAndStringsTable_->size()));
        }
        return;
    }

    if (obj.isEqHashTable()) {
        EqHashTable* const ht = obj.toEqHashTable();
        Vector* const keys = ht->keys().toVector();
        const int length = keys->length();
        for (int i = 0; i < length; i++) {
            const Object key = keys->ref(i);
            MOSH_ASSERT(key.isSymbol());
            collectSymbolsAndStrings(key);
            collectSymbolsAndStrings(ht->ref(key, Object::False));
        }
        return;
    }
    if (obj.isPair()) {
        collectSymbolsAndStrings(obj.car());
        obj = obj.cdr();
        goto loop;
    }
    if (obj.isVector()) {
        Vector* const vector = obj.toVector();
        const int length = vector->length();
        for (int i = 0; i < length; i++) {
            collectSymbolsAndStrings(vector->ref(i));
        }
        return;
    }
    if (obj.isRecord()) {
        Record* const record = obj.toRecord();
        collectSymbolsAndStrings(record->rtd());
        const int length = record->fieldsLength();
        for (int i = 0; i < length; i++) {
            collectSymbolsAndStrings(record->fieldAt(i));
        }
        return;
    }

    if (obj.isChar()                ||
        obj.isByteVector()          ||
        obj.isRegexp()              ||
        obj.isBoolean()             ||
        obj.isCompilerInstruction() ||
        obj.isInstruction()         ||
        obj.isFlonum()              ||
        obj.isFixnum()              ||
        obj.isBignum())
        {
        return;
    }
    LOG1("obj=~a\n", obj);
    throwIOError("not supported serialization");
}


void FaslReader::getSymbolsAndStrings()
{
    const int count = fetchU32();
    symbolsAndStringsArray_ = Object::makeObjectArray(count);
    for (int i = 0; i < count; i++) {
        uint8_t tag = fetchU8();
        uint32_t uid = fetchU32();
        uint32_t len = fetchU32();
        ucs4string text;
        text.reserve(64);
        if (tag == Fasl::TAG_ASCII_SYMBOL || tag == Fasl::TAG_ASCII_STRING) {
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU8();
            }
        } else {
            for (uint32_t i = 0; i < len; i++) {
                text += fetchU32();
            }
        }
        switch (tag) {
        case Fasl::TAG_SYMBOL:
        case Fasl::TAG_ASCII_SYMBOL:
            symbolsAndStringsArray_[uid] = Symbol::intern(text.strdup());
            break;
        case Fasl::TAG_STRING:
        case Fasl::TAG_ASCII_STRING:
            symbolsAndStringsArray_[uid] = text;
            break;
        default:
            MOSH_ASSERT(false);
        }
    }
}

FaslWriter::FaslWriter(BinaryOutputPort* outputPort) : symbolsAndStringsTable_(new EqHashTable),
                                                       outputPort_(outputPort)
{
}

void FaslWriter::emitString(const ucs4string& string)
{
    emitU32(string.size());
    for (uint32_t i = 0; i < string.size(); i++) {
        emitU32(string[i]);
    }
}

void FaslWriter::emitAsciiString(const ucs4string& string)
{
    emitU32(string.size());
    for (uint32_t i = 0; i < string.size(); i++) {
        printf("<%c>", string[i]);
        emitU8(string[i]);
    }
}

void FaslWriter::putList(Object obj)
{
    ObjectVector v;
    while (obj.isPair()) {
        v.push_back(obj.car());
        obj = obj.cdr();
    }
    if (obj.isNil()) {
        emitU8(Fasl::TAG_PLIST);
        emitU32(v.size());
    } else {
        emitU8(Fasl::TAG_DLIST);
        emitU32(v.size());
        putDatum(obj);
    }
    for (ObjectVector::reverse_iterator it = v.rbegin(); it != v.rend(); ++it) {
        putDatum(*it);
    }
}

void FaslWriter::putSymbolsAndStrings()
{
    const int size = symbolsAndStringsTable_->size();
    Object* objects = Object::makeObjectArray(size);
    Vector* const keys = symbolsAndStringsTable_->keys().toVector();
    for (int i = 0; i < size; i++) {
        const Object key = keys->ref(i);
        const Object value = symbolsAndStringsTable_->ref(key, Object::False);
        MOSH_ASSERT(!value.isFalse());
        MOSH_ASSERT(value.isFixnum());
        MOSH_ASSERT(value.toFixnum() < size);
        objects[value.toFixnum()] = key;
    }
    emitU32(size);
    for (int i = 0; i < size; i++) {
        const Object obj = objects[i];
        if (obj.isSymbol()) {
            Symbol* const symbol = obj.toSymbol();
            ucs4string text = symbol->c_str();
            if (text.is_ascii()) {
                emitU8(Fasl::TAG_ASCII_SYMBOL);
                emitU32(i);
                emitAsciiString(text);
            } else {
                emitU8(Fasl::TAG_SYMBOL);
                emitU32(i);
                emitString(text);
            }
        } else if (obj.isString()) {
            String* const string = obj.toString();
            ucs4string text = string->data();
            if (text.is_ascii()) {
                emitU8(Fasl::TAG_ASCII_STRING);
                emitU32(i);
                emitAsciiString(text);
            } else {
                emitU8(Fasl::TAG_STRING);
                emitU32(i);
                emitString(text);
            }
        } else {
            MOSH_ASSERT(false);
        }
    }
}

void FaslWriter::putDatum(Object obj)
{
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

    if (obj.isRecord()) {
        emitU8(Fasl::TAG_RECORD);
        Record* const record = obj.toRecord();
        putDatum(record->rtd());
        const int length = record->fieldsLength();
        putDatum(Object::makeFixnum(length));
        for (int i = 0; i < length; i++) {
            putDatum(record->fieldAt(i));
        }
        return;
    }
    if (obj.isRecordTypeDescriptor()) {
        RecordTypeDescriptor* const rtd = obj.toRecordTypeDescriptor();
        MOSH_ASSERT(rtd->parent().isFalse()); // parent not supported
        emitU8(Fasl::TAG_RTD);
        putDatum(rtd->name());
        return;
    }
    if (obj.isSymbol() || obj.isString()) {
        emitU8(Fasl::TAG_LOOKUP);
        const Object id = symbolsAndStringsTable_->ref(obj, Object::False);
        MOSH_ASSERT(!id.isFalse());
        emitU32(id.toFixnum());
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
        emitU64(obj.toBignum()->toS64());
        return;
    }
    if (obj.isFixnum()) {
        const int n = obj.toFixnum();
        if (n >= 0 && n <= 255) {
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
    collectSymbolsAndStrings(obj);
    putSymbolsAndStrings();
    putDatum(obj);
}

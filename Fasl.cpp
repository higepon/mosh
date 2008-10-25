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
#include "Fasl.h"

using namespace scheme;

FaslReader::FaslReader(BinaryInputPort* inputPort) :inputPort_(inputPort)
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
    if (obj.isSymbol() || obj.isString()) {
        if (symbolsAndStringsTable_->containsP(obj)) {
            return;
        } else {
            symbolsAndStringsTable_->set(obj, Object::makeInt(symbolsAndStringsTable_->size()));
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
    if (obj.isChar()                ||
        obj.isByteVector()          ||
        obj.isRegexp()              ||
        obj.isBoolean()             ||
        obj.isCompilerInstruction() ||
        obj.isInstruction()         ||
        obj.isInt()) {
        return;
    }
    VM_LOG1("~a", obj);
    MOSH_ASSERT(false);
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
        for (uint32_t i = 0; i < len; i++) {
            text += fetchU32();
        }
        switch (tag) {
        case Fasl::TAG_SYMBOL:
            symbolsAndStringsArray_[uid] = Symbol::intern(text.strdup());
            break;
        case Fasl::TAG_STRING:
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
        MOSH_ASSERT(value.isInt());
        MOSH_ASSERT(value.toInt() < size);
        objects[value.toInt()] = key;
    }
    emitU32(size);
    for (int i = 0; i < size; i++) {
        const Object obj = objects[i];
        if (obj.isSymbol()) {
            Symbol* const symbol = obj.toSymbol();
            emitU8(Fasl::TAG_SYMBOL);
            emitU32(i);
            emitString(symbol->c_str());
        } else if (obj.isString()) {
            String* const string = obj.toString();
            emitU8(Fasl::TAG_STRING);
            emitU32(i);
            emitString(string->data());
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
    if (obj.isSymbol() || obj.isString()) {
        emitU8(Fasl::TAG_LOOKUP);
        const Object id = symbolsAndStringsTable_->ref(obj, Object::False);
        MOSH_ASSERT(!id.isFalse());
        emitU32(id.toInt());
        return;
    }
    if (obj.isRegexp()) {
        emitU8(Fasl::TAG_REGEXP);
        emitString(obj.toRegexp()->pattern());
        return;
    }
    if (obj.isInt()) {
        emitU8(Fasl::TAG_FIXNUM);
        emitU32(obj.toInt());
        return;
    }
    if (obj.isInstruction()) {
        emitU8(Fasl::TAG_INSTRUCTION);
        emitU32(obj.toInstruction());
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

void FaslWriter::emitU32(uint32_t value)
{
    outputPort_->putU8(value);
    outputPort_->putU8(value >> 8);
    outputPort_->putU8(value >> 16);
    outputPort_->putU8(value >> 24);
}

void FaslWriter::put(Object obj)
{
    collectSymbolsAndStrings(obj);
    putSymbolsAndStrings();
    putDatum(obj);
}

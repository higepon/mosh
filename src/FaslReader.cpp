/*
 * FaslReader.cpp - FASL reaader.
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
 *  $Id: FaslReader.cpp 183 2008-07-04 06:19:28Z higepon $
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
#include "FaslReader.h"
#include "SharedReference.h"


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244) // convert from uint64_t to uint8_t
#endif

using namespace scheme;

FaslReader::FaslReader(VM* theVM, BinaryInputPort* inputPort) : inputPort_(inputPort), theVM_(theVM), sharedObjects_(new EqHashTable) 
{
}

Object FaslReader::get()
{
    Object obj = getDatum();
    if (isLinkNeeded_) {
        linkShared(obj, new EqHashTable);
    }
    return obj;
}

Object FaslReader::getShared(int index)
{
    Object obj = sharedObjects_->ref(Object::makeFixnum(index), Object::Ignore);
    MOSH_ASSERT(obj != Object::Ignore);
    return obj;
}

void FaslReader::linkShared(Object obj, EqHashTable* seen)
{
    if (seen->ref(obj, Object::False).isTrue()) {
        return;
    }
    seen->set(obj, Object::True);
    if (obj.isPair()) {
        if (obj.car().isSharedReference()) {
            int index = obj.car().toSharedReference()->index();
            obj.car() = getShared(index);
        } else {
            linkShared(obj.car(), seen);
        }
        if (obj.cdr().isSharedReference()) {
            int index = obj.cdr().toSharedReference()->index();
            obj.cdr() = getShared(index);
        } else {
            linkShared(obj.cdr(), seen);
        }
        return;
    }
    if (obj.isVector()) {
        Vector* v = obj.toVector();
        int n = v->length();
        for (int i = 0; i < n; i++) {
            if (v->ref(i).isSharedReference()) {
                v->set(i, getShared(v->ref(i).toSharedReference()->index()));
            } else {
                linkShared(v->ref(i), seen);
            }
        }
        return;
    }
    if (obj.isSimpleStruct()) {
        SimpleStruct* const record = obj.toSimpleStruct();
        const int length = record->fieldCount();
        for (int i = 0; i < length; i++) {
            Object o = record->ref(i);
            if (o.isSharedReference()) {
                record->set(i, getShared(o.toSharedReference()->index()));
            } else {
                linkShared(o, seen);
            }
        }
        return;
    }
}


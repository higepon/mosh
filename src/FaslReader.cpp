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
#include "Record.h"
#include "RecordTypeDescriptor.h"
#include "EqHashTable.h"
#include "SimpleStruct.h"
#include "FaslReader.h"


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244) // convert from uint64_t to uint8_t
#endif

using namespace scheme;

FaslReader::FaslReader(VM* theVM, BinaryInputPort* inputPort) : inputPort_(inputPort), theVM_(theVM)
{
}

Object FaslReader::get()
{
    getSymbolsAndStrings();
    return getDatum();
}


void FaslReader::getSymbolsAndStrings()
{
    const int tableSize = fetchU32();
    sharedObjects_ = Object::makeObjectArray(tableSize);
    for (int i = 0; i < tableSize; i++) {
        sharedObjects_[i] = Object::Ignore; // use Ignore for marking as not initialized
    }
    for (int i = tableSize - 1; i >= 0; i--) {
        sharedObjects_[i] = getDatum();
    }
}

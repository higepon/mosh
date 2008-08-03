/*
 * RecordConstructorDescriptor.cpp - R6RS record-constructor-descriptor
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
 *  $Id: RecordConstructorDescriptor.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "RecordConstructorDescriptor.h"
#include "RecordProcedures.h"
#include "VM.h"

using namespace scheme;

extern scheme::VM* theVM;

RecordConstructorDescriptor::RecordConstructorDescriptor(Object rtd,
                                                         Object parentRcd,
                                                         Object protocol)
  : rtd_(rtd),
    parentRcd_(parentRcd),
    protocol_(protocol)
{
}

RecordConstructorDescriptor::~RecordConstructorDescriptor()
{
}

Object RecordConstructorDescriptor::rtd() const
{
    return rtd_;
}

Object RecordConstructorDescriptor::constructor(RecordConstructorInternal* childConstructor)
{
    // protocol は自分のものを使う。再帰しないほうが良い。
    if (protocol_.isFalse()) {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        return Object::makeCallable(new RecordConstructorInternal(this, childConstructor, rtd_.toRecordTypeDescriptor()->fieldsLengthTotal()));
    } else if (parentRcd_.isFalse()) {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        return theVM->callClosure(protocol_, Object::makeCallable(new RecordConstructorInternal(this, childConstructor, rtd_.toRecordTypeDescriptor()->fieldsLength())));

        // ここはうまく親が見られていないね。x
    } else {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        return parentRcd_.toRecordConstructorDescriptor()->constructor(new RecordConstructorInternal(this, childConstructor, rtd_.toRecordTypeDescriptor()->fieldsLength()));
    }
//     const int fieldsLength = rtd_.toRecordTypeDescriptor()->fieldsLengthTotal();
//     if (protocol_.isFalse()) {
//         return Object::makeCallable(new DefaultRecordConstructor(this, fieldsLength));
//     } else {
//         return theVM->callClosure(protocol_, Object::makeCallable(new DefaultRecordConstructor(this, fieldsLength)));
//     }
}

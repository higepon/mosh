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
//     // set default protocol
//     if (protocol_.isFalse()) {
//         const int fieldsLength = rtd.toRecordTypeDescriptor()->fieldsLength();
//         protocol_ = Object::makeCallable(new DefaultProtocol(fieldsLength));
//     }
}

RecordConstructorDescriptor::~RecordConstructorDescriptor()
{
}

Object RecordConstructorDescriptor::rtd() const
{
    return rtd_;
}

Object RecordConstructorDescriptor::parentRcd() const
{
    return parentRcd_;
}

Object RecordConstructorDescriptor::protocol() const
{
    return protocol_;
}

Object RecordConstructorDescriptor::makeConstructor()
{
    if (protocol_.isFalse()) {
        return Object::makeCallable(new RecordConstructorInternal(this, NULL, rtd_.toRecordTypeDescriptor()->fieldsLengthTotal()));
    }

    // create internal constructor
    RecordConstructorDescriptor* rcd = this;
    RecordConstructorInternal* child = NULL;
    RecordTypeDescriptor* rtd = rcd->rtd().toRecordTypeDescriptor();
    for (;;) {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        child = new RecordConstructorInternal(this, child, rtd->fieldsLength());
        if (rtd->parent().isFalse()) {
            break;
        } else {
            rtd = rtd->parent().toRecordTypeDescriptor();
        }
    }
    const Object internalConstructor = Object::makeCallable(child);

    // collect protocols
    ObjectVector protocols;
    rcd = this;
    for (;;) {
        protocols.push_back(rcd->protocol());
        if (rcd->parentRcd().isFalse()) {
            break;
        } else {
            rcd = rcd->parentRcd().toRecordConstructorDescriptor();
        }
    }

    Object initalizer = internalConstructor;
    for (ObjectVector::reverse_iterator it = protocols.rbegin(); it != protocols.rend(); ++it) {
        if (!(*it).isFalse()) {
            initalizer = theVM->callClosure(*it, initalizer);
        }
    }
    return initalizer;

}
//     // protocol は自分のものを使う。再帰しないほうが良い。
//     if (protocol_.isFalse()) {
//         return Object::makeCallable(new RecordConstructorInternal(this, NULL, rtd_.toRecordTypeDescriptor()->fieldsLengthTotal()));
//     } else if (parentRcd_.isFalse()) {
//         return theVM->callClosure(protocol_, Object::makeCallable(new RecordConstructorInternal(this, NULL, rtd_.toRecordTypeDescriptor()->fieldsLength())));
//         // ここはうまく親が見られていないね。x
//     } else {
//         RecordConstructorInternal* child = NULL;
//         RecordConstructorDescriptor* rcd = this;
//         for (;;) {
//             if (rcd->protocol().isFalse()) {
//                 child = new RecordConstructorInternal(this, child, rcd->rtd().toRecordTypeDescriptor()->fieldsLength());
//             } else {

//             }
//             if (rcd->parentRcd().isFalse()) {
//                 return theVM->callClosure(protocol_, Object::makeCallable(child));
//             } else {
//                 rcd = rcd->parentRcd().toRecordConstructorDescriptor();
//             }
//         }
//         printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//         return parentRcd_.toRecordConstructorDescriptor()->constructor(new RecordConstructorInternal(this, childConstructor, rtd_.toRecordTypeDescriptor()->fieldsLength()));
//    }
//     const int fieldsLength = rtd_.toRecordTypeDescriptor()->fieldsLengthTotal();
//     if (protocol_.isFalse()) {
//         return Object::makeCallable(new DefaultRecordConstructor(this, fieldsLength));
//     } else {
//         return theVM->callClosure(protocol_, Object::makeCallable(new DefaultRecordConstructor(this, fieldsLength)));
//     }
//}

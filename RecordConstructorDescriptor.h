/*
 * RecordConstructorDescriptor.h - R6RS record-constructor-descriptor
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
 *  $Id: RecordConstructorDescriptor.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_RECORD_CONSTRUCTOR_DESCRIPTOR_
#define SCHEME_RECORD_CONSTRUCTOR_DESCRIPTOR_

#include "scheme.h"

namespace scheme {

class RecordInitializer;

class RecordConstructorDescriptor EXTEND_GC
{
public:
    RecordConstructorDescriptor(VM* theVM, Object rtd, Object parentRcd, Object protocol);
    ~RecordConstructorDescriptor();

    Object makeConstructor();
    Object rtd() const;
    Object parentRcd() const;
    Object protocol() const;

private:
    Object makeRecordInitializer();
    void collectProtocols(ObjectVector& protocols);
    VM* vm_;
    Object rtd_;
    Object parentRcd_;
    Object protocol_;
};

} // namespace scheme

#endif // SCHEME_RECORD_CONSTRUCTOR_DESCRIPTOR_

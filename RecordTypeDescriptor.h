/*
 * RecordTypeDescriptor.h - 
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
 *  $Id: RecordTypeDescriptor.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_RECORD_TYPE_DESCRIPTOR__
#define __SCHEME_RECORD_TYPE_DESCRIPTOR__

#include "scheme.h"

namespace scheme {

class RecordTypeDescriptor EXTEND_GC
{
public:
    RecordTypeDescriptor(Object name, Object parent, Object uid, Object isSealed, Object isOpaque, Object fields);
    ~RecordTypeDescriptor();

    int fieldsLength() const;
    int fieldsLengthTotal() const;
    int parentFieldsLengthTotal() const;
    Object name() const;
    bool isFieldMutable(int index) const;
    bool isA(const RecordTypeDescriptor* rtd) const;
    bool isOpaque() const;
    bool isSealed() const;
    bool isGenerative() const;
    Object parent() const;
    Object uid() const;
    Object fieldNames() const;

private:
    Object name_;
    Object parent_;
    Object uid_;
    const bool isSealed_;
    const bool isOpaque_;
    Object fields_;
    const int fieldsLength_;
};

}; // namespace scheme

#endif // __SCHEME_RECORD_TYPE_DESCRIPTOR__

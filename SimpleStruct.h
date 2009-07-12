/*
 * SimpleStruct.h -
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: OSCompatSocket.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_SIMPLE_STRUCT_
#define SCHEME_SIMPLE_STRUCT_

#include "scheme.h"
#include "SimpleStruct.h"

namespace scheme {

    // Back-end for psyntax
    class SimpleStruct EXTEND_GC
    {
    private:
        const Object name_;
        const int fieldCount_;
        Object* fields_;
    public:
        SimpleStruct(Object name, int fieldCount) : name_(name), fieldCount_(fieldCount)
        {
            MOSH_ASSERT(name_.isSymbol());
            MOSH_ASSERT(fieldCount_ >= 0);
            fields_ = Object::makeObjectArray(fieldCount_);
            for (int i = 0; i< fieldCount; i++) {
                fields_[i] = Object::Undef;
            }
        }
        // not virtual for size and performance
        ~SimpleStruct() {}

        Object name() const { return name_; }
        int fieldCount() const { return fieldCount_; }

        bool isValidIndex(int index) const
        {
            return 0 <= index && index < fieldCount_;
        }

        Object ref(int index)
        {
            MOSH_ASSERT(isValidIndex(index));
            return fields_[index];
        }

        void set(int index, Object value)
        {
            MOSH_ASSERT(isValidIndex(index));
            fields_[index] = value;
        }
    };
}; // namespace scheme

#endif // SCHEME_SIMPLE_STRUCT_

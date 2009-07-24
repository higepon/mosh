/*
 * RecordProcedures.h - R6RS Record procedures.
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
 *  $Id: RecordProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_RECORD_PROCEDURES_
#define SCHEME_RECORD_PROCEDURES_

#include "Callable.h"

namespace scheme {

    Object simpleStructPEx(VM* theVM, int argc, const Object* argv);
    Object simpleStructSetDEx(VM* theVM, int argc, const Object* argv);
    Object simpleStructRefEx(VM* theVM, int argc, const Object* argv);
    Object makeSimpleStructEx(VM* theVM, int argc, const Object* argv);
    Object simpleStructNameEx(VM* theVM, int argc, const Object* argv);

    Object makeRecordTypeDescriptorEx(VM* theVM, int argc, const Object* argv);
    Object makeRecordConstructorDescriptorEx(VM* theVM, int argc, const Object* argv);
    Object recordPredicateEx(VM* theVM, int argc, const Object* argv);
    Object recordConstructorEx(VM* theVM, int argc, const Object* argv);
    Object recordAccessorEx(VM* theVM, int argc, const Object* argv);
    Object recordMutatorEx(VM* theVM, int argc, const Object* argv);
    Object recordFieldMutablePEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeFieldNamesEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeOpaquePEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeSealedPEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeGenerativePEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeUidEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeParentEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeNameEx(VM* theVM, int argc, const Object* argv);
    Object recordRtdEx(VM* theVM, int argc, const Object* argv);
    Object recordPEx(VM* theVM, int argc, const Object* argv);
    Object recordTypeDescriptorPEx(VM* theVM, int argc, const Object* argv);

    class RecordInitializer : public Callable
    {
    public:
        RecordInitializer(RecordConstructorDescriptor* rcd, RecordInitializer* childConstructor, int fieldsLength);
        ~RecordInitializer();

        Object call(VM* vm, int argc, const Object* argv);
        void setParentFields(Object* parentFields, int parentFieldsLength);
        ucs4string toString() const;
    private:
        RecordConstructorDescriptor* rcd_;
        RecordInitializer* childConstructor_;
        const int fieldsLength_;
        Object* parentFields_;
        int parentFieldsLength_;

    };

    class RecordPrediate : public Callable
    {
    public:
        RecordPrediate(Object rtd);
        ~RecordPrediate();

        Object call(VM* vm, int argc, const Object* argv);
        ucs4string toString() const;
    private:
        const Object rtd_;
    };

    class RecordAccessor : public Callable
    {
    public:
        RecordAccessor(Object rtd, int index);
        ~RecordAccessor();

        Object call(VM* vm, int argc, const Object* argv);
        ucs4string toString() const;
    private:
        const Object rtd_;
        const int index_;
        ucs4string name_;
    };

    class RecordMutator : public Callable
    {
    public:
        RecordMutator(Object rtd, int index);
        ~RecordMutator();

        Object call(VM* vm, int argc, const Object* argv);
        ucs4string toString() const;
    private:
        const Object rtd_;
        const int index_;
    };

} // namespace scheme

#endif // SCHEME_RECORD_PROCEDURES_

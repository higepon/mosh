/*
 * RecordProcedures.cpp -
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
 *  $Id: RecordProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "RecordProcedures.h"
#include "VM.h"

using namespace scheme;

extern scheme::VM* theVM;

Object scheme::makeRecordTypeDescriptorEx(int argc, const Object* argv)
{
    DeclareProcedureName("make-record-type-descriptor");
    checkArgumentLength(6);
    argumentCheckSymbol(0, name);
    argumentCheckRecordTypeDescriptorOrFalse(1, parent);
    argumentCheckSymbolOrFalse(2, uid);
    argumentCheckBoolean(3, isSealed);
    argumentCheckBoolean(4, isOpaque);
    argumentCheckVector(5, fields);
    return Object::makeRecordTypeDescriptor(name,
                                            parent,
                                            uid,
                                            isSealed,
                                            isOpaque,
                                            fields);
}

Object scheme::makeRecordConstructorDescriptorEx(int argc, const Object* argv)
{
    DeclareProcedureName("make-record-constructor-descriptor");
    checkArgumentLength(3);
    argumentCheckRecordTypeDescriptor(0, rtd);
    argumentCheckRecordConstructorDescriptorOrFalse(1, parentRcd);
    argumentCheckClosureOrFalse(2, protocol);
    return Object::makeRecordConstructorDescriptor(rtd, parentRcd, protocol);
}

Object scheme::recordPredicateEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-prediate");
    checkArgumentLength(1);
    argumentCheckRecordTypeDescriptor(0, rtd);
    return Object::makeCallable(new RecordPrediate(rtd));
}

Object scheme::recordConstructorEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-constructor");
    checkArgumentLength(1);
    argumentAsRecordConstructorDescriptor(0, recordConstructorDescriptor);
    return recordConstructorDescriptor->makeConstructor();
}

Object scheme::recordAccessorEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-accessor");
    checkArgumentLength(2);
    argumentCheckRecordTypeDescriptor(0, rtd);
    argumentAsInt(1, index);
    if (index < 0 || index >= rtd.toRecordTypeDescriptor()->fieldsLength()) {
        VM_RAISE1("index out of range on ~s", Object::makeString(procedureName));
    }
    return Object::makeCallable(new RecordAccessor(rtd, index + rtd.toRecordTypeDescriptor()->parentFieldsLengthTotal()));
}

Object scheme::recordMutatorEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-mutator");
    checkArgumentLength(2);
    argumentCheckRecordTypeDescriptor(0, rtd);
    argumentAsInt(1, index);
    const RecordTypeDescriptor* const recordTypeDescriptor = rtd.toRecordTypeDescriptor();
    if (index < 0 || index >= recordTypeDescriptor->fieldsLength()) {
        VM_RAISE1("index out of range on ~s", Object::makeString(procedureName));
    }
    if (recordTypeDescriptor->isFieldMutable(index)) {
        return Object::makeCallable(new RecordMutator(rtd, index + rtd.toRecordTypeDescriptor()->parentFieldsLengthTotal()));
    } else {
        VM_RAISE1("~a required mutable field, but got immutable field index", Object::makeString(procedureName));
    }
    return Object::Undef;
}

RecordPrediate::RecordPrediate(Object rtd) : rtd_(rtd)
{
}

RecordPrediate::~RecordPrediate()
{
}

Object RecordPrediate::call(VM* vm, int argc, const Object* argv)
{
    DeclareProcedureName("record-predicate for record");
    checkArgumentLength(1);
    const Object object = argv[0];
    if (object.isRecord()) {
        Record* record = object.toRecord();
        return Object::makeBool(record->isA(rtd_.toRecordTypeDescriptor()));
    } else {
        return Object::False;
    }
}


RecordAccessor::RecordAccessor(Object rtd, int index) : rtd_(rtd), index_(index)
{
}

RecordAccessor::~RecordAccessor()
{
}

Object RecordAccessor::call(VM* vm, int argc, const Object* argv)
{
    DeclareProcedureName("record-accessor for record");
    checkArgumentLength(1);
    argumentAsRecord(0, record);
    const RecordTypeDescriptor* rtd = record->recordTypeDescriptor();

    if (rtd->isA(rtd_.toRecordTypeDescriptor())) {
        return record->fieldAt(index_);
    } else {
        VM_RAISE2("accessor for ~a can't be used as accessor for ~a",
                  rtd_.toRecordTypeDescriptor()->name(),
                  rtd->name());
    }
    return Object::Undef;
}

RecordMutator::RecordMutator(Object rtd, int index) : rtd_(rtd), index_(index)
{
}

RecordMutator::~RecordMutator()
{
}

Object RecordMutator::call(VM* vm, int argc, const Object* argv)
{
    DeclareProcedureName("record-mutator for record");
    checkArgumentLength(2);
    argumentAsRecord(0, record);
    const Object value = argv[1];
    const RecordTypeDescriptor* rtd = record->recordTypeDescriptor();
    if (rtd->isA(rtd_.toRecordTypeDescriptor())) {
        record->setFieldAt(index_, value);
    } else {
        VM_RAISE2("mutator for ~a can't be used as mutator for ~a",
                  rtd_.toRecordTypeDescriptor()->name(),
                  rtd->name());
    }
    return Object::Undef;
}


RecordInitializer::RecordInitializer(RecordConstructorDescriptor* rcd, RecordInitializer* childConstructor, int fieldsLength) :
    rcd_(rcd), childConstructor_(childConstructor), fieldsLength_(fieldsLength),
    parentFields_(NULL), parentFieldsLength_(0)
{
}

RecordInitializer::~RecordInitializer()
{
}

void RecordInitializer::setParentFields(Object* parentFields, int parentFieldsLength)
{
    parentFields_ = parentFields;
    parentFieldsLength_ = parentFieldsLength;
}

Object RecordInitializer::call(VM* vm, int argc, const Object* argv)
{
    DeclareProcedureName("record-constructor-internal");
    checkArgumentLength(fieldsLength_);
    const int fieldsLength = fieldsLength_ + parentFieldsLength_;

    // allocate fields and copy
    Object* fields = Object::makeObjectArray(fieldsLength);
    for (int i = 0; i < parentFieldsLength_; i++) {
        fields[i] = parentFields_[i];
    }
    for (int i = 0; i < argc; i++) {
        fields[i + parentFieldsLength_] = argv[i];
    }
    if (NULL == childConstructor_) {
        return Object::makeRecord(rcd_->rtd(), fields, fieldsLength);
    } else {
        childConstructor_->setParentFields(fields, fieldsLength);
        return Object::makeCallable(childConstructor_);
    }
}

Object scheme::recordFieldMutablePEx(int argc, const Object* argv)
{
}

Object scheme::recordTypeFieldNamesEx(int argc, const Object* argv)
{
}

Object scheme::recordTypeOpaquePEx(int argc, const Object* argv)
{
}

Object scheme::recordTypeSealedPEx(int argc, const Object* argv)
{
}

Object scheme::recordTypeGenerativePEx(int argc, const Object* argv)
{
}

Object scheme::recordTypeUidEx(int argc, const Object* argv)
{
}

Object scheme::recordTypeParentEx(int argc, const Object* argv)
{
}

Object scheme::recordTypeNameEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-type-name");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return recordTypeDescriptor->name();
}

Object scheme::recordPEx(int argc, const Object* argv)
{
    DeclareProcedureName("record?");
    checkArgumentLength(1);
    const Object object = argv[0];
    if (object.isRecord()) {
        const bool isOpaque = object.toRecord()->recordTypeDescriptor()->isOpaque();
        return Object::makeBool(!isOpaque);
    } else {
        return Object::False;
    }
}

Object scheme::recordRtdEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-rtd");
    checkArgumentLength(1);
    argumentAsRecord(0, record);
    if (record->recordTypeDescriptor()->isOpaque()) {
        // todo &assertion
        VM_RAISE1("~a: record is opaque", Object::makeString(procedureName));
    } else {
        return record->rtd();
    }
    return Object::Undef;
}


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

#define checkType(index, variableName, pred, required) \
    const Object variableName = argv[index]; \
    if (!variableName.pred()) { \
        VM_RAISE2("~a " #required " required, but got ~a\n", Object::makeString(procedureName), variableName); } \

#define checkTypeOrFalse(index, variableName, pred, required) \
    const Object variableName = argv[index]; \
    if (!variableName.pred() && !variableName.isFalse()) { \
        VM_RAISE2("~a " #required " or #f required, but got ~a\n", Object::makeString(procedureName), variableName); } \

#define argumentAsInt(index, variableName) checkType(index, variableName, isInt, number)
#define argumentAsRecord(index, variableName) checkType(index, variableName, isRecord, record)

#define argumentAsVector(index, variableName) checkType(index, variableName, isVector, vector)

#define argumentAsSymbol(index, variableName) checkType(index, variableName, isSymbol, symbol)
#define argumentAsSymbolOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isSymbol, symbol)

#define argumentAsBoolean(index, variableName) checkType(index, variableName, isBoolean, boolean)
#define argumentAsClosure(index, variableName) checkType(index, variableName, isClosure, closure)
#define argumentAsClosureOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isClosure, closure)
#define argumentAsRecordTypeDescriptorOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isRecordTypeDescriptor, record-type-descriptor)

#define argumentAsRecordTypeDescriptor(index, variableName) checkType(index, variableName, isRecordTypeDescriptor, record-type-descriptor)

#define argumentAsRecordConstructorDescriptor(index, variableName) checkType(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor)
#define argumentAsRecordConstructorDescriptorOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor)

#define DeclareProcedureName(name) const ucs4char* procedureName = UC(name);

#define checkArgLength(required)   \
    if (argc != required) { \
        VM_RAISE3("wrong number of argument for ~a required ~d, got ~d\n", Object::makeString(procedureName), Object::makeInt(required), Object::makeInt(argc)); \
    } \


Object scheme::makeRecordTypeDescriptorEx(int argc, const Object* argv)
{
    DeclareProcedureName("make-record-type-descriptor");
    checkArgLength(6);
    argumentAsSymbol(0, name);
    argumentAsRecordTypeDescriptorOrFalse(1, parent);
    argumentAsSymbolOrFalse(2, uid);
    argumentAsBoolean(3, isSealed);
    argumentAsBoolean(4, isOpaque);
    argumentAsVector(5, fields);
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
    checkArgLength(3);
    argumentAsRecordTypeDescriptor(0, rtd);
    argumentAsRecordConstructorDescriptorOrFalse(1, parentRcd);
    argumentAsClosureOrFalse(2, protocol);
    return Object::makeRecordConstructorDescriptor(rtd, parentRcd, protocol);
}

Object scheme::recordPredicateEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-prediate");
    checkArgLength(1);
    argumentAsRecordTypeDescriptor(0, rtd);
    return Object::makeCallable(new RecordPrediate(rtd));
}

Object scheme::recordConstructorEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-constructor");
    checkArgLength(1);
    argumentAsRecordConstructorDescriptor(0, constructorDescriptor);
    return constructorDescriptor.toRecordConstructorDescriptor()->makeConstructor();
}

Object scheme::recordAccessorEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-accessor");
    checkArgLength(2);
    argumentAsRecordTypeDescriptor(0, rtd);
    argumentAsInt(1, k);
    const int index = k.toInt();
    if (index < 0 || index >= rtd.toRecordTypeDescriptor()->fieldsLength()) {
        VM_RAISE1("index out of range on ~s", Object::makeString(procedureName));
    }
    return Object::makeCallable(new RecordAccessor(rtd, index + rtd.toRecordTypeDescriptor()->parentFieldsLengthTotal()));
}

Object scheme::recordMutatorEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-mutator");
    checkArgLength(2);
    argumentAsRecordTypeDescriptor(0, rtd);
    argumentAsInt(1, k);
    const int index = k.toInt();
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


DefaultRecordConstructor::DefaultRecordConstructor(const RecordConstructorDescriptor* rcd,
                                                   int fieldsLength) : rcd_(rcd), fieldsLength_(fieldsLength)
{
}

DefaultRecordConstructor::~DefaultRecordConstructor()
{
}

Object DefaultRecordConstructor::call(VM* vm, int argc, const Object* argv)
{
    DeclareProcedureName("default-record-constructor");
    checkArgLength(fieldsLength_);
    return Object::makeRecord(rcd_, argv, fieldsLength_);
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
    checkArgLength(1);
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
    checkArgLength(1);
    argumentAsRecord(0, record);
    const Object rtd = record.toRecord()->rtd();

    if (rtd_.toRecordTypeDescriptor()->isA(rtd_.toRecordTypeDescriptor())) {
        return record.toRecord()->fieldAt(index_);
    } else {
        VM_RAISE2("accessor for ~a can't be used as accessor for ~a",
                  rtd_.toRecordTypeDescriptor()->name(),
                  rtd.toRecordTypeDescriptor()->name());
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
    checkArgLength(2);
    argumentAsRecord(0, record);
    const Object value = argv[1];
    const Object rtd = record.toRecord()->rtd();
    if (rtd_.toRecordTypeDescriptor()->isA(rtd_.toRecordTypeDescriptor())) {
        record.toRecord()->setFieldAt(index_, value);
    } else {
        VM_RAISE2("mutator for ~a can't be used as mutator for ~a",
                  rtd_.toRecordTypeDescriptor()->name(),
                  rtd.toRecordTypeDescriptor()->name());
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
    checkArgLength(fieldsLength_);
    const int fieldsLength = fieldsLength_ + parentFieldsLength_;

    // parent が NULL のときに 非効率
    Object* fields = new(GC) Object[fieldsLength];
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    for (int i = 0; i < parentFieldsLength_; i++) {
        fields[i] = parentFields_[i];
    }

    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    for (int i = 0; i < argc; i++) {
        fields[i + parentFieldsLength_] = argv[i];
    }
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    if (NULL == childConstructor_) {
        return Object::makeRecord(rcd_, fields, fieldsLength);
    } else {
        childConstructor_->setParentFields(fields, fieldsLength);
        return Object::makeCallable(childConstructor_);
    }
}

// DefaultProtocol::DefaultProtocol(int fieldsLength) : fieldsLength_(fieldsLength)
// {
// }

// DefaultProtocol::~DefaultProtocol()
// {
// }

// Object RecordInitializer::call(VM* vm, int argc, const Object* argv)
// {
//     DeclareProcedureName("record-default-protocol");
//     checkArgLength(1);
//     t

// }


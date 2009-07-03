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

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Symbol.h"
#include "SString.h"
#include "Record.h"
#include "RecordConstructorDescriptor.h"
#include "RecordTypeDescriptor.h"
#include "VM.h"
#include "Closure.h"
#include "EqHashTable.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "RecordProcedures.h"
#include "ProcedureMacro.h"


using namespace scheme;

bool isSubTypeOfCondition(VM* theVM, Object rtd);
static void setValueWithPostfix(VM* theVM, Object id, const ucs4char* postfix, Object values);

// return &conditon-rtd
Object getConditionRtd(VM* theVM)
{
    static Object conditionRtd = Object::False;
    if (conditionRtd.isFalse()) {
        conditionRtd = theVM->getTopLevelGlobalValueOrFalse(Symbol::intern(UC("&condition-rtd")));
    }
    return conditionRtd;
}

// subtype of &condition ?
bool isSubTypeOfCondition(VM* theVM, Object rtd)
{
    const Object conditionRtd = getConditionRtd(theVM);
    if (conditionRtd.isFalse()) {
        return false;
    }
    return rtd.toRecordTypeDescriptor()->isA(conditionRtd.toRecordTypeDescriptor());
}

void setValueWithPostfix(VM* theVM, Object id, const ucs4char* postfix, Object value)
{
    MOSH_ASSERT(id.isSymbol());
    ucs4string name = id.toSymbol()->c_str();
    name += postfix;
    theVM->setValueString(name.strdup(), value);
}

Object scheme::makeRecordTypeDescriptorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-record-type-descriptor");
    checkArgumentLength(6);
    argumentCheckSymbol(0, name);
    argumentCheckRecordTypeDescriptorOrFalse(1, parent);
    argumentCheckSymbolOrFalse(2, uid);
    argumentCheckBoolean(3, isSealed);
    argumentCheckBoolean(4, isOpaque);
    argumentCheckVector(5, fields);
    Object rtd;
    // check sealed?
    if (parent.isRecordTypeDescriptor()) {
        RecordTypeDescriptor* const rtd = parent.toRecordTypeDescriptor();
        if (rtd->isSealed()) {
            callAssertionViolationAfter(theVM, procedureName, "cannot extend sealed parent", L2(name, parent));
            return Object::Undef;
        }
    }

    // nongenerative
    if (uid.isFalse()) {
        rtd = Object::makeRecordTypeDescriptor(name,
                                               parent,
                                               uid,
                                               isSealed,
                                               isOpaque,
                                               fields);
    // generative
    } else {
        const Object found = theVM->findGenerativeRtd(uid);
        if (found.isFalse()) {
            rtd = Object::makeRecordTypeDescriptor(name,
                                                   parent,
                                                   uid,
                                                   isSealed,
                                                   isOpaque,
                                                   fields);
            theVM->addGenerativeRtd(uid, rtd);
        } else {
            return found;
        }
    }
    // psyntax requires &xxx-rtd global defined.
    if (name == Symbol::intern(UC("&condition"))) {
        theVM->setValueString(UC("&condition-rtd"), rtd);
    } else if (isSubTypeOfCondition(theVM, rtd)) {
        setValueWithPostfix(theVM, name, UC("-rtd"), rtd);
    } else {
        // for Fasl
        setValueWithPostfix(theVM, name, UC("-rtd$"), rtd);
    }
    return rtd;
}

Object scheme::makeRecordConstructorDescriptorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-record-constructor-descriptor");
    checkArgumentLength(3);
    argumentCheckRecordTypeDescriptor(0, rtd);
    argumentCheckRecordConstructorDescriptorOrFalse(1, parentRcd);
    argumentCheckClosureOrFalse(2, protocol);

    const Object rcd =  Object::makeRecordConstructorDescriptor(theVM, rtd, parentRcd, protocol);

    // psyntax requires &xxx-rcd global defined.
    if (isSubTypeOfCondition(theVM, rtd)) {
        setValueWithPostfix(theVM, rtd.toRecordTypeDescriptor()->name(), UC("-rcd"), rcd);
    }
    return rcd;
}

Object scheme::recordPredicateEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-prediate");
    checkArgumentLength(1);
    argumentCheckRecordTypeDescriptor(0, rtd);
    return Object::makeCallable(new RecordPrediate(rtd));
}

Object scheme::recordConstructorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-constructor");
    checkArgumentLength(1);
    argumentAsRecordConstructorDescriptor(0, recordConstructorDescriptor);
    return recordConstructorDescriptor->makeConstructor();
}

Object scheme::recordAccessorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-accessor");
    checkArgumentLength(2);
    argumentCheckRecordTypeDescriptor(0, rtd);
    argumentAsFixnum(1, index);
    if (index < 0 || index >= rtd.toRecordTypeDescriptor()->fieldsLength()) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(Object::makeFixnum(index)));
        return Object::Undef;
    }
    return Object::makeCallable(new RecordAccessor(rtd, index + rtd.toRecordTypeDescriptor()->parentFieldsLengthTotal()));
}

Object scheme::recordMutatorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-mutator");
    checkArgumentLength(2);
    argumentCheckRecordTypeDescriptor(0, rtd);
    argumentAsFixnum(1, index);
    const RecordTypeDescriptor* const recordTypeDescriptor = rtd.toRecordTypeDescriptor();
    if (index < 0 || index >= recordTypeDescriptor->fieldsLength()) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(Object::makeFixnum(index)));
        return Object::Undef;
    }
    if (recordTypeDescriptor->isFieldMutable(index)) {
        return Object::makeCallable(new RecordMutator(rtd, index + rtd.toRecordTypeDescriptor()->parentFieldsLengthTotal()));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "required mutable field, but got immutable field index", L1(Object::makeFixnum(index)));
        return Object::Undef;
    }
    return Object::Undef;
}

RecordPrediate::RecordPrediate(Object rtd) : rtd_(rtd)
{
}

RecordPrediate::~RecordPrediate()
{
}

Object RecordPrediate::call(VM* theVM, int argc, const Object* argv)
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

const ucs4char* RecordAccessor::name() const
{
    Object name = format(NULL, UC("Record ~a accessor ~d"), Pair::list2(rtd_.toRecordTypeDescriptor()->name(), Object::makeFixnum(index_)));
    return name.toString()->data().c_str();
}

// N.B.
// For kindly understandable error message, we don't use MACROS defined on ProcedureMacro.h.
// We should take care of performance, error message is dynamically genarated only when error occurs.
Object RecordAccessor::call(VM* theVM, int argc, const Object* argv)
{
    if (argc != 1) {
        callWrongNumberOfArgumentsViolationAfter(theVM, name(), 1, argc);
        return Object::Undef;
    }

    if (!argv[0].isRecord()) {
        callWrongTypeOfArgumentViolationAfter(theVM, name(), "record", argv[0]);
        return Object::Undef;
    }
    Record* record = argv[0].toRecord();

    const RecordTypeDescriptor* rtd = record->recordTypeDescriptor();

    if (rtd->isA(rtd_.toRecordTypeDescriptor())) {
        return record->fieldAt(index_);
    } else {
        callAssertionViolationAfter(theVM,
                                    name(),
                                    "invalid accessor for record",
                                    L2(rtd_.toRecordTypeDescriptor()->name(), rtd->name()));
        return Object::Undef;
    }
}

RecordMutator::RecordMutator(Object rtd, int index) : rtd_(rtd), index_(index)
{
}

RecordMutator::~RecordMutator()
{
}



Object RecordMutator::call(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-mutator for record");
    checkArgumentLength(2);
    argumentAsRecord(0, record);
    const Object value = argv[1];
    const RecordTypeDescriptor* rtd = record->recordTypeDescriptor();
    if (rtd->isA(rtd_.toRecordTypeDescriptor())) {
        record->setFieldAt(index_, value);
        return Object::Undef;
    } else {
        callAssertionViolationAfter(theVM,
                                    procedureName,
                                    "invalid mutator for record",
                                    L2(rtd_.toRecordTypeDescriptor()->name(), rtd->name()));
        return Object::Undef;
    }
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

Object RecordInitializer::call(VM* theVM, int argc, const Object* argv)
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

Object scheme::recordFieldMutablePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-field-mutable?");
    checkArgumentLength(2);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    argumentAsFixnum(1, index);
    return Object::makeBool(recordTypeDescriptor->isFieldMutable(index));
}

Object scheme::recordTypeFieldNamesEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-field-names");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return recordTypeDescriptor->fieldNames();

}

Object scheme::recordTypeOpaquePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-opaque?");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return Object::makeBool(recordTypeDescriptor->isOpaque());
}

Object scheme::recordTypeSealedPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-sealed?");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return Object::makeBool(recordTypeDescriptor->isSealed());
}

Object scheme::recordTypeGenerativePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-generative?");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return Object::makeBool(recordTypeDescriptor->isGenerative());

}

Object scheme::recordTypeUidEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-ui");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return recordTypeDescriptor->uid();
}

Object scheme::recordTypeParentEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-parent");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return recordTypeDescriptor->parent();
}

Object scheme::recordTypeNameEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-name");
    checkArgumentLength(1);
    argumentAsRecordTypeDescriptor(0, recordTypeDescriptor);
    return recordTypeDescriptor->name();
}

Object scheme::recordPEx(VM* theVM, int argc, const Object* argv)
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

Object scheme::recordRtdEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-rtd");
    checkArgumentLength(1);
    argumentAsRecord(0, record);
    if (record->recordTypeDescriptor()->isOpaque()) {
        callAssertionViolationAfter(theVM, procedureName, "record is opaque", L1(argv[0]));
        return Object::Undef;
    } else {
        return record->rtd();
    }
}

Object scheme::recordTypeDescriptorPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("record-type-descriptor?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isRecordTypeDescriptor());
}

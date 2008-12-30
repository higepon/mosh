/*
 * ConditionProcedures.cpp - 
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
 *  $Id: ConditionProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Vector.h"
#include "SString.h"
#include "ConditionProcedures.h"
#include "ProcedureMacro.h"
#include "Record.h"
#include "CompoundCondition.h"
#include "TextualOutputPort.h"

using namespace scheme;

extern bool isSubTypeOfCondition(VM* theVM, Object rtd);

Object scheme::conditionAccessorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition-accessor");
    checkArgumentLength(2);
    argumentCheckRecordTypeDescriptor(0, rtd);
    argumentCheckProcedure(1, proc);
    return Object::makeCallable(new ConditionAccessor(rtd, proc));
}

Object scheme::conditionPredicateEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition-prediate");
    checkArgumentLength(1);
    argumentCheckRecordTypeDescriptor(0, rtd);
    return Object::makeCallable(new ConditionPredicate(rtd));
}

Object scheme::conditionPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition?");
    checkArgumentLength(1);
    const Object object = argv[0];
    if (object.isRecord()) {
        const Object rtd = object.toRecord()->rtd();
        return Object::makeBool(isSubTypeOfCondition(theVM, rtd));
    } else if (object.isCompoundCondition()) {
        return Object::True;
    } else {
        return Object::False;
    }
}

Object scheme::simpleConditionsEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("simple-conditions");
    checkArgumentLength(1);
    const Object condition = argv[0];
    if (condition.isCompoundCondition()) {
        return condition.toCompoundCondition()->conditionsList();
    } else {
        // simple condition
        return Pair::list1(condition);
    }
}

Object scheme::conditionEx(VM* theVM, int argc, const Object* argv)
{
    return Object::makeCompoundCondition(argc, argv);
}

ConditionPredicate::ConditionPredicate(Object rtd) : rtd_(rtd)
{
}

ConditionPredicate::~ConditionPredicate()
{
}

Object ConditionPredicate::call(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition-predicate for condition");
    checkArgumentLength(1);

    argumentCheckRecordOrCompoundConditon(0, object);

    if (object.isRecord()) {
        Record* record = object.toRecord();
        return Object::makeBool(record->isA(rtd_.toRecordTypeDescriptor()));
    } else {
        const ObjectVector conditions = object.toCompoundCondition()->conditions();
        for (ObjectVector::const_iterator it = conditions.begin(); it != conditions.end(); ++it) {
            const Object condition = *it;
            if (condition.isRecord() && condition.toRecord()->isA(rtd_.toRecordTypeDescriptor())) {
                return Object::True;
            }
        }
        return Object::False;
    }
}

ConditionAccessor::ConditionAccessor(Object rtd, Object proc) : rtd_(rtd), proc_(proc)
{
}

ConditionAccessor::~ConditionAccessor()
{
}

Object ConditionAccessor::call(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition-accessor for condition");
    checkArgumentLength(1);

    argumentCheckRecordOrCompoundConditon(0, object);

    if (object.isRecord()) {
        return theVM->callClosure1(proc_, object);
    } else { // CompoundCondition
        const ObjectVector conditions = object.toCompoundCondition()->conditions();
        for (ObjectVector::const_iterator it = conditions.begin(); it != conditions.end(); ++it) {
            const Object condition = *it;
            if (condition.isRecord() && condition.toRecord()->isA(rtd_.toRecordTypeDescriptor())) {
                return theVM->callClosure1(proc_, condition);
            }
        }
        callAssertionViolationAfter(theVM, procedureName, "invalid condition", L1(object));
        return Object::Undef;
    }
}

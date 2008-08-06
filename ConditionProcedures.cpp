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

#include "ConditionProcedures.h"
#include "VM.h"

using namespace scheme;

extern scheme::VM* theVM;

Object scheme::conditionAccessorEx(int argc, const Object* argv)
{
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
}

Object scheme::conditionPredicateEx(int argc, const Object* argv)
{
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
}

Object scheme::conditionPEx(int argc, const Object* argv)
{
    // todo
    return Object::True;
}

Object scheme::simpleConditionsEx(int argc, const Object* argv)
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

Object scheme::conditionEx(int argc, const Object* argv)
{
    DeclareProcedureName("condition");
    return Object::makeCompoundCondition(argc, argv);
}

ConditionPredicate::ConditionPredicate(Object rtd) : rtd_(rtd)
{
}

ConditionPredicate::~ConditionPredicate()
{
}

Object ConditionPredicate::call(VM* vm, int argc, const Object* argv)
{
    DeclareProcedureName("condition-predicate for condition");
    checkArgumentLength(1);
    const Object object = argv[0];
    if (object.isRecord()) {
        Record* record = object.toRecord();
        return Object::makeBool(record->isA(rtd_.toRecordTypeDescriptor()));
    } else if (object.isCompoundCondition()) {
        const ObjectVector conditions = object.toCompoundCondition()->conditions();
        for (ObjectVector::const_iterator it = conditions.begin(); it != conditions.end(); ++it) {
            const Object condition = *it;
            if (condition.isRecord() && condition.toRecord()->isA(rtd_.toRecordTypeDescriptor())) {
                return Object::True;
            }
        }
        return Object::False;
    } else {
        return Object::False;
    }
}

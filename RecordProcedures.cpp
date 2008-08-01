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
        VM_RAISE2("wrong number of argument for ~a required " #required ", got ~d\n", Object::makeString(procedureName), Object::makeInt(argc)); \
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
    return Symbol::intern(UC("record-predicatehoge"));
}

Object scheme::recordConstructorEx(int argc, const Object* argv)
{
    DeclareProcedureName("record-constructor");
    checkArgLength(1);
    argumentAsRecordConstructorDescriptor(0, constructorDescriptor);
    return constructorDescriptor.toRecordConstructorDescriptor()->constructor();
}

Object scheme::recordAccessorEx(int argc, const Object* argv)
{
    return Symbol::intern(UC("record-accessorhoge"));
}

Object scheme::recordMutatorEx(int argc, const Object* argv)
{
    return Symbol::intern(UC("record-mutatorhoge"));
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
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    return Object::makeRecord(rcd_, argv, fieldsLength_);
}

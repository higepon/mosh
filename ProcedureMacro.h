/*
 * ProcedureMacro.h - 
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
 *  $Id: ProcedureMacro.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_PROCEDURE_MACRO__
#define __SCHEME_PROCEDURE_MACRO__

#include "scheme.h"

#define checkType(index, variableName, pred, required) \
    const Object variableName = argv[index]; \
    if (!variableName.pred()) { \
        callWrongTypeOfArgumentViolationAfter(procedureName, #required, variableName); \
        return Object::Undef; \
    } \

#define castArgument(index, variableName, pred, required, type, castFunction)    \
    const Object obj ## variableName = argv[index]; \
    if (!obj ## variableName.pred()) { \
        callWrongTypeOfArgumentViolationAfter(procedureName, #required, obj ## variableName); \
        return Object::Undef; \
    } \
    type variableName = obj ## variableName.castFunction();


#define checkTypeOrFalse(index, variableName, pred, required) \
    const Object variableName = argv[index]; \
    if (!variableName.pred() && !variableName.isFalse()) { \
        callWrongTypeOfArgumentViolationAfter(procedureName, #required " or #f", variableName); \
        return Object::Undef; \
    } \

#define argumentAsInt(index, variableName) castArgument(index, variableName, isInt, number, int, toInt)
#define argumentCheckInt(index, variableName) checkType(index, variableName, isInt, number)
#define argumentAsRecord(index, variableName) castArgument(index, variableName, isRecord, record, Record*, toRecord)
#define argumentCheckRecord(index, variableName) checkType(index, variableName, isRecord, record)

#define argumentCheckProcedure(index, variableName) checkType(index, variableName, isProcedure, procedure)

#define argumentCheckVector(index, variableName) checkType(index, variableName, isVector, vector)

#define argumentCheckString(index, variableName) checkType(index, variableName, isString, string)
#define argumentCheckSymbol(index, variableName) checkType(index, variableName, isSymbol, symbol)
#define argumentCheckSymbolOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isSymbol, symbol)

#define argumentCheckBoolean(index, variableName) checkType(index, variableName, isBoolean, boolean)
#define argumentCheckClosure(index, variableName) checkType(index, variableName, isClosure, closure)
#define argumentCheckClosureOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isClosure, closure)
#define argumentCheckRecordTypeDescriptorOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isRecordTypeDescriptor, record-type-descriptor)

#define argumentCheckRecordTypeDescriptor(index, variableName) checkType(index, variableName, isRecordTypeDescriptor, record-type-descriptor)
#define argumentAsRecordTypeDescriptor(index, variableName) castArgument(index, variableName, isRecordTypeDescriptor, record-type-descriptor, RecordTypeDescriptor*, toRecordTypeDescriptor)

#define argumentAsRecordConstructorDescriptor(index, variableName) castArgument(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor, RecordConstructorDescriptor*, toRecordConstructorDescriptor)
#define argumentCheckRecordConstructorDescriptor(index, variableName) checkType(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor)
#define argumentCheckRecordConstructorDescriptorOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor)


#define DeclareProcedureName(name) const ucs4char* procedureName = UC(name);

#define checkArgumentLength(required)   \
    if (argc != required) { \
        callWrongNumberOfArgumentsViolationAfter(procedureName, required, argc); \
        return Object::Undef;\
    } \

#define checkArgumentLengthBetween(start, end)             \
    if (argc < start || argc > end) { \
        callWrongNumberOfArgumentsBetweenViolationAfter(procedureName, start, end, argc); \
        return Object::Undef;\
    } \


#endif // __SCHEME_PROCEDURE_MACRO__

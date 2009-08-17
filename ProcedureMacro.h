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

#ifndef SCHEME_PROCEDURE_MACRO_
#define SCHEME_PROCEDURE_MACRO_

#include "scheme.h"
#include "ErrorProcedures.h"
#include "StringProcedures.h"
#include "VM.h"


#define checkPortIsOpen(port, obj) \
    if (port->isClosed()) { \
        return callIOPortErrorAfter(theVM, obj, procedureName, "port is closed"); \
    }


// N.B For BinaryInputOutputPort Class, we use multiple inheritance.
// It is dangerous to reinterpret_cast<BinaryInput*>(theInstance).
// So we use special versio of argumentAsBinaryOutputPort and argumentAsBinaryInputPort.

#define argumentAsPort(index, variableName) \
    const Object obj ## variableName = argv[index];     \
    Port* variableName;                                  \
    if (obj ## variableName.isBinaryOutputPort()) { \
        variableName = obj ## variableName.toBinaryOutputPort(); \
    } else if (obj ## variableName.isBinaryInputPort()) {   \
        variableName = obj ## variableName.toBinaryInputPort(); \
    } else if (obj ## variableName.isTextualInputPort()) {   \
        variableName = obj ## variableName.toTextualInputPort(); \
    } else if (obj ## variableName.isTextualOutputPort()) {   \
        variableName = obj ## variableName.toTextualOutputPort(); \
    } else if (obj ## variableName.isTextualInputOutputPort()) { \
        variableName = obj ## variableName.toTextualInputOutputPort(); \
    } else if (obj ## variableName.isBinaryInputOutputPort()) { \
        variableName = obj ## variableName.toBinaryInputOutputPort(); \
    } else { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "port", obj ## variableName); \
        return Object::Undef; \
    }


#define argumentAsBinaryOutputPort(index, variableName) \
    const Object obj ## variableName = argv[index];     \
    BinaryOutputPort* variableName;                                  \
    if (obj ## variableName.isBinaryOutputPort()) { \
        variableName = obj ## variableName.toBinaryOutputPort(); \
    } else if (obj ## variableName.isBinaryInputOutputPort()) { \
        variableName = obj ## variableName.toBinaryInputOutputPort(); \
    } else { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "binary-output-port", obj ## variableName); \
        return Object::Undef; \
    }

#define argumentAsBinaryInputPort(index, variableName) \
    const Object obj ## variableName = argv[index];     \
    BinaryInputPort* variableName;                                  \
    if (obj ## variableName.isBinaryInputPort()) { \
        variableName = obj ## variableName.toBinaryInputPort(); \
    } else if (obj ## variableName.isBinaryInputOutputPort()) { \
        variableName = obj ## variableName.toBinaryInputOutputPort(); \
    } else { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "binary-input-port", obj ## variableName); \
        return Object::Undef; \
    }

#define argumentAsTextualOutputPort(index, variableName) \
    const Object obj ## variableName = argv[index];     \
    TextualOutputPort* variableName;                                  \
    if (obj ## variableName.isTextualOutputPort()) { \
        variableName = obj ## variableName.toTextualOutputPort(); \
    } else if (obj ## variableName.isTextualInputOutputPort()) { \
        variableName = obj ## variableName.toTextualInputOutputPort(); \
    } else { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "textual-output-port", obj ## variableName); \
        return Object::Undef; \
    }

#define argumentAsTextualInputPort(index, variableName) \
    const Object obj ## variableName = argv[index];     \
    TextualInputPort* variableName;                                  \
    if (obj ## variableName.isTextualInputPort()) { \
        variableName = obj ## variableName.toTextualInputPort(); \
    } else if (obj ## variableName.isTextualInputOutputPort()) { \
        variableName = obj ## variableName.toTextualInputOutputPort(); \
    } else { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "textual-input-port", obj ## variableName); \
        return Object::Undef; \
    }



#define checkType(index, variableName, pred, required) \
    const Object variableName = argv[index]; \
    if (!variableName.pred()) { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, #required, variableName); \
        return Object::Undef; \
    } \

#define castArgument(index, variableName, pred, required, type, castFunction)    \
    const Object obj ## variableName = argv[index]; \
    if (!obj ## variableName.pred()) { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, #required, obj ## variableName); \
        return Object::Undef; \
    } \
    type variableName = obj ## variableName.castFunction();


#define checkTypeOrFalse(index, variableName, pred, required) \
    const Object variableName = argv[index]; \
    if (!variableName.pred() && !variableName.isFalse()) { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, #required " or #f", variableName); \
        return Object::Undef; \
    } \

#define checkTypeOr(index, variableName, pred1, pred2, required1, required2)  \
    const Object variableName = argv[index]; \
    if (!variableName.pred1() && !variableName.pred2()) { \
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, #required1 " or " #required2, variableName); \
        return Object::Undef; \
    } \

#define argumentCheckList(index, variableName) checkType(index, variableName, isList, list)
#define argumentAsSymbol(index, variableName) castArgument(index, variableName, isSymbol, symbol, Symbol*, toSymbol)
#define argumentAsVM(index, variableName) castArgument(index, variableName, isVM, vm, VM*, toVM)
#define argumentAsMutex(index, variableName) castArgument(index, variableName, isMutex, mutex, Mutex*, toMutex)
#define argumentAsConditionVariable(index, variableName) castArgument(index, variableName, isConditionVariable, condition-variable, ConditionVariable*, toConditionVariable)
#define argumentAsVector(index, variableName) castArgument(index, variableName, isVector, vector, Vector*, toVector)
#define argumentAsCodeBuilder(index, variableName) castArgument(index, variableName, isCodeBuilder, code-builder, CodeBuilder*, toCodeBuilder)


#define argumentAsPointer(index, variableName) castArgument(index, variableName, isPointer, pointer, Pointer*, toPointer)
#define argumentAsSocket(index, variableName) castArgument(index, variableName, isSocket, socket, Socket*, toSocket)
#define argumentAsSimpleStruct(index, variableName) castArgument(index, variableName, isSimpleStruct, simple-struct, SimpleStruct*, toSimpleStruct)
#define argumentAsFlonum(index, variableName) castArgument(index, variableName, isFlonum, flonum, Flonum*, toFlonum)
#define argumentAsFixnum(index, variableName) castArgument(index, variableName, isFixnum, fixnum, int, toFixnum)
// #define argumentAsPositiveFixnum(index, variableName) \
//     argumentAsFixnum(index, variableName);            \
//     if (variableName < 0) {                                             \
//         callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "positive integer required", variableName); \
//         return Object::Undef;                                           \
//     }


#define argumentAsOctet(index, variableName) castArgument(index, variableName, isOctet, octet, uint8_t, toFixnum)
#define argumentAsCompnum(index, variableName) castArgument(index, variableName, isCompnum, Complex number, Compnum*, toCompnum)
#define argumentAsHashTable(index, variableName) castArgument(index, variableName, isHashTable, hashtable, HashTable*, toHashTable)
#define argumentCheckChar(index, variableName) checkType(index, variableName, isChar, char)
#define argumentCheckFixnum(index, variableName) checkType(index, variableName, isFixnum, fixnum)
#define argumentCheckFlonum(index, variableName) checkType(index, variableName, isFlonum, flonum)
#define argumentCheckExactInteger(index, variableName) checkType(index, variableName, isExactInteger, exact integer)
#define argumentCheckIntegerValued(index, variableName) checkType(index, variableName, isIntegerValued, integer)
#define argumentCheckNumber(index, variableName) checkType(index, variableName, isNumber, number)
#define argumentCheckRational(index, variableName) checkType(index, variableName, isRational, rational number)
#define argumentCheckReal(index, variableName) checkType(index, variableName, isReal, real)
#define argumentAsRecord(index, variableName) castArgument(index, variableName, isRecord, record, Record*, toRecord)
#define argumentAsClosure(index, variableName) castArgument(index, variableName, isClosure, closure, Closure*, toClosure)
#define argumentCheckRecord(index, variableName) checkType(index, variableName, isRecord, record)

#define argumentCheckProcedure(index, variableName) checkType(index, variableName, isProcedure, procedure)

#define argumentCheckVector(index, variableName) checkType(index, variableName, isVector, vector)

#define argumentCheckString(index, variableName) checkType(index, variableName, isString, string)
#define argumentCheckSymbol(index, variableName) checkType(index, variableName, isSymbol, symbol)
#define argumentCheckSymbolOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isSymbol, symbol)
#define argumentCheckStringOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isString, string)
#define argumentCheckRecordOrCompoundConditon(index, variableName) checkTypeOr(index, variableName, isRecord, isCompoundCondition, record, compound-condition)

#define argumentCheckBoolean(index, variableName) checkType(index, variableName, isBoolean, boolean)
#define argumentCheckClosure(index, variableName) checkType(index, variableName, isClosure, closure)
#define argumentCheckClosureOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isClosure, closure)
#define argumentCheckRecordTypeDescriptorOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isRecordTypeDescriptor, record-type-descriptor)

#define argumentCheckRecordTypeDescriptor(index, variableName) checkType(index, variableName, isRecordTypeDescriptor, record-type-descriptor)
#define argumentAsRecordTypeDescriptor(index, variableName) castArgument(index, variableName, isRecordTypeDescriptor, record-type-descriptor, RecordTypeDescriptor*, toRecordTypeDescriptor)

#define argumentAsRecordConstructorDescriptor(index, variableName) castArgument(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor, RecordConstructorDescriptor*, toRecordConstructorDescriptor)
#define argumentCheckRecordConstructorDescriptor(index, variableName) checkType(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor)
#define argumentCheckRecordConstructorDescriptorOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isRecordConstructorDescriptor, record-constructor-descriptor)

//#define argumentAsPort(index, variableName) castArgument(index, variableName, isPort, port, Port*, toPort)
#define argumentCheckPair(index, variableName) checkType(index, variableName, isPair, pair)

//#define argumentAsTextualOutputPort(index, variableName) castArgument(index, variableName, isTextualOutputPort, textual-output-port, TextualOutputPort*, toTextualOutputPort)

#define argumentAsRegexp(index, variableName) castArgument(index, variableName, isRegexp, regexp, Regexp*, toRegexp)
#define argumentAsRegMatch(index, variableName) castArgument(index, variableName, isRegMatch, regexp, RegMatch*, toRegMatch)
#define argumentAsString(index, variableName) castArgument(index, variableName, isString, string, String*, toString)


#define argumentAsChar(index, variableName) castArgument(index, variableName, isChar, character, ucs4char, toChar)

#define argumentCheckProcedure(index, variableName) checkType(index, variableName, isProcedure, procedure)
#define argumentCheckProcedureOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isProcedure, procedure)

#define argumentAsByteVector(index, variableName) castArgument(index, variableName, isByteVector, bytevector, ByteVector*, toByteVector)
#define argumentAsTranscoder(index, variableName) castArgument(index, variableName, isTranscoder, transcoder, Transcoder*, toTranscoder)
#define argumentAsCodec(index, variableName) castArgument(index, variableName, isCodec, codec, Codec*, toCodec)
#define argumentCheckTextualInputPort(index, variableName) checkType(index, variableName, isTextualInputPort, textual-input-port)
#define argumentCheckTextualOutputPort(index, variableName) checkType(index, variableName, isTextualOutputPort, textual-output-port)
#define argumentCheckOutputPort(index, variableName) checkType(index, variableName, isOutputPort, output-port)
#define argumentCheckPort(index, variableName) checkType(index, variableName, isPort, port)
#define argumentCheckTranscoderOrFalse(index, variableName) checkTypeOrFalse(index, variableName, isTranscoder, transcoder)


#define DeclareProcedureName(name) const ucs4char* procedureName = UC(name);

#define checkArgumentLength(required)   \
    if (argc != required) { \
        callWrongNumberOfArgumentsViolationAfter(theVM, procedureName, required, argc); \
        return Object::Undef;\
    } \

#define checkArgumentLengthBetween(start, end)             \
    if (argc < start || argc > end) { \
        callWrongNumberOfArgumentsBetweenViolationAfter(theVM, procedureName, start, end, argc); \
        return Object::Undef;\
    } \

//#define argumentAsTextualInputPort(index, variableName) castArgument(index, variableName, isTextualInputPort, textual-input-port, TextualInputPort*, toTextualInputPort)

#define checkArgumentLengthAtLeast(required)             \
    if (argc < required) { \
        callWrongNumberOfArgumentsAtLeastViolationAfter(theVM, procedureName, required, argc); \
        return Object::Undef;\
    } \

#endif // SCHEME_PROCEDURE_MACRO_

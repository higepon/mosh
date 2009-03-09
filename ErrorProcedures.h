/*
 * ErrorProcedures.h - violations.
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
 *  $Id: ErrorProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_VIOLATION_PROCEDURES__
#define __SCHEME_VIOLATION_PROCEDURES__

#include "scheme.h"
#include <setjmp.h>

namespace scheme {


typedef struct IOError
{
    IOError(int type, Object message, Object irritants) :
        type(type),
        message(message),
        irritants(irritants) {}

    IOError() {}

    int type;
    Object port;
    Object who;
    Object message;
    Object irritants;
    enum {
        DECODE,
        ENCODE,
        forbidden_comma
    };
} IOError;

    extern jmp_buf ioErrorJmpBuf;
    extern Object  ioErrorMessage;
    extern IOError ioError;
#ifdef DEBUG_VERSION
    extern bool isErrorBufInitialized;
#endif


#ifdef DEBUG_VERSION

#define TRY2 isErrorBufInitialized = true; if (setjmp(ioErrorJmpBuf) == 0)

#define TRY_IO isErrorBufInitialized = true; if (setjmp(ioErrorJmpBuf) == 0)
#else
#define TRY_IO if (setjmp(ioErrorJmpBuf) == 0)
#endif
#define CATCH_IO else
#define CATCH2(x) else
#define IO_ERROR_MESSAGE ioErrorMessage

    class VM;

    Object throwIOError2(int type, Object message, Object irritants = Object::Nil);
    Object callIOErrorAfter(VM* theVM, IOError e);
    Object callIOInvalidPositionAfter(VM* theVM, Object who, Object message, Object irritants, Object position);
    Object throwIOError(Object message);
    Object throwEx(VM* theVM, int argc, const Object* argv);
    Object errorEx(VM* theVM, int argc, const Object* argv);
    Object assertionViolationEx(VM* theVM, int argc, const Object* argv);
    void callIoFileNotExist(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callIoFileAlreadyExist(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callLexicalAndIOReadAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callIoFileNameErrorAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callAssertionViolationAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callUndefinedViolationAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callErrorAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callAssertionViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callImplementationRestrictionAfter(VM* theVM, Object who, Object message, Object irritants);
    void callLexicalViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callNotImplementedAssertionViolationAfter(VM* theVM, Object who, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsAtLeastViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsBetweenViolationAfter(VM* theVM, Object who, int startCounts, int endCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongTypeOfArgumentViolationAfter(VM* theVM, Object who, Object requiredType, Object gotValue, Object irritants = Object::Nil);



}; // namespace scheme

#endif // __SCHEME_VIOLATION_PROCEDURES__

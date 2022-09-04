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

#ifndef SCHEME_VIOLATION_PROCEDURES_
#define SCHEME_VIOLATION_PROCEDURES_

#include "scheme.h"
#include <setjmp.h>

namespace scheme {

typedef struct IOError
{
    IOError(int type, Object message, Object irritants) :
        type(type),
        arg1(Object::Nil),
        arg2(Object::Nil),
        who(Object::Nil),
        message(message),
        irritants(irritants) {}

    IOError() = default;

    int type;
    Object arg1;
    Object arg2;
    Object who;
    Object message;
    Object irritants;
    enum {
        DECODE,
        ENCODE,
        READ,
        WRITE,
        forbidden_comma
    };
} IOError;

extern jmp_buf ioErrorJmpBuf;
extern IOError ioError;
#ifdef DEBUG_VERSION
extern bool isErrorBufInitialized;
#endif


#ifdef DEBUG_VERSION
#define TRY_WITHOUT_DSTR isErrorBufInitialized = true; if (setjmp(ioErrorJmpBuf) == 0) {
#define CATCH(x) isErrorBufInitialized = false; } else { isErrorBufInitialized = false;
#else
#define TRY_WITHOUT_DSTR if (setjmp(ioErrorJmpBuf) == 0) {
#define CATCH(x) } else {
#endif

#define END_TRY }


    class VM;

    Object throwIOError2(int type, Object message, Object irritants = Object::Nil);
    Object throwIOError2(int type, const ucs4string& message, Object irritants = Object::Nil);

    Object callIOErrorAfter(VM* theVM, IOError e);
    Object callIOErrorAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    Object callIOErrorAfter(VM* theVM, Object who, const ucs4string& message, Object irritants = Object::Nil);
    Object callIOErrorAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object irritants = Object::Nil);    

    Object callIOInvalidPositionAfter(VM* theVM, Object who, Object message, Object irritants, Object position);
    Object callIOInvalidPositionAfter(VM* theVM, const ucs4char* who, Object message, Object irritants, Object position);    
    Object callIOInvalidPositionAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object irritants, Object position);    

    Object callIOReadErrorAfter(VM* theVM, Object who, Object message, Object port);
    Object callIOReadErrorAfter(VM* theVM, Object who, const ucs4char* message, const ucs4char* port);    
    Object callIOReadErrorAfter(VM* theVM, const ucs4char* who, Object message, Object port);
    Object callIOReadErrorAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object port);

    Object throwEx(VM* theVM, int argc, const Object* argv);

    Object callIoFileNotExistAfter(VM* theVM, Object who, Object message, Object filename);
    Object callIoFileNotExistAfter(VM* theVM, const ucs4char* who, Object message, Object filename);
    Object callIoFileNotExistAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object filename);    

    Object callLexicalAndIOReadAfter(VM* theVM, Object who, Object message);
    Object callLexicalAndIOReadAfter(VM* theVM, const ucs4char* who, Object message);    

    Object callIoFileNameErrorAfter(VM* theVM, Object who, Object message, Object filename);
    Object callIoFileNameErrorAfter(VM* theVM, const ucs4char* who, Object message, Object filename); 
    Object callIoFileNameErrorAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object filename);    

    Object callAssertionViolationAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    Object callAssertionViolationAfter(VM* theVM, const ucs4char* who, Object message, Object irritants = Object::Nil);  
    Object callAssertionViolationAfter(VM* theVM, Object who, const ucs4string& message, Object irritants = Object::Nil); 
    Object callAssertionViolationAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object irritants = Object::Nil);

    Object callUndefinedViolationAfter(VM* theVM, Object who, Object message);
    Object callUndefinedViolationAfter(VM* theVM, Object who, const ucs4string& message);    

    void callAssertionViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callAssertionViolationImmidiaImmediately(VM* theVM, const ucs4char* who, const ucs4char* message, Object irritants = Object::Nil);

    Object callImplementationRestrictionAfter(VM* theVM, Object who, Object message, Object irritants);
    Object callImplementationRestrictionAfter(VM* theVM, const ucs4char* who, Object message, Object irritants);
    Object callImplementationRestrictionAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object irritants);    

    void callLexicalViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    void callLexicalViolationImmidiaImmediately(VM* theVM, const ucs4char* who, Object message, Object irritants = Object::Nil);    

    void callNotImplementedAssertionViolationAfter(VM* theVM, Object who, Object irritants = Object::Nil);
    void callNotImplementedAssertionViolationAfter(VM* theVM, const ucs4char* who, Object irritants = Object::Nil);

    void callWrongNumberOfArgumentsViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsViolationAfter(VM* theVM, const ucs4char* who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);    

    void callWrongNumberOfArgumentsAtLeastViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsAtLeastViolationAfter(VM* theVM, const ucs4char* who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);

    void callWrongNumberOfArgumentsBetweenViolationAfter(VM* theVM, Object who, int startCounts, int endCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsBetweenViolationAfter(VM* theVM, const ucs4char* who, int startCounts, int endCounts, int gotCounts, Object irritants = Object::Nil);

    void callWrongTypeOfArgumentViolationAfter(VM* theVM, Object who, Object requiredType, Object gotValue, Object irritants = Object::Nil);
    void callWrongTypeOfArgumentViolationAfter(VM* theVM, const ucs4char* who, Object requiredType, Object gotValue, Object irritants = Object::Nil);
    void callWrongTypeOfArgumentViolationAfter(VM* theVM, const ucs4char* who, const ucs4char* requiredType, Object gotValue, Object irritants = Object::Nil);

    Object callIoFileAlreadyExistAfter(VM* theVM, Object who, Object message, Object filname);
    Object callIoFileAlreadyExistAfter(VM* theVM, const ucs4char* who, Object message, Object filname);   
    Object callIoFileAlreadyExistAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object filname);      

    Object callIoFileProtectionAfter(VM* theVM, Object who, Object message, Object filename);
    Object callIoFileProtectionAfter(VM* theVM, Object who, const ucs4string& message, Object filename);    
    Object callIoFileProtectionAfter(VM* theVM, Object who, const ucs4string& message, Object filename);
    Object callIoFileProtectionAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object filename);

    Object callIoFileReadOnlyAfter(VM* theVM, Object who, Object message, Object filename);
    Object callIoFileReadOnlyAfter(VM* theVM, Object who, const ucs4string& message, Object filename);
    Object callIoFileReadOnlyAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object filename);

    Object callErrorAfter(VM* theVM, Object who, Object message, Object irritants = Object::Nil);
    Object callErrorAfter(VM* theVM, const ucs4char* who, Object message, Object irritants = Object::Nil);    
    Object callErrorAfter(VM* theVM, Object who, const ucs4string& message, Object irritants = Object::Nil);
    Object callErrorAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object irritants = Object::Nil);    
} // namespace scheme

#endif // SCHEME_VIOLATION_PROCEDURES_

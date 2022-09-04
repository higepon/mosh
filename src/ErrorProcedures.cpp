/*
 * ErrorProcedures.cpp -
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
 *  $Id: ViolationProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "Symbol.h"
#include "Closure.h"
#include "VM.h"
#include "ErrorProcedures.h"
#include "ProcedureMacro.h"
#include "PortProcedures.h"
#include "TextualOutputPort.h"
#include "StringProcedures.h"
#include "Closure.h"

using namespace scheme;

jmp_buf scheme::ioErrorJmpBuf;
IOError scheme::ioError;

#ifdef DEBUG_VERSION
bool scheme::isErrorBufInitialized = false;
#endif

static Object raiseAfter2(VM* theVM, const ucs4char* procName, Object who, Object message)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(procName));
    if (procedure.isFalse()) {
        const Object content =  format(theVM, UC(" WARNING: Error occured before (~e ...) defined\n"), Pair::list1(Object(procName)));
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, content);
        const Object condition =  format(theVM,
                                         UC(" Condition components:\n"
                                            "    1. ~e\n"
                                            "    2. &who: ~e\n"
                                            "    3. &message: ~s\n"), Pair::list3(Object(procName), who, message));
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger2(procedure, who, message);
    }
    return Object::Undef;
}

static Object raiseAfter3(VM* theVM, const ucs4char* procName, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(procName));
    if (procedure.isFalse()) {
        const Object content =  format(theVM, UC(" WARNING: Error occured before (~e ...) defined\n"), Pair::list1(Object(procName)));
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, content);
        const Object condition =  format(theVM,
                                         UC(" Condition components:\n"
                                            "    1. ~e\n"
                                            "    2. &who: ~e\n"
                                            "    3. &message: ~s\n"
                                            "    4. &irritants: ~e\n"), Pair::list4(Object(procName), who, message, irritants));
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, irritants);
    }
    return Object::Undef;
}

static Object raiseAfter4(VM* theVM, const ucs4char* procName, Object who, Object message, Object irritant1, Object irritant2)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(procName));
    if (procedure.isFalse()) {
        const Object content =  format(theVM, UC(" WARNING: Error occured before (~e ...) defined\n"), Pair::list1(Object(procName)));
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, content);
        const Object condition =  format(theVM,
                                         UC(" Condition components:\n"
                                            "    1. ~e\n"
                                            "    2. &who: ~e\n"
                                            "    3. &message: ~s\n"
                                            "    4. &irritants: ~e\n"), Pair::list4(Object(procName), who, message, Pair::list2(irritant1, irritant2)));
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger4(procedure, who, message, irritant1, irritant2);
    }
    return Object::Undef;
}

Object scheme::callIOReadErrorAfter(VM* theVM, Object who, Object message, Object port)
{
    return raiseAfter3(theVM, UC("raise-i/o-read-error"), who, message, port);
}

Object scheme::callIOReadErrorAfter(VM* theVM, const ucs4char* who, Object message, Object port)
{
    return callIOReadErrorAfter(theVM, Object(who), message, port);
}

Object scheme::callIOReadErrorAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object port)
{
    return callIOReadErrorAfter(theVM, Object(who), Object(message), port);
}

Object scheme::callIOReadErrorAfter(VM* theVM, Object who, const ucs4char* message, const ucs4char* port)
{
    return callIOReadErrorAfter(theVM, who, Object(message), Object(port));
}

Object scheme::callIOErrorAfter(VM* theVM, Object who, Object message, Object irritants)
{
    return raiseAfter3(theVM, UC("raise-i/o-read-error"), who, message, irritants);
}

Object scheme::callIOErrorAfter(VM* theVM, Object who, const ucs4string& message, Object irritants)
{
    return callIOErrorAfter(theVM, who, Object(message), irritants);
}

Object scheme::callIOErrorAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object irritants)
{
    return callIOErrorAfter(theVM, Object(who), Object(message), irritants);
}

Object scheme::callIOErrorAfter(VM* theVM, IOError e)
{
    switch(e.type) {
    case IOError::DECODE:
    {
        return raiseAfter3(theVM, UC("raise-i/o-decoding-error"), e.who, e.message, e.arg1);
    }
    case IOError::ENCODE:
    {
        return raiseAfter4(theVM, UC("raise-i/o-encoding-error"), e.who, e.message, e.arg1, e.irritants);
    }
    case IOError::READ:
    {
        return callIOReadErrorAfter(theVM, e.who, e.message, e.irritants);
    }
    case IOError::WRITE:
    {
        return raiseAfter3(theVM, UC("raise-i/o-write-error"), e.who, e.message, e.irritants);
    }
    default:
        callAssertionViolationAfter(theVM, e.who, e.message, e.irritants);
        break;

    }
    return Object::Undef;
}

Object scheme::throwIOError2(int type, const ucs4string& message, Object irritants /* = Object::Nil */)
{
    return throwIOError2(type, Object(message), irritants);
}

Object scheme::throwIOError2(int type, Object message, Object irritants /* = Object::Nil */)
{
    ioError = IOError(type, message, irritants);
    MOSH_ASSERT(isErrorBufInitialized);
    longjmp(ioErrorJmpBuf, -1);
    return Object::Undef;
}

Object scheme::throwEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("throw");
    checkArgumentLength(1);
    theVM->throwException(argv[0]);
    return Object::Undef;
}

void scheme::callNotImplementedAssertionViolationAfter(VM* theVM, Object who, Object irritants /* = Object::Nil */)
{
    callAssertionViolationAfter(theVM, who, "not implemented", irritants);
}

void scheme::callNotImplementedAssertionViolationAfter(VM* theVM, const ucs4char* who, Object irritants /* = Object::Nil */)
{
    callNotImplementedAssertionViolationAfter(theVM, Object(who), irritants);
}

void scheme::callWrongTypeOfArgumentViolationAfter(VM* theVM, Object who, Object requiredType, Object gotValue, Object irritants /* = Object::Nil */)
{
    const Object message = format(theVM, UC("~e required, but got ~e"),
                                  Pair::list2(requiredType, gotValue));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongTypeOfArgumentViolationAfter(VM* theVM, const ucs4char* who, Object requiredType, Object gotValue, Object irritants /* = Object::Nil */)
{
    callWrongTypeOfArgumentViolationAfter(theVM, Object(who), requiredType, gotValue, irritants);
}

void scheme::callWrongNumberOfArgumentsBetweenViolationAfter(VM* theVM, Object who, int startCounts, int endCounts, int gotCounts, Object irritants /* = Object::Nil */)
{
    const Object message = format(theVM, UC("wrong number of arguments (required between ~d and ~d, got ~d)"),
                                  Pair::list3(Object::makeFixnum(startCounts),
                                              Object::makeFixnum(endCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsBetweenViolationAfter(VM* theVM, const ucs4char* who, int startCounts, int endCounts, int gotCounts, Object irritants /* = Object::Nil */)
{
    callWrongNumberOfArgumentsBetweenViolationAfter(theVM, Object(who), startCounts, endCounts, gotCounts, irritants);
}

void scheme::callWrongNumberOfArgumentsViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object message = format(theVM, UC("wrong number of arguments (required ~d, got ~d)"),
                                  Pair::list2(Object::makeFixnum(requiredCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsViolationAfter(VM* theVM, const ucs4char* who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    callWrongNumberOfArgumentsViolationAfter(theVM, Object(who), requiredCounts, gotCounts, irritants);
}

void scheme::callWrongNumberOfArgumentsAtLeastViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object message = format(theVM, UC("wrong number of arguments (required at least ~d, got ~d)"),
                                  Pair::list2(Object::makeFixnum(requiredCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsAtLeastViolationAfter(VM* theVM, const ucs4char* who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    callWrongNumberOfArgumentsAtLeastViolationAfter(theVM, Object(who), requiredCounts, gotCounts, irritants);
}


// we can't catch this!
void scheme::callAssertionViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    const Object condition =  format(theVM,
                                    UC(" Condition components:\n"
                                       "    1. &assertion\n"
                                       "    2. &who: ~e\n"
                                       "    3. &message: ~s\n"
                                       "    4. &irritants: ~e\n"), Pair::list3(who, message, irritants));
    theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (assertion-violation ...) defined\n");
    theVM->throwException(condition);
}

Object scheme::callIOInvalidPositionAfter(VM* theVM, Object who, Object message, Object irritants, Object position)
{
    return raiseAfter4(theVM, UC("raise-i/o-invalid-position-error"), who, message, irritants, position);
}

Object scheme::callIOInvalidPositionAfter(VM* theVM, const ucs4char* who, Object message, Object irritants, Object position)
{
    return callIOInvalidPositionAfter(theVM, Object(who), message, irritants, position);
}

Object scheme::callAssertionViolationAfter(VM* theVM, Object who, const ucs4string& message, Object irritants /* = Object::Nil */)
{
    return callAssertionViolationAfter(theVM, who, Object(message), irritants);
}

Object scheme::callAssertionViolationAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object irritants /* = Object::Nil */)
{
    return callAssertionViolationAfter(theVM, Object(who), Object(message), irritants);
}

Object scheme::callAssertionViolationAfter(VM* theVM, const ucs4char* who, Object message, Object irritants /* = Object::Nil */)
{
    return callAssertionViolationAfter(theVM, Object(who), message, irritants);
}

Object scheme::callAssertionViolationAfter(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    if (theVM->isR6RSMode()) {
        return raiseAfter3(theVM, UC("assertion-violation"), who, message, irritants);
    } else {
        const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise")));
        // Error occured before (raise ...) is defined.
        if (procedure.isFalse()) {
            Object condition = format(theVM,
                                  UC(
                                      " Condition components:\n"
                                      "    1. ~e\n"
                                      "    2. &who: ~e\n"
                                      "    3. &message: ~s\n"
                                      "    4. &irritants: ~e\n"), Pair::list4("&assertion", who, message, irritants));

            theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (assertion-violation ...) defined\n");
            theVM->throwException(condition);
        } else {
            Object condition = format(theVM,
                                  UC(
                                      " Condition components:\n"
                                      "    1. ~e\n"
                                      "    2. &who: ~e\n"
                                      "    3. &message: ~s\n"
                                      "    4. &irritants: ~e\n"), Pair::list4("&assertion", who, message, irritants));

            theVM->setAfterTrigger1(procedure, condition);
        }
    }
    return Object::Undef;
}

Object scheme::callUndefinedViolationAfter(VM* theVM, Object who, Object message)
{
    return raiseAfter2(theVM, UC("undefined-violation"), who, message);
}

// we can't catch this!
void scheme::callLexicalViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    const Object condition = format(theVM,
                              UC(
                                 " Condition components:\n"
                                 "    1. &lexical\n"
                                 "    2. &who: ~e\n"
                                 "    3. &message: ~s\n"
                                 "    4. &irritants: ~e\n"), Pair::list3(who, message, irritants));
    theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (lexical ...) defined\n");
    theVM->throwException(condition);
}

Object scheme::callImplementationRestrictionAfter(VM* theVM, Object who, Object message, Object irritants)
{
    return raiseAfter3(theVM, UC("implementation-restriction-violation"), who, message, irritants);
}

Object scheme::callImplementationRestrictionAfter(VM* theVM, const ucs4char* who, Object message, Object irritants)
{
    return callImplementationRestrictionAfter(theVM, Object(who), message, irritants);
}

Object scheme::callImplementationRestrictionAfter(VM* theVM, const ucs4char* who, const ucs4char* message, Object irritants)
{
    return callImplementationRestrictionAfter(theVM, Object(who), Object(message), irritants);
}

Object scheme::callLexicalAndIOReadAfter(VM* theVM, Object who, Object message)
{
    return raiseAfter2(theVM, UC("raise-lexical-violation-read-error"), who, message);
}

Object scheme::callLexicalAndIOReadAfter(VM* theVM, const ucs4char* who, Object message)
{
    return callLexicalAndIOReadAfter(theVM, Object(who), message);
}

Object scheme::callIoFileNameErrorAfter(VM* theVM, Object who, Object message, Object filename)
{
    return raiseAfter3(theVM, UC("raise-i/o-filename-error"), who, message, filename);
}

Object scheme::callIoFileNameErrorAfter(VM* theVM, const ucs4char* who, Object message, Object filename)
{
    return callIoFileNameErrorAfter(theVM, Object(who), message, filename);
}

Object scheme::callIoFileNotExistAfter(VM* theVM, Object who, Object message, Object filename)
{
    return raiseAfter3(theVM, UC("raise-i/o-file-does-not-exist-error"), who, message, filename);
}

Object scheme::callIoFileNotExistAfter(VM* theVM, const ucs4char* who, Object message, Object filename)
{
    return callIoFileNotExistAfter(theVM, Object(who), message, filename);
}

Object scheme::callIoFileAlreadyExistAfter(VM* theVM, Object who, Object message, Object filename)
{
    return raiseAfter3(theVM, UC("raise-i/o-file-already-exists-error"), who, message, filename);
}

Object scheme::callIoFileAlreadyExistAfter(VM* theVM, const ucs4char* who, Object message, Object filename)
{
    return callIoFileAlreadyExistAfter(theVM, Object(who), message, filename);
}

Object scheme::callIoFileProtectionAfter(VM* theVM, Object who, Object message, Object filename)
{
    return raiseAfter3(theVM, UC("raise-i/o-file-protection-error"), who, message, filename);
}

Object scheme::callIoFileProtectionAfter(VM* theVM, Object who, const ucs4string& message, Object filename)
{
    return callIoFileProtectionAfter(theVM, who, Object(message), filename);
}

Object scheme::callIoFileProtectionAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object filename)
{
    return callIoFileProtectionAfter(theVM, Object(who), Object(message), filename);
}

Object scheme::callIoFileReadOnlyAfter(VM* theVM, Object who, Object message, Object filename)
{
    return raiseAfter3(theVM, UC("raise-i/o-file-is-read-only-error"), who, message, filename);
}

Object scheme::callIoFileReadOnlyAfter(VM* theVM, Object who, const ucs4string& message, Object filename)
{
    return callIoFileReadOnlyAfter(theVM, who, Object(message), filename);
}

Object scheme::callIoFileReadOnlyAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object filename)
{
    return callIoFileReadOnlyAfter(theVM, Object(who), Object(message), filename);
}

Object scheme::callErrorAfter(VM* theVM, const ucs4char* who, Object message, Object irritants /* = Object::Nil */)
{
    return raiseAfter3(theVM, UC("error"), Object(who), message, irritants);
}

Object scheme::callErrorAfter(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    return raiseAfter3(theVM, UC("error"), who, message, irritants);
}

Object scheme::callErrorAfter(VM* theVM, Object who, const ucs4string& message, Object irritants /* = Object::Nil */)
{
    return raiseAfter3(theVM, UC("error"), who, Object(message), irritants);
}

Object scheme::callErrorAfter(VM* theVM, const ucs4char* who, const ucs4string& message, Object irritants /* = Object::Nil */)
{
    return raiseAfter3(theVM, UC("error"), Object(who), Object(message), irritants);
}

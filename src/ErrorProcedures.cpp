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

Object scheme::callIOPortErrorAfter(VM* theVM, Object port, Object who, Object message)
{
    MOSH_ASSERT(false);
}


Object scheme::callIOErrorAfter(VM* theVM, Object who, Object message, Object irritants)
{
    MOSH_ASSERT(false);
}


// Alias for callIOErrorAfter now.
Object scheme::callSocketErrorAfter(VM* theVM, Object who, Object message, Object irritants)
{
    MOSH_ASSERT(false);
}

Object scheme::callIOErrorAfter(VM* theVM, IOError e)
{
    switch(e.type) {
    case IOError::DECODE:
    {
        const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-decoding-error")));
        Object condition = Object::Nil;
        // Error occured before (raise ...) is defined.
        if (procedure.isFalse()) {
            theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
            theVM->throwException(condition);
        } else {
            theVM->setAfterTrigger3(procedure, e.who, e.message, e.arg1);
        }
        break;
    }
    case IOError::ENCODE:
    {
        const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-encoding-error")));
        Object condition = Object::Nil;
        // Error occured before (raise ...) is defined.
        if (procedure.isFalse()) {
            theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
            theVM->throwException(condition);
        } else {
            theVM->setAfterTrigger4(procedure, e.who, e.message, e.arg1, e.irritants);
        }
        break;
    }
 
//         MOSH_ASSERT(e.irritants.isPair() && Pair::length(e.irritants) == 1);
//         raiseAfter2(theVM, UC("&i/o-encoding-rcd"), UC("&i/o-encoding"), e.arg1, e.irritants.car(), e.who, e.message, e.irritants);
//        break;
    case IOError::READ:
    {
        const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-read-error")));
        Object condition = Object::Nil;
        // Error occured before (raise ...) is defined.
        if (procedure.isFalse()) {
            theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
            theVM->throwException(condition);
        } else {
            theVM->setAfterTrigger3(procedure, e.who, e.message, e.irritants);
        }
        break;
    }
    case IOError::WRITE:
    {
        const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-write-error")));
        Object condition = Object::Nil;
        // Error occured before (raise ...) is defined.
        if (procedure.isFalse()) {
            theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
            theVM->throwException(condition);
        } else {
            theVM->setAfterTrigger3(procedure, e.who, e.message, e.irritants);
        }
        break;
    }
    default:
        callAssertionViolationAfter(theVM, e.who, e.message, e.irritants);
        break;

    }
    return Object::Undef;
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

void scheme::callWrongTypeOfArgumentViolationAfter(VM* theVM, Object who, Object requiredType, Object gotValue, Object irritants /* = Object::Nil */)
{
    const Object message = format(theVM, UC("~a required, but got ~a"),
                                  Pair::list2(requiredType, gotValue));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsBetweenViolationAfter(VM* theVM, Object who, int startCounts, int endCounts, int gotCounts, Object irritants /* = Object::Nil */)
{
    const Object message = format(theVM, UC("wrong number of arguments (required between ~d and ~d, got ~d)"),
                                  Pair::list3(Object::makeFixnum(startCounts),
                                              Object::makeFixnum(endCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object message = format(theVM, UC("wrong number of arguments (required ~d, got ~d)"),
                                  Pair::list2(Object::makeFixnum(requiredCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsAtLeastViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object message = format(theVM, UC("wrong number of arguments (required at least ~d, got ~d)"),
                                  Pair::list2(Object::makeFixnum(requiredCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

// we can't catch this!
void scheme::callAssertionViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    const Object condition =  format(theVM,
                                    UC(" Condition components:\n"
                                       "    1. &assertion\n"
                                       "    2. &who: ~a\n"
                                       "    3. &message: ~s\n"
                                       "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));
    theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
    theVM->throwException(condition);
}

Object scheme::callIOInvalidPositionAfter(VM* theVM, Object who, Object message, Object irritants, Object position)
{
    MOSH_ASSERT(false);
//    raiseAfter1(theVM, UC("&i/o-invalid-position-rcd"), UC("&i/o-invalid-position"), position, who, message, irritants);
    return Object::Undef;
}


Object scheme::callAssertionViolationAfter(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
//     LOG1("message=~a\n", message);
//     LOG1("who=~a\n", who);
//     LOG1("irritants=~a\n", irritants);
//    raiseAfter(theVM, UC("&assertion-rcd"), UC("&assertion"), 0, who, message, irritants);
//     MOSH_ASSERT(theVM);
//     MOSH_ASSERT(irritants.isPair() || irritants.isNil());
//     MOSH_ASSERT(who.isSymbol() || who.isString() || who.isFalse());
//     MOSH_ASSERT(message.isString());
    Object condition = Object::Nil;

    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("assertion-violation")));

    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger2(procedure,message, irritants);
    }
    return Object::Undef;
}

Object scheme::callUndefinedViolationAfter(VM* theVM, Object who, Object message)
{
//    raiseAfter(theVM, UC("&undefined-rcd"), UC("&undefined"), 0, who, message, irritants);
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("undefined-violation")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger2(procedure, who, message);
    }
    return Object::Undef;
}

// we can't catch this!
void scheme::callLexicalViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    const Object condition = format(theVM,
                              UC(
                                 " Condition components:\n"
                                 "    1. &lexical\n"
                                 "    2. &who: ~a\n"
                                 "    3. &message: ~s\n"
                                 "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));
    theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
    theVM->throwException(condition);
}

Object scheme::callImplementationRestrictionAfter(VM* theVM, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("implementation-restriction-violation")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, irritants);
    }
    return Object::Undef;

}

Object scheme::callLexicalAndIOReadAfter(VM* theVM, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("lexical-violation")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger2(procedure, who, message);
    }
    return Object::Undef;

}

Object scheme::callIoFileNameErrorAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-filename-error")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, filename);
    }
    return Object::Undef;
}

Object scheme::callIoFileNotExistAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-file-does-not-exist-error")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, filename);
    }
    return Object::Undef;
}

Object scheme::callIoFileAlreadyExistAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-file-already-exists-error")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, filename);
    }
    return Object::Undef;
}

Object scheme::callIoFileProtectionAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-file-protection-error")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, filename);
    }
    return Object::Undef;
}

Object scheme::callIoFileReadOnlyAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("raise-i/o-file-is-read-only")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, filename);
    }
    return Object::Undef;
}


Object scheme::callErrorAfter(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    const Object procedure = theVM->getGlobalValueOrFalse(Symbol::intern(UC("error")));

    Object condition = Object::Nil;
    // Error occured before (raise ...) is defined.
    if (procedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(theVM, " WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger3(procedure, who, message, irritants);
    }
    return Object::Undef;
}

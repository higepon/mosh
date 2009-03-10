/*
 * ViolationProcedures.cpp -
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
#include "RecordConstructorDescriptor.h"
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

static Object makeMessageCondition(VM* theVM, Object message);
static Object makeIrritantsCondition(VM* theVM, Object irritants);
static Object makeWhoCondition(VM* theVM, Object who);
//static Object makeAssertionCondition();
static Object makeCondition(VM* theVM, const ucs4char* rcdName);
static Object makeCondition(VM* theVM, const ucs4char* rcdName, Object conent);
static Object raiseAfter(VM* theVM,
                       const ucs4char* errorRcdName,
                       const ucs4char* errorName,
                       int argumentCount,
                       Object who,
                       Object message,
                       Object irritants = Object::Nil);

static Object raiseAfter1(VM* theVM,
                        const ucs4char* errorRcdName,
                        const ucs4char* errorName,
                        Object argument1,
                        Object who,
                        Object message,
                        Object irritants = Object::Nil);


static Object raiseAfter2(VM* theVM,
                const ucs4char* errorRcdName1,
                const ucs4char* errorName1,
                int argumentCount1,
                const ucs4char* errorRcdName2,
                const ucs4char* errorName2,
                int argumentCount2,
                Object who,
                Object message,
                Object irritants = Object::Nil);

Object scheme::callIOErrorAfter(VM* theVM, IOError e)
{
    switch(e.type) {
    case IOError::DECODE:
        raiseAfter1(theVM, UC("&i/o-decoding-rcd"), UC("&i/o-decoding"), e.port, e.who, e.message, e.irritants);
        break;
    case IOError::ENCODE:
        raiseAfter1(theVM, UC("&i/o-encoding-rcd"), UC("&i/o-encoding"), e.port, e.who, e.message, e.irritants);
        break;
    case IOError::READ:
        raiseAfter(theVM, UC("&i/o-read-rcd"), UC("&i/o-read"), 0, e.who, e.message, e.irritants);
    case IOError::WRITE:
        raiseAfter(theVM, UC("&i/o-read-rcd"), UC("&i/o-read"), 0, e.who, e.message, e.irritants);
        break;
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
    const Object message = format(UC("~a required, but got ~a"),
                                  Pair::list2(requiredType, gotValue));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsBetweenViolationAfter(VM* theVM, Object who, int startCounts, int endCounts, int gotCounts, Object irritants /* = Object::Nil */)
{
    const Object message = format(UC("wrong number of arguments (required between ~d and ~d, got ~d)"),
                                  Pair::list3(Object::makeFixnum(startCounts),
                                              Object::makeFixnum(endCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object message = format(UC("wrong number of arguments (required ~d, got ~d)"),
                                  Pair::list2(Object::makeFixnum(requiredCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsAtLeastViolationAfter(VM* theVM, Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object message = format(UC("wrong number of arguments (required at least ~d, got ~d)"),
                                  Pair::list2(Object::makeFixnum(requiredCounts),
                                              Object::makeFixnum(gotCounts)));
    callAssertionViolationAfter(theVM, who, message, irritants);
}

// we can't catch this!
void scheme::callAssertionViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
   const Object condition =  format(UC(" Condition components:\n"
                                       "    1. &assertion\n"
                                       "    2. &who: ~a\n"
                                       "    3. &message: ~s\n"
                                       "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));
    theVM->currentErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
    theVM->throwException(condition);
}

Object scheme::callIOInvalidPositionAfter(VM* theVM, Object who, Object message, Object irritants, Object position)
{
    raiseAfter1(theVM, UC("&i/o-invalid-position-rcd"), UC("&i/o-invalid-position"), position, who, message, irritants);
    return Object::Undef;
}


void scheme::callAssertionViolationAfter(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    raiseAfter(theVM, UC("&assertion-rcd"), UC("&assertion"), 0, who, message, irritants);
}

void scheme::callUndefinedViolationAfter(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    raiseAfter(theVM, UC("&undefined-rcd"), UC("&undefined"), 0, who, message, irritants);
}

// we can't catch this!
void scheme::callLexicalViolationImmidiaImmediately(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    const Object condition = format(UC(" Condition components:\n"
                                 "    1. &lexical\n"
                                 "    2. &who: ~a\n"
                                 "    3. &message: ~s\n"
                                 "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));
    theVM->currentErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
    theVM->throwException(condition);
}

void scheme::callImplementationRestrictionAfter(VM* theVM, Object who, Object message, Object irritants)
{
    raiseAfter(theVM, UC("&implementation-restriction-rcd"), UC("&implementation-restriction"), 0, who, message, irritants);
}

Object scheme::callLexicalAndIOReadAfter(VM* theVM, Object who, Object message, Object irritants)
{
    return raiseAfter2(theVM, UC("&lexical-rcd"), UC("&lexical"), 0, UC("&i/o-read-rcd"), UC("&i/o-read"), 0, who, message, irritants);
}

Object scheme::callIoFileNameErrorAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    return raiseAfter1(theVM, UC("&i/o-filename-rcd"), UC("&i/o-filename"), filename, who, message, irritants);
}

Object scheme::callIoFileNotExistAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    return raiseAfter1(theVM, UC("&i/o-file-does-not-exist-rcd"), UC("&i/o-file-does-not-exist"), filename, who, message, irritants);
}

Object scheme::callIoFileAlreadyExistAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    return raiseAfter1(theVM, UC("&i/o-file-already-exists-rcd"), UC("&i/o-file-already-exists"), filename, who, message, irritants);
}

Object scheme::callIoFileProtectionAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    return raiseAfter1(theVM, UC("&i/o-file-protection-rcd"), UC("&i/o-file-protection"), filename, who, message, irritants);
}

Object scheme::callIoFileReadOnlyAfter(VM* theVM, Object filename, Object who, Object message, Object irritants)
{
    return raiseAfter1(theVM, UC("&i/o-file-is-read-only-rcd"), UC("&i/o-file-is-read-only"), filename, who, message, irritants);
}


void scheme::callErrorAfter(VM* theVM, Object who, Object message, Object irritants /* = Object::Nil */)
{
    raiseAfter(theVM, UC("&error-rcd"), UC("&error"), 0, who, message, irritants);
}

Object scheme::assertionViolationEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("assertion-violation");
    checkArgumentLengthAtLeast(2);
    const Object who = argv[0];
    if (!who.isFalse() && !who.isString() && !who.isSymbol()) {
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "symbol, string or #f", who);
        return Object::Undef;
    }
    argumentCheckString(1, message);

    Object irritants = Object::Nil;
    for (int i = 2; i < argc; i++) {
        irritants = Object::cons(argv[i], irritants);
    }
    callAssertionViolationAfter(theVM, who, message, irritants);
    return Object::Undef;
}

Object scheme::errorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("error");
    checkArgumentLengthAtLeast(2);
    const Object who = argv[0];
    if (!who.isFalse() && !who.isString() && !who.isSymbol()) {
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "symbol, string or #f", who);
        return Object::Undef;
    }
    argumentCheckString(1, message);
    Object irritants = Object::Nil;
    for (int i = argc - 1; i >= 2; i--) {
        irritants = Object::cons(argv[i], irritants);
    }
    callErrorAfter(theVM, who, message, irritants);
    return Object::Undef;
}

// private
Object makeWhoCondition(VM* theVM, Object who)
{
    return makeCondition(theVM, UC("&who-rcd"), who);
}

Object makeMessageCondition(VM* theVM, Object message)
{
    return makeCondition(theVM, UC("&message-rcd"), message);
}

Object makeIrritantsCondition(VM* theVM, Object irritants)
{
    return makeCondition(theVM, UC("&irritants-rcd"), irritants);
}

Object makeCondition(VM* theVM, const ucs4char* rcdName)
{
    const Object rcd = theVM->getTopLevelGlobalValue(Symbol::intern(rcdName));
    MOSH_ASSERT(!rcd.isFalse());
    return theVM->callClosure0(rcd.toRecordConstructorDescriptor()->makeConstructor());
}

Object makeCondition(VM* theVM, const ucs4char* rcdName, Object content)
{
    const Object rcd = theVM->getTopLevelGlobalValue(Symbol::intern(rcdName));
    MOSH_ASSERT(!rcd.isFalse());
    return theVM->callClosure1(rcd.toRecordConstructorDescriptor()->makeConstructor(), content);
}

Object raiseAfter1(VM* theVM,
                const ucs4char* errorRcdName,
                const ucs4char* errorName,
                Object argument1,
                Object who,
                Object message,
                Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    MOSH_ASSERT(irritants.isPair() || irritants.isNil());
    MOSH_ASSERT(who.isSymbol() || who.isString() || who.isFalse());
    MOSH_ASSERT(message.isString());
    Object condition = Object::Nil;
    if (theVM->isR6RSMode()) {
        Object conditions = Object::Nil;

        // even if irritants is nil, we create irritants condition.
        conditions = Object::cons(makeIrritantsCondition(theVM, irritants), conditions);

        conditions = Object::cons(makeMessageCondition(theVM, message), conditions);

        if (!who.isFalse()) {
            conditions = Object::cons(makeWhoCondition(theVM, who), conditions);
        }

        conditions = Object::cons(makeCondition(theVM, errorRcdName, argument1), conditions);
        condition = Object::makeCompoundCondition(conditions);
    } else {
        condition = format(UC(" Condition components:\n"
                              "    1. ~a\n"
                              "    2. &who: ~a\n"
                              "    3. &message: ~s\n"
                              "    4. &irritants: ~a\n"), Pair::list4(Object::makeString(errorName), who, message, irritants));
    }

    const Object raiseProcedure = theVM->getTopLevelGlobalValueOrFalse(Symbol::intern(UC("raise")));

    // Error occured before (raise ...) is defined.
    if (raiseProcedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger1(raiseProcedure, condition);
    }
    return Object::Undef;
}


Object raiseAfter(VM* theVM,
                const ucs4char* errorRcdName,
                const ucs4char* errorName,
                int argumentCount,
                Object who,
                Object message,
                Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    MOSH_ASSERT(irritants.isPair() || irritants.isNil());
    MOSH_ASSERT(who.isSymbol() || who.isString() || who.isFalse());
    MOSH_ASSERT(message.isString());
    Object condition = Object::Nil;
    if (theVM->isR6RSMode()) {
        Object conditions = Object::Nil;

        // even if irritants is nil, we create irritants condition.
        conditions = Object::cons(makeIrritantsCondition(theVM, irritants), conditions);

        conditions = Object::cons(makeMessageCondition(theVM, message), conditions);

        if (!who.isFalse()) {
            conditions = Object::cons(makeWhoCondition(theVM, who), conditions);
        }

        MOSH_ASSERT(argumentCount >= 0 && argumentCount <= 1);
        if (0 == argumentCount) {
            conditions = Object::cons(makeCondition(theVM, errorRcdName), conditions);
        } else {
            conditions = Object::cons(makeCondition(theVM, errorRcdName, Object::Nil), conditions);
        }
        condition = Object::makeCompoundCondition(conditions);
    } else {
        condition = format(UC(" Condition components:\n"
                              "    1. ~a\n"
                              "    2. &who: ~a\n"
                              "    3. &message: ~s\n"
                              "    4. &irritants: ~a\n"), Pair::list4(Object::makeString(errorName), who, message, irritants));
    }

    const Object raiseProcedure = theVM->getTopLevelGlobalValueOrFalse(Symbol::intern(UC("raise")));

    // Error occured before (raise ...) is defined.
    if (raiseProcedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger1(raiseProcedure, condition);
    }
    return Object::Undef;
}

Object raiseAfter2(VM* theVM,
                const ucs4char* errorRcdName1,
                const ucs4char* errorName1,
                int argumentCount1,
                const ucs4char* errorRcdName2,
                const ucs4char* errorName2,
                int argumentCount2,
                Object who,
                Object message,
                Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    MOSH_ASSERT(irritants.isPair() || irritants.isNil());
    MOSH_ASSERT(who.isSymbol() || who.isString() || who.isFalse());
    MOSH_ASSERT(message.isString());
    Object condition = Object::Nil;
    if (theVM->isR6RSMode()) {
        Object conditions = Object::Nil;

        // even if irritants is nil, we create irritants condition.
        conditions = Object::cons(makeIrritantsCondition(theVM, irritants), conditions);

        conditions = Object::cons(makeMessageCondition(theVM, message), conditions);

        if (!who.isFalse()) {
            conditions = Object::cons(makeWhoCondition(theVM, who), conditions);
        }

        MOSH_ASSERT(argumentCount1 >= 0 && argumentCount1 <= 1);
        if (0 == argumentCount1) {
            conditions = Object::cons(makeCondition(theVM, errorRcdName1), conditions);
        } else {
            conditions = Object::cons(makeCondition(theVM, errorRcdName1, Object::Nil), conditions);
        }

        MOSH_ASSERT(argumentCount2 >= 0 && argumentCount2 <= 1);
        if (0 == argumentCount2) {
            conditions = Object::cons(makeCondition(theVM, errorRcdName2), conditions);
        } else {
            conditions = Object::cons(makeCondition(theVM, errorRcdName2, Object::Nil), conditions);
        }

        condition = Object::makeCompoundCondition(conditions);
    } else {
        condition = format(UC(" Condition components:\n"
                              "    1. ~a\n"
                              "    2. ~a\n"
                              "    3. &who: ~a\n"
                              "    4. &message: ~s\n"
                              "    5. &irritants: ~a\n"), Pair::list5(Object::makeString(errorName1),
                                                                      Object::makeString(errorName2),
                                                                      who,
                                                                      message,
                                                                      irritants));
    }

    const Object raiseProcedure = theVM->getTopLevelGlobalValueOrFalse(Symbol::intern(UC("raise")));

    // Error occured before (raise ...) is defined.
    if (raiseProcedure.isFalse()) {
        theVM->currentErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger1(raiseProcedure, condition);
    }
    return Object::Undef;
}

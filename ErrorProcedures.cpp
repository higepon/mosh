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
#include "ErrorProcedures.h"
#include "ProcedureMacro.h"
#include "PortProcedures.h"
#include "TextualOutputPort.h"
#include "Closure.h"

using namespace scheme;

jmp_buf scheme::ioErrorJmpBuf;
Object  scheme::ioErrorMessage;
#ifdef DEBUG_VERSION
bool scheme::isErrorBufInitialized = false;
#endif

static Object makeMessageCondition(Object message);
static Object makeIrritantsCondition(Object irritants);
static Object makeWhoCondition(Object who);
static Object makeAssertionCondition();
static Object makeCondition(const ucs4char* rcdName);
static Object makeCondition(const ucs4char* rcdName, Object conent);
static void raiseAfter(const ucs4char* errorRcdName,
                       const ucs4char* errorName,
                       int argumentCount,
                       Object who,
                       Object message,
                       Object irritants = Object::Nil);

Object scheme::throwIOError(Object message)
{
    ioErrorMessage = message;
    MOSH_ASSERT(isErrorBufInitialized);
    longjmp(ioErrorJmpBuf, -1);
}

Object scheme::throwEx(int argc, const Object* argv)
{
    DeclareProcedureName("throw");
    checkArgumentLength(1);
    theVM->throwException(argv[0]);
    return Object::Undef;
}

void scheme::callNotImplementedAssertionViolationAfter(Object who, Object irritants /* = Object::Nil */)
{
    callAssertionViolationAfter(who, "not implemented", irritants);
}

void scheme::callWrongTypeOfArgumentViolationAfter(Object who, Object requiredType, Object gotValue, Object irritants /* = Object::Nil */)
{
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC("~a required, but got ~a"),
                              Pair::list2(requiredType, gotValue));
    const Object message = sysGetOutputStringEx(1, &stringOutputPort);
    callAssertionViolationAfter(who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsBetweenViolationAfter(Object who, int startCounts, int endCounts, int gotCounts, Object irritants /* = Object::Nil */)
{
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC("wrong number of arguments (required between ~d and ~d, got ~d)"),
                              Pair::list3(Object::makeFixnum(startCounts),
                                          Object::makeFixnum(endCounts),
                                          Object::makeFixnum(gotCounts)));
    const Object message = sysGetOutputStringEx(1, &stringOutputPort);
    callAssertionViolationAfter(who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsViolationAfter(Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC("wrong number of arguments (required ~d, got ~d)"),
                              Pair::list2(Object::makeFixnum(requiredCounts),
                                          Object::makeFixnum(gotCounts)));
    const Object message = sysGetOutputStringEx(1, &stringOutputPort);
    callAssertionViolationAfter(who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsAtLeastViolationAfter(Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC("wrong number of arguments (required at least ~d, got ~d)"),
                              Pair::list2(Object::makeFixnum(requiredCounts),
                                          Object::makeFixnum(gotCounts)));
    const Object message = sysGetOutputStringEx(1, &stringOutputPort);
    callAssertionViolationAfter(who, message, irritants);
}

// we can't catch this!
void scheme::callAssertionViolationImmidiaImmediately(Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC(" Condition components:\n"
                                 "    1. &assertion\n"
                                 "    2. &who: ~a\n"
                                 "    3. &message: ~s\n"
                                 "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));

    const Object condition = sysGetOutputStringEx(1, &stringOutputPort);
    theVM->getErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
    theVM->throwException(condition);
}

void scheme::callAssertionViolationAfter(Object who, Object message, Object irritants /* = Object::Nil */)
{
    raiseAfter(UC("&assertion-rcd"), UC("&assertion"), 0, who, message, irritants);
}

// we can't catch this!
void scheme::callLexicalViolationImmidiaImmediately(Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC(" Condition components:\n"
                                 "    1. &lexical\n"
                                 "    2. &who: ~a\n"
                                 "    3. &message: ~s\n"
                                 "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));

    const Object condition = sysGetOutputStringEx(1, &stringOutputPort);
    theVM->getErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
    theVM->throwException(condition);
}


void scheme::callLexicalViolationAfter(Object who, Object message, Object irritants)
{
    raiseAfter(UC("&lexical-rcd"), UC("&lexical"), 0, who, message, irritants);
}
void scheme::callIoFileNameErrorAfter(Object who, Object message, Object irritants)
{
    raiseAfter(UC("&i/o-filename-rcd"), UC("&i/o-filename"), 1, who, message, irritants);
}

void scheme::callErrorAfter(Object who, Object message, Object irritants /* = Object::Nil */)
{
    raiseAfter(UC("&error-rcd"), UC("&error"), 0, who, message, irritants);
}

Object scheme::assertionViolationEx(int argc, const Object* argv)
{
    DeclareProcedureName("assertion-violation");
    checkArgumentLengthAtLeast(2);
    const Object who = argv[0];
    if (!who.isFalse() && !who.isString() && !who.isSymbol()) {
        callWrongTypeOfArgumentViolationAfter(procedureName, "symbol, string or #f", who);
        return Object::Undef;
    }
    argumentCheckString(1, message);

    Object irritants = Object::Nil;
    for (int i = 2; i < argc; i++) {
        irritants = Object::cons(argv[i], irritants);
    }
    callAssertionViolationAfter(who, message, irritants);
    return Object::Undef;
}

Object scheme::errorEx(int argc, const Object* argv)
{
    DeclareProcedureName("error");
    checkArgumentLengthAtLeast(2);
    const Object who = argv[0];
    if (!who.isFalse() && !who.isString() && !who.isSymbol()) {
        callWrongTypeOfArgumentViolationAfter(procedureName, "symbol, string or #f", who);
        return Object::Undef;
    }
    VM_LOG1("<~a>", argv[0]);
    VM_LOG1("<~a>", argv[1]);
    argumentCheckString(1, message);
    Object irritants = Object::Nil;
    for (int i = 2; i < argc; i++) {
        irritants = Object::cons(argv[i], irritants);
    }
    callErrorAfter(who, message, irritants);
    return Object::Undef;
}


// private
Object makeWhoCondition(Object who)
{
    return makeCondition(UC("&who-rcd"), who);
}

Object makeMessageCondition(Object message)
{
    return makeCondition(UC("&message-rcd"), message);
}

Object makeIrritantsCondition(Object irritants)
{
    return makeCondition(UC("&irritants-rcd"), irritants);
}

Object makeCondition(const ucs4char* rcdName)
{
    const Object rcd = theVM->getTopLevelGlobalValue(Symbol::intern(rcdName));
    MOSH_ASSERT(!rcd.isFalse());
    return theVM->callClosure0(rcd.toRecordConstructorDescriptor()->makeConstructor());
}

Object makeCondition(const ucs4char* rcdName, Object content)
{
    const Object rcd = theVM->getTopLevelGlobalValue(Symbol::intern(rcdName));
    MOSH_ASSERT(!rcd.isFalse());
    return theVM->callClosure1(rcd.toRecordConstructorDescriptor()->makeConstructor(), content);
}

void raiseAfter(const ucs4char* errorRcdName,
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
        if (irritants.isPair()) {
            conditions = Object::cons(makeIrritantsCondition(irritants), conditions);
        }

        conditions = Object::cons(makeMessageCondition(message), conditions);

        if (!who.isFalse()) {
            conditions = Object::cons(makeWhoCondition(who), conditions);
        }

        MOSH_ASSERT(argumentCount >= 0 && argumentCount <= 1);
        if (0 == argumentCount) {
            conditions = Object::cons(makeCondition(errorRcdName), conditions);
        } else {
            conditions = Object::cons(makeCondition(errorRcdName, Object::Nil), conditions);
        }
        condition = Object::makeCompoundCondition(conditions);
    } else {
        const Object stringOutputPort = Object::makeStringOutputPort();
        TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

        textualOutputPort->format(UC(" Condition components:\n"
                                     "    1. ~a\n"
                                     "    2. &who: ~a\n"
                                     "    3. &message: ~s\n"
                                     "    4. &irritants: ~a\n"), Pair::list4(Object::makeString(errorName), who, message, irritants));

        condition = sysGetOutputStringEx(1, &stringOutputPort);
    }

    const Object raiseProcedure = theVM->getTopLevelGlobalValueOrFalse(Symbol::intern(UC("raise")));

    // Error occured before (raise ...) is defined.
    if (raiseProcedure.isFalse()) {
        theVM->getErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger1(raiseProcedure, condition);
    }
}


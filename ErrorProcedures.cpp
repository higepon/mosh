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

#include "ErrorProcedures.h"
#include "ProcedureMacro.h"
#include "PortProcedures.h"

using namespace scheme;

static Object makeMessageCondition(Object message);

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
                              Pair::list3(Object::makeInt(startCounts),
                                          Object::makeInt(endCounts),
                                          Object::makeInt(gotCounts)));
    const Object message = sysGetOutputStringEx(1, &stringOutputPort);
    callAssertionViolationAfter(who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsViolationAfter(Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC("wrong number of arguments (required ~d, got ~d)"),
                              Pair::list2(Object::makeInt(requiredCounts),
                                          Object::makeInt(gotCounts)));
    const Object message = sysGetOutputStringEx(1, &stringOutputPort);
    callAssertionViolationAfter(who, message, irritants);
}

void scheme::callWrongNumberOfArgumentsAtLeastViolationAfter(Object who, int requiredCounts, int gotCounts, Object irritants /* Object::Nil */ )
{
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

    textualOutputPort->format(UC("wrong number of arguments (required at least ~d, got ~d)"),
                              Pair::list2(Object::makeInt(requiredCounts),
                                          Object::makeInt(gotCounts)));
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
    MOSH_ASSERT(theVM);
    MOSH_ASSERT(irritants.isPair() || irritants.isNil());
    MOSH_ASSERT(who.isSymbol() || who.isString() || who.isFalse());
    MOSH_ASSERT(message.isString());
    Object condition = Object::Nil;
    if (theVM->isR6RSMode()) {
        Object conditions = Object::Nil;
        if (irritants.isPair()) {
            const Object irritantsRcd = theVM->getTopLevelGlobalValue(UC("&irritants-rcd"));
            const Object irritantsCondition = theVM->callClosure1(irritantsRcd.toRecordConstructorDescriptor()->makeConstructor(), irritants);
            conditions = Object::cons(irritantsCondition, conditions);
        }

        const Object messageCondition = makeMessageCondition(message);
        conditions = Object::cons(messageCondition, conditions);

        if (!who.isFalse()) {
            const Object whoRcd = theVM->getTopLevelGlobalValue(UC("&who-rcd"));
            const Object whoCondition = theVM->callClosure1(whoRcd.toRecordConstructorDescriptor()->makeConstructor(), who);
            conditions = Object::cons(whoCondition, conditions);
        }

        const Object assertionViolationRcd = theVM->getTopLevelGlobalValue(UC("&assertion-rcd"));
        const Object assertionViolationCondition = theVM->callClosure0(assertionViolationRcd.toRecordConstructorDescriptor()->makeConstructor());
        conditions = Object::cons(assertionViolationCondition, conditions);

        condition = Object::makeCompoundCondition(conditions);
    } else {
        const Object stringOutputPort = Object::makeStringOutputPort();
        TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

        textualOutputPort->format(UC(" Condition components:\n"
                                     "    1. &assertion\n"
                                     "    2. &who: ~a\n"
                                     "    3. &message: ~s\n"
                                     "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));

        condition = sysGetOutputStringEx(1, &stringOutputPort);
    }

    const Object raiseProcedure = theVM->getTopLevelGlobalValueOrFalse(UC("raise"));

    // Error occured before (raise ...) is defined.
    if (raiseProcedure.isFalse()) {
        theVM->getErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger1(raiseProcedure, condition);
    }
}

void scheme::callIoFileNameErrorAfter(Object who, Object message, Object irritants)
{
    MOSH_ASSERT(theVM);
    MOSH_ASSERT(irritants.isPair() || irritants.isNil());
    MOSH_ASSERT(who.isSymbol() || who.isString() || who.isFalse());
    MOSH_ASSERT(message.isString());
    Object condition = Object::Nil;
    if (theVM->isR6RSMode()) {
        Object conditions = Object::Nil;
        if (irritants.isPair()) {
            const Object irritantsRcd = theVM->getTopLevelGlobalValue(UC("&irritants-rcd"));
            const Object irritantsCondition = theVM->callClosure1(irritantsRcd.toRecordConstructorDescriptor()->makeConstructor(), irritants);
            conditions = Object::cons(irritantsCondition, conditions);
        }

        const Object messageCondition = makeMessageCondition(message);
        conditions = Object::cons(messageCondition, conditions);

        if (!who.isFalse()) {
            const Object whoRcd = theVM->getTopLevelGlobalValue(UC("&who-rcd"));
            const Object whoCondition = theVM->callClosure1(whoRcd.toRecordConstructorDescriptor()->makeConstructor(), who);
            conditions = Object::cons(whoCondition, conditions);
        }

        const Object errorRcd = theVM->getTopLevelGlobalValue(UC("&i/o-filename-rcd"));
        const Object errorCondition = theVM->callClosure1(errorRcd.toRecordConstructorDescriptor()->makeConstructor(), "hige");
        conditions = Object::cons(errorCondition, conditions);

        condition = Object::makeCompoundCondition(conditions);
    } else {
        const Object stringOutputPort = Object::makeStringOutputPort();
        TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

        textualOutputPort->format(UC(" Condition components:\n"
                                     "    1. &error\n"
                                     "    2. &who: ~a\n"
                                     "    3. &message: ~s\n"
                                     "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));

        condition = sysGetOutputStringEx(1, &stringOutputPort);
    }

    const Object raiseProcedure = theVM->getTopLevelGlobalValueOrFalse(UC("raise"));

    // Error occured before (raise ...) is defined.
    if (raiseProcedure.isFalse()) {
        theVM->getErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger1(raiseProcedure, condition);
    }
}

void scheme::callErrorAfter(Object who, Object message, Object irritants /* = Object::Nil */)
{
    MOSH_ASSERT(theVM);
    MOSH_ASSERT(irritants.isPair() || irritants.isNil());
    MOSH_ASSERT(who.isSymbol() || who.isString() || who.isFalse());
    MOSH_ASSERT(message.isString());
    Object condition = Object::Nil;
    if (theVM->isR6RSMode()) {
        Object conditions = Object::Nil;
        if (irritants.isPair()) {
            const Object irritantsRcd = theVM->getTopLevelGlobalValue(UC("&irritants-rcd"));
            const Object irritantsCondition = theVM->callClosure1(irritantsRcd.toRecordConstructorDescriptor()->makeConstructor(), irritants);
            conditions = Object::cons(irritantsCondition, conditions);
        }

        const Object messageCondition = makeMessageCondition(message);
        conditions = Object::cons(messageCondition, conditions);

        if (!who.isFalse()) {
            const Object whoRcd = theVM->getTopLevelGlobalValue(UC("&who-rcd"));
            const Object whoCondition = theVM->callClosure1(whoRcd.toRecordConstructorDescriptor()->makeConstructor(), who);
            conditions = Object::cons(whoCondition, conditions);
        }

        const Object errorRcd = theVM->getTopLevelGlobalValue(UC("&error-rcd"));
        const Object errorCondition = theVM->callClosure0(errorRcd.toRecordConstructorDescriptor()->makeConstructor());
        conditions = Object::cons(errorCondition, conditions);

        condition = Object::makeCompoundCondition(conditions);
    } else {
        const Object stringOutputPort = Object::makeStringOutputPort();
        TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();

        textualOutputPort->format(UC(" Condition components:\n"
                                     "    1. &error\n"
                                     "    2. &who: ~a\n"
                                     "    3. &message: ~s\n"
                                     "    4. &irritants: ~a\n"), Pair::list3(who, message, irritants));

        condition = sysGetOutputStringEx(1, &stringOutputPort);
    }

    const Object raiseProcedure = theVM->getTopLevelGlobalValueOrFalse(UC("raise"));

    // Error occured before (raise ...) is defined.
    if (raiseProcedure.isFalse()) {
        theVM->getErrorPort().toTextualOutputPort()->display(" WARNING: Error occured before (raise ...) defined\n");
        theVM->throwException(condition);
    } else {
        theVM->setAfterTrigger1(raiseProcedure, condition);
    }
}


Object scheme::assertionViolationEx(int argc, const Object* argv)
{
    DeclareProcedureName("assertion-violation");
    checkArgumentLengthBetween(2, 3);
    const Object who = argv[0];
    if (!who.isFalse() && !who.isString() && !who.isSymbol()) {
        callWrongTypeOfArgumentViolationAfter(procedureName, "symbol, string or #f", who);
        return Object::Undef;
    }
    argumentCheckString(1, message);
    const Object irritants = (argc == 3) ? argv[2] : Object::Nil;
    if (!irritants.isNil() && !irritants.isPair()) {
        callWrongTypeOfArgumentViolationAfter(procedureName, "list", irritants);
        return Object::Undef;
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
    argumentCheckString(1, message);
    Object irritants = Object::Nil;
    for (int i = 2; i < argc; i++) {
        irritants = Object::cons(argv[i], irritants);
    }
    callErrorAfter(who, message, irritants);
    return Object::Undef;
}


// private
Object makeMessageCondition(Object message)
{
    const Object messageRcd = theVM->getTopLevelGlobalValue(UC("&message-rcd"));
    const Object messageCondition = theVM->callClosure1(messageRcd.toRecordConstructorDescriptor()->makeConstructor(), message);
    return messageCondition;
}

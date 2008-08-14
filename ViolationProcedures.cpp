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

#include "ViolationProcedures.h"
#include "VM.h"

using namespace scheme;

extern scheme::VM* theVM;

// caller should check type of arguments.
void scheme::callAssertionViolationAfter(Object who, Object message, Object irritants)
{
    Object conditions = Object::Nil;
    if (irritants.isPair()) {
        const Object irritantsRcd = theVM->getTopLevelGlobalValue(UC("&irritants-rcd"));
        const Object irritantsCondition = theVM->callClosure(irritantsRcd.toRecordConstructorDescriptor()->makeConstructor(), irritants);
        conditions = Object::cons(irritantsCondition, conditions);
    }

    if (who.isSymbol()) {
        const Object whoRcd = theVM->getTopLevelGlobalValue(UC("&who-rcd"));
        const Object whoCondition = theVM->callClosure(whoRcd.toRecordConstructorDescriptor()->makeConstructor(), who);
        conditions = Object::cons(whoCondition, conditions);
    }

    const Object assertionViolationRcd = theVM->getTopLevelGlobalValue(UC("&assertion-rcd"));
    const Object assertionViolationCondition = theVM->callClosure0(assertionViolationRcd.toRecordConstructorDescriptor()->makeConstructor());
    conditions = Object::cons(assertionViolationCondition, conditions);
    Object proc =  theVM->getTopLevelGlobalValue(UC("raise"));
    theVM->setAfterTrigger1(proc, Object::makeCompoundCondition(conditions));

   // callClosure -> callClosure1
    // makeCompoundCondition を可変長にしようぜ。
//todo message-condition
}


Object scheme::assertionViolationEx(int argc, const Object* argv)
{
    // todo
    callAssertionViolationAfter(argv[0], argv[1], Object::Nil);
    return Object::Undef;
}

/*
 * ViolationProcedures.h - violations.
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
 *  $Id: ViolationProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_VIOLATION_PROCEDURES__
#define __SCHEME_VIOLATION_PROCEDURES__

#include "scheme.h"

namespace scheme {

    Object throwEx(int argc, const Object* argv);

    void callAssertionViolationAfter(Object who, Object message, Object irritants = Object::Nil);
    void callAssertionViolationImmidiaImmediately(Object who, Object message, Object irritants = Object::Nil);
    void callNotImplementedAssertionViolationAfter(Object who, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsViolationAfter(Object who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsAtLeastViolationAfter(Object who, int requiredCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongNumberOfArgumentsBetweenViolationAfter(Object who, int startCounts, int endCounts, int gotCounts, Object irritants = Object::Nil);
    void callWrongTypeOfArgumentViolationAfter(Object who, Object requiredType, Object gotValue, Object irritants = Object::Nil);



}; // namespace scheme

#endif // __SCHEME_VIOLATION_PROCEDURES__

/*
 * ListProcedures.cpp - 
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
 *  $Id: ListProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Vector.h"
#include "SString.h"
#include "Closure.h"
#include "Record.h"
#include "Equivalent.h"
#include "ListProcedures.h"
#include "ProcedureMacro.h"
#include "TextualOutputPort.h"

using namespace scheme;

Object scheme::memberEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("member");
    checkArgumentLength(2);

    const Object arg1 = argv[0];
    argumentCheckList(1, p);
    for (Object o = p; o != Object::Nil; o = o.cdr()) {
        if (o.car().equal(theVM, arg1)) {
            return o;
        }
    }
    return Object::False;
}

Object scheme::consEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cons");
    checkArgumentLength(2);
    return Object::cons(argv[0], argv[1]);
}

Object scheme::carEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("car");
    checkArgumentLength(1);
    argumentCheckPair(0, p);
    return p.car();
}

Object scheme::cdrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdr");
    checkArgumentLength(1);
    argumentCheckPair(0, p);
    return p.cdr();
}

Object scheme::sourceInfoEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("source-info");
    checkArgumentLength(1);
    const Object arg = argv[0];
    if (arg.isPair()) {
        return arg.sourceInfo();
    } else if (arg.isClosure()) {
        return arg.toClosure()->sourceInfo;
    } else {
        return Object::False;
    }
}

Object scheme::setSourceInfoDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-source-info!");
    checkArgumentLength(2);

    const Object target = argv[0];
    const Object sourceInfo = argv[1];
    if (target.isClosure()) {
        target.toClosure()->sourceInfo = sourceInfo;
    } else if (target.isPair()) {
        target.toPair()->sourceInfo = sourceInfo;
    } else {
    }
    return target;
}

Object scheme::nullPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("null?!");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isNil());
}

Object scheme::setCarDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-car!");
    checkArgumentLength(2);
    argumentCheckPair(0, p);

    p.car() = argv[1];
    return Object::UnBound;
}

Object scheme::setCdrDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-cdr!");
    checkArgumentLength(2);
    argumentCheckPair(0, p);

    p.cdr() = argv[1];
    return Object::UnBound;
}

Object scheme::reverseEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("reverse");
    checkArgumentLength(1);
    argumentCheckList(0, p);
    return Pair::reverse(p);
}

Object scheme::listPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("list?");
    checkArgumentLength(1);
    return Object::makeBool(Pair::isList(argv[0]));
//     Object obj = argv[0];
//     Object seen = obj;
//     for (;;) {
//         if (obj.isNil()) return Object::True;
//         if (!obj.isPair()) return Object::False; // dot pair
//         obj = obj.cdr();
//         if (obj.isNil()) return Object::True;
//         if (!obj.isPair()) return Object::False; // dot pair
//         obj = obj.cdr();
//         seen = seen.cdr();
//         if (obj == seen) return Object::False; // circular
//     }
//     callAssertionViolationAfter(procedureName, "internal bug?");
//     return Object::Undef;
}

Object scheme::memqEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("memq");
    checkArgumentLength(2);

    const Object arg1 = argv[0];
    argumentCheckList(1, p);

    for (Object o = p; o != Object::Nil; o = o.cdr()) {
        if (o.car() == arg1) {
            return o;
        }
    }
    return Object::False;
}

Object scheme::memvEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("memv");
    checkArgumentLength(2);

    const Object arg1 = argv[0];
    argumentCheckList(1, p);
    for (Object o = p; o != Object::Nil; o = o.cdr()) {
        if (eqv(o.car(), arg1)) {
            return o;
        }
    }
    return Object::False;
}

Object scheme::assqEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("assq");
    checkArgumentLength(2);

    const Object obj = argv[0];
    argumentCheckList(1, list);

    for (Object o = list; o != Object::Nil; o = o.cdr()) {
        if (o.car().car() == obj) {
            return o.car();
        }
    }
    return Object::False;
}

Object scheme::appendEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("append");
    if (0 == argc) return Object::Nil;
    Object ret = argv[argc - 1];
    for (int i = argc - 2; i >= 0; i--) {
        argumentCheckList(i, p);
        ret = Pair::append2(p, ret);
    }
    return ret;
}

Object scheme::append2Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("append2");
    checkArgumentLength(2);

    argumentCheckPair(0, list);
    return Pair::append2(list, argv[1]);
}

Object scheme::appendDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("append!");
    if (0 == argc) return Object::Nil;
    Object ret = argv[argc - 1];
    for (int i = argc - 2; i >= 0; i--) {
        argumentCheckList(i, p);
        ret = Pair::appendD2(p, ret);
    }
    return ret;
}

Object scheme::uniq(Object list)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        if (!memq(p.car(), ret).isFalse()) {
            continue;
        } else {
            ret = Object::cons(p.car(), ret);
        }
    }
    return ret;
}

Object scheme::lengthEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("length");
    checkArgumentLength(1);
    argumentCheckList(0, list);
    int ret = 0;
    for (Object p = list; !p.isNil(); p = p.cdr()) {
        if (!p.isPair()) {
            callAssertionViolationAfter(theVM, procedureName, "proper-list required", Pair::list1(list));
            return Object::Undef;
        }
        ret++;
    }
    return Object::makeFixnum(ret);
}

Object scheme::listTovectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("list->vector");
    checkArgumentLength(1);

    argumentCheckList(0, list);

    if (list.isPair()) {
        return Object::makeVector(list);
    } else {
        return Object::makeVector(0);
    }
}


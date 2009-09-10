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
#include "Ratnum.h"
#include "Flonum.h"
#include "Equivalent.h"
#include "ListProcedures.h"
#include "ProcedureMacro.h"
#include "TextualOutputPort.h"
#include "ExecutableMemory.h"

using namespace scheme;

// Originaly from Ypsilon Scheme.
static Object do_transpose(int each_len, int argc, Object* argv)
{
    Object ans = Object::Nil;
    Object ans_tail = Object::Nil;
    for (int i = 0; i < each_len; i++) {
        Object elt = Object::cons(argv[0].car(), Object::Nil);
        Object elt_tail = elt;
        argv[0] = argv[0].cdr();
        for (int n = 1; n < argc; n++) {
            elt_tail.cdr() = Object::cons(argv[n].car(), Object::Nil);
            elt_tail = elt_tail.cdr();
            argv[n] = argv[n].cdr();
        }
        if (ans == Object::Nil) {
            ans = Object::cons(elt, Object::Nil);
            ans_tail = ans;
        } else {
            ans_tail.cdr() = Object::cons(elt, Object::Nil);
            ans_tail = ans_tail.cdr();
        }
    }
    return ans;
}

Object scheme::returnJitEx(VM* theVM, int argc, const Object* argv)
{
    ExecutableMemory* mem = new ExecutableMemory(256);
    if (!mem->allocate()) {
        fprintf(stderr, "returnJit Error");
        exit(-1);
    }

    mem->push(0x48); // movq   $0xd,0x8(%rdi)
    mem->push(0xc7); 
    mem->push(0x47); 
    mem->push(0x08);
    mem->push(0x0d);
    mem->push(0x00);
    mem->push(0x00);
    mem->push(0x00);

    mem->push(0x48);// movq    0x8(%rdi),%rax
    mem->push(0x8b);
    mem->push(0x47);
    mem->push(0x08);

    mem->push(0xc3);// retq


    uint8_t* address = mem->address();

    return Object::makeCProcedure(((Object (*)(VM* vm, int, const Object*))address));
}

Object scheme::listRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("list-ref");
    checkArgumentLength(2);
    argumentAsFixnum(1, index);

    Object obj = argv[0];
    while (--index >= 0) {
        if (obj.isPair()) {
            obj = obj.cdr();
        } else {
            return callAssertionViolationAfter(theVM, procedureName, "index out of range", Pair::list2(argv[0], argv[1]));
        }
    }

    if (obj.isPair()) {
        return obj.car();
    } else {
        return callAssertionViolationAfter(theVM, procedureName, "proper list required", Pair::list2(argv[0], argv[1]));
    }
}

Object scheme::listTailEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("list-tail");
    checkArgumentLength(2);
    argumentAsFixnum(1, index);

    if (index < 0) {
        return callAssertionViolationAfter(theVM, procedureName, "index out of range", Pair::list2(argv[0], argv[1]));
    }
    Object obj = argv[0];

    while (--index >= 0) {
        if (obj.isPair()) {
            obj = obj.cdr();
        } else {
            return callAssertionViolationAfter(theVM, procedureName, "proper list required", Pair::list2(argv[0], argv[1]));
        }
    }
    return obj;
}


Object scheme::memberEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("member");
    checkArgumentLength(2);

    const Object arg1 = argv[0];
    argumentCheckList(1, p);
    Equal e;
    for (Object o = p; o != Object::Nil; o = o.cdr()) {
        if (e.equal(o.car(), arg1)) {
            return o;
        }
    }
    return Object::False;
}

Object scheme::consMulEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cons*");
    checkArgumentLengthAtLeast(1);
    if (argc == 1) {
        return argv[0];
    } else {
        Object obj = Object::cons(argv[0], Object::Nil);
        Object tail = obj;
        for (int i = 1; i < argc - 1; i++) {
            Object e = Object::cons(argv[i], Object::Nil);
            tail.cdr()  = e;
            tail = e;
        }
        tail.cdr() = argv[argc - 1];
        return obj;
    }

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

Object scheme::assvEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("assv");
    checkArgumentLength(2);

    const Object obj = argv[0];
    argumentCheckList(1, list);

    for (Object o = list; o != Object::Nil; o = o.cdr()) {
        if (eqv(o.car().car(), obj)) {
            return o.car();
        }
    }
    return Object::False;
}

Object scheme::assocEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("assoc");
    checkArgumentLength(2);
    const Object obj = argv[0];
    argumentCheckList(1, list);

    Equal e;
    for (Object o = list; o != Object::Nil; o = o.cdr()) {
        if (e.equal(o.car().car(), obj)) {
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

// (make-vector k . fill)
Object scheme::makeVectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-vector");
    checkArgumentLengthBetween(1, 2);
    argumentAsFixnum(0, k);
    if (argc == 1) {
        return Object::makeVector(k);
    } else {
        return Object::makeVector(k, argv[1]);
    }
}

// (vector-length v)
Object scheme::vectorLengthEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vector-length");
    checkArgumentLength(1);
    argumentAsVector(0, v);
    return Object::makeFixnum(v->length());
}

// (vector-ref v k)
Object scheme::vectorRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vector-ref");
    checkArgumentLength(2);
    argumentAsVector(0, v);
    argumentAsFixnum(1, k);
    if (v->isValidIndex(k)) {
        return v->ref(k);
    } else {
        callAssertionViolationAfter(theVM,
                                    procedureName,
                                    "index out of range",
                                    L1(argv[1]));

        return Object::Undef;
    }
}

// (vector-set! v k value)
Object scheme::vectorSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vector-set!");
    checkArgumentLength(3);
    argumentAsVector(0, v);
    argumentAsFixnum(1, k);
    if (v->isValidIndex(k)) {
        v->set(k, argv[2]);
    } else {
        callAssertionViolationAfter(theVM,
                                    procedureName,
                                    "index out of range",
                                    L1(argv[1]));
    }
    return Object::Undef;
}

Object scheme::caaaarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caaaar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caaadrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caaadr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caaarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caaar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caadarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caadar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caaddrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caaddr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caadrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caadr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::cadaarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cadaar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::cadadrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cadadr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::cadarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cadar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caddarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caddar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::cadddrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cadddr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::caddrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("caddr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::cadrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cadr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    return p;
}
Object scheme::cdaaarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdaaar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdaadrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdaadr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdaarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdaar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdadarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdadar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdaddrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdaddr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdadrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdadr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cddaarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cddaar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cddadrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cddadr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cddarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cddar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdddarEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdddar");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.car();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cddddrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cddddr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cdddrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cdddr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}
Object scheme::cddrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("cddr");
    checkArgumentLength(1);
    argumentCheckPair(0, q);
    Object p = q;
    p = p.cdr();
    if (!p.isPair()) {
        callAssertionViolationAfter(theVM, procedureName, "pair required", Pair::list1(argv[0]));
        return Object::Undef;
    }
    p = p.cdr();
    return p;
}

Object scheme::listTransposeAddEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("list-transpose+");
    checkArgumentLengthAtLeast(1);
    const Object lst0 = argv[0];
    if (!lst0.isList()) {
        return Object::False;
    }
    const int length = Pair::length(lst0);
    for (int i = 1; i < argc; i++) {
        const Object lst = argv[i];
        if (lst.isList()) {
            if (Pair::length(lst) != length) {
                return Object::False;
            }
        } else {
            return Object::False;
        }
    }
    return do_transpose(length, argc, /* un-const, we know it's safe */(Object*)argv);
}

// (list . obj)
// gambit-benchmarks deriv requires this.
Object scheme::listEx(VM* theVM, int argc, const Object* argv)
{
    Object obj = Object::Nil;
    for (int i = argc - 1; i >= 0; i--) {
        obj = Object::cons(argv[i], obj);
    }
    return obj;
}

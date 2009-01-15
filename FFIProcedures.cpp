/*
 * FFIProcedures.cpp -
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: FFIProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <errno.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "VM.h"
#include "ProcedureMacro.h"
#include "FFIProcedures.h"
#include "FFI.h"
#include "Bignum.h"

using namespace scheme;

static intptr_t callStub(uintptr_t func, intptr_t* frame, int argc)
{
    const int bytes = (argc * sizeof(intptr_t) + 15) & ~15;
    intptr_t ret;
    asm volatile(
        "movl    %%esp, %%edx ;"
        "subl    %1   , %%esp ;"
        "movl    %%esp, %%edi ;" // copy arguments to stack
        "rep                  ;"
        "movsb                ;"
        "movl    %%edx, %%edi ;"
        "call    *%%eax       ;"
        "movl    %%edi, %%esp ;"
        : "=a" (ret)
        : "c" (bytes), "S" (frame), "0" (func) // c:ecx, S:esi
        : "edi", "edx", "memory"); // we have a memory destruction.
    return ret;
}

Object scheme::internalFfiCallTovoidMulEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->void*");
    checkArgumentLengthAtLeast(1);
    argumentAsUintptr_t(0, func, "Invalid FFI function");

    CStack cstack;
    for (int i = 1; i < argc; i++) {
        if (!cstack.push(argv[i])) {
            callAssertionViolationAfter(theVM, procedureName, "argument error", L2(cstack.getLastError(),
                                                                                   argv[i]));
            return Object::Undef;
        }
    }

    const uintptr_t ret = callStub(func, cstack.frame(), argc);
    return Bignum::makeIntegerFromUintprt_t(ret);
}

Object scheme::internalFfiCallTovoidEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->void");
    checkArgumentLengthAtLeast(1);
    argumentAsUintptr_t(0, func, "Invalid FFI function");
    CStack cstack;
    for (int i = 1; i < argc; i++) {
        if (!cstack.push(argv[i])) {
            callAssertionViolationAfter(theVM, procedureName, "argument error", L2(cstack.getLastError(),
                                                                                   argv[i]));
            return Object::Undef;
        }
    }

    callStub(func, cstack.frame(), argc);
    return Object::Undef;
}

Object scheme::internalFfiCallTointEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->int");
    checkArgumentLengthAtLeast(1);
    argumentAsUintptr_t(0, func, "Invalid FFI function");

    CStack cstack;
    for (int i = 1; i < argc; i++) {
        if (!cstack.push(argv[i])) {
            callAssertionViolationAfter(theVM, procedureName, "argument error", L2(cstack.getLastError(),
                                                                                   argv[i]));
            return Object::Undef;
        }
    }

    const intptr_t ret = callStub(func, cstack.frame(), argc);
    return Bignum::makeIntegerFromIntprt_t(ret);
}
Object scheme::internalFfiLookupEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-lookup");
    checkArgumentLength(2);
    argumentAsUintptr_t(0, handle, "invalid shared library handle");
    argumentAsString(1, name);

    void* symbol = FFI::lookup((void*)handle, name->data().ascii_c_str());

    if (NULL == symbol) {
        return Object::False;
    } else {
        return Bignum::makeIntegerFromUintprt_t(reinterpret_cast<uintptr_t>(symbol));
    }
}

Object scheme::internalFfiOpenEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-open");
    checkArgumentLength(1);
    argumentAsString(0, name);
    void* handle = FFI::open(name->data().ascii_c_str());
    if (NULL == handle) {
        callAssertionViolationAfter(theVM, procedureName, "shared library not found", L2(FFI::lastError(), argv[0]));
        return Object::Undef;
    } else {
        return Bignum::makeIntegerFromUintprt_t(reinterpret_cast<uintptr_t>(handle));
    }
}

Object scheme::internalFfiVoidMulTostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-void*->string");
    checkArgumentLength(1);
    argumentAsUintptr_t(0, p, "pointer required");
    return Object::makeString((char*)p);
}

Object scheme::internalFfiPointerRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-pinter-ref");
    checkArgumentLength(1);
    argumentAsUintptr_t(0, p, "pointer required");
    return Bignum::makeIntegerFromUintprt_t(*(uint32_t*)p);
}

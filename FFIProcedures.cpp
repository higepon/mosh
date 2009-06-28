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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <errno.h>
#define __STDC_LIMIT_MACROS
#include <stdint.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "VM.h"
#include "ProcedureMacro.h"
#include "FFIProcedures.h"
#include "FFI.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Symbol.h"


using namespace scheme;

#ifdef _WIN32
    #undef ARCH_IA32
#endif

#ifdef ARCH_IA32
#define FFI_SUPPORTED 1
#elif defined ARCH_X86_64
#define FFI_SUPPORTED 1
#endif

static double callStubDouble(uintptr_t func, CStack* cstack)
{
#ifdef ARCH_IA32
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
    double ret;
    asm volatile(
        "movl    %%esp, %%edx ;"
        "subl    %1   , %%esp ;"
        "movl    %%esp, %%edi ;" // copy arguments to stack
        "rep                  ;"
        "movsb                ;"
        "movl    %%edx, %%edi ;"
        "call    *%%eax       ;"
        "movl    %%edi, %%esp ;"
        : "=t" (ret) // t: st[0]
        : "c" (bytes), "S" (cstack->frame()), "a" (func) // c:ecx, S:esi a:eax
        : "edi", "edx", "memory"); // we have memory destructions.
    return ret;
#elif defined ARCH_X86_64
    double ret;
    if (cstack->count() == 0) {
        asm volatile("movq %%rsi, %%r10;"
                     "movsd   (%%rdi), %%xmm0 ;"
                     "movsd  8(%%rdi), %%xmm1 ;"
                     "movsd 16(%%rdi), %%xmm2 ;"
                     "movsd 24(%%rdi), %%xmm3 ;"
                     "movsd 32(%%rdi), %%xmm4 ;"
                     "movsd 40(%%rdi), %%xmm5 ;"
                     "movsd 48(%%rdi), %%xmm6 ;"
                     "movsd 56(%%rdi), %%xmm7 ;"
                     "movq 0(%%r10), %%rdi ;"   // register argument 1
                     "movq 8(%%r10), %%rsi ;"   // register argument 2
                     "movq 16(%%r10), %%rdx ;"  // register argument 3
                     "movq 24(%%r10), %%rcx ;"  // register argument 4
                     "movq 32(%%r10), %%r8 ;"   // register argument 5
                     "movq 40(%%r10), %%r9 ;"   // register argument 6
                     "call *%%rax ;"
                     "movq %%xmm0, %%rax ;"
                     : "=a" (ret)
                     : "0" (func), "S" (cstack->reg()), "D"(cstack->xmm())
                     : "memory"
            );
    } else {
        const int bytes = (cstack->count()) * 8;
        asm volatile(
            "movq %%rsi, %%r10;" // r10 for pointing register frame
            "movq %%rdx, %%r11;" // r11 for pointer to stack argument in frame
            "movq %%r11, %%r12;"
            "addq %%rcx, %%r12;"  // r12 end of stack argument in frame
            "movsd   (%%rdi), %%xmm0 ;"
            "movsd  8(%%rdi), %%xmm1 ;"
            "movsd 16(%%rdi), %%xmm2 ;"
            "movsd 24(%%rdi), %%xmm3 ;"
            "movsd 32(%%rdi), %%xmm4 ;"
            "movsd 40(%%rdi), %%xmm5 ;"
            "movsd 48(%%rdi), %%xmm6 ;"
            "movsd 56(%%rdi), %%xmm7 ;"
            "movq 0(%%r10), %%rdi ;"   // register argument 1
            "movq 8(%%r10), %%rsi ;"   // register argument 2
            "movq 16(%%r10), %%rdx ;"  // register argument 3
            "movq 24(%%r10), %%rcx ;"  // register argument 4
            "movq 32(%%r10), %%r8 ;"   // register argument 5
            "movq 40(%%r10), %%r9 ;"   // register argument 6
            "movq %%rsp, %%r13;"
            "1:   ;"
            "cmpq %%r11, %%r12;"
            "je 2f;"
            "movq 0(%%r11), %%r14;"
            "movq %%r14, 0(%%r13);"
            "addq $8, %%r11;"
            "addq $8, %%r13;"
            "jmp 1b;"
            "2:   ;"
            "call *%%rax ;"
            "movq %%xmm0, %%rax ;"
            : "=a" (ret)
            : "c" (bytes), "0" (func), "S" (cstack->reg()), "D" (cstack->xmm()), "d" (cstack->frame())
            : "memory"
            );
    }
    return ret;
#else
    return 0.0;
#endif
}

static intptr_t callStub(uintptr_t func, CStack* cstack)
{
#ifdef ARCH_IA32
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
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
        : "c" (bytes), "S" (cstack->frame()), "0" (func) // c:ecx, S:esi 0:match to the 0th output
        : "edi", "edx", "memory"); // we have memory destructions.
    return ret;
#elif defined ARCH_X86_64
    intptr_t ret = 0;
    if (cstack->count() == 0) {
        asm volatile("movq %%rsi, %%r10;"
                     "movsd   (%%rdi), %%xmm0 ;"
                     "movsd  8(%%rdi), %%xmm1 ;"
                     "movsd 16(%%rdi), %%xmm2 ;"
                     "movsd 24(%%rdi), %%xmm3 ;"
                     "movsd 32(%%rdi), %%xmm4 ;"
                     "movsd 40(%%rdi), %%xmm5 ;"
                     "movsd 48(%%rdi), %%xmm6 ;"
                     "movsd 56(%%rdi), %%xmm7 ;"
                     "movq 0(%%r10), %%rdi ;"   // register argument 1
                     "movq 8(%%r10), %%rsi ;"   // register argument 2
                     "movq 16(%%r10), %%rdx ;"  // register argument 3
                     "movq 24(%%r10), %%rcx ;"  // register argument 4
                     "movq 32(%%r10), %%r8 ;"   // register argument 5
                     "movq 40(%%r10), %%r9 ;"   // register argument 6
                     "call *%%rax ;"
                     : "=a" (ret)
                     : "0" (func), "S" (cstack->reg()), "D"(cstack->xmm())
                     : "memory"
            );
    } else {
        const int bytes = (cstack->count()) * 8;
        asm volatile(
            "movq %%rsi, %%r10;" // r10 for pointing register frame
            "movq %%rdx, %%r11;" // r11 for pointer to stack argument in frame
            "movq %%r11, %%r12;"
            "addq %%rcx, %%r12;"  // r12 end of stack argument in frame
            "movsd   (%%rdi), %%xmm0 ;"
            "movsd  8(%%rdi), %%xmm1 ;"
            "movsd 16(%%rdi), %%xmm2 ;"
            "movsd 24(%%rdi), %%xmm3 ;"
            "movsd 32(%%rdi), %%xmm4 ;"
            "movsd 40(%%rdi), %%xmm5 ;"
            "movsd 48(%%rdi), %%xmm6 ;"
            "movsd 56(%%rdi), %%xmm7 ;"
            "movq 0(%%r10), %%rdi ;"   // register argument 1
            "movq 8(%%r10), %%rsi ;"   // register argument 2
            "movq 16(%%r10), %%rdx ;"  // register argument 3
            "movq 24(%%r10), %%rcx ;"  // register argument 4
            "movq 32(%%r10), %%r8 ;"   // register argument 5
            "movq 40(%%r10), %%r9 ;"   // register argument 6
            "movq %%rsp, %%r13;"
            "1:   ;"
            "cmpq %%r11, %%r12;"
            "je 2f;"
            "movq 0(%%r11), %%r14;"
            "movq %%r14, 0(%%r13);"
            "addq $8, %%r11;"
            "addq $8, %%r13;"
            "jmp 1b;"
            "2:   ;"
            "call *%%rax ;"
            : "=a" (ret)
            : "c" (bytes), "0" (func), "S" (cstack->reg()), "D" (cstack->xmm()), "d" (cstack->frame())
            : "memory"
            );
    }

    return ret;

#else
    return 0;
#endif
}

Object scheme::internalFfiSupportedPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-supported?");
    checkArgumentLengthAtLeast(0);
#ifdef FFI_SUPPORTED
    return Object::True;
#else
    return Object::False;
#endif
}

Object scheme::internalFfiCallTodoubleEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->double");
#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

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

    const double ret = callStubDouble(func, &cstack);
    return Object::makeFlonum(ret);
}

Object scheme::internalFfiCallTovoidMulEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->void*");
#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

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

    const uintptr_t ret = callStub(func, &cstack);
    return Bignum::makeIntegerFromUnsigned<uintptr_t>(ret);
}

Object scheme::internalFfiCallTovoidEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->void");
#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

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

    callStub(func, &cstack);
    return Object::Undef;
}

Object scheme::internalFfiCallTointEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->int");
#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

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

    const intptr_t ret = callStub(func, &cstack);
    return Bignum::makeIntegerFromSigned<intptr_t>(ret);
}
Object scheme::internalFfiLookupEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-lookup");

#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

    checkArgumentLength(2);
    argumentAsUintptr_t(0, handle, "invalid shared library handle");
    argumentAsSymbol(1, name);

    ucs4string n = name->c_str();
    void* symbol = FFI::lookup((void*)handle, n.ascii_c_str());

    if (NULL == symbol) {
        return Object::False;
    } else {
        return Bignum::makeIntegerFromUnsigned<uintptr_t>(reinterpret_cast<uintptr_t>(symbol));
    }
}

Object scheme::internalFfiOpenEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-open");

#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

    checkArgumentLength(1);
    argumentAsString(0, name);
    void* handle = FFI::open(name->data().ascii_c_str());
    if (NULL == handle) {
        callAssertionViolationAfter(theVM, procedureName, "shared library not found", L2(FFI::lastError(), argv[0]));
        return Object::Undef;
    } else {
        return Bignum::makeIntegerFromUnsigned<uintptr_t>(reinterpret_cast<uintptr_t>(handle));
    }
}

Object scheme::internalFfiCallTostringOrZeroEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-call->string-or-zero");

#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

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

    const uintptr_t ret = callStub(func, &cstack);
    if (ret == 0) {
        return Object::makeFixnum(0);
    } else {

        return Object::makeString((char*)ret);
    }

}

Object scheme::internalFfiPointerTostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-pointer->string");

#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
    return Object::Undef;
#endif

    checkArgumentLength(1);
    argumentAsUintptr_t(0, p, "pointer required");
    return Object::makeString((char*)p);
}

// Object scheme::internalFfiPointerRefEx(VM* theVM, int argc, const Object* argv)
// {
//     DeclareProcedureName("%ffi-pinter-ref");

// #ifndef FFI_SUPPORTED
//     callAssertionViolationAfter(theVM, procedureName, "ffi not supported on this architecture");
//     return Object::Undef;
// #endif

//     checkArgumentLengthBetween(1, 2);
//     argumentAsUintptr_t(0, p, "pointer required");
//     if (argc == 1) {
//         return Bignum::makeIntegerFromUnsigned<uintptr_t>(*(uintptr_t*)p);
//     } else { // argc == 2
//         argumentAsFixnum(1, index);
//         return Bignum::makeIntegerFromUnsigned<uintptr_t>(((uintptr_t*)p)[index]);
//     }
// }

// Object scheme::internalFfiPointerValueEx(VM* theVM, int argc, const Object* argv)
// {
//     DeclareProcedureName("%ffi-pointer-value");
//     checkArgumentLength(1);
//     const Object obj = argv[0];
//     if (obj.isHeapObject()) {
//         // we know it may over flow, but enough
// //        return Object::makeFixnum(reinterpret_cast<uintptr_t>(reinterpret_cast<HeapObject*>(val)->obj));
//         return Bignum::makeIntegerFromUintprt_t(static_cast<uintptr_t>(reinterpret_cast<HeapObject*>(obj.val)->obj));

//     } else {
//         callAssertionViolationAfter(theVM, procedureName, "not a pointer object", L1(obj));
//         return Object::Undef;
//     }
// }

// (pointer? obj) => boolean
Object scheme::pointerPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isPointer());
}

// (pointer->integer pointer) => integer
Object scheme::pointerTointegerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer->integer");
    checkArgumentLength(1);
    argumentAsPointer(0, pointer);
    return Bignum::makeIntegerFromUnsigned<uintptr_t>(pointer->pointer());
}

// (integer->pointer integer) => pointer
Object scheme::integerTopointerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("integer->pointer");
    checkArgumentLength(1);
    argumentCheckExactInteger(0, integer);
    if (integer.isBignum()) {
        return Object::makePointer((void*)integer.toBignum()->toUintptr_t());
    } else {
        return Object::makePointer((void*)integer.toFixnum());
    }
}

Object scheme::pointerSetCDoubleDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-set-c-double!");
    checkArgumentLength(3);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    argumentAsFlonum(2, value);
    pointer->set<double>(offset, value->value());
    return Object::Undef;
}

Object scheme::pointerSetCFloatDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-set-c-float!");
    checkArgumentLength(3);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    argumentAsFlonum(2, value);
    pointer->set<float>(offset, value->value());
    return Object::Undef;
}

template <typename T> static Object pointerSet(const ucs4char* procedureName, T min, T max, VM* theVM, int argc, const Object* argv)
{
    checkArgumentLength(3);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    argumentCheckExactInteger(2, v);
    intptr_t value;
    if (v.isBignum()) {
        value = v.toBignum()->toIntptr_t();
    } else {
        value = v.toFixnum();
    }
    if (value >= static_cast<intptr_t>(min) && value <= static_cast<intptr_t>(max)) {
        pointer->set<T>(offset, static_cast<T>(value));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[1]));
    }
    return Object::Undef;
}

Object scheme::pointerSetCInt8DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<int8_t>(UC("pointer-set-c-int8!"), INT8_MIN, INT8_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCInt16DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<int16_t>(UC("pointer-set-c-int16!"), INT16_MIN, INT16_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCInt32DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<int32_t>(UC("pointer-set-c-int32!"), INT32_MIN, INT32_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCInt64DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<int64_t>(UC("pointer-set-c-int64!"), INT64_MIN, INT64_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCLongLongDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<long long>(UC("pointer-set-c-long-long!"), LLONG_MIN, LLONG_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCPointerDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<uintptr_t>(UC("pointer-set-c-pointer!"), 0, UINTPTR_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCLongDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<long>(UC("pointer-set-c-long!"), LONG_MIN, LONG_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCIntDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<int>(UC("pointer-set-c-int!"), INT_MIN, INT_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCShortDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<short>(UC("pointer-set-c-short!"), SHRT_MIN, SHRT_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCCharDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSet<char>(UC("pointer-set-c-char!"), SCHAR_MIN, SCHAR_MAX, theVM, argc, argv);
}

Object scheme::pointerRefCDoubleEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-ref-c-double");
    checkArgumentLength(2);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    return Object::makeFlonum(pointer->ref<double>(offset));
}

Object scheme::pointerRefCFloatEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-ref-c-float");
    checkArgumentLength(2);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    return Object::makeFlonum(pointer->ref<float>(offset));
}

template <typename T> static Object pointerRefU(const ucs4char* procedureName, VM* theVM, int argc, const Object* argv)
{
    checkArgumentLength(2);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    return Bignum::makeIntegerFromUnsigned<T>(pointer->ref<T>(offset));
}

template <typename T> static Object pointerRefS(const ucs4char* procedureName, VM* theVM, int argc, const Object* argv)
{
    checkArgumentLength(2);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    return Bignum::makeIntegerFromSigned<T>(pointer->ref<T>(offset));
}

Object scheme::pointerRefCUint8Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<uint8_t>(UC("pointer-ref-c-uint8"), theVM, argc, argv);
}

Object scheme::pointerRefCUint16Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<uint16_t>(UC("pointer-ref-c-uint16"), theVM, argc, argv);
}

Object scheme::pointerRefCUint32Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<uint32_t>(UC("pointer-ref-c-uint32"), theVM, argc, argv);
}

Object scheme::pointerRefCUint64Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<uint64_t>(UC("pointer-ref-c-uint64"), theVM, argc, argv);
}

Object scheme::pointerRefCInt8Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<int8_t>(UC("pointer-ref-c-int8"), theVM, argc, argv);
}

Object scheme::pointerRefCInt16Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<int16_t>(UC("pointer-ref-c-int16"), theVM, argc, argv);
}

Object scheme::pointerRefCInt32Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<int32_t>(UC("pointer-ref-c-int32"), theVM, argc, argv);
}

Object scheme::pointerRefCInt64Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<int64_t>(UC("pointer-ref-c-int64"), theVM, argc, argv);
}

Object scheme::pointerRefCPointerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-ref-c-pointer");
    checkArgumentLength(2);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    return Object::makePointer((void*)pointer->ref<uintptr_t>(offset));
}

Object scheme::pointerRefCUnsignedLongLongEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<unsigned long long>(UC("pointer-ref-c-unsigned-long-long"), theVM, argc, argv);
}

Object scheme::pointerRefCSignedLongLongEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<signed long long>(UC("pointer-ref-c-signed-long-long"), theVM, argc, argv);
}

Object scheme::pointerRefCUnsignedLongEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<unsigned long>(UC("pointer-ref-c-unsigned-long"), theVM, argc, argv);
}

Object scheme::pointerRefCSignedLongEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<signed long>(UC("pointer-ref-c-signed-long"), theVM, argc, argv);
}

Object scheme::pointerRefCUnsignedIntEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<unsigned int>(UC("pointer-ref-c-unsigned-int"), theVM, argc, argv);
}

Object scheme::pointerRefCSignedIntEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<signed int>(UC("pointer-ref-c-signed-int"), theVM, argc, argv);
}

Object scheme::pointerRefCUnsignedShortEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<unsigned short>(UC("pointer-ref-c-unsigned-short"), theVM, argc, argv);
}

Object scheme::pointerRefCSignedShortEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<signed short>(UC("pointer-ref-c-signed-short"), theVM, argc, argv);
}

Object scheme::pointerRefCUnsignedCharEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefU<unsigned char>(UC("pointer-ref-c-unsigned-char"), theVM, argc, argv);
}

Object scheme::pointerRefCSignedCharEx(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<signed char>(UC("pointer-ref-c-signed-char"), theVM, argc, argv);
}

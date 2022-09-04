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

/*
 *  A part of FFI functions are originally from Ypsilon Scheme by Yoshikatsu Fujita.
 *  They are ported or modified for Mosh.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <errno.h>
#ifdef _MSC_VER
#include <xmmintrin.h>
#else
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#include <stdint.h>
#endif
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ByteVector.h"
#include "VM.h"
#include "ProcedureMacro.h"
#include "FFIProcedures.h"
#include "FFI.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Symbol.h"
#include "ExecutableMemory.h"
#include "MultiVMProcedures.h"

using namespace scheme;

#ifdef ARCH_IA32
#define FFI_SUPPORTED 1
#elif defined ARCH_X86_64
#define FFI_SUPPORTED 1
#endif

class SynchronizedErrno : public gc_cleanup
{
    VM* vm_;
public:
    SynchronizedErrno(VM* vm) {
        vm_ = vm;
        errno = vm_->getErrno();
#if _MSC_VER
        SetLastError(vm_->getErrno());
#endif
    }
    ~SynchronizedErrno() override {
        vm_->setErrno(errno);
#if _MSC_VER
        vm_->setErrno(GetLastError());
#endif
    }
};

static void inline memoryStoreFence()
{
#ifdef _MSC_VER
    _mm_sfence();
#else
    asm volatile ("sfence" ::: "memory");
#endif
}

extern "C" void        c_callback_stub_intptr();
extern "C" void        c_callback_stub_int64();
extern "C" void        c_callback_stub_double();
extern "C" void        c_callback_stub_intptr_x64();

#if defined(ARCH_IA32) && !defined(WITHOUT_FFI_STUB)
Object callbackScheme(intptr_t uid, intptr_t signatures, intptr_t* stack)
{
    VM* vm = currentVM();
    Object closure = vm->getCallBackTrampoline(uid);
    MOSH_ASSERT(closure.isProcedure());

    const char* sigs = reinterpret_cast<const char*>(signatures);
    const int argc = strlen(sigs);

    int offset = 0;
    Object args = Object::Nil;
    for (int i = 0; i < argc; i++) {
        char c = *(const char*)(signatures + i);
        switch (c) {
        case 'L': {
            int8_t s8 = stack[offset];
            args = Object::cons(s8 ? Object::makeFixnum(1) : Object::makeFixnum(0), args);
            offset += 1;
        } break;
        case 'u': {
            int8_t s8 = stack[offset];
            args = Object::cons(Object::makeFixnum(s8), args);
            offset += 1;
        } break;
        case 'U': {
            uint8_t u8 = stack[offset];
            args = Object::cons(Object::makeFixnum(u8), args);
            offset += 1;
        } break;
        case 'b': {
            int16_t s16 = stack[offset];
            args = Object::cons(Object::makeFixnum(s16), args);
            offset += 1;
        } break;
        case 'B': {
            uint16_t u16 = stack[offset];
            args = Object::cons(Object::makeFixnum(u16), args);
            offset += 1;
        } break;
        case 'q': {
            int32_t s32 = stack[offset];
            args = Object::cons(Object::makeFixnum(s32), args);
            offset += 1;
        } break;
        case 'Q': {
            uint32_t u32 = stack[offset];
            args = Object::cons(Bignum::makeIntegerFromU32(u32), args);
            offset += 1;
        } break;
        case 'P': {
            uint32_t u32 = stack[offset];
            args = Object::cons(Object::makePointer((void*)u32), args);
            offset += 1;
        } break;
        case 'o': {
            int64_t* s64 = (int64_t*)(&stack[offset]);
            args = Object::cons(Bignum::makeIntegerFromS64(*s64), args);
            offset += 2;
        } break;
        case 'O': {
            uint64_t* u64 = (uint64_t*)(&stack[offset]);
            args = Object::cons(Bignum::makeIntegerFromS64(*u64), args);
            offset += 2;
        } break;
        case 'f': {
            float* f32 = (float*)(&stack[offset]);
            args = Object::cons(Object::makeFlonum(*f32), args);
            offset += 1;
        } break;
        case 'd': {
            double* f64 = (double*)(&stack[offset]);
            args = Object::cons(Object::makeFlonum(*f64), args);
            offset += 2;
        } break;

        default:
            MOSH_FATAL("fatal: invalid callback argument signature \n[exit]\n");
            break;
        }
    }
    // this reverse can be omitted for optimization
    args = Pair::reverse(args);
    return vm->apply(closure, args);
}

extern "C" double c_callback_double(intptr_t uid, intptr_t signatures, intptr_t* stack)
{
    Object ret = callbackScheme(uid, signatures, stack);
    if (ret.isNumber() && Arithmetic::isRealValued(ret)) {
        return Arithmetic::realToDouble(ret);
    } else {
        return 0.0;
    }
}

extern "C" intptr_t c_callback_intptr(uintptr_t uid, intptr_t signatures, intptr_t* stack)
{
    Object ret = callbackScheme(uid, signatures, stack);
    if (ret.isExactInteger()) {
        if (ret.isBignum()) {
            return ret.toBignum()->toS64();
        } else {
            return ret.toFixnum();
        }
    } else if (ret.isPointer()) {
        return ret.toPointer()->pointer();
    } else {
        return 0;
    }
}

extern "C" int64_t c_callback_int64(uintptr_t uid, intptr_t signatures, intptr_t* stack)
{
    Object ret = callbackScheme(uid, signatures, stack);
    if (ret.isExactInteger()) {
        if (ret.isBignum()) {
            return ret.toBignum()->toS64();
        } else {
            return ret.toFixnum();
        }
    } else {
        return 0;
    }
}

#ifndef _MSC_VER

struct CallBackTrampoline // gcc version
{
    uint8_t     mov_ecx_imm32;  // B9           : mov ecx, imm16/32
    uint32_t    imm32_uid;      // 00 00 00 00
    uint8_t     mov_eax_imm32;  // B8           ; mov eax, imm16/32
    uint32_t    imm32_stub;     // 00 00 00 00
    uint8_t     jmp_eax[2];     // FF 20        ; jmp [eax]
    uint8_t     ud2[2];         // 0F 0B
    intptr_t    stub;
    intptr_t    uid;
    intptr_t    signatures;
    char        signatures_buffer[CStack::MAX_ARGC];

public:
    CallBackTrampoline(intptr_t stub, Object closure, const char* sig)
    {
        strncpy(signatures_buffer, sig, sizeof(signatures_buffer));
        this->signatures = reinterpret_cast<intptr_t>(&signatures_buffer[0]);
        this->stub = stub;
        MOSH_ASSERT(closure.isProcedure());
        mov_ecx_imm32 = 0xB9;
        imm32_uid = reinterpret_cast<intptr_t>(&uid);
        mov_eax_imm32 = 0xB8;
        imm32_stub = static_cast<uint32_t>(stub);
        jmp_eax[0] = 0xFF;
        jmp_eax[1] = 0xe0;
        ud2[0] = 0x0F;
        ud2[1] = 0x0B;
        uid = currentVM()->registerCallBackTrampoline(closure);
        memoryStoreFence();
    }

    ~CallBackTrampoline()
    {
        currentVM()->unregisterCallBackTrampoline(uid);
    }

    static void* operator new(size_t size)
    {
        ExecutableMemory* ex = ::new ExecutableMemory(size);
        ex->allocate();
        return static_cast<void*>(ex->address());
    }
} __attribute__((packed));

#else // defined _MSC_VER

#pragma pack(push, 1)

struct CallBackTrampolineWithStub // gcc version
{
#define CODE_SIZE 48
#define STUBCODE_SIZE 40
#define OFF_UID 1
#define OFF_STUB 6
#define STUBOFF_STUB 1
    unsigned char code[CODE_SIZE];
    intptr_t    stub;
    intptr_t    uid;
    intptr_t    signatures;
    char        signatures_buffer[CStack::MAX_ARGC];

public:
    CallBackTrampolineWithStub(intptr_t stub, Object closure, const char* sig)
    {
        unsigned char thecode[CODE_SIZE] = {
           0xb9,                    /* change below */
           0x00, 0x00, 0x00, 0x00,  /* mov    UID,%ecx */
           0xba,                    /* change below */
           0x00, 0x00, 0x00, 0x00,  /* mov    FUNC_ADDR,%edx */
           0x55,                    /* push   %ebp */
           0x89, 0xe5,              /* mov    %esp,%ebp */
           0x83, 0xec, 0x18,        /* sub    $0x18,%esp */
           0x8b, 0x01,              /* mov    (%ecx),%eax */
           0x89, 0x04, 0x24,        /* mov    %eax,(%esp) */
           0x8b, 0x41, 0x04,        /* mov    0x4(%ecx),%eax */
           0x89, 0x44, 0x24, 0x04,  /* mov    %eax,0x4(%esp) */
           0x8d, 0x45, 0x08,        /* lea    0x8(%ebp),%eax */
           0x89, 0x44, 0x24, 0x08,  /* mov    %eax,0x8(%esp) */
           0xff, 0xd2,              /* call   *%edx */
           0x89, 0xec,              /* mov    %ebp,%esp */
           0x5d,                    /* pop    %ebp */
           0xc3,                    /* ret */
           0x0f, 0x0b,              /* UD2: generated code ends here */
           0x90, 0x90, 0x90, 0x90,  /* padding */
           0x90
        };
        strncpy(signatures_buffer, sig, sizeof(signatures_buffer));
        memcpy(&this->code,thecode,CODE_SIZE);
        this->signatures = reinterpret_cast<intptr_t>(&signatures_buffer[0]);
        this->stub = stub;//reinterpret_cast<intptr_t>(&stubcode);
        MOSH_ASSERT(closure.isProcedure());
        *reinterpret_cast<uint32_t*>(&this->code[OFF_UID])
            = reinterpret_cast<uint32_t>(&uid);
        *reinterpret_cast<intptr_t*>(&this->code[OFF_STUB])
            = stub;
        uid = currentVM()->registerCallBackTrampoline(closure);
        memoryStoreFence();
    }

    ~CallBackTrampolineWithStub()
    {
        currentVM()->unregisterCallBackTrampoline(uid);
    }

    static void* operator new(size_t size)
    {
        ExecutableMemory* ex = ::new ExecutableMemory(size);
        ex->allocate();
        return static_cast<void*>(ex->address());
    }
};

#pragma pack(pop)

#endif

#elif defined(ARCH_X86_64) && !defined(WITHOUT_FFI_STUB)

Object callbackScheme(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
{
    VM* vm = currentVM();
    Object closure = vm->getCallBackTrampoline(uid);
    MOSH_ASSERT(closure.isProcedure());

    int argc = strlen((const char*)signatures);
    Object args = Object::Nil;
    int reg_offset = 0;
    int sse_offset = 0;
    int stack_offset = 0;
    for (int i = 0; i < argc; i++) {
        char c = *(const char*)(signatures + i);
        switch (c) {
        case 'L': {
            int8_t s8;
            if (reg_offset < 6) {
                s8 = reg[reg_offset];
                reg_offset += 1;
            } else {
                s8 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(s8 ? Object::makeFixnum(1) : Object::makeFixnum(0), args);
        } break;
        case 'u': {
            int8_t s8;
            if (reg_offset < 6) {
                s8 = reg[reg_offset];
                reg_offset += 1;
            } else {
                s8 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Object::makeFixnum(s8), args);
        } break;
        case 'U': {
            uint8_t u8;
            if (reg_offset < 6) {
                u8 = reg[reg_offset];
                reg_offset += 1;
            } else {
                u8 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Object::makeFixnum(u8), args);
        } break;
        case 'b': {
            int16_t s16;
            if (reg_offset < 6) {
                s16 = reg[reg_offset];
                reg_offset += 1;
            } else {
                s16 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Object::makeFixnum(s16), args);
        } break;
        case 'B': {
            uint16_t u16;
            if (reg_offset < 6) {
                u16 = reg[reg_offset];
                reg_offset += 1;
            } else {
                u16 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Object::makeFixnum(u16), args);
        } break;
        case 'q': {
            int32_t s32;
            if (reg_offset < 6) {
                s32 = reg[reg_offset];
                reg_offset += 1;
            } else {
                s32 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Bignum::makeInteger(s32), args);
        } break;
        case 'Q': {
            uint32_t u32;
            if (reg_offset < 6) {
                u32 = reg[reg_offset];
                reg_offset += 1;
            } else {
                u32 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Bignum::makeIntegerFromU32(u32), args);
        } break;
        case 'o': {
            int64_t s64;
            if (reg_offset < 6) {
                s64 = reg[reg_offset];
                reg_offset += 1;
            } else {
                s64 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Bignum::makeIntegerFromS64(s64), args);
        } break;
        case 'O': {
            uint64_t u64;
            if (reg_offset < 6) {
                u64 = reg[reg_offset];
                reg_offset += 1;
            } else {
                u64 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Bignum::makeIntegerFromU64(u64), args);
        } break;
        case 'P': {
            uint64_t u64;
            if (reg_offset < 6) {
                u64 = reg[reg_offset];
                reg_offset += 1;
            } else {
                u64 = stack[stack_offset];
                stack_offset += 1;
            }
            args = Object::cons(Object::makePointer((void*)u64), args);
        } break;
        case 'f': {
            float* f32;
            if (sse_offset < 8) {
                f32 = (float*)(&reg[6 + sse_offset]);
                sse_offset += 1;
            } else {
                f32 = (float*)(&stack[stack_offset]);
                stack_offset += 1;
            }
            args = Object::cons(Object::makeFlonum(*f32), args);
        } break;
        case 'd': {
            double* f64;
            if (sse_offset < 8) {
                f64 = (double*)(&reg[6 + sse_offset]);
                sse_offset += 1;
            } else {
                f64 = (double*)(&stack[stack_offset]);
                stack_offset += 1;
            }
            args = Object::cons(Object::makeFlonum(*f64), args);
        } break;


        default:
            MOSH_FATAL("fatal: invalid callback argument signature \n[exit]\n");
            break;
        }
    }
    // this reverse can be omitted for optimization
    args = Pair::reverse(args);
    const Object ret = vm->apply(closure, args);
    return ret;
}

extern "C" float c_callback_float(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
{
    Object ret = callbackScheme(uid, signatures, reg, stack);
    if (Arithmetic::isRealValued(ret)) {
        return Arithmetic::realToDouble(ret);
    } else {
        return 0.0;
    }
}

extern "C" double c_callback_double(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
{
    Object ret = callbackScheme(uid, signatures, reg, stack);
    if (ret.isNumber() && Arithmetic::isRealValued(ret)) {
        return Arithmetic::realToDouble(ret);
    } else {
        return 0.0;
    }
}

extern "C" intptr_t c_callback_intptr(uintptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
{
    Object ret = callbackScheme(uid, signatures, reg, stack);
    if (ret.isExactInteger()) {
        if (ret.isBignum()) {
            return ret.toBignum()->toS64();
        } else {
            return ret.toFixnum();
        }
    } else if (ret.isPointer()) {
        return ret.toPointer()->pointer();
    } else {
        return 0;
    }
}

extern "C" int64_t c_callback_int64(uintptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
{
    Object ret = callbackScheme(uid, signatures, reg, stack);
    if (ret.isExactInteger()) {
        if (ret.isBignum()) {
            return ret.toBignum()->toS64();
        } else {
            return ret.toFixnum();
        }
    } else {
        return 0;
    }
}

struct CallBackTrampoline
{
    uint8_t     mov_r10_imm64[2];   // 49 BA                    : mov r10, imm64
    uint64_t    imm64_uid;          // 00 00 00 00 00 00 00 00
    uint8_t     mov_r11_imm64[2];   // 49 BB                    : mov r11, imm64
    uint64_t    imm64_stub;         // 00 00 00 00 00 00 00 00
    uint8_t     jmp_r11[3];         // 41 FF 23                 : jmp [r11]
    uint8_t     ud2[2];             // 0F 0B
    intptr_t    stub;
    intptr_t    uid;
    intptr_t    signatures;
    char        buffer[CStack::MAX_ARGC];
public:
    CallBackTrampoline(intptr_t stub, Object closure, const char* sig)
    {
        strncpy(buffer, sig, sizeof(buffer));
        this->stub = stub;
        uid = currentVM()->registerCallBackTrampoline(closure);
        signatures = (intptr_t)(&buffer[0]);
        mov_r10_imm64[0] = 0x49;
        mov_r10_imm64[1] = 0xBA;
        imm64_uid = (uint64_t)&(this->uid);
        mov_r11_imm64[0] = 0x49;
        mov_r11_imm64[1] = 0xBB;
        imm64_stub = (uint64_t)&(this->stub);
        jmp_r11[0] = 0x41;
        jmp_r11[1] = 0xFF;
        jmp_r11[2] = 0x23;
        ud2[0] = 0x0F;
        ud2[1] = 0x0B;
        memoryStoreFence();
    }

    ~CallBackTrampoline()
    {
        currentVM()->unregisterCallBackTrampoline(uid);
    }

    static void* operator new(size_t size)
    {
        ExecutableMemory* ex = ::new ExecutableMemory(size);
        ex->allocate();
        return static_cast<void*>(ex->address());
    }
} __attribute__((packed));

#endif
extern "C" intptr_t c_func_stub_intptr_x64(uintptr_t func, intptr_t* reg, intptr_t* frame, int stackArgSizeInBytes, intptr_t* xmm);
extern "C" double c_func_stub_double_x64(uintptr_t func, intptr_t* reg, intptr_t* frame, int stackArgSizeInBytes, intptr_t* xmm);
static double callStubDouble(Pointer* func, CStack* cstack)
{
#if defined(ARCH_IA32) && !defined(_MSC_VER)
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
        : "c" (bytes), "S" (cstack->frame()), "a" (func->pointer()) // c:ecx, S:esi a:eax
        : "edi", "edx", "memory"); // we have memory destructions.
    return ret;
#elif defined(ARCH_IA32) && defined(_MSC_VER)
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
    uintptr_t p = func->pointer();
    void* s = cstack->frame();
    double dret;
    __asm {
        MOV    EDX,ESP
        MOV    ESI,s
        MOV    ECX,bytes
        SUB    ESP,bytes
        MOV    EDI,ESP
        REP    MOVSB
        MOV    EDI,EDX
        CALL   p
        MOV    ESP,EDI
        FSTP   dret
    }
    return dret;
#elif defined(ARCH_X86_64) && !defined(WITHOUT_FFI_STUB)
        const int stackArgSizeInBytes = (cstack->count()) * 8;
        return c_func_stub_double_x64(func->pointer(), cstack->reg(), cstack->frame(), stackArgSizeInBytes, cstack->xmm());
#else
    return 0.0;
#endif
}

static float callStubFloat(Pointer* func, CStack* cstack)
{
#if defined(ARCH_IA32) && !defined(_MSC_VER)
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
    float ret;
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
        : "c" (bytes), "S" (cstack->frame()), "a" (func->pointer()) // c:ecx, S:esi a:eax
        : "edi", "edx", "memory"); // we have memory destructions.
    return ret;
#elif defined(ARCH_IA32) && defined(_MSC_VER)
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
    uintptr_t p = func->pointer();
    void* s = cstack->frame();
    float dret;
    __asm {
        MOV    EDX,ESP
        MOV    ESI,s
        MOV    ECX,bytes
        SUB    ESP,bytes
        MOV    EDI,ESP
        REP    MOVSB
        MOV    EDI,EDX
        CALL   p
        MOV    ESP,EDI
        FSTP   dret
    }
    return dret;
#elif defined(ARCH_X86_64)
#ifdef __clang__
    int
#else
    float
#endif
    ret;
    if (cstack->count() == 0) {
        asm volatile("movq %%rsi, %%r10\n"
                     "movsd   (%%rdi), %%xmm0 \n"
                     "movsd  8(%%rdi), %%xmm1 \n"
                     "movsd 16(%%rdi), %%xmm2 \n"
                     "movsd 24(%%rdi), %%xmm3 \n"
                     "movsd 32(%%rdi), %%xmm4 \n"
                     "movsd 40(%%rdi), %%xmm5 \n"
                     "movsd 48(%%rdi), %%xmm6 \n"
                     "movsd 56(%%rdi), %%xmm7 \n"
                     "movq 0(%%r10), %%rdi \n"   // register argument 1
                     "movq 8(%%r10), %%rsi \n"   // register argument 2
                     "movq 16(%%r10), %%rdx \n"  // register argument 3
                     "movq 24(%%r10), %%rcx \n"  // register argument 4
                     "movq 32(%%r10), %%r8 \n"   // register argument 5
                     "movq 40(%%r10), %%r9 ;"   // register argument 6
                     "call *%%rax \n"
#if !defined(__APPLE__)&&!defined(__FreeBSD__)&&!defined(__clang__)&&!defined(__NetBSD__)
                     "movq %%xmm0, %%rax \n"
#else /* Apple's assembler doesn't support movq in that style . */
                     "movd %%xmm0, %%rax \n"
#endif
                     : "=a" (ret)
                     : "0" (func->pointer()), "S" (cstack->reg()), "D"(cstack->xmm())
                     : "memory"
            );
    } else {
        const int bytes = (cstack->count()) * 8;
        asm volatile(
            "movq %%rsi, %%r10\n" // r10 for pointing register frame
            "movq %%rdx, %%r11\n" // r11 for pointer to stack argument in frame
            "movq %%r11, %%r12\n"
            "addq %%rcx, %%r12\n"  // r12 end of stack argument in frame
            "movsd   (%%rdi), %%xmm0 \n"
            "movsd  8(%%rdi), %%xmm1 \n"
            "movsd 16(%%rdi), %%xmm2 \n"
            "movsd 24(%%rdi), %%xmm3 \n"
            "movsd 32(%%rdi), %%xmm4 \n"
            "movsd 40(%%rdi), %%xmm5 \n"
            "movsd 48(%%rdi), %%xmm6 \n"
            "movsd 56(%%rdi), %%xmm7 \n"
            "movq 0(%%r10), %%rdi \n"   // register argument 1
            "movq 8(%%r10), %%rsi \n"   // register argument 2
            "movq 16(%%r10), %%rdx \n"  // register argument 3
            "movq 24(%%r10), %%rcx \n"  // register argument 4
            "movq 32(%%r10), %%r8 \n"   // register argument 5
            "movq 40(%%r10), %%r9 \n"   // register argument 6
            "movq %%rsp, %%r13\n"
            "1:   \n"
            "cmpq %%r11, %%r12\n"
            "je 2f\n"
            "movq 0(%%r11), %%r14\n"
            "movq %%r14, 0(%%r13)\n"
            "addq $8, %%r11\n"
            "addq $8, %%r13\n"
            "jmp 1b\n"
            "2:   \n"
            "call *%%rax \n"
#if !defined(__APPLE__)&&!defined(__FreeBSD__)&&!defined(__clang__)&&!defined(__NetBSD__)
            "movq %%xmm0, %%rax \n"
#else
            "movd %%xmm0, %%rax \n" /* Apple's assembler doesn't support that movq*/
#endif
            : "=a" (ret)
            : "c" (bytes), "0" (func->pointer()), "S" (cstack->reg()), "D" (cstack->xmm()), "d" (cstack->frame())
            : "memory"
            );
    }
#ifndef __clang__
    return ret;
#else
    float retf; // clang dislikes =a (float) constraint..
    retf = *(float *)&ret;
    return retf;
#endif // ifndef __clang__
#else
    return 0.0;
#endif
}

static intptr_t callStubIntptr_t(Pointer* func, CStack* cstack)
{
#if defined(ARCH_IA32) && !defined(_MSC_VER)
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
        : "c" (bytes), "S" (cstack->frame()), "0" (func->pointer()) // c:ecx, S:esi 0:match to the 0th output
        : "edi", "edx", "memory"); // we have memory destructions.
    return ret;
#elif defined(ARCH_IA32) && defined(_MSC_VER)
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
    uintptr_t p = func->pointer();
    intptr_t temp;
    void* s = cstack->frame();
    __asm {
        MOV    ECX,bytes
        MOV    ESI,s
        MOV    EDX,ESP
        SUB    ESP,bytes
        MOV    EDI,ESP
        REP    MOVSB
        MOV    EDI,EDX
        CALL   p
        MOV    ESP,EDI
        MOV    temp,EAX
    }
    return temp;
#elif defined(ARCH_X86_64) && !defined(WITHOUT_FFI_STUB)
    const int stackArgSizeInBytes = (cstack->count()) * 8;
    return c_func_stub_intptr_x64(func->pointer(), cstack->reg(), cstack->frame(), stackArgSizeInBytes, cstack->xmm());
#else
    return 0;
#endif
}

static int64_t callStubInt64_t(Pointer* func, CStack* cstack)
{
#if defined(ARCH_IA32) && !defined(_MSC_VER)
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
    union {
        int64_t s64;
        struct {
            uint32_t lo;
            uint32_t hi;
        } s32;
    } ret;

    asm volatile(
        "movl    %%esp, %%edx ;"
        "subl    %2   , %%esp ;"
        "movl    %%esp, %%edi ;" // copy arguments to stack
        "rep                  ;"
        "movsb                ;"
        "movl    %%edx, %%edi ;"
        "call    *%%eax       ;"
        "movl    %%edi, %%esp ;"
        : "=a" (ret.s32.lo), "=d" (ret.s32.hi)
        : "c" (bytes), "S" (cstack->frame()), "0" (func->pointer()) // c:ecx, S:esi 0:match to the 0th output
        : "edi", "memory"); // we have memory destructions.
    return ret.s64;
#elif defined(ARCH_IA32) && defined(_MSC_VER)
    const int bytes = (cstack->count() * sizeof(intptr_t) + 15) & ~15;
    uintptr_t p = func->pointer();
    union {
        int64_t s64;
        struct {
            uint32_t lo;
            uint32_t hi;
        } s32;
    } retarea;
    void* s = cstack->frame();
    __asm {
        MOV    ECX,bytes
        MOV    ESI,s
        MOV    EDX,ESP
        SUB    ESP,bytes
        MOV    EDI,ESP
        REP    MOVSB
        MOV    EDI,EDX
        CALL   p
        MOV    ESP,EDI
        MOV    retarea.s32.lo,EAX
        MOV    retarea.s32.hi,EDX
    }
    return retarea.s64;
#elif defined ARCH_X86_64
    return callStubIntptr_t(func, cstack);
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

#define FFI_RETURN_TYPE_VOID        0x0000
#define FFI_RETURN_TYPE_BOOL        0x0001
#define FFI_RETURN_TYPE_SHORT       0x0002
#define FFI_RETURN_TYPE_INT         0x0003
#define FFI_RETURN_TYPE_INTPTR      0x0004
#define FFI_RETURN_TYPE_USHORT      0x0005
#define FFI_RETURN_TYPE_UINT        0x0006
#define FFI_RETURN_TYPE_UINTPTR     0x0007
#define FFI_RETURN_TYPE_FLOAT       0x0008
#define FFI_RETURN_TYPE_DOUBLE      0x0009
#define FFI_RETURN_TYPE_STRING      0x000a
#define FFI_RETURN_TYPE_SIZE_T      0x000b
#define FFI_RETURN_TYPE_INT8_T      0x000c
#define FFI_RETURN_TYPE_UINT8_T     0x000d
#define FFI_RETURN_TYPE_INT16_T     0x000e
#define FFI_RETURN_TYPE_UINT16_T    0x000f
#define FFI_RETURN_TYPE_INT32_T     0x0010
#define FFI_RETURN_TYPE_UINT32_T    0x0011
#define FFI_RETURN_TYPE_INT64_T     0x0012
#define FFI_RETURN_TYPE_UINT64_T    0x0013
#define FFI_RETURN_TYPE_POINTER     0x0014

Object scheme::internalFfiCallEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("ffi-call");
#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, UC("ffi not supported on this architecture"));
    return Object::Undef;
#endif

    checkArgumentLengthAtLeast(3);
    argumentAsFixnum(0, retType);
    argumentAsString(1, signatures);
    argumentAsPointer(2, func);
    MOSH_ASSERT(argc - 3 == signatures->length());
    CStack cstack;
    for (int i = 3; i < argc; i++) {
        if (!cstack.push(argv[i], signatures->charAt(i - 3))) {
            callAssertionViolationAfter(theVM, procedureName, UC("argument error"), L2(Object(cstack.getLastError()),
                                                                                   argv[i]));
            return Object::Undef;
        }
    }
    SynchronizedErrno s(theVM);
    switch(retType) {
    case FFI_RETURN_TYPE_VOID:
    {
        callStubIntptr_t(func, &cstack);
        return Object::Undef;
    }
    case FFI_RETURN_TYPE_BOOL:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Object::makeBool(ret == 1);
    }
    case FFI_RETURN_TYPE_SHORT:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromSigned<short>(static_cast<short>(ret));
    }
    case FFI_RETURN_TYPE_INT:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromSigned<int>(static_cast<int>(ret));
    }
    case FFI_RETURN_TYPE_INTPTR:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromSigned<intptr_t>(ret);
    }
    case FFI_RETURN_TYPE_USHORT:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromUnsigned<unsigned short>(static_cast<unsigned short>(ret));
    }
    case FFI_RETURN_TYPE_UINT:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromUnsigned<uintptr_t>(static_cast<uintptr_t>(ret));
    }
    case FFI_RETURN_TYPE_UINTPTR:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromUnsigned<uintptr_t>(static_cast<uintptr_t>(ret));
    }
    case FFI_RETURN_TYPE_FLOAT:
    {
        const float ret = callStubFloat(func, &cstack);
        return Object::makeFlonum(static_cast<double>(ret));
    }
    case FFI_RETURN_TYPE_DOUBLE:
    {
       const double ret = callStubDouble(func, &cstack);
       return Object::makeFlonum(ret);
    }
    case FFI_RETURN_TYPE_STRING:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        if (ret == 0) {
            return Object::makePointer(reinterpret_cast<void*>(0));
        } else {
            return Object::makeString(reinterpret_cast<char*>(ret));
        }
    }
    case FFI_RETURN_TYPE_SIZE_T:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromUnsigned<size_t>(static_cast<size_t>(ret));
    }
    case FFI_RETURN_TYPE_INT8_T:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Object::makeFixnum(static_cast<int8_t>(ret));
    }
    case FFI_RETURN_TYPE_UINT8_T:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Object::makeFixnum(static_cast<uint8_t>(ret));
    }
    case FFI_RETURN_TYPE_INT16_T:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Object::makeFixnum(static_cast<int16_t>(ret));
    }
    case FFI_RETURN_TYPE_UINT16_T:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Object::makeFixnum(static_cast<uint16_t>(ret));
    }
    case FFI_RETURN_TYPE_INT32_T:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromSigned<int32_t>(static_cast<int32_t>(ret));
    }
    case FFI_RETURN_TYPE_UINT32_T:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Bignum::makeIntegerFromUnsigned<uint32_t>(static_cast<uint32_t>(ret));
    }
    case FFI_RETURN_TYPE_INT64_T:
    {
        const int64_t ret = callStubInt64_t(func, &cstack);
        return Bignum::makeIntegerFromSigned<int64_t>(ret);
    }
    case FFI_RETURN_TYPE_UINT64_T:
    {
        const int64_t ret = callStubInt64_t(func, &cstack);
        return Bignum::makeIntegerFromUnsigned<uint64_t>(static_cast<uint64_t>(ret));
    }
    case FFI_RETURN_TYPE_POINTER:
    {
        const intptr_t ret = callStubIntptr_t(func, &cstack);
        return Object::makePointer(reinterpret_cast<void*>(ret));
    }
    default:
    {
        callAssertionViolationAfter(theVM, UC("c-function"), UC("invalid return type"), L1(argv[0]));
        return Object::Undef;
    }
    }
}

Object scheme::internalFfiLookupEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("lookup-shared-library");

#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, UC("ffi not supported on this architecture"));
    return Object::Undef;
#endif

    checkArgumentLength(2);
    argumentAsPointer(0, handle);
    argumentAsSymbol(1, name);

    ucs4string n = name->c_str();
    void* symbol = FFI::lookup((void*)handle->pointer(), n.ascii_c_str());

    if (nullptr == symbol) {
        return Object::False;
    } else {
        return Object::makePointer(symbol);
    }
}

Object scheme::internalFfiOpenEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%ffi-open");

#ifndef FFI_SUPPORTED
    callAssertionViolationAfter(theVM, procedureName, UC("ffi not supported on this architecture"));
    return Object::Undef;
#endif

    checkArgumentLength(1);
    argumentAsString(0, name);
    void* handle = FFI::open(name->data().ascii_c_str());
    if (nullptr == handle) {
        callAssertionViolationAfter(theVM, procedureName, UC("shared library not found"), L2(Object(FFI::lastError()), argv[0]));
        return Object::Undef;
    } else {
        return Object::makePointer(handle);
    }
}

// (close-shared-library handle) => #<void>
// There's no real reason for this library to have the internal prefix, as it's
// simply exported unmodified - we do this merely for consistency with the other
// dl* wrappers.
// Win32's FreeLibrary() function inverts the exit status, but since we only
// support POSIX presently, assume zero result indicates success.
Object scheme::internalFfiCloseEx(VM* theVM, int argc, const Object* argv) {
    DeclareProcedureName("close-shared-library");    // user visible name
    checkArgumentLength(1);
    argumentAsPointer(0, handle);
    int result = FFI::close((void*) handle->pointer());
    if (result != 0) {
        callAssertionViolationAfter(theVM,
                                    procedureName,
                                    UC("cannot close shared library"));
    }

    return Object::Undef;
}

Object scheme::internalFfiErrorEx(VM* theVM, int argc, const Object* argv) {
    DeclareProcedureName("shared-library-error");
    checkArgumentLength(0);
    const char *message = FFI::lastError();
    if (message == nullptr) {
        return Object::False;
    } else {
        return Object::makeString(message);
    }
}


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
        return Object::makePointer((void*)((intptr_t)integer.toFixnum()));
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
    pointer->set<float>(offset, static_cast<float>(value->value()));
    return Object::Undef;
}

template <typename T, typename S, bool isSigned> static Object pointerSet(const ucs4char* procedureName, T min, T max, VM* theVM, int argc, const Object* argv)
{
    checkArgumentLength(3);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    argumentCheckExactInteger(2, v);
    if (!isSigned) {
        if (Arithmetic::lt(v, Object::makeFixnum(0))) {
            callAssertionViolationAfter(theVM, procedureName, UC("value out of range"), L1(argv[2]));
            return Object::Undef;
        }
    }

    S value;
    if (v.isBignum()) {
        if (isSigned) {
            if (v.toBignum()->fitsS64()) {
                value = v.toBignum()->toS64();
            } else {
                callAssertionViolationAfter(theVM, procedureName, UC("value out of range"), L1(argv[2]));
                return Object::Undef;
            }
        } else {
            if (v.toBignum()->fitsU64()) {
                value = v.toBignum()->toU64();
            } else {
                callAssertionViolationAfter(theVM, procedureName, UC("value out of range"), L1(argv[2]));
                return Object::Undef;
            }
        }
    } else {
        value = v.toFixnum();
    }
    MOSH_ASSERT(sizeof(S) <= sizeof(int64_t));
    if (value >= static_cast<S>(min) && value <= static_cast<S>(max)) {
        pointer->set<T>(offset, static_cast<T>(value));
    } else {
        callAssertionViolationAfter(theVM, procedureName, UC("value out of range"), L1(argv[2]));
    }
    return Object::Undef;
}

template <typename T> static Object pointerSetU(const ucs4char* procedureName, T min, T max, VM* theVM, int argc, const Object* argv)
{
    return pointerSet<T,uint64_t, false>(procedureName, min, max, theVM, argc, argv);
}

template <typename T> static Object pointerSetS(const ucs4char* procedureName, T min, T max, VM* theVM, int argc, const Object* argv)
{
    return pointerSet<T, int64_t, true>(procedureName, min, max, theVM, argc, argv);
}

Object scheme::pointerSetCUint8DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetU<uint8_t>(UC("pointer-set-c-uint8!"), 0, UINT8_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCUint16DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetU<uint16_t>(UC("pointer-set-c-uint16!"), 0, UINT16_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCUint32DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetU<uint32_t>(UC("pointer-set-c-uint32!"), 0, UINT32_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCUint64DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetU<uint64_t>(UC("pointer-set-c-uint64!"), 0, UINT64_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCInt8DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<int8_t>(UC("pointer-set-c-int8!"), INT8_MIN, INT8_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCInt16DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<int16_t>(UC("pointer-set-c-int16!"), INT16_MIN, INT16_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCInt32DEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<int32_t>(UC("pointer-set-c-int32!"), INT32_MIN, INT32_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCInt64DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-set-c-int64!");
    checkArgumentLength(3);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    argumentCheckExactInteger(2, v);
    static const Object minVal = Arithmetic::negate(Arithmetic::expt(Object::makeFixnum(2), Object::makeFixnum(63)));
    static const Object maxVal = Arithmetic::sub(Arithmetic::expt(Object::makeFixnum(2), Object::makeFixnum(63)),
                                           Object::makeFixnum(1));
    if (Arithmetic::lt(v, minVal) || Arithmetic::gt(v, maxVal)) {
        callAssertionViolationAfter(theVM, procedureName, UC("value out of range"), L1(argv[2]));
    }
    int64_t value;
    if (v.isBignum()) {
        value = v.toBignum()->toS64();
    } else {
        value = v.toFixnum();
    }
    pointer->set<int64_t>(offset, value);
    return Object::Undef;
}

#ifndef LLONG_MIN
#warning LLONG_MIN is missing..
#define LLONG_MIN INTMAX_MIN
#endif

#ifndef LLONG_MAX
#warning LLONG_MAX is missing..
#define LLONG_MAX INTMAX_MAX
#endif

Object scheme::pointerSetCLongLongDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<long long>(UC("pointer-set-c-long-long!"), LLONG_MIN, LLONG_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCPointerDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-set-c-pointer!");
    checkArgumentLength(3);
    argumentAsPointer(0, pointer);
    argumentAsFixnum(1, offset);
    argumentAsPointer(2, p);
    pointer->set<uintptr_t>(offset, p->pointer());
    return Object::Undef;
}

Object scheme::pointerSetCLongDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<long>(UC("pointer-set-c-long!"), LONG_MIN, LONG_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCIntDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<int>(UC("pointer-set-c-int!"), INT_MIN, INT_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCShortDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<short>(UC("pointer-set-c-short!"), SHRT_MIN, SHRT_MAX, theVM, argc, argv);
}

Object scheme::pointerSetCCharDEx(VM* theVM, int argc, const Object* argv)
{
    return pointerSetS<char>(UC("pointer-set-c-char!"), SCHAR_MIN, SCHAR_MAX, theVM, argc, argv);
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
    return pointerRefS<int8_t>(UC("pointer-ref-c-int8"), theVM, argc, argv);
}

Object scheme::pointerRefCInt16Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<int16_t>(UC("pointer-ref-c-int16"), theVM, argc, argv);
}

Object scheme::pointerRefCInt32Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<int32_t>(UC("pointer-ref-c-int32"), theVM, argc, argv);
}

Object scheme::pointerRefCInt64Ex(VM* theVM, int argc, const Object* argv)
{
    return pointerRefS<int64_t>(UC("pointer-ref-c-int64"), theVM, argc, argv);
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

Object scheme::sharedErrnoEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("shared-errno");
    checkArgumentLengthBetween(0, 1);
    if (0 == argc) {
#ifdef _MSC_VER
        return Bignum::makeIntegerFromUnsigned<uint32_t>(theVM->getErrno());
#else
        return Bignum::makeIntegerFromSigned<int>(theVM->getErrno());
#endif
    } else {
        argumentCheckExactInteger(0, val);
        if (val.isBignum()) {
#ifdef _MSC_VER
            theVM->setErrno(val.toBignum()->toU32());
#else
            theVM->setErrno(val.toBignum()->toS32());
#endif
        } else {
            theVM->setErrno(val.toFixnum());
        }
        return Object::Undef;
    }
}

Object scheme::internalFfiFreeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("free");
    checkArgumentLength(1);
    argumentAsPointer(0, p);
    free((void*)p->pointer());
    return Object::Undef;
}

Object scheme::internalFfiMallocEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("malloc");
    checkArgumentLength(1);
    argumentCheckExactInteger(0, size);
    if (!Arithmetic::fitsU64(size)) { // FIXME: malloc cannot allocate full-64bit size (evenif we were on 64bit-os)
        callAssertionViolationAfter(theVM, procedureName, UC("size out of range"), L1(argv[0]));
        return Object::Undef;
    }
    const uint64_t u64Size = Arithmetic::toU64(size);
    return Object::makePointer(malloc((size_t)u64Size));
}

#define CALLBACK_RETURN_TYPE_INTPTR     0x0000
#define CALLBACK_RETURN_TYPE_INT64_T    0x0001
#define CALLBACK_RETURN_TYPE_FLOAT      0x0002
#define CALLBACK_RETURN_TYPE_DOUBLE     0x0003
#define CALLBACK_RETURN_TYPE_MASK       0x00ff
#define CALLBACK_CALL_TYPE_STDCALL      0x0100
#define CALLBACK_CALL_TYPE_MASK         0xff00


Object scheme::internalFfiFreeCCallbackTrampolineEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("free-c-callback-trampoline");

#if defined(FFI_SUPPORTED) && !defined(WITHOUT_FFI_STUB)
    checkArgumentLength(1);
    argumentAsPointer(0, callback);
#ifndef _MSC_VER
    CallBackTrampoline* trampoline = (CallBackTrampoline*)callback->pointer();
#else
    CallBackTrampolineWithStub* trampoline = (CallBackTrampolineWithStub*)callback->pointer();
#endif
#ifndef MOSH_MINGW32
    delete trampoline;
#else
    //VirtualFree here
#endif
    return Object::Undef;
#else
    callAssertionViolationAfter(theVM, procedureName, UC("ffi not supported on this architecture"));
    return Object::Undef;
#endif
}
// (make-c-callback-trampoline type signatures proc)
Object scheme::internalFfiMakeCCallbackTrampolineEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-c-callback-trampoline");

#if defined(FFI_SUPPORTED) && !defined(WITHOUT_FFI_STUB)
    checkArgumentLength(3);
    argumentAsFixnum(0, type);
    argumentAsString(1, signatures);
    argumentCheckProcedure(2, closure);

#ifndef _MSC_VER
    CallBackTrampoline* thunk;
#else
    CallBackTrampolineWithStub* thunk;
#endif
    switch (type & CALLBACK_RETURN_TYPE_MASK) {
// 32bit
#ifdef ARCH_IA32
#ifndef _MSC_VER
    case CALLBACK_RETURN_TYPE_INTPTR:
        thunk = new CallBackTrampoline((intptr_t)c_callback_stub_intptr, closure, signatures->data().ascii_c_str());
        break;
    case CALLBACK_RETURN_TYPE_INT64_T:
        thunk = new CallBackTrampoline((intptr_t)c_callback_stub_int64, closure, signatures->data().ascii_c_str());
        break;
#else
    case CALLBACK_RETURN_TYPE_INTPTR:
        thunk = new CallBackTrampolineWithStub((intptr_t)c_callback_intptr, closure, signatures->data().ascii_c_str());
        break;
    case CALLBACK_RETURN_TYPE_INT64_T:
        thunk = new CallBackTrampolineWithStub((intptr_t)c_callback_int64, closure, signatures->data().ascii_c_str());
        break;
#endif
// 64bit
#else
    case CALLBACK_RETURN_TYPE_INTPTR:
    case CALLBACK_RETURN_TYPE_INT64_T:
        thunk = new CallBackTrampoline((intptr_t)c_callback_stub_intptr, closure, signatures->data().ascii_c_str());
        break;
#endif
#ifndef _MSC_VER
    case CALLBACK_RETURN_TYPE_FLOAT:
    case CALLBACK_RETURN_TYPE_DOUBLE:
        thunk = new CallBackTrampoline((intptr_t)c_callback_stub_double, closure, signatures->data().ascii_c_str());
        break;
#else
    case CALLBACK_RETURN_TYPE_FLOAT:
    case CALLBACK_RETURN_TYPE_DOUBLE:
        thunk = new CallBackTrampolineWithStub((intptr_t)c_callback_double, closure, signatures->data().ascii_c_str());
        break;
#endif
    default:
        callAssertionViolationAfter(theVM, procedureName, UC("invalid callback type specifier"), L1(argv[1]));
        return Object::Undef;
    }
    return Object::makePointer(thunk);
#else
    callAssertionViolationAfter(theVM, procedureName, UC("ffi not supported on this architecture"));
    return Object::Undef;
#endif
}

/* for SQLite */

Object scheme::pointerCopyDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer-copy!");
    checkArgumentLength(3);
    argumentAsPointer(0, p0); // source
    argumentAsPointer(1, p1); // destination
    argumentAsFixnum(2, len);
    memcpy((void *)p1->pointer(),(const void *)p0->pointer(),len);
    return Object::Undef;
}

Object scheme::bytevectorPointerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-pointer");
    checkArgumentLength(1);
    argumentAsByteVector(0, bv);
    return Object::makePointer(bv->data());
}

Object scheme::objectTopointerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("object->pointer");
    checkArgumentLength(1);
    return Object::makePointer((void *)argv[0].val);
}

Object scheme::pointerToobjectEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pointer->object");
    checkArgumentLength(1);
    argumentAsPointer(0, p0);
    return Object::makeRaw(p0->pointer());
}

/*
 * FFI.h - 
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
 *  $Id: Arithmetic.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_FFI_
#define SCHEME_FFI_

#include "scheme.h"

namespace scheme {

class FFI EXTEND_GC
{
private:
    FFI();
    ~FFI();

public:
    static void* open(const char* name);
    static void* lookup(void* handle, const char* symbol);
    static int close(void* handle);
    static const char* lastError();
};

class Pointer EXTEND_GC
{
public:
    Pointer(void* pointer) : pointer_((uintptr_t)pointer)
    {
    }


    ~Pointer()
    {
    }

    uintptr_t pointer() const
    {
        return pointer_;
    }

    template <typename T> T ref(int offset)
    {
        return *((T*)(pointer_ + offset));
    }

    template <typename T> void set(int offset, T value)
    {
        *((T*)(pointer_ + offset)) = value;
    }

private:
    const uintptr_t pointer_;
};

class CStack EXTEND_GC
{
public:
    enum
    {
        MAX_ARGC = 32,
        MAX_REG  = 6,
    };
    CStack();
    ~CStack();
    bool push(Object obj);
    intptr_t* frame();
#ifdef ARCH_X86_64
    intptr_t* xmm() { return xmm_; }
    intptr_t* reg() { return reg_; }
    int regCount() const { return regCount_; }
#endif
    int count() const;
    const ucs4char* getLastError() const;

private:
    bool pushInt(intptr_t val);
    bool pushDouble(double val);

    intptr_t frame_[MAX_ARGC];
    int count_;
#ifdef ARCH_X86_64
    intptr_t xmm_[8];
    int xmmCount_;
    intptr_t reg_[6];
    int regCount_;
#endif
    const ucs4char* lastError_;
};


} // namespace scheme

#endif // SCHEME_FFI_

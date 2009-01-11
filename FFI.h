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

#ifndef __SCHEME_FFI__
#define __SCHEME_FFI__

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
};

class CStack EXTEND_GC
{
public:
    enum
    {
        MAX_ARGC = 16,
    };
    CStack();
    ~CStack();
    bool push(Object obj);
    intptr_t* frame();
    int count() const;
    const ucs4char* getLastError() const;

private:
    intptr_t frame_[MAX_ARGC];
    int count_;
    const ucs4char* lastError_;
};

#define FFI_MAX_ARGC    32

#if ARCH_IA32
    class c_stack_frame_t {
        intptr_t m_frame[FFI_MAX_ARGC];
        int m_count;
        VM* m_vm;
    public:
        c_stack_frame_t(VM* vm) : m_vm(vm), m_count(0) {}
        const char* push(scm_obj_t obj);
        intptr_t* frame() { return m_frame; }
        int count() { return m_count; }
    };
#endif


}; // namespace scheme

#endif // __SCHEME_FFI__

/*
 * FFI.cpp -
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
 *  $Id: FFI.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <dlfcn.h>
#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "Bignum.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Arithmetic.h"
#include "FFI.h"

using namespace scheme;

void* FFI::open(const char* name)
{
    return dlopen(name, RTLD_LAZY | RTLD_GLOBAL);
}

void* FFI::lookup(void* handle, const char* symbol)
{
    return dlsym(handle, symbol);
}

int FFI::close(void* handle)
{
    return dlclose(handle);
}

const char* FFI::lastError()
{
    return dlerror();
}

#ifdef ARCH_X86_64
CStack::CStack() : count_(0), xmmCount_(0), regCount_(0)
{
    memset(frame_, 0, sizeof(frame_));
    memset(xmm_, 0, sizeof(xmm_));
    memset(reg_, 0, sizeof(reg_));
}

bool CStack::pushInt(intptr_t val)
{
    if (regCount_ < (int)(sizeof(reg_) / sizeof(intptr_t))) {
        reg_[regCount_++] = val;
        return true;
    } else if (count_ < MAX_ARGC) {
        frame_[count_++] = val;
        return true;
    } else {
        lastError_ = UC("too many ffi arguments");
        return false;
    }
}

bool CStack::pushDouble(double val)
{
    if (xmmCount_ < (int)(sizeof(xmm_) / sizeof(intptr_t))) {
        union {
            double fvalue;
            uint64_t uval;
        } v;
        v.fvalue = val;
        xmm_[xmmCount_++] = v.uval;
        return true;
    } else if (count_ < MAX_ARGC) {
        union {
            double fvalue;
            uint64_t uval;
        } v;
        v.fvalue = val;
        frame_[count_++] = v.uval;
        return true;
    } else {
        lastError_ = UC("too many ffi arguments");
        return false;
    }
}

#else
CStack::CStack() : count_(0)
{
    memset(frame_, 0, sizeof(frame_));
}

bool CStack::pushInt(intptr_t val)
{
    if (count_ < MAX_ARGC) {
        frame_[count_++] = val;
        return true;
    } else {
        lastError_ = UC("too many ffi arguments");
        return false;
    }
}

bool CStack::pushDouble(double val)
{
    if (MAX_ARGC - count_ < 2) {
        lastError_ = UC("too many ffi double arguments");
        return false;
    }
    union {
        double fvalue;
        struct {
            uint32_t low;
            uint32_t high;
        } u32;
    } v;
    v.fvalue = val;
    frame_[count_++] = v.u32.low;
    frame_[count_++] = v.u32.high;
    return true;
}

#endif
CStack::~CStack()
{
}

intptr_t* CStack::frame()
{
    return frame_;
}

int CStack::count() const
{
    return count_;
}


bool CStack::push(Object obj)
{
    // Fixnum -> int
    if (obj.isFixnum()) {
        return pushInt(obj.toFixnum());
    // Flonum -> double
    } else if (obj.isFlonum()) {
        return pushDouble(obj.toFlonum()->value());
    } else if (obj.isBignum()) {
        if (Arithmetic::isNegative(obj)) {
            return pushInt(obj.toBignum()->toIntptr_t());
        } else {
            return pushInt(obj.toBignum()->toUintptr_t());
        }
    // String -> char* (utf-8 ascii only)
    } else if (obj.isString()) {
        return pushInt((intptr_t)(obj.toString()->data().ascii_c_str()));
    // ByteVector -> char*
    } else if (obj.isByteVector()) {
        return pushInt((intptr_t)(obj.toByteVector()->data()));
    } else {
        lastError_ = UC("unsupported ffi argument");
        return false;
    }
}

const ucs4char* CStack::getLastError() const
{
    return lastError_;
}

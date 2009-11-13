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

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif
#include "scheme.h"
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
#include "StringProcedures.h"

using namespace scheme;

/*
 *  A part of FFI functions are originally from Ypsilon Scheme by Yoshikatsu Fujita.
 *  They are ported or modified for Mosh.
 */

void* FFI::open(const char* name)
{
#ifdef _WIN32
    return (void *)LoadLibraryA(name); // FIXME : handle MBCS path
#else
    return dlopen(name, RTLD_LAZY | RTLD_GLOBAL);
#endif
}

void* FFI::lookup(void* handle, const char* symbol)
{
#ifdef _WIN32
    return (void *)GetProcAddress((HMODULE)handle,symbol);
#else
    return dlsym(handle, symbol);
#endif
}

int FFI::close(void* handle)
{
#ifdef _WIN32
    return FreeLibrary((HMODULE)handle);
#else
    return dlclose(handle);
#endif
}

const char* FFI::lastError()
{
#ifdef _WIN32
    return "win32 error"; //FIXME : stub
#else
    return dlerror();
#endif
}

#ifdef ARCH_X86_64
CStack::CStack() : count_(0), xmmCount_(0), regCount_(0)
{
    memset(frame_, 0, sizeof(frame_));
    memset(xmm_, 0, sizeof(xmm_));
    memset(reg_, 0, sizeof(reg_));
}

bool CStack::pushIntptr_t(intptr_t val)
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

bool CStack::pushInt64_t(int64_t val)
{
    return pushIntptr_t(val);
}

// argument is double
bool CStack::pushFloat(double val)
{
    if (xmmCount_ < (int)(sizeof(xmm_) / sizeof(intptr_t))) {
        union {
            int64_t value;
            struct {
                float low;
                int32_t high;
            } u32;
        } v;
        v.u32.low = (double)val;
        v.u32.high = -1;
        xmm_[xmmCount_++] = v.value;
        return true;
    } else if (count_ < MAX_ARGC) {
        union {
            int64_t value;
            struct {
                float low;
                int32_t high;
            } u32;
        } v;
        v.u32.low = (double)val;
        v.u32.high = -1;
        frame_[count_++] = v.value;
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

bool CStack::pushIntptr_t(intptr_t val)
{
    if (count_ < MAX_ARGC) {
        frame_[count_++] = val;
        return true;
    } else {
        lastError_ = UC("too many ffi arguments");
        return false;
    }
}

bool CStack::pushFloat(double val)
{
    if (count_ < MAX_ARGC) {
        union { float f32; uintptr_t u32; } n;
        n.f32 = (float)val;
        frame_[count_++] = n.u32;
        return true;
    } else {
        lastError_ = UC("too many ffi arguments");
        return false;
    }
}

bool CStack::pushInt64_t(int64_t val)
{
    if (MAX_ARGC - count_ < 2) {
        lastError_ = UC("too many ffi int64_t arguments");
        return false;
    }
    union {
        int64_t value;
        struct {
            uint32_t low;
            uint32_t high;
        } u32;
    } v;
    v.value = val;
    frame_[count_++] = v.u32.low;
    frame_[count_++] = v.u32.high;
    return true;
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

bool CStack::push(Object obj, ucs4char signature)
{
    // Fixnum -> int
    if (obj.isFixnum()) {
        switch (signature) {
        case SIGNATURE_BOOL:
            lastError_ = format(NULL, UC("'bool' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT:
            return pushIntptr_t(obj.toFixnum());
        case SIGNATURE_FLOAT:
            lastError_ = format(NULL, UC("'float' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_DOUBLE:
            lastError_ = format(NULL, UC("'double' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT64:
            return pushInt64_t(obj.toFixnum());
        case SIGNATURE_POINTER:
            lastError_ = format(NULL, UC("'pointer' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        default:
            MOSH_ASSERT(false);
            return false;
        }
    // Flonum -> double
    } else if (obj.isFlonum()) {
        switch (signature) {
        case SIGNATURE_BOOL:
            lastError_ = format(NULL, UC("'bool' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT:
            lastError_ = format(NULL, UC("'int' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_FLOAT:
            return pushFloat(obj.toFlonum()->value());
        case SIGNATURE_DOUBLE:
            return pushDouble(obj.toFlonum()->value());
        case SIGNATURE_INT64:
            lastError_ = format(NULL, UC("'int64_t' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_POINTER:
            lastError_ = format(NULL, UC("'pointer' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        default:
            MOSH_ASSERT(false);
            return false;
        }
    } else if (obj.isBignum()) {
        switch (signature) {
        case SIGNATURE_BOOL:
            lastError_ = format(NULL, UC("'bool' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT:
            return pushIntptr_t(obj.toBignum()->toS64());
        case SIGNATURE_FLOAT:
            lastError_ = format(NULL, UC("'float' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_DOUBLE:
            lastError_ = format(NULL, UC("'double' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT64:
            return pushInt64_t(obj.toBignum()->toS64());
        case SIGNATURE_POINTER:
            lastError_ = format(NULL, UC("'pointer' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        default:
            MOSH_ASSERT(false);
            return false;
        }
    // String -> char* (utf-8 ascii only)
    } else if (obj.isString()) {
        switch (signature) {
        case SIGNATURE_BOOL:
            lastError_ = format(NULL, UC("'bool' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT:
            lastError_ = format(NULL, UC("'int' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_FLOAT:
            lastError_ = format(NULL, UC("'float' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_DOUBLE:
            lastError_ = format(NULL, UC("'double' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT64:
            lastError_ = format(NULL, UC("'int64_t' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_POINTER:
            return pushIntptr_t((intptr_t)(obj.toString()->data().ascii_c_str()));
        default:
            MOSH_ASSERT(false);
            return false;
        }
    // ByteVector -> char*
    } else if (obj.isByteVector()) {
        switch (signature) {
        case SIGNATURE_BOOL:
            lastError_ = format(NULL, UC("'bool' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT:
            lastError_ = format(NULL, UC("'int' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_FLOAT:
            lastError_ = format(NULL, UC("'float' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_DOUBLE:
            lastError_ = format(NULL, UC("'double' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT64:
            lastError_ = format(NULL, UC("'int64_t' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_POINTER:
            return pushIntptr_t((intptr_t)(obj.toByteVector()->data()));
        default:
            MOSH_ASSERT(false);
            return false;
        }
    } else if (obj.isPointer()) {
        switch (signature) {
        case SIGNATURE_BOOL:
            lastError_ = format(NULL, UC("'bool' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT:
            lastError_ = format(NULL, UC("'int' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_FLOAT:
            lastError_ = format(NULL, UC("'float' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_DOUBLE:
            lastError_ = format(NULL, UC("'double' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT64:
            lastError_ = format(NULL, UC("'int64_t' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_POINTER:
            return pushIntptr_t(obj.toPointer()->pointer());
        default:
            MOSH_ASSERT(false);
            return false;
        }
    } else if (obj.isBoolean()) {
        switch (signature) {
        case SIGNATURE_BOOL:
            return pushIntptr_t(obj.isTrue() ? 1 : 0);
        case SIGNATURE_INT:
            lastError_ = format(NULL, UC("'int' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_FLOAT:
            lastError_ = format(NULL, UC("'float' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_DOUBLE:
            lastError_ = format(NULL, UC("'double' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_INT64:
            lastError_ = format(NULL, UC("'int64_t' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        case SIGNATURE_POINTER:
            lastError_ = format(NULL, UC("'pointer' required but got ~a"), Pair::list1(obj)).toString()->data();
            return false;
        default:
            MOSH_ASSERT(false);
            return false;
        }
    } else {
        lastError_ = UC("unsupported ffi argument");
        return false;
    }
}

const ucs4char* CStack::getLastError() const
{
    return lastError_.c_str();
}

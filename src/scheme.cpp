/*
 * scheme.cpp - Scheme system objects and functions.
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
 *  $Id$
 */

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h> // for socklen_t
#elif !defined(MONA)
#define _XOPEN_SOURCE 700 // for srandom(SUSv4)
#endif

#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Symbol.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "OSCompat.h"
extern "C" {
#include <gmp.h>
}
#include <time.h>
#if HAVE_OPENSSL
#include <openssl/ssl.h>
#include <openssl/err.h>
#endif

#ifdef MONA
#include <sys/error.h>
#endif
#include "OSCompatThread.h"

using namespace scheme;

#define USE_GMP_WITH_GC 0

#if USE_GMP_WITH_GC
static void* gmp_alloc(size_t size)
{
    return GC_malloc(size);
}

static void* gmp_realloc(void *ptr, size_t oldSize, size_t newSize)
{
    return GC_REALLOC(ptr, newSize);
}

static void gmp_free(void *ptr, size_t size)
{
    GC_free(ptr);
}

#else

static void* gmp_alloc(size_t size)
{
    return malloc(size);
}

static void* gmp_realloc(void *ptr, size_t oldSize, size_t newSize)
{
    static uintptr_t totalSize = 0;
    totalSize += newSize;
    // At least every 30MB, we invoke GC()
    if (totalSize > 30 * 1024 * 1024) {
        GC_gcollect();
        totalSize = 0;
    }
    return realloc(ptr, newSize);
}


static void gmp_free(void *ptr, size_t size)
{
    free(ptr);
}
#endif

#ifdef _MSC_VER
BOOL WINAPI handler(DWORD ctrlChar)
{
    if(CTRL_C_EVENT == ctrlChar){
        exit(-1);
        return FALSE;
    }
    return TRUE;
}
#endif


#if defined(_WIN32) || defined(MONA)
#define srandom srand
#endif

extern void initNonGenerativeRtd();

#ifdef _WIN32
// nothing to do here..
void null_gc_warn_proc(char* msg, GC_word arg){
    (void)msg;
    (void)arg;
}
#endif

void mosh_init()
{
    // MOSH_GENSYM_PREFIX and equal? need this.
    srandom(static_cast<unsigned int>(time(nullptr)));

#if HAVE_OPENSSL
    SSL_load_error_strings();
    SSL_library_init();
#endif

#ifdef MONA
#if 0
    intptr_t ret = monapi_enable_stacktrace("/APPS/MOSH.APP/MOSH.MAP");
    if (ret != M_OK) {
        fprintf(stderr, "syscall_stack_trace_enable error %d\n", ret);
        exit(-1);
    }
#endif
#endif // MONA
#ifdef USE_BOEHM_GC
    GC_INIT();
#ifdef _WIN32
    // override default GC warn proc
    GC_set_warn_proc(null_gc_warn_proc); 
#endif
    // N.B
    // Since GNU MP mpz makes many many "false pointer",
    // we allocate gmp buffers by malloc not GC_malloc.
    // Allocated memory are freed on Bignum's destructor.
#ifndef MONA
    mp_set_memory_functions(gmp_alloc, gmp_realloc, gmp_free);
#endif
#endif
// moved to VM constructor
//    initCprocedures();
    Flonum::initialize();
    Thread::initialize();
    Symbol::initBuitinSymbols();


#ifdef _WIN32
    WSADATA data;
    WSAStartup(MAKEWORD(2, 2), &data);
#endif

#ifdef _MSC_VER
    ::SetConsoleCtrlHandler(handler, TRUE);
#endif
    initOSConstants();

//     // psyntax pre-compilation requires MOSH_GENSYM_PREFIX
//     if (NULL == getEnv(UC("MOSH_GENSYM_PREFIX"))) {
//         // 'A', 'B' and 'C' are reserved for psyntax expansion
//         const char* data = "abcdefghijklmopqrstuvwxyzDEFGHIJKLMOPQRSTUVWXYZ";
//         ucs4char prefix = data[random() % strlen(data)];
// //        printf("[%c]", prefix);
//         setEnv(UC("MOSH_GENSYM_PREFIX"), &prefix);
//     }

}



extern "C" void dont_free(void* p)
{
}

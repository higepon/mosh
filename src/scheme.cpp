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
#endif
#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Symbol.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "OSCompat.h"
#include <gmp.h>
#include "OSCompatThread.h"
#include "ErrorProcedures.h"

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


#ifdef _WIN32
#define srandom srand
#endif

extern void jitStackShowTrace();

static void signal_handler(int sig, siginfo_t *si, void *unused)
{
    printf("\n**** ");
    switch(sig) {
    case SIGSEGV:
        printf("SIGSEGV");
        break;
    case SIGILL:
        printf("SIGILL");
        break;
    case SIGTRAP:
        printf("SIGTRAP");
        break;
    case SIGBUS:
        printf("SIGBUS");
        break;
    default:
        printf("unknown signal %d", sig);
    }
    printf("\n");
    jitStackShowTrace();
    signal(sig, SIG_DFL);
}

static void setSignalHandler()
{
    stack_t ss;
    ss.ss_sp = GC_malloc(SIGSTKSZ);
    if (ss.ss_sp == NULL) {
        fprintf(stderr, "setSignalHandler malloc failed\n");
        exit(-1);
    }
    ss.ss_size = SIGSTKSZ;
    ss.ss_flags = 0;
    if (sigaltstack(&ss, NULL) == -1) {
        fprintf(stderr, "setSignalHandler sigaltstack failed\n");
        exit(-1);
    }

    struct sigaction sa;
    sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = signal_handler;
    if (sigaction(SIGSEGV, &sa, NULL) != 0 ||
        sigaction(SIGTRAP, &sa, NULL) != 0 ||
        sigaction(SIGBUS, &sa, NULL) != 0 ||
        sigaction(SIGILL, &sa, NULL) != 0) {
        fprintf(stderr, "setSignalHandler sigaction failed\n");
        exit(-1);
    }
}

void mosh_init()
{
    // For Jit compiler, we handle SIGSEGV and etc.
    // Some SIGSEGV can't be handled with this. Why? Boehm GC?
    setSignalHandler();

    // MOSH_GENSYM_PREFIX and equal? need this.
    srandom(time(NULL));
#ifdef USE_BOEHM_GC
    GC_INIT();
    // N.B
    // Since GNU MP mpz makes many many "false pointer",
    // we allocate gmp buffers by malloc not GC_malloc.
    // Allocated memory are freed on Bignum's destructor.
    mp_set_memory_functions(gmp_alloc, gmp_realloc, gmp_free);
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

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
#include <gc.h>
#include <gmp.h>

using namespace scheme;
extern void initCprocedures();
void mosh_init()
{
#ifdef USE_BOEHM_GC
    GC_INIT();
    mp_set_memory_functions(GC_malloc, my_realloc, my_dont_free);
#endif
    initCprocedures();
    Flonum::initialize();
    Symbol::initBuitinSymbols();
#ifdef _WIN32
	WSADATA data;
	WSAStartup(MAKEWORD(2, 2), &data);
#endif
    initOSConstants();
}

void* my_realloc(void *ptr, size_t oldSize, size_t newSize)
{
    return GC_REALLOC(ptr, newSize);
}

void my_dont_free(void *ptr, size_t size)
{
}

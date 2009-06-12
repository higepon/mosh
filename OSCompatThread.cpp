/*
 * OSCompatThread.cpp - thread interfaces.
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
 *  $Id: OSCompatThread.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "OSCompat.h"
#include "OSCompatThread.h"

using namespace scheme;

ThreadSpecificKey* Thread::selfKey;

struct StubInfo
{
    void* (*func)(void*);
    void* argument;
    Thread* thread;
    ThreadSpecificKey* selfKey;
};

static void* stubFunction(void* param)
{
    StubInfo* info = (StubInfo*)param;
    if (!Thread::setSpecific(info->selfKey, info->thread)) {
        fprintf(stderr, "fatal : Thread store self\n");
        exit(-1);
    }
    return info->func(info->argument);
}

bool Thread::create(void* (*start)(void*), void* arg)
{
    StubInfo* info = new StubInfo;
    info->func = start;
    info->argument = arg;
    info->thread = this;
    info->selfKey = selfKey;
//    pthread_attr_t thattr;
//    pthread_attr_init(&thattr);
//    pthread_attr_setdetachstate(&thattr, PTHREAD_CREATE_DETACHED);
    if (GC_pthread_create(&thread_, NULL, stubFunction , info) == 0) {
//        pthread_attr_destroy(&thattr);
        return true;
    } else {
        setLastError();
//        pthread_attr_destroy(&thattr);
        return false;
    }
}

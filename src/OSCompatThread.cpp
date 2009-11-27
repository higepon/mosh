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
#include "gc.h"

using namespace scheme;

ThreadSpecificKey* Thread::selfKey;


#ifdef _WIN32
static unsigned int __stdcall stubFunction(void* param)
#else
static void* stubFunction(void* param)
#endif
{
    Thread::StubInfo* info = (Thread::StubInfo*)param;
    if (!Thread::setSpecific(info->selfKey, info->thread)) {
        fprintf(stderr, "fatal : Thread store self\n");
        exit(-1);
    }
    info->returnValue = info->func(info->argument);
#ifdef _WIN32
	return (unsigned int)info->returnValue;
#else
    return info->returnValue;
#endif
}

bool Thread::create(void* (*start)(void*), void* arg)
{
    stubInfo_ = new StubInfo;
    stubInfo_->func = start;
    stubInfo_->argument = arg;
    stubInfo_->thread = this;
    stubInfo_->selfKey = selfKey;
#ifdef _WIN32
    unsigned int threadId;
    thread_ = (HANDLE)GC_beginthreadex(0, 0, stubFunction,stubInfo_, 0, &threadId);
    return thread_ != 0;
#else
    if (GC_pthread_create(&thread_, NULL, stubFunction , stubInfo_) == 0) {
        return true;
    } else {
        setLastError();
        return false;
    }
#endif
}

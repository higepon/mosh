/*
 * VM-Profiler.cpp - 
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
 *  $Id: VM-Profiler.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <sys/time.h>
#include <signal.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "VM.h"
#include "ErrorProcedures.h"
#include "TextualOutputPort.h"
#include "EqHashTable.h"
#include "StringProcedures.h"

using namespace scheme;

#ifdef ENABLE_PROFILER
extern VM* theVM;
extern Object stringTosymbolEx(Object args);
const int VM::SAMPLE_NUM = 50000;

extern  void signal_handler(int signo);
// static void signal_handler(int signo)
// {
//     theVM->collectProfile();
// }

void VM::initProfiler()
{
    samples_ = Object::makeObjectArray(SAMPLE_NUM);
    callSamples_ = Object::makeObjectArray(SAMPLE_NUM);
    callHash_ = Object::makeEqHashTable();
    totalSampleCount_ = 0;
    for (int i = 0; i < SAMPLE_NUM; i++) {
        samples_[i] = Object::Nil;
        callSamples_[i] = Object::Nil;
    }
    struct sigaction act;
    act.sa_handler = &signal_handler; // set signal_handler
    act.sa_flags = SA_RESTART;        // restart system call after signal handler

    if (sigaction(SIGPROF, &act, NULL) != 0) {
        callAssertionViolationImmidiaImmediately(this, "profiler", "sigaction failed");
    }
    startTimer();
}

void VM::stopProfiler()
{
    stopTimer();
}

void VM::startTimer()
{
    const int INTERVAL_USEC = 10 * 1000;
    struct itimerval tval, oval;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = INTERVAL_USEC;
    tval.it_value.tv_sec = 0;
    tval.it_value.tv_usec = INTERVAL_USEC;
    setitimer(ITIMER_PROF, &tval, &oval);
    profilerRunning_ = true;
}

void VM::stopTimer()
{
    profilerRunning_ = false;
    struct itimerval tval, oval;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = 0;
    tval.it_value.tv_sec = 0;
    tval.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &tval, &oval);
}

void VM::collectProfile()
{
    static int i = 0;
    if (!profilerRunning_) return;
    if (i >= SAMPLE_NUM) {
        errorPort_.toTextualOutputPort()->display(UC("buffer full profiler stopped."));
        stopTimer();
    } else if ((*pc_).val == labelReturn_ && ac_.isCProcedure()) {
        samples_[i++] = ac_;
    } else {
        samples_[i++] = cl_;
    }
    totalSampleCount_++;
}

// this is slow, because it creates namespace hash for each call.
Object VM::getClosureName(Object closure)
{
    EqHashTable* nameSpace = nameSpace_.toEqHashTable()->swap().toEqHashTable();
    if (closure.isCProcedure()) {
        return getCProcedureName(closure);
    } else if (closure.isClosure()) {
        const Object name = nameSpace->ref(closure, notFound_);
        if (name == notFound_) {
            return Object::False;
        } else {
            return stringTosymbol(name.cdr());
        }
    } else {
        return Object::False;
    }
}

void VM::storeCallSample()
{
    EqHashTable* callHash = callHash_.toEqHashTable();
    for (int i = 0; i < SAMPLE_NUM; i++) {
        const Object proc = callSamples_[i];
        if (proc.isNil()) continue;
        const Object count = callHash->ref(proc, Object::False);
        if (count.isNil()) {
            /* */
        } else if (count.isFixnum()) {
            callHash->set(proc, Object::makeFixnum(count.toFixnum() + 1));
        } else {
            callHash->set(proc, Object::makeFixnum(1));
        }
        callSamples_[i] = Object::Nil;
    }
}

Object VM::values(int num, const Object* v)
{
    if (0 == num) {
        numValues_ = 0;
        return Object::Undef;
    }
    for (int i = 1; i < num; i++) {
        if (i >= maxNumValues_) {
            callAssertionViolationAfter(this, "values", "too many values", Pair::list1(Object::makeFixnum(i)));
            return Object::Undef;
        }
        values_[i - 1] = v[i];
    }
    numValues_ = num;
    return v[0]; // set to ac_ later.
}

Object VM::getProfileResult()
{
    profilerRunning_ = false;
    stopProfiler();
    Object ret = Object::Nil;
    for (int i = 0; i < SAMPLE_NUM; i++) {
        const Object o = samples_[i];
        if (o.isProcedure()) {
            ret = Pair::append2(ret, L1(o));
        }
    }

    storeCallSample();
    return Object::cons(Object::makeFixnum(totalSampleCount_), Object::cons(callHash_, ret));
}

#endif // ENABLE_PROFILER


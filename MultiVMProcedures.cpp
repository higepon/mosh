/*
 * MultiVMProcedures.cpp - Mulitple VM.
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
 *  $Id: MultiVMProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "Symbol.h"
#include "scheme.h"
#include "Closure.h"
#include "EqHashTable.h"
#include "VM.h"
#include "VM-inl.h"
#include "VMFactory.h"
#include "ProcedureMacro.h"
#include "Transcoder.h"
#include "OSCompat.h"
#include "OSCompatThread.h"
#include "MultiVMProcedures.h"

using namespace scheme;

static void* vmEntry(void* param);

// this should be global and shared between VM instances.
static EqHashTable* processes = NULL;
ThreadSpecificKey* scheme::vmKey = NULL;

static EqHashTable* processesTable()
{
    if (NULL == processes) {
        processes = new EqHashTable;
    }
    return processes;
}

// (register name #<process>)
Object scheme::registerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("register");
    checkArgumentLength(2);
    argumentCheckSymbol(0, name);
    processesTable()->set(name, argv[1]);
    return Object::Undef;
}

// (whereis name) => registered or false
Object scheme::whereisEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("whereis");
    checkArgumentLength(1);
    argumentCheckSymbol(0, name);
    return processesTable()->ref(name, Object::False);
}

// (vm-self) => vm
Object scheme::vmSelfEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vm-self");
    checkArgumentLength(0);
    return Object::makeVM(theVM);
}

// (main-vm?) => boolean
Object scheme::mainVmPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("main-vm?");
    checkArgumentLength(0);
    return Object::makeBool(theVM->isMainThread());
}


// (vm? obj) => boolean
Object scheme::vmPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vm?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isVM());
}

// (vm-start! vm) => undef
Object scheme::vmStartDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vm-start!");
    checkArgumentLength(1);
    argumentAsVM(0, vm);
    Thread* thread = new Thread;
    vm->setThread(thread);

    // N.B.
    // We store the VM instance in thread specific storage.
    // Used for storing yylex and re2c which has only global interfaces.
    thread->create(vmEntry, vm);
    return Object::Undef;
}

// (vm-set-value! vm key value)
Object scheme::vmSetValueDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vm-set-value!");
    checkArgumentLength(3);
    argumentAsVM(0, vm);
    argumentCheckSymbol(1, key);
    vm->setValueSymbol(key, argv[2]);
    return Object::Undef;
}

// (vm-join! vm) => return value of thread's exit value.
Object scheme::vmJoinDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vm-join!");
    checkArgumentLength(1);
    argumentAsVM(0, vm);
    Object* ret;
    vm->thread()->join((void**)&ret);
    return *ret;
}

// (make-vm thunk-sexp import-spec-sexp . name) => #<vm>
Object scheme::makeVmEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-vm");
    checkArgumentLengthBetween(2, 3);
    argumentCheckPair(0, thunkSexp);
    argumentCheckPair(1, importSpecSexp);
    VMFactory factory;
    const int INITIAL_STACK_SIZE = 5000;
    const bool isProfilerON = false;
    VM* vm = factory.create(INITIAL_STACK_SIZE, isProfilerON);
    vm->setValueString(UC("%loadpath"), Object::False); // todo not false
    vm->setValueString(UC("%verbose"), Object::False);
    vm->setValueString(UC("*command-line-args*"), Pair::list1("./test/stack-trace2.scm"));
    vm->setValueString(UC("%vm-import-spec"), importSpecSexp);
    vm->setValueString(UC("%vm-thunk"), thunkSexp);
    if (argc == 3) {
        argumentAsString(2, vmName);
        vm->setName(vmName->data());
    }
    return Object::makeVM(vm);
}

// (make-condition-variable . name) => #<condition variable>
Object scheme::makeConditionVariableEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-condition-variable");
    checkArgumentLengthBetween(0, 1);
    if (argc == 0) {
        return Object::makeConditionVariable(new ConditionVariable);
    } else {
        argumentAsString(0, name);
        return Object::makeConditionVariable(new ConditionVariable(name->data()));
    }
}

// (condition-variable-notify-all! c) => boolean
Object scheme::conditionVariableNotifyAllDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition-variable-notify-all!");
    checkArgumentLength(1);
    argumentAsConditionVariable(0, c);
    return Object::makeBool(c->notifyAll());
}

// (condition-variable-notify! c) => boolean
Object scheme::conditionVariableNotifyDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition-variable-notify!");
    checkArgumentLength(1);
    argumentAsConditionVariable(0, c);
    return Object::makeBool(c->notify());
}

// (condition-variable-wait c mutex . timeout-msec) => boolean (returns false when timeout)
Object scheme::conditionVariableWaitDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("condition-variable-wait!");
    checkArgumentLengthBetween(2, 3);
    argumentAsConditionVariable(0, c);
    argumentAsMutex(1, mutex);
    if (2 == argc) {
        return Object::makeBool(c->wait(mutex));
    } else {
        argumentAsFixnum(2, timeout);
        return Object::makeBool(c->waitWithTimeout(mutex, timeout));
    }
}

// (mutex-unlock! mutex) => undef
Object scheme::mutexUnlockDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("mutex-unlock!");
    checkArgumentLength(1);
    argumentAsMutex(0, mutex);
    mutex->unlock();
    return Object::Undef;
}

// (mutex-try-lock! mutex) => boolean
Object scheme::mutexTryLockDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("mutex-try-lock!");
    checkArgumentLength(1);
    argumentAsMutex(0, mutex);
    return Object::makeBool(mutex->tryLock());
}

// (mutex-lock! mutex) => undef
Object scheme::mutexLockDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("mutex-lock!");
    checkArgumentLength(1);
    argumentAsMutex(0, mutex);
    mutex->lock();
    return Object::Undef;
}

// (make-mutex) => #<mutex>
Object scheme::makeMutexEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-mutex");
    checkArgumentLength(0);
    return Object::makeMutex(new Mutex);
}

// (mutex? obj) => boolean
Object scheme::mutexPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("mutex?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isMutex());
}

// thread start stub
void* vmEntry(void* param)
{
    VM* vm = (VM*)param;
    setCurrentVM(vm);
    const Object ret = vm->activateR6RSMode(false);
//    Thread::exit();
    return new Object(ret);
}


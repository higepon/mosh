/*
 * ProcessProcedures.cpp -
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
 *  $Id: ProcessProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#ifdef _WIN32
    #include <stdlib.h>
    #include <io.h>
    #include <direct.h>
    #include <process.h>
#else
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/wait.h>
#endif
#include <sys/types.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "scheme.h"
#include "EqHashTable.h"
#include "VM.h"
#include "ProcessProcedures.h"
#include "ProcedureMacro.h"
#include "BinaryOutputPort.h"
#include "BinaryInputPort.h"
#include "Bignum.h"
#include "OSCompat.h"
#include "Symbol.h"

using namespace scheme;

Object scheme::currentDirectoryEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("current-directory");
    checkArgumentLength(0);
    const Object path = getCurrentDirectory();
    if (path.isFalse()) {
        callAssertionViolationAfter(theVM, procedureName, "current-directory failed", L1(Object(getLastErrorMessage())));
        return Object::Undef;
    } else {
        return path;
    }
}

Object scheme::setCurrentDirectoryDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-current-directory!");
    checkArgumentLength(1);
    argumentAsString(0, path);
    if (setCurrentDirectory(path->data())) {
        return Object::Undef;
    } else {
        callAssertionViolationAfter(theVM, procedureName, "set-current-directory! failed", L2(Object(getLastErrorMessage()), argv[0]));
        return Object::Undef;
    }
}


Object scheme::internalForkEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%fork");
#if defined(_WIN32) || defined(MONA)
    callAssertionViolationAfter(theVM, procedureName, "can't fork");
    return Object::makeString(UC("<not-supported>"));
#else
    checkArgumentLength(0);
    const pid_t pid = fork();
    if (-1 == pid) {
        callAssertionViolationAfter(theVM, procedureName, "can't fork", L1(Object(getLastErrorMessage())));
        return Object::Undef;
    }

//     // for Shell mode.
//     // VM(=parent) ignores SIGINT, but child use default handler
//     if (pid == 0) {
//         signal(SIGINT, SIG_DFL);
//     }
    return Bignum::makeIntegerFromU64(pid);
#endif
}

Object scheme::internalWaitpidEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%waitpid");
#if defined(_WIN32) || defined(MONA)
    callAssertionViolationAfter(theVM, procedureName, "failed");
    return Object::makeString(UC("<not-supported>"));
#else
    checkArgumentLength(1);
    argumentCheckIntegerValued(0, pid);
    MOSH_ASSERT(pid.isBignum() || pid.isFixnum());
    pid_t target;
    if (pid.isBignum()) {
        target = pid.toBignum()->toU64();
    } else {
        target = pid.toFixnum();
    }
    int status;
    pid_t child = waitpid(target, &status, 0);
    if (-1 == child) {
        callAssertionViolationAfter(theVM, procedureName, "failed", L2(argv[0], Object(getLastErrorMessage())));
        return Object::Undef;
    }

    return theVM->values3(Bignum::makeIntegerFromU64(child),
                          processExitValue(status),
                          processTerminationSignal(status));
#endif
}

Object scheme::internalPipeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%pipe");
#if defined(_WIN32) || defined(MONA)
    callAssertionViolationAfter(theVM, procedureName, "pipe() failed");
    return Object::makeString(UC("<not-supported>"));
#else
    checkArgumentLength(0);
    int fds[2];
    if (-1 == pipe(fds)) {
        callAssertionViolationAfter(theVM, procedureName, "pipe() failed");
        return Object::Undef;
    }
    const Object fds_0 = Object::makeBinaryInputPort(new File(fds[0]));
    const Object fds_1 = Object::makeBinaryOutputPort(new File(fds[1]));
    theVM->registerPort(fds_1);
    return theVM->values2(fds_0, fds_1);
#endif
}

// (%exec command args in out err)
// in : binary input port or #f. #f means "Use stdin".
Object scheme::internalExecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%exec");
#ifdef MONA
    return callImplementationRestrictionAfter(theVM, procedureName, "not implmented", Pair::list1(argv[0]));
#else
    checkArgumentLength(3);
    argumentAsString(0, command);
    argumentCheckList(1, args);
    argumentCheckList(2, env);

    int length;
    Object pair;

    const char *path_ = command->data().ascii_c_str();

    // XXX: Check in scheme wrapper that this is non-null.
    length = Pair::length(args);
    char** argv_ = new(GC) char*[length + 1];
    pair = args;
    for (int i = 0; i < length; i++) {
        argv_[i] = pair.car().toString()->data().ascii_c_str();
        pair = pair.cdr();
    }
    argv_[length] = (char*)nullptr;

    length = Pair::length(env);
    char** envp_ = new(GC) char*[length + 1];
    pair = env;
    for (int i = 0; i < length; i++) {
        envp_[i] = pair.car().toString()->data().ascii_c_str();
        pair = pair.cdr();
    }
    envp_[length] = (char*)nullptr;

    execve(path_, (char* const*) argv_, (char* const*) envp_);
    
    // If we ever return we have failed by definition.
    return Object::makeFixnum(errno);
#endif
}

Object scheme::internalGetpidEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%getpid");
    checkArgumentLength(0);
#ifdef _WIN32
    return Object::makeFixnum(GetCurrentProcessId());
#elif defined(MONA)
    return Object::makeFixnum(MonAPI::System::getProcessID());
#else
    return Object::makeFixnum(getpid());
#endif
}

Object scheme::internalDupEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%dup");
    checkArgumentLength(2);

    File** files = new(GC) File*[argc];
    for (int i = 0; i < argc; i++) {
        Object obj = argv[i];
        BinaryPort* p;

        if (obj.isBinaryInputPort()) {
            p = obj.toBinaryInputPort();
        } else if (obj.isBinaryOutputPort()) {
            p = obj.toBinaryOutputPort();
        } else {
            callAssertionViolationAfter(theVM,
                                        procedureName,
                                        "invalid port type",
                                        L1(argv[i]));
            return Object::Undef;
        }

        files[i] = p->getFile();
    }

    if (files[0]->dup(*(files[1]))) {
        return Object::True;
    } else {
        return Object::False;
    }
}    

Object scheme::processListEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("process-list");
    checkArgumentLength(0);
#ifdef MONA
    syscall_set_ps_dump();
    PsInfo info;
    Object ret = Object::Nil;
    while (syscall_read_ps_dump(&info) == M_OK) {
        ret = Object::cons(Pair::list6(Object::cons(Symbol::intern(UC("tid")), Bignum::makeIntegerFromU32(info.tid)),
                                       Object::cons(Symbol::intern(UC("state")), Symbol::intern(info.state ? UC("running") : UC("waiting"))),
                                       Object::cons(Symbol::intern(UC("cr3")), Bignum::makeIntegerFromU32(info.cr3)),
                                       Object::cons(Symbol::intern(UC("name")), info.name),
                                       Object::cons(Symbol::intern(UC("esp")), Bignum::makeIntegerFromU32(info.esp)),
                                       Object::cons(Symbol::intern(UC("eip")), Bignum::makeIntegerFromU32(info.eip))),
                           ret);
    }
    return ret;
#else
    return Object::Nil;
#endif
}

Object scheme::processTerminateDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("process-terminate!");
    checkArgumentLength(1);
#ifdef MONA
    argumentAsU32(0, tid);
    if (syscall_kill_thread(tid) == M_OK) {
        return Object::True;
    } else {
        return Object::False;
    }
#else
    return callImplementationRestrictionAfter(theVM, procedureName, "not implmented", Pair::list1(argv[0]));
#endif

}



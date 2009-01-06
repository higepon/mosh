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

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "scheme.h"
#include "VM.h"
#include "ProcessProcedures.h"
#include "ProcedureMacro.h"
#include "Bignum.h"

using namespace scheme;

Object scheme::internalForkEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%fork");
    checkArgumentLength(0);
    const pid_t pid = fork();
    if (-1 == pid) {
        callAssertionViolationAfter(theVM, procedureName, "can't fork", L1(strerror(errno)));
        return Object::Undef;
    }
    return Bignum::makeIntegerFromU64(pid);
}

Object scheme::internalWaitpidEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%waitpid");
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
        callAssertionViolationAfter(theVM, procedureName, "failed", L1(strerror(errno)));
        return Object::Undef;
    }

    static Object values[2];
    values[0] = Bignum::makeIntegerFromU64(child);
    values[1] = Bignum::makeInteger(WEXITSTATUS(status));
    return theVM->values(2, values);
}

Object scheme::internalExecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%exec");
    checkArgumentLength(2);
    argumentAsString(0, command);
    argumentCheckList(1, args);

    const int length = Pair::length(args);
    char** p = new(GC) char*[length + 1];
    Object pair = args;
    for (int i = 0; i < length; i++) {
        p[i] = pair.car().toString()->data().ascii_c_str();
        pair = pair.cdr();
    }
    p[length] = (char*)NULL;
    const int ret = execvp(command->data().ascii_c_str(), p);

    if (-1 == ret) {
        callAssertionViolationAfter(theVM, procedureName, "failed", L1(strerror(errno)));
        return Object::Undef;
    }
    // this procedure doesn't return
    return Object::Undef;
}

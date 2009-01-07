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
#include "BinaryOutputPort.h"
#include "BinaryInputPort.h"
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
        callAssertionViolationAfter(theVM, procedureName, "failed", L2(argv[0], strerror(errno)));
        return Object::Undef;
    }

    return theVM->values2(Bignum::makeIntegerFromU64(child),
                          Bignum::makeInteger(WEXITSTATUS(status)));
}

Object scheme::internalPipeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%pipe");
    checkArgumentLength(0);
    int fds[2];
    if (-1 == pipe(fds)) {
        callAssertionViolationAfter(theVM, procedureName, "pipe() failed");
        return Object::Undef;
    }
    return theVM->values2(Bignum::makeInteger(fds[0]), Object::makeBinaryOutputPort(fds[1]));
}

// (%exec command args in out err)
// in : binary input port or #f. #f means "Use stdin".
Object scheme::internalExecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%exec");
    checkArgumentLength(5);
    argumentAsString(0, command);
    argumentCheckList(1, args);

    const Object in  = argv[2];
    const Object out = argv[3];
    const Object err = argv[4];

    if (in.isBinaryInputPort()) {
        const int newfd = in.toBinaryInputPort()->fileno();
        if (newfd == BinaryInputPort::INVALID_FILENO) {
            callAssertionViolationAfter(theVM, procedureName, "input port is not file port", L1(in));
            return Object::Undef;
        }
        if (-1 == dup2(newfd, fileno(stdin))) {
            callAssertionViolationAfter(theVM, procedureName, "dup failed", L1(strerror(errno)));
            return Object::Undef;
        }
    }

    if (out.isBinaryOutputPort()) {
        const int newfd = out.toBinaryOutputPort()->fileno();
        if (newfd == BinaryOutputPort::INVALID_FILENO) {
            callAssertionViolationAfter(theVM, procedureName, "output port is not file port", L1(argv[2]));
            return Object::Undef;
        }
        if (-1 == dup2(newfd, fileno(stdout))) {
            callAssertionViolationAfter(theVM, procedureName, "dup failed", L1(strerror(errno)));
            return Object::Undef;
        }
    }

    if (err.isBinaryOutputPort()) {
        const int newfd = err.toBinaryOutputPort()->fileno();
        if (newfd == BinaryOutputPort::INVALID_FILENO) {
            callAssertionViolationAfter(theVM, procedureName, "error output port is not file port", L1(argv[2]));
            return Object::Undef;
        }
        if (-1 == dup2(newfd, fileno(stderr))) {
            callAssertionViolationAfter(theVM, procedureName, "dup failed", L1(strerror(errno)));
            return Object::Undef;
        }
    }

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
        callAssertionViolationAfter(theVM, procedureName, "failed", L2(argv[0], strerror(errno)));
        return Object::Undef;
    }

    // this procedure doesn't return
    return Object::Undef;
}

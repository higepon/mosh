/*
 * TestingVM.cpp - 
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
 *  $Id: TestingVM.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "TestingVM.h"
#include "ErrorProcedures.h"
#include "PortProcedures.h"
#include "Closure.h"
#include "EqHashTable.h"
#include "Symbol.h"
#include "VM.h"
#include "Gloc.h"
#include "VM-inl.h"
#include <setjmp.h>

using namespace scheme;

int TestingVM::exit(int status)
{
    flushAllPorts();
    return status;
}

void TestingVM::flushAllPorts()
{
    Ports::iterator it = activePorts_.begin();
    while (it != activePorts_.end()) {
        const Object outputPort = *it;
        if (outputPort.isBinaryOutputPort()) {
            lastFlushType_ = FLUSH_BINARY_OUTPUT_PORT;
        } else if (outputPort.isBinaryInputOutputPort()) {
            lastFlushType_ = FLUSH_BINARY_INPUT_OUTPUT_PORT;
        } else if (outputPort.isTextualOutputPort()) {
            lastFlushType_ = FLUSH_TEXTUAL_OUTPUT_PORT;
        } else if (outputPort.isTextualInputOutputPort()) {
            lastFlushType_ = FLUSH_TEXTUAL_INPUT_OUTPUT_PORT;
        }
        it++;
    }
    VM::flushAllPorts();
}

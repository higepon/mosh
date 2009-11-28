/*
 * VMFactory.cpp - 
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
 *  $Id: VMFactory.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "EqHashTable.h"
#include "Symbol.h"
#include "Closure.h"
#include "VM.h"
#include "VM-inl.h"
#include "OSCompat.h"
#include "StandardOutputPort.h"
#include "StandardErrorPort.h"
#include "StandardInputPort.h"


#include "VMFactory.h"

using namespace scheme;

VM* VMFactory::create(int initialStackSize, bool isProfilerOn)
{
    // At first, we shared the standard ports between VMs.
    // But it causes inconsistent buffering state on Buffered port.
    // So we never share the standard ports.

    // N.B. For debug safety, we never close() the standard ports.
    const Object inPort    = Object::makeTextualInputPort(new StandardInputPort,
                                                          File::STANDARD_IN.isUTF16Console() ? createNativeConsoleTranscoder() : createNativeTranscoder());
    const Object outPort   = Object::makeTextualOutputPort(new StandardOutputPort,
                                                           File::STANDARD_OUT.isUTF16Console() ? createNativeConsoleTranscoder() : createNativeTranscoder());
    const Object errorPort = Object::makeTextualOutputPort(new StandardErrorPort,
                                                           File::STANDARD_OUT.isUTF16Console() ? createNativeConsoleTranscoder() : createNativeTranscoder());

    VM* vm = new VM(initialStackSize, outPort, errorPort, inPort, isProfilerOn);
    vm->registerPort(outPort);
    vm->loadCompiler();
    return vm;
}

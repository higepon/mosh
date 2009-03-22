/*
 * BinaryInputPort.h - <binary input port>
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
 *  $Id: BinaryInputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_BINARY_INPUT_PORT_
#define SCHEME_BINARY_INPUT_PORT_

#include "BinaryPort.h"

namespace scheme {

class ByteVector;

class BinaryInputPort : virtual public BinaryPort // for closing port on destructors, we extend gc_cleanup
{
public:
    virtual ~BinaryInputPort() {};
    virtual int getU8() = 0;
    virtual int lookaheadU8() = 0;
    virtual int readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured) = 0;
    virtual int readSome(uint8_t** buf, bool& isErrorOccured) = 0;
    virtual int readAll(uint8_t** buf, bool& isErrorOccured) = 0;
};

} // namespace scheme

#endif // SCHEME_BINARY_INPUT_PORT_

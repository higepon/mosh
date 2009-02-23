/*
 * BufferedFileBinaryInputOutputPort.cpp - 
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
 *  $Id: BufferedFileBinaryInputOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h> // memcpy
#include "Object.h"
#include "Object-inl.h"
#include "HeapObject.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "Symbol.h"
#include "Bignum.h"
#include "BufferedFileBinaryInputOutputPort.h"

using namespace scheme;


BufferedFileBinaryInputOutputPort::BufferedFileBinaryInputOutputPort(ucs4string file){}
BufferedFileBinaryInputOutputPort::~BufferedFileBinaryInputOutputPort(){}

    // port interfaces
bool BufferedFileBinaryInputOutputPort::hasPosition() const{}
bool BufferedFileBinaryInputOutputPort::hasSetPosition() const{}
Object BufferedFileBinaryInputOutputPort::position() const{}
int BufferedFileBinaryInputOutputPort::close(){}
bool BufferedFileBinaryInputOutputPort::setPosition(int position) {}
ucs4string BufferedFileBinaryInputOutputPort::toString(){}

    // binary port interfaces
int BufferedFileBinaryInputOutputPort::open(){}
bool BufferedFileBinaryInputOutputPort::isClosed() const{}
int BufferedFileBinaryInputOutputPort::fileNo() const{}

    // input interfaces
int BufferedFileBinaryInputOutputPort::getU8(){}
int BufferedFileBinaryInputOutputPort::lookaheadU8(){}
int BufferedFileBinaryInputOutputPort::readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured){}
int BufferedFileBinaryInputOutputPort::readSome(uint8_t** buf, bool& isErrorOccured){}
int BufferedFileBinaryInputOutputPort::readAll(uint8_t** buf, bool& isErrorOccured){}

    // output interfaces
int BufferedFileBinaryInputOutputPort::putU8(uint8_t v){}
int BufferedFileBinaryInputOutputPort::putU8(uint8_t* v, int size){}
int BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start /* = 0 */){}
int BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start, int count){}
void BufferedFileBinaryInputOutputPort::bufFlush(){}

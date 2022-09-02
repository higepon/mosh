/*
 * SocketBinaryInputOutputPort.cpp -
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
 *  $Id: SocketBinaryInputOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */
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
#include "SocketBinaryInputOutputPort.h"
#include "ErrorProcedures.h"
#include "OSCompat.h"
#include "PortProcedures.h"
#include "OSCompatSocket.h"

using namespace scheme;

SocketBinaryInputOutputPort::SocketBinaryInputOutputPort(Socket* socket) :
    socket_(socket),
    isClosed_(false),
    isPseudoClosed_(false),
    lastU8_(0xffff)
{
}

SocketBinaryInputOutputPort::~SocketBinaryInputOutputPort()
{
}

// port interfaces
bool SocketBinaryInputOutputPort::hasPosition() const
{
    return false;
}

bool SocketBinaryInputOutputPort::hasSetPosition() const
{
    return false;
}

Object SocketBinaryInputOutputPort::position() const
{
    MOSH_ASSERT(false);
    return Object::Undef;
}

int SocketBinaryInputOutputPort::close()
{
    if (!isClosed_) {
        isClosed_ = true;
        socket_->close();
    }
    return MOSH_SUCCESS;
}

int SocketBinaryInputOutputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

bool SocketBinaryInputOutputPort::setPosition(int64_t position)
{
    MOSH_ASSERT(false);
    return false;
}

ucs4string SocketBinaryInputOutputPort::toString()
{
    ucs4string ret = UC("<binary-input/output-port ");
    ret += socket_->toString();
    ret += UC(">");
    return ret;
}

// binary port interfaces
int SocketBinaryInputOutputPort::open()
{
    if (socket_->isOpen()) {
        return MOSH_SUCCESS;
    } else {
        return MOSH_FAILURE;
    }
}

bool SocketBinaryInputOutputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

ucs4string SocketBinaryInputOutputPort::getLastErrorMessage()
{
    return socket_->getLastErrorMessage();
}

// input interfaces
int SocketBinaryInputOutputPort::getU8()
{
    if (hasLastU8()) {
        return getLastU8();
    } else {
        uint8_t c;
        const int ret = socket_->receive(&c, 1, 0);
        if (0 == ret) {
            return EOF;
        } else if (-1 == ret) {
            throwIOError2(IOError::READ, socket_->getLastErrorMessage());
            return -1;
        } else {
            return c;
        }
    }
}

int SocketBinaryInputOutputPort::lookaheadU8()
{
    const uint8_t ret = getU8();
    setLastU8(ret);
    return ret;
}

int64_t SocketBinaryInputOutputPort::readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured)
{
    if (hasLastU8() && reqSize > 0) {
        buf[0] = getLastU8();
        buf++;
        reqSize--;
    }
    const int readSize = socket_->receive(buf, reqSize, 0);
    if (-1 == readSize) {
        throwIOError2(IOError::READ, socket_->getLastErrorMessage());
        return -1;
    }
    return readSize;
}

int64_t SocketBinaryInputOutputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    gc_vector<uint8_t> data;
    uint8_t readBuf[1024];

    for (;;) {
        const int readSize = socket_->receive(readBuf, 1024, 0);
        if (-1 == readSize) {
            throwIOError2(IOError::READ, socket_->getLastErrorMessage());
            return -1;
        } else if (0 == readSize) {
            break; // EOF
        } else {
            for (int i = 0; i < readSize; i++) {
                data.push_back(readBuf[i]);
            }
        }
    }
    uint8_t* dest = allocatePointerFreeU8Array(data.size());
    for (size_t i = 0; i < data.size(); i++) {
        dest[i] = data[i];
    }
    *buf = dest;
    return data.size();
}

int64_t SocketBinaryInputOutputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    return readAll(buf, isErrorOccured);
}

// output interfaces
int SocketBinaryInputOutputPort::putU8(uint8_t v)
{
    return static_cast<int>(putU8(&v, 1));
}

int64_t SocketBinaryInputOutputPort::putU8(uint8_t* v, int64_t size)
{
    const int64_t writtenSize = socket_->send(v, size, 0);
    if (-1 == writtenSize) {
        throwIOError2(IOError::WRITE, socket_->getLastErrorMessage());
        return -1;
    }
    return writtenSize;
}

int64_t SocketBinaryInputOutputPort::putByteVector(ByteVector* bv, int64_t start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int64_t SocketBinaryInputOutputPort::putByteVector(ByteVector* bv, int64_t start, int64_t count)
{
    uint8_t* buf = bv->data();
    const int64_t writtenSize = socket_->send(&buf[start], count, 0);
    if (-1 == writtenSize) {
        throwIOError2(IOError::WRITE, socket_->getLastErrorMessage());
        return -1;
    }
    return writtenSize;
}

void SocketBinaryInputOutputPort::flush()
{
}


File* SocketBinaryInputOutputPort::getFile()
{
    return NULL;
}

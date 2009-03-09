/*
 * FileBinaryInputOutputPort.cpp -
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
 *  $Id: FileBinaryInputOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
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
#include "FileBinaryInputOutputPort.h"

using namespace scheme;

FileBinaryInputOutputPort::FileBinaryInputOutputPort(const ucs4string& file, int openFlags) :
    fileName_(file),
    isClosed_(false)
{
    fd_ = ::open(file.ascii_c_str(), O_RDWR | O_CREAT | openFlags, 0644);
}

FileBinaryInputOutputPort::~FileBinaryInputOutputPort()
{
}

// port interfaces
bool FileBinaryInputOutputPort::hasPosition() const
{
    return true;
}

bool FileBinaryInputOutputPort::hasSetPosition() const
{
    return true;
}

Object FileBinaryInputOutputPort::position() const
{
    return Bignum::makeInteger(lseek(fd_, 0, SEEK_CUR));
}

int FileBinaryInputOutputPort::close()
{
    if (!isClosed() && fd_ != INVALID_FILENO) {
        isClosed_ = true;
        ::close(fd_);
    }
    return MOSH_SUCCESS;
}

bool FileBinaryInputOutputPort::setPosition(int position)
{
    const int currentOffset = lseek(fd_, position, SEEK_SET);
    if (position == currentOffset) {
        return true;
    } else {
        return false;
    }
}

ucs4string FileBinaryInputOutputPort::toString()
{
    ucs4string ret = UC("<binary-input/output-port ");
    ret += fileName_;
    ret += UC(">");
    return ret;
}

// binary port interfaces
int FileBinaryInputOutputPort::open()
{
    if (INVALID_FILENO == fd_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
}

bool FileBinaryInputOutputPort::isClosed() const
{
    return isClosed_;
}

int FileBinaryInputOutputPort::fileNo() const
{
    return fd_;
}

// input interfaces
int FileBinaryInputOutputPort::getU8()
{
    uint8_t c;
    if (0 == readFromFile(&c, 1)) {
        return EOF;
    } else {
        return c;
    }
}

int FileBinaryInputOutputPort::lookaheadU8()
{
    uint8_t c;
    const int origPositon = lseek(fd_, 0, SEEK_CUR);
    MOSH_ASSERT(origPositon >= 0);
    if (0 == readFromFile(&c, 1)) {
        const int result = lseek(fd_, origPositon, SEEK_SET);
        MOSH_ASSERT(result >= 0);
        return EOF;
    } else {
        const int result = lseek(fd_, origPositon, SEEK_SET);
        MOSH_ASSERT(result >= 0);
        return c;
    }
}

int FileBinaryInputOutputPort::readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured)
{
    const int readSize = readFromFile(buf, reqSize);
    return readSize;
}

int FileBinaryInputOutputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    struct stat st;
    const int result = fstat(fd_, &st);
    MOSH_ASSERT(result == 0); // will never happen?

    const int currentOffset = lseek(fd_, 0, SEEK_CUR);
    MOSH_ASSERT(currentOffset >= 0);
    const int restSize = st.st_size - currentOffset;
    MOSH_ASSERT(restSize >= 0);
    if (restSize == 0) {
        return 0;
    }

    uint8_t* dest = allocatePointerFreeU8Array(restSize);
    const int readSize = readFromFile(dest, restSize);
    *buf = dest;
    return readSize;
}

int FileBinaryInputOutputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    return readAll(buf, isErrorOccured);
}

// output interfaces
int FileBinaryInputOutputPort::putU8(uint8_t v)
{
    return putU8(&v, 1);
}

int FileBinaryInputOutputPort::putU8(uint8_t* v, int size)
{
    const int writtenSize = writeToFile(v, size);
    return writtenSize;
}

int FileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int FileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start, int count)
{
    uint8_t* buf = bv->data();
    const int writtenSize = writeToFile(&buf[start], count);
    return writtenSize;
}

void FileBinaryInputOutputPort::flush()
{
}

// private
int FileBinaryInputOutputPort::readFromFile(uint8_t* buf, size_t size)
{
    for (;;) {
        const int result = read(fd_, buf, size);
        if (result < 0 && errno == EINTR) {
            // read again
            errno = 0;
        } else {
            return result;
        }
    }
}

int FileBinaryInputOutputPort::writeToFile(uint8_t* buf, size_t count)
{
    MOSH_ASSERT(fd_ != INVALID_FILENO);

    for (;;) {
        const int result = write(fd_, buf, count);
        if (result < 0 && errno == EINTR) {
            // write again
            errno = 0;
        } else {
            if (result >= 0) {
                return result;
            } else {
                MOSH_FATAL("todo");
                // todo error check. we may have isErrorOccured flag.
                return result;
            }
        }
    }
}


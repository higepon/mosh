/*
 * FileBinaryOutputPort.cpp - 
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
 *   Copyright (c) 2009  Kokosabu(MIURA Yasuyuki)  <kokosabu@gmail.com>
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
 *  $Id$
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h> // memcpy
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "FileBinaryOutputPort.h"
#include "ByteVector.h"
#include "Symbol.h"
#include "Bignum.h"
#include "ErrorProcedures.h"

using namespace scheme;

FileBinaryOutputPort::FileBinaryOutputPort(int fd) : fd_(fd), fileName_(UC("unknown file")), isClosed_(false), position_(0)
{
}

FileBinaryOutputPort::FileBinaryOutputPort(ucs4string file) : fileName_(file), isClosed_(false), position_(0)
{
    // todo fileOptions process
    fd_ = ::open(file.ascii_c_str(), O_WRONLY | O_CREAT, 0644);
}

FileBinaryOutputPort::FileBinaryOutputPort(ucs4string file, Object list) : fileName_(file), isClosed_(false), position_(0)
{
    // todo fileOptions process
    fd_ = ::open(file.ascii_c_str(), O_WRONLY | O_CREAT, 0644);
}

FileBinaryOutputPort::~FileBinaryOutputPort()
{
    close();
}

bool FileBinaryOutputPort::isClosed() const
{
    return isClosed_;
}

int FileBinaryOutputPort::putU8(uint8_t v)
{
    return putU8(&v, 1);
}

int FileBinaryOutputPort::putU8(uint8_t* v, int size)
{
    return writeToFile(v, size);
}

int FileBinaryOutputPort::putByteVector(ByteVector* bv, int start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int FileBinaryOutputPort::putByteVector(ByteVector* bv, int start, int count)
{
    uint8_t* buf = bv->data();
    return writeToFile(&buf[start], count);
}

int FileBinaryOutputPort::open()
{
    if (INVALID_FILENO == fd_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
}

int FileBinaryOutputPort::close()
{
    if (!isClosed() && fd_ != INVALID_FILENO) {
        isClosed_ = true;
        ::close(fd_);
    }
    return MOSH_SUCCESS;
}

int FileBinaryOutputPort::fileNo() const
{
    return fd_;
}

void FileBinaryOutputPort::flush()
{
}

ucs4string FileBinaryOutputPort::toString()
{
    return fileName_;
}

bool FileBinaryOutputPort::hasPosition() const
{
    return true;
}

bool FileBinaryOutputPort::hasSetPosition() const
{
    return true;
}

Object FileBinaryOutputPort::position() const
{
    return Bignum::makeInteger(position_);
}

bool FileBinaryOutputPort::setPosition(int position)
{
    const int ret = lseek(fd_, position, SEEK_SET);
    if (position == ret) {
        position_ = position;
        return true;
    } else {
        return false;
    }
}


// private
int FileBinaryOutputPort::writeToFile(uint8_t* buf, size_t size)
{
    MOSH_ASSERT(fd_ != INVALID_FILENO);

    for (;;) {
        const int result = write(fd_, buf, size);
        if (result < 0 && errno == EINTR) {
            // write again
            errno = 0;
        } else {
            if (result >= 0) {
                position_ += result;
                return result;
            } else {
                MOSH_FATAL("todo");
                // todo error check. we may have isErrorOccured flag.
                return result;
            }
        }
    }
}

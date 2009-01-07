/*
 * FileBinaryOutputPort.cpp - 
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
 *  $Id: FileBinaryOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "Object.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "FileBinaryOutputPort.h"
#include "ByteVector.h"

using namespace scheme;

FileBinaryOutputPort::FileBinaryOutputPort(int fd) : fd_(fd), isClosed_(false)
{
}

FileBinaryOutputPort::FileBinaryOutputPort(ucs4string file)
{
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
    MOSH_ASSERT(fd_ != INVALID_FILENO);
    return write(fd_, &v, 1);
}

int FileBinaryOutputPort::putU8(uint8_t* v, int size)
{
    return write(fd_, v, size);
}

int FileBinaryOutputPort::putByteVector(ByteVector bv, int start /* = 0 */)
{
    return putByteVector(bv, start, bv.length() - start);
}

int FileBinaryOutputPort::putByteVector(ByteVector bv, int start, int count)
{
    uint8_t* buf = bv.data();
    return write(fd_, &buf[start], count);
}

int FileBinaryOutputPort::open()
{
    if (-1 == fd_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
}

int FileBinaryOutputPort::close()
{
    if (!isClosed() && INVALID_FILENO != fd_) {
        isClosed_ = true;
        if (fd_ != ::fileno(stdout) && fd_ != ::fileno(stderr)) {
            ::close(fd_);
        }
    }
    return MOSH_SUCCESS;
}

int FileBinaryOutputPort::fileno() const
{
    return fd_;
}

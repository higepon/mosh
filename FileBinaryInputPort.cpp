/*
 * FileBinaryInputPort.cpp -
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
 *  $Id: FileBinaryInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
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
#include "FileBinaryInputPort.h"

using namespace scheme;

FileBinaryInputPort::FileBinaryInputPort(int fd) : fd_(fd), fileName_(UC("<unknown file>")), isClosed_(false), u8Buf_(EOF), bufferMode_(BLOCK)
{
}

FileBinaryInputPort::FileBinaryInputPort(ucs4string file) : fileName_(file), isClosed_(false), u8Buf_(EOF), bufferMode_(BLOCK)
{
    fd_ = ::open(file.ascii_c_str(), O_RDONLY);
}

FileBinaryInputPort::FileBinaryInputPort(const char* file) : isClosed_(false), u8Buf_(EOF)
{
    fileName_ = Object::makeString(file).toString()->data();
    fd_ = ::open(file, O_RDONLY);
}

FileBinaryInputPort::FileBinaryInputPort(ucs4string file, Object fileOptions, Object bufferMode) : fileName_(file), isClosed_(false), u8Buf_(EOF)
{
    fd_ = ::open(file.ascii_c_str(), O_RDONLY);

    // todo bufferMode process
    if (bufferMode == Symbol::NONE) {
        bufferMode_ = NONE;
    } else if (bufferMode == Symbol::LINE) {
        bufferMode_ = LINE;
    } else {
        bufferMode_ = BLOCK;
    }
}

int FileBinaryInputPort::open()
{
    if (INVALID_FILENO == fd_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
}

ucs4string FileBinaryInputPort::toString()
{
    return fileName_;
}

FileBinaryInputPort::~FileBinaryInputPort()
{
    close();
}

int FileBinaryInputPort::getU8()
{
    uint8_t c;

    if (EOF != u8Buf_) {
        c = u8Buf_;
        u8Buf_ = EOF;
        return c;
    }

    if (0 == bufRead(&c, 1)) {
        return EOF;
    } else {
        return c;
    }
}

int FileBinaryInputPort::lookaheadU8()
{
    if (EOF != u8Buf_) {
        return u8Buf_;
    }

    uint8_t c;
    if (0 == bufRead(&c, 1)) {
        return EOF;
    } else {
        u8Buf_ = c;
        return c;
    }
}

ByteVector* FileBinaryInputPort::getByteVector(uint32_t size)
{
#ifdef USE_BOEHM_GC
    uint8_t* buf = new(PointerFreeGC) uint8_t[size];
#else
    uint8_t* buf = new uint8_t[size];
#endif
    int ret;
    if (EOF != u8Buf_) {
        buf[0] = u8Buf_;
        u8Buf_ = EOF;
        ret = bufRead(buf+1, size);
    } else {
        ret = bufRead(buf, size);
    }

    return new ByteVector(ret, buf);
}

bool FileBinaryInputPort::isClosed() const
{
    return isClosed_;
}

int FileBinaryInputPort::close()
{
    if (!isClosed() && fd_ != INVALID_FILENO) {
        isClosed_ = true;
        if (fd_ != ::fileno(stdin)) {
            ::close(fd_);
        }
    }
    return MOSH_SUCCESS;
}

int FileBinaryInputPort::fileno() const
{
    return fd_;
}

void FileBinaryInputPort::bufFill()
{
    bufLen_ = read(fd_, buffer_, BUF_SIZE);
    bufIdx_ = 0;
}

int FileBinaryInputPort::bufRead(uint8_t* data, int size)
{
    if (bufferMode_ == NONE) {
        return read(fd_, data, size);
    }
    if (bufferMode_ == BLOCK || bufferMode_ == LINE) {
        return read(fd_, data, size); // temporary
    }

    // working
    if (bufferMode_ == BLOCK || bufferMode_ == LINE) {
        const int diff = bufLen_ - bufIdx_;
        if (diff >= size) {
            memcpy(data, buffer_+bufIdx_, size);
            bufIdx_ += size;
        } else {
            memcpy(data, buffer_+bufIdx_, diff);
            bufFill();
        }
    }
    return size;
}

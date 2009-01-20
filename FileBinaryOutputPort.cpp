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

using namespace scheme;

FileBinaryOutputPort::FileBinaryOutputPort(int fd) : fd_(fd), isClosed_(false), bufferMode_(BLOCK)
{
    if (fd == 1) {
        bufferMode_ = LINE;
    } else if (fd == 2) {
        bufferMode_ = NONE;
    }

#ifdef USE_BOEHM_GC
    buffer_ = new(PointerFreeGC) uint8_t[BUF_SIZE];
#else
    buffer_ = new uint8_t[BUF_SIZE];
#endif
}

FileBinaryOutputPort::FileBinaryOutputPort(ucs4string file) : isClosed_(false), bufferMode_(BLOCK)
{
    // todo fileOptions process
    fd_ = ::open(file.ascii_c_str(), O_WRONLY | O_CREAT, 0644);
#ifdef USE_BOEHM_GC
    buffer_ = new(PointerFreeGC) uint8_t[BUF_SIZE];
#else
    buffer_ = new uint8_t[BUF_SIZE];
#endif
}

FileBinaryOutputPort::FileBinaryOutputPort(ucs4string file, Object fileOptions, Object bufferMode) : isClosed_(false)
{
    // todo fileOptions process
    fd_ = ::open(file.ascii_c_str(), O_WRONLY | O_CREAT, 0644);

    if (bufferMode == Symbol::NONE) {
        bufferMode_ = NONE;
    } else if (bufferMode == Symbol::LINE) {
        bufferMode_ = LINE;
    } else {
        bufferMode_ = BLOCK;
    }

    if (bufferMode_ == LINE || bufferMode_ == BLOCK) {
#ifdef USE_BOEHM_GC
        buffer_ = new(PointerFreeGC) uint8_t[BUF_SIZE];
#else
        buffer_ = new uint8_t[BUF_SIZE];
#endif
    }
}

FileBinaryOutputPort::~FileBinaryOutputPort()
{
#ifdef USE_BOEHM_GC
#else
    delete buffer_;
#endif
    if (buffer_ != NULL && bufIdx_ != 0) {
        bufFlush();
    }
    close();
}

bool FileBinaryOutputPort::isClosed() const
{
    return isClosed_;
}

int FileBinaryOutputPort::putU8(uint8_t v)
{
    MOSH_ASSERT(fd_ != INVALID_FILENO);
    return bufWrite(&v, 1);
}

int FileBinaryOutputPort::putU8(uint8_t* v, int size)
{
    return bufWrite(v, size);
}

int FileBinaryOutputPort::putByteVector(ByteVector bv, int start /* = 0 */)
{
    return putByteVector(bv, start, bv.length() - start);
}

int FileBinaryOutputPort::putByteVector(ByteVector bv, int start, int count)
{
    uint8_t* buf = bv.data();
    return bufWrite(&buf[start], count);
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
    if (buffer_ != NULL & bufIdx_ != 0) {
        bufFlush();
    }
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

void FileBinaryOutputPort::bufFlush()
{
    if (bufferMode_ == LINE || bufferMode_ == BLOCK) {
        bufWriteLen_ = write(fd_, buffer_, bufIdx_);
        bufIdx_ = 0;
    }
}


int FileBinaryOutputPort::bufWrite(uint8_t* data, int reqSize)
{
    if (bufferMode_ == NONE) {
        return write(fd_, data, reqSize);
    }
    if (bufferMode_ == LINE) {
        int writeSize = 0;
        while (writeSize < reqSize) {
            int bufDiff = BUF_SIZE - bufIdx_;
            if (bufDiff == 0) {
                bufFlush();
                if (bufWriteLen_ < BUF_SIZE) {
                    // todo
                    break;
                }
            }
            *(buffer_+bufIdx_) = *(data+writeSize);
            bufIdx_++;
            writeSize++;
            if (buffer_[bufIdx_-1] == '\n') {
                bufFlush();
            }
        }
        return writeSize;
    }
    if (bufferMode_ == BLOCK) {
        int writeSize = 0;
        while (writeSize < reqSize) {
            int bufDiff = BUF_SIZE - bufIdx_;
            int sizeDiff = reqSize - writeSize;
            if (bufDiff >= sizeDiff) {
                memcpy(buffer_+bufIdx_, data+writeSize, sizeDiff);
                bufIdx_ += sizeDiff;
                writeSize += sizeDiff;
            } else {
                memcpy(buffer_+bufIdx_, data+writeSize, bufDiff);
                writeSize += bufDiff;
                bufFlush(); // (bufIdx_ = 0)
                if (bufWriteLen_ < BUF_SIZE) {
                    // todo
                    break;
                }
            }
        }
        return writeSize;
    }

    // Error
}

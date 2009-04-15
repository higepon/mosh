/*
 * BufferedFileBinaryOutputPort.cpp -
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

// #ifdef _WIN32
//     #include <io.h>
// #else
// #include <unistd.h>
// #endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h> // memcpy
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "Symbol.h"
#include "Bignum.h"
#include "BufferedFileBinaryOutputPort.h"
#include "OSCompat.h"
#include "SString.h"
#include "ErrorProcedures.h"

using namespace scheme;

BufferedFileBinaryOutputPort::BufferedFileBinaryOutputPort(File* file) : file_(file), fileName_(UC("<unknown file>")), isClosed_(false), isPseudoClosed_(false), bufIdx_(0), position_(0)
{
    initializeBuffer();
}

BufferedFileBinaryOutputPort::BufferedFileBinaryOutputPort(const ucs4string& file) : file_(new File), fileName_(file), isClosed_(false), isPseudoClosed_(false), bufIdx_(0), position_(0)
{
    file_->open(file, File::Write | File::Create);
    initializeBuffer();
}

BufferedFileBinaryOutputPort::BufferedFileBinaryOutputPort(const ucs4string& file, int openFlags) : file_(new File), fileName_(file), isClosed_(false), isPseudoClosed_(false), bufIdx_(0), position_(0)
{
    file_->open(file, File::Write | File::Create | openFlags);
    initializeBuffer();
}

BufferedFileBinaryOutputPort::~BufferedFileBinaryOutputPort()
{
#ifdef USE_BOEHM_GC
#else
    delete buffer_;
#endif
    close();
}

bool BufferedFileBinaryOutputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

int BufferedFileBinaryOutputPort::putU8(uint8_t v)
{
    return static_cast<int>(putU8(&v, 1));
}

File* BufferedFileBinaryOutputPort::getFile()
{
    return file_;
}

int64_t BufferedFileBinaryOutputPort::putU8(uint8_t* v, int64_t size)
{
    const int64_t result = writeToBuffer(v, size);
    position_ += result;
    return result;
}

int64_t BufferedFileBinaryOutputPort::putByteVector(ByteVector* bv, int64_t start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int64_t BufferedFileBinaryOutputPort::putByteVector(ByteVector* bv, int64_t start, int64_t count)
{
    uint8_t* buf = bv->data();
    const int64_t result = writeToBuffer(&buf[start], count);
    position_ += result;
    return result;
}

int BufferedFileBinaryOutputPort::open()
{
    if (file_->isOpen()) {
        return MOSH_SUCCESS;
    } else {
        return MOSH_FAILURE;
    }
}

int BufferedFileBinaryOutputPort::close()
{
    flush();
    if (!isClosed()) {
        isClosed_ = true;
        file_->close();
    }
    return MOSH_SUCCESS;
}

int BufferedFileBinaryOutputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

void BufferedFileBinaryOutputPort::flush()
{
    uint8_t* buf = buffer_;
    while (bufIdx_ > 0) {
        const int64_t result = file_->write(buf, bufIdx_);
        if (result < 0) {
            throwIOError2(IOError::WRITE, file_->getLastErrorMessage());
            return;
        }
        buf += result;
        bufIdx_ -= result;
    }
    MOSH_ASSERT(bufIdx_ == 0);
}

ucs4string BufferedFileBinaryOutputPort::toString()
{
    ucs4string ret = UC("<binary-output-port ");
    ret += fileName_;
    ret += UC(">");
    return ret;
}

bool BufferedFileBinaryOutputPort::hasPosition() const
{
    return true;
}

bool BufferedFileBinaryOutputPort::hasSetPosition() const
{
    return true;
}

Object BufferedFileBinaryOutputPort::position() const
{
    return Bignum::makeIntegerFromS64(position_);
}

bool BufferedFileBinaryOutputPort::setPosition(int64_t position)
{
    flush();
    const int64_t ret = file_->seek(position);
    if (position == ret) {
        position_ =  position;
        return true;
    } else {
        return false;
    }
}


void BufferedFileBinaryOutputPort::initializeBuffer()
{
#ifdef USE_BOEHM_GC
    buffer_ = new(PointerFreeGC) uint8_t[BUF_SIZE];
#else
    buffer_ = new uint8_t[BUF_SIZE];
#endif
}

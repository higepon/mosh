/*
 * BufferedFileBinaryInputPort.cpp -
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
 *  $Id: BufferedFileBinaryInputPort.cpp 1200 2009-02-17 13:27:08Z higepon $
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
#include "OSCompat.h"
#include "BufferedFileBinaryInputPort.h"
#include "Symbol.h"
#include "Bignum.h"
#include "OSCompat.h"

using namespace scheme;

BufferedFileBinaryInputPort::BufferedFileBinaryInputPort(int fd) : file_(new File(fd)), fileName_(UC("<unknown file>")), isClosed_(false), isPseudoClosed_(false), position_(0)
{
    initializeBuffer();
}

BufferedFileBinaryInputPort::BufferedFileBinaryInputPort(ucs4string file) : file_(new File), fileName_(file), isClosed_(false), isPseudoClosed_(false), position_(0)
{
    file_->open(fileName_, File::Read);
    initializeBuffer();
}

BufferedFileBinaryInputPort::BufferedFileBinaryInputPort(const char* file) : file_(new File), isClosed_(false), isPseudoClosed_(false), position_(0)
{
    fileName_ = ucs4string::from_c_str(file, strlen(file));
    file_->open(fileName_, File::Read);
    initializeBuffer();
}

BufferedFileBinaryInputPort::~BufferedFileBinaryInputPort()
{
#ifdef USE_BOEHM_GC
#else
    delete buffer_;
#endif
    close();
}

int BufferedFileBinaryInputPort::open()
{
    if (file_->isOpen()) {
        return MOSH_SUCCESS;

    } else {
        return MOSH_FAILURE;
    }
}

ucs4string BufferedFileBinaryInputPort::toString()
{
    ucs4string ret = UC("<binary-input-port ");
    ret += fileName_;
    ret += UC(">");
    return ret;
}

int BufferedFileBinaryInputPort::getU8()
{
    uint8_t c;
    position_++;
    if (0 == readFromBuffer(&c, 1)) {
        return EOF;
    } else {
        return c;
    }
}

int BufferedFileBinaryInputPort::lookaheadU8()
{
    uint8_t c;
    if (0 == readFromBuffer(&c, 1)) {
        return EOF;
    } else {
        bufIdx_--;
        return c;
    }
}

int BufferedFileBinaryInputPort::readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured)
{
    const int ret = readFromBuffer(buf, reqSize);
    position_ += ret;
    return ret;
}

int BufferedFileBinaryInputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    const int restSize = file_->size() - position_;
    MOSH_ASSERT(restSize >= 0);
    if (restSize == 0) {
        return 0;
    }

    uint8_t* dest = allocatePointerFreeU8Array(restSize);
    const int ret = readFromBuffer(dest, restSize);
    position_ += ret;
    *buf = dest;
    return ret;
}

int BufferedFileBinaryInputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    const int bufferedSize = bufLen_ > bufIdx_;

    // if we have buffered data, return them only.
    const int tryReadSize = (bufferedSize > 0) ? bufferedSize : BUF_SIZE;
    uint8_t* dest = allocatePointerFreeU8Array(tryReadSize);
    const int ret = readFromBuffer(dest, tryReadSize);
    position_ += ret;
    *buf = dest;
    return ret;
}

bool BufferedFileBinaryInputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

int BufferedFileBinaryInputPort::close()
{
    if (!isClosed_) {
        isClosed_ = true;
        file_->close();
    }
    return MOSH_SUCCESS;
}

int BufferedFileBinaryInputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

void BufferedFileBinaryInputPort::fillBuffer()
{
    int readSize = 0;
    while (readSize < BUF_SIZE) {
        const int result = file_->read(buffer_ + readSize, BUF_SIZE - readSize);
        MOSH_ASSERT(result >= 0); // error will be raised by longjmp
        if (0 == result) { // EOF
            break;
        } else {
            readSize += result;
        }
    }
    bufLen_ = readSize;
    bufIdx_ = 0;
}

int BufferedFileBinaryInputPort::readFromBuffer(uint8_t* dest, int reqSize)
{
    int readSize = 0;
    while (readSize < reqSize) {
        const int bufDiff = bufLen_ - bufIdx_;
        MOSH_ASSERT(bufLen_ >= bufIdx_);
        const int sizeDiff = reqSize - readSize;
        MOSH_ASSERT(readSize >= readSize);
        // we found datum in buffer
        if (bufDiff >= sizeDiff) {
            memcpy(dest + readSize, buffer_ + bufIdx_, sizeDiff);
            bufIdx_ += sizeDiff;
            readSize += sizeDiff;
        } else {
            memcpy(dest + readSize, buffer_ + bufIdx_, bufDiff);
            readSize += bufDiff;
            fillBuffer();
            if (bufLen_ == 0) { // EOF
                break;
            }
        }
    }
    return readSize;
}

// private
void BufferedFileBinaryInputPort::initializeBuffer()
{
    buffer_ = allocatePointerFreeU8Array(BUF_SIZE);
}

// binary-ports should support position.
bool BufferedFileBinaryInputPort::hasPosition() const
{
    return true;
}

bool BufferedFileBinaryInputPort::hasSetPosition() const
{
    return true;
}

Object BufferedFileBinaryInputPort::position() const
{
    return Bignum::makeInteger(position_);
}

bool BufferedFileBinaryInputPort::setPosition(int position)
{
    const int ret = file_->seek(position);
    if (position == ret) {
        position_ =  position;

        // Now we just invalidate buffer.
        // If this has performance problem, we can fix it.
        invalidateBuffer();
        return true;
    } else {
        return false;
    }
}

void BufferedFileBinaryInputPort::invalidateBuffer()
{
    bufLen_ = 0;
    bufIdx_ = 0;
}

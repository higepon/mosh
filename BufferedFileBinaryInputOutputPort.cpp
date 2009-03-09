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

// N.B. About port position.
//
//   writeToBuffer and readFromBuffer does NOT change the fd's postion.
//   Because user don't care whether written bytes are written in buffer or file.
//
//   position_ should be maintained by public functions which call writeToBuffer and readFromBuffer.
//

//#define DEBUG_SHOW_POSITION() printf("**** %s lseek=%d position_=%d line:%d\n", __func__, (int)lseek(fd_, 0, SEEK_CUR), position_, __LINE__)

#define DEBUG_SHOW_POSITION() /* */

BufferedFileBinaryInputOutputPort::BufferedFileBinaryInputOutputPort(const ucs4string& file, int openFlags) :
    fileName_(file),
    buffer_(NULL),
    isDirty_(false),
    position_(0),
    isClosed_(false),
    bufferSize_(0),
    bufferIndex_(0)
{
    fd_ = ::open(file.ascii_c_str(), O_RDWR | O_CREAT | openFlags, 0644);
    initializeBuffer();
}

BufferedFileBinaryInputOutputPort::~BufferedFileBinaryInputOutputPort()
{
#ifdef USE_BOEHM_GC
#else
    delete buffer_;
#endif
    close();
}

// port interfaces
bool BufferedFileBinaryInputOutputPort::hasPosition() const
{
    return true;
}

bool BufferedFileBinaryInputOutputPort::hasSetPosition() const
{
    return true;
}

Object BufferedFileBinaryInputOutputPort::position() const
{
    return Bignum::makeInteger(position_);
}

int BufferedFileBinaryInputOutputPort::close()
{
    if (!isClosed() && fd_ != INVALID_FILENO) {

        flush();
        isClosed_ = true;
        ::close(fd_);
    }
    return MOSH_SUCCESS;
}

bool BufferedFileBinaryInputOutputPort::setPosition(int position)
{
    DEBUG_SHOW_POSITION();
    if (isBufferDirty()) {
        flush();
    } else {
        // Now we just invalidate buffer.
        // If this has performance problem, we can fix it.

        invalidateBuffer();
    }

    const int currentOffset = lseek(fd_, position, SEEK_SET);
    if (position == currentOffset) {
        // Don't change postion_ before flush() done.
        position_ =  position;
        return true;
    } else {
        return false;
    }
}

ucs4string BufferedFileBinaryInputOutputPort::toString()
{
    ucs4string ret = UC("<binary-input/output-port ");
    ret += fileName_;
    ret += UC(">");
    return ret;
}

// binary port interfaces
int BufferedFileBinaryInputOutputPort::open()
{
    if (INVALID_FILENO == fd_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
}

bool BufferedFileBinaryInputOutputPort::isClosed() const
{
    return isClosed_;
}

int BufferedFileBinaryInputOutputPort::fileNo() const
{
    return fd_;
}

// input interfaces
int BufferedFileBinaryInputOutputPort::getU8()
{
    DEBUG_SHOW_POSITION();
    uint8_t c;
    if (0 == readFromBuffer(&c, 1)) {
        return EOF;
    } else {
        forwardPosition(1);
        return c;
    }
}

int BufferedFileBinaryInputOutputPort::lookaheadU8()
{
    DEBUG_SHOW_POSITION();
    uint8_t c;
    if (0 == readFromBuffer(&c, 1)) {
        return EOF;
    } else {
        MOSH_ASSERT(bufferIndex_ >= 0);
        bufferIndex_--;
        MOSH_ASSERT(bufferIndex_ >= 0);
        return c;
    }
}

int BufferedFileBinaryInputOutputPort::readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured)
{
    DEBUG_SHOW_POSITION();
    const int readSize = readFromBuffer(buf, reqSize);
    forwardPosition(readSize);
    DEBUG_SHOW_POSITION();
    return readSize;
}

int BufferedFileBinaryInputOutputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    DEBUG_SHOW_POSITION();
    struct stat st;
    const int result = fstat(fd_, &st);
    MOSH_ASSERT(result == 0); // will never happen?

    const int restSize = st.st_size - position_;
    MOSH_ASSERT(restSize >= 0);
    if (restSize == 0) {
        return 0;
    }

    uint8_t* dest = allocatePointerFreeU8Array(restSize);
    const int readSize = readFromBuffer(dest, restSize);
    forwardPosition(readSize);
    *buf = dest;
    return readSize;
}

int BufferedFileBinaryInputOutputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    DEBUG_SHOW_POSITION();
    const int bufferedSize = bufferSize_ > bufferIndex_;

    // if we have buffered data, return them only.
    const int tryReadSize = (bufferedSize > 0) ? bufferedSize : BUF_SIZE;
    uint8_t* dest = allocatePointerFreeU8Array(tryReadSize);
    const int readSize = readFromBuffer(dest, tryReadSize);
    forwardPosition(readSize);
    *buf = dest;
    return readSize;
}

// output interfaces
int BufferedFileBinaryInputOutputPort::putU8(uint8_t v)
{
    return putU8(&v, 1);
}

int BufferedFileBinaryInputOutputPort::putU8(uint8_t* v, int size)
{
    DEBUG_SHOW_POSITION();
    const int writtenSize = writeToBuffer(v, size);
    forwardPosition(writtenSize);
    DEBUG_SHOW_POSITION();
    return writtenSize;
}

int BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start, int count)
{
    DEBUG_SHOW_POSITION();

    uint8_t* buf = bv->data();
    const int writtenSize = writeToBuffer(&buf[start], count);
    forwardPosition(writtenSize);
    return writtenSize;
}

void BufferedFileBinaryInputOutputPort::flush()
{
    const int result = lseek(fd_, position_ - bufferIndex_, SEEK_SET);
//    MOSH_ASSERT(result >= 0);
    internalFlush();
}

void BufferedFileBinaryInputOutputPort::internalFlush()
{
    DEBUG_SHOW_POSITION();
    uint8_t* buf = buffer_;
    while (bufferIndex_ > 0) {
        const int writtenSize = writeToFile(buf, bufferIndex_);
        buf += writtenSize;
        bufferIndex_ -= writtenSize;
        MOSH_ASSERT(bufferIndex_ >= 0);
    }
    // Now read/write buffer is empty
    MOSH_ASSERT(bufferIndex_ == 0);
    invalidateBuffer();

    // there's no dirty data
    isDirty_ = false;
}



// private
void BufferedFileBinaryInputOutputPort::initializeBuffer()
{
    buffer_ = allocatePointerFreeU8Array(BUF_SIZE);
}

bool BufferedFileBinaryInputOutputPort::fillBuffer()
{
    // we need to flush to disk, before reading new data.
    if (isBufferDirty()) {
        flush();
    }
    int readSize = 0;
    while (readSize < BUF_SIZE) {
        const int result = readFromFile(buffer_ + readSize, BUF_SIZE - readSize);
        if (0 == result) { // EOF
            break;
        } else if (result < 0) { // error
            return false;
        } else {
            readSize += result;
        }
    }
    bufferSize_ = readSize;
    bufferIndex_ = 0;
    return true;

}

int BufferedFileBinaryInputOutputPort::readFromFile(uint8_t* buf, size_t size)
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

int BufferedFileBinaryInputOutputPort::writeToFile(uint8_t* buf, size_t count)
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

// N.B. readFromBuffer doesn't change the fd's position.
int BufferedFileBinaryInputOutputPort::readFromBuffer(uint8_t* dest, int requestSize)
{
    MOSH_ASSERT(dest != NULL);
    MOSH_ASSERT(requestSize >= 0);

    const int origPositon = lseek(fd_, 0, SEEK_CUR);
    MOSH_ASSERT(origPositon >= 0);
    bool needUnwind = false;

    for (int readSize = 0 ;;) {
        MOSH_ASSERT(bufferIndex_ >= 0);
        const int bufferedSize = bufferSize_ - bufferIndex_;
        const int restSize = requestSize - readSize;
        // we have enough data in the buffer.
        if (bufferedSize >= restSize) {
            memcpy(dest + readSize, buffer_ + bufferIndex_, restSize);
            bufferIndex_ += restSize;
            // unwind postion
            if (needUnwind) {
                const int result = lseek(fd_, origPositon, SEEK_SET);
                MOSH_ASSERT(result >= 0);
            }
            // done
            return requestSize;
        } else {
            // read whole buffered data.
            memcpy(dest + readSize, buffer_ + bufferIndex_, bufferedSize);
            readSize += bufferedSize;
            // we need more
            if (!fillBuffer()) {
                MOSH_FATAL("todo");
                return EOF;
            }
            needUnwind = true;
            // EOF
            if (0 == bufferSize_) {
                if (needUnwind) {
                    const int result = lseek(fd_, origPositon, SEEK_SET);
                    MOSH_ASSERT(result >= 0);
                }
                return readSize;
            }
        }
    }
}

void BufferedFileBinaryInputOutputPort::invalidateBuffer()
{
    bufferSize_ = 0;
    bufferIndex_ = 0;
}

void BufferedFileBinaryInputOutputPort::forwardPosition(int offset)
{
    position_ += offset;
    const int currentPosition = lseek(fd_, position_, SEEK_SET);
    MOSH_ASSERT(position_ == currentPosition);
}

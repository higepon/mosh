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
#include "HeapObject.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "Symbol.h"
#include "Bignum.h"
#include "BufferedFileBinaryInputOutputPort.h"
#include "PortProcedures.h"
#include "Transcoder.h"
#include "OSCompat.h"

using namespace scheme;

// N.B. About port position.
//
//   writeToBuffer and readFromBuffer does NOT change the fd's postion.
//   Because user don't care whether written bytes are written in buffer or file.
//
//   position_ should be maintained by public functions which call writeToBuffer and readFromBuffer.
//

//#define DEBUG_SHOW_POSITION() printf("**** %s lseek=%d position_=%d line:%d\n", __func__, (int)file_->seek(0, SEEK_CUR), position_, __LINE__)

#define DEBUG_SHOW_POSITION() /* */

BufferedFileBinaryInputOutputPort::BufferedFileBinaryInputOutputPort(const ucs4string& file, int openFlags) :
    file_(new File),
    fileName_(file),
    buffer_(NULL),
    isDirty_(false),
    position_(0),
    isClosed_(false),
    isPseudoClosed_(false),
    bufferSize_(0),
    bufferIndex_(0)
{
    file_->open(fileName_, File::Read | File::Write | File::Create | openFlags);
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
    return Bignum::makeIntegerFromS64(position_);
}

int BufferedFileBinaryInputOutputPort::close()
{
    if (!isClosed_) {

        flush();
        isClosed_ = true;
        file_->close();
    }
    return MOSH_SUCCESS;
}

int BufferedFileBinaryInputOutputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

bool BufferedFileBinaryInputOutputPort::setPosition(int64_t position)
{
    DEBUG_SHOW_POSITION();
    if (isBufferDirty()) {
        flush();
    } else {
        // Now we just invalidate buffer.
        // If this has performance problem, we can fix it.

        invalidateBuffer();
    }

    const int64_t currentOffset = file_->seek(position);
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
    if (file_->isOpen()) {
        return MOSH_SUCCESS;
    } else {
        return MOSH_FAILURE;
    }
}

bool BufferedFileBinaryInputOutputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
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

int64_t BufferedFileBinaryInputOutputPort::readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured)
{
    DEBUG_SHOW_POSITION();
    const int64_t readSize = readFromBuffer(buf, reqSize);
    forwardPosition(readSize);
    DEBUG_SHOW_POSITION();
    return readSize;
}

int64_t BufferedFileBinaryInputOutputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    DEBUG_SHOW_POSITION();

    const int64_t restSize = file_->size() - position_;
    MOSH_ASSERT(restSize >= 0);
    if (restSize == 0) {
        return 0;
    }

    uint8_t* dest = allocatePointerFreeU8Array(restSize);
    const int64_t readSize = readFromBuffer(dest, restSize);
    forwardPosition(readSize);
    *buf = dest;
    return readSize;
}

int64_t BufferedFileBinaryInputOutputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    DEBUG_SHOW_POSITION();
    const int64_t bufferedSize = bufferSize_ - bufferIndex_;

    // if we have buffered data, return them only.
    const int64_t tryReadSize = (bufferedSize > 0) ? bufferedSize : BUF_SIZE;
    uint8_t* dest = allocatePointerFreeU8Array(tryReadSize);
    const int64_t readSize = readFromBuffer(dest, tryReadSize);
    forwardPosition(readSize);
    *buf = dest;
    return readSize;
}

// output interfaces
int BufferedFileBinaryInputOutputPort::putU8(uint8_t v)
{
    return static_cast<int>(putU8(&v, 1));
}

int64_t BufferedFileBinaryInputOutputPort::putU8(uint8_t* v, int64_t size)
{
    DEBUG_SHOW_POSITION();
    const int64_t writtenSize = writeToBuffer(v, size);
    forwardPosition(writtenSize);
    DEBUG_SHOW_POSITION();
    return writtenSize;
}

int64_t BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int64_t start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int64_t BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int64_t start, int64_t count)
{
    DEBUG_SHOW_POSITION();
    uint8_t* buf = bv->data();
    const int64_t writtenSize = writeToBuffer(&buf[start], count);
    forwardPosition(writtenSize);
    DEBUG_SHOW_POSITION();
    return writtenSize;
}

void BufferedFileBinaryInputOutputPort::flush()
{
    const int64_t result = file_->seek(position_ - bufferIndex_);
    MOSH_ASSERT(result >= 0);
    internalFlush();
}

void BufferedFileBinaryInputOutputPort::internalFlush()
{
    DEBUG_SHOW_POSITION();
    uint8_t* buf = buffer_;
    while (bufferIndex_ > 0) {
        const int64_t writtenSize = file_->write(buf, bufferIndex_);
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

void BufferedFileBinaryInputOutputPort::fillBuffer()
{
    // we need to flush to disk, before reading new data.
    if (isBufferDirty()) {
        flush();
    }
    int64_t readSize = 0;
    while (readSize < BUF_SIZE) {
        const int64_t result = file_->read(buffer_ + readSize, BUF_SIZE - readSize);
        MOSH_ASSERT(result >= 0); // error will raised by longjmp
        MOSH_ASSERT(result >= 0); // error will be raised by longjmp
        if (0 == result) { // EOF
            break;
        } else {
            readSize += result;
        }
    }
    bufferSize_ = readSize;
    bufferIndex_ = 0;
}

// N.B. readFromBuffer doesn't change the fd's position.
int64_t BufferedFileBinaryInputOutputPort::readFromBuffer(uint8_t* dest, int64_t requestSize)
{
    MOSH_ASSERT(dest != NULL);
    MOSH_ASSERT(requestSize >= 0);

    const int64_t origPositon = file_->seek(0, File::Current);
    MOSH_ASSERT(origPositon >= 0);
    bool needUnwind = false;

    for (int64_t readSize = 0 ;;) {
        MOSH_ASSERT(bufferIndex_ >= 0);
        const int64_t bufferedSize = bufferSize_ - bufferIndex_;
        const int64_t restSize = requestSize - readSize;
        // we have enough data in the buffer.
        if (bufferedSize >= restSize) {
            moshMemcpy(dest + readSize, buffer_ + bufferIndex_, restSize);
            bufferIndex_ += restSize;
            // unwind postion
            if (needUnwind) {
                const int64_t result = file_->seek(origPositon);
                MOSH_ASSERT(result >= 0);
            }
            // done
            return requestSize;
        } else {
            // read whole buffered data.
            moshMemcpy(dest + readSize, buffer_ + bufferIndex_, bufferedSize);
            readSize += bufferedSize;
            // we need more
            fillBuffer();
            needUnwind = true;
            // EOF
            if (0 == bufferSize_) {
                if (needUnwind) {
                    const int64_t result = file_->seek(origPositon);
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

void BufferedFileBinaryInputOutputPort::forwardPosition(int64_t offset)
{
    position_ += offset;
    const int64_t currentPosition = file_->seek(position_);
    MOSH_ASSERT(position_ == currentPosition);
}

File* BufferedFileBinaryInputOutputPort::getFile()
{
    return file_;
}

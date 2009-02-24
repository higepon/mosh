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

BufferedFileBinaryInputOutputPort::BufferedFileBinaryInputOutputPort(ucs4string file) : fileName_(file),
                                                                                        buffer_(NULL),
                                                                                        isDirty_(false),
                                                                                        position_(0),
                                                                                        bufferSize_(0),
                                                                                        bufferIndex_(0)
{
    fd_ = ::open(file.ascii_c_str(), O_RDWR | O_CREAT, 0644);
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

int BufferedFileBinaryInputOutputPort::close(){}
bool BufferedFileBinaryInputOutputPort::setPosition(int position) {}
ucs4string BufferedFileBinaryInputOutputPort::toString(){}

    // binary port interfaces
int BufferedFileBinaryInputOutputPort::open()
{
    if (INVALID_FILENO == fd_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
}

bool BufferedFileBinaryInputOutputPort::isClosed() const{}
int BufferedFileBinaryInputOutputPort::fileNo() const
{
    return fd_;
}

// input interfaces
int BufferedFileBinaryInputOutputPort::getU8()
{
    uint8_t c;
    position_++;
    if (0 == readFromBuffer(&c, 1)) {
        return EOF;
    } else {
        return c;
    }
}


int BufferedFileBinaryInputOutputPort::lookaheadU8(){}
int BufferedFileBinaryInputOutputPort::readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured){}
int BufferedFileBinaryInputOutputPort::readSome(uint8_t** buf, bool& isErrorOccured){}
int BufferedFileBinaryInputOutputPort::readAll(uint8_t** buf, bool& isErrorOccured){}

    // output interfaces
int BufferedFileBinaryInputOutputPort::putU8(uint8_t v){}
int BufferedFileBinaryInputOutputPort::putU8(uint8_t* v, int size){}
int BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start /* = 0 */){}
int BufferedFileBinaryInputOutputPort::putByteVector(ByteVector* bv, int start, int count){}
void BufferedFileBinaryInputOutputPort::bufFlush(){}

// private
void BufferedFileBinaryInputOutputPort::initializeBuffer()
{
    buffer_ = allocatePointerFreeU8Array(BUF_SIZE);
}

int BufferedFileBinaryInputOutputPort::readFromBuffer(uint8_t* dest, int requestSize)
{
    MOSH_ASSERT(dest != NULL);
    MOSH_ASSERT(requestSize >= 0);

    for (int readSize = 0 ;;) {
        const int bufferedSize = bufferSize_ - bufferIndex_;
        MOSH_ASSERT(bufferIndex_ >= 0);
        const int restSize = requestSize - readSize;
        // we have enough data in the buffer.
        if (bufferSize_ >= restSize) {
            memcpy(dest + readSize, buffer_ + bufferIndex_, restSize);
            bufferIndex_ += restSize;
            // done
            return requestSize;
        } else {
            // read whole buffered data.
            memcpy(dest + readSize, buffer_ + bufferIndex_, bufferSize_);

        }

    }
//     while (readSize < reqSize) {
//         const int bufDiff = bufLen_ - bufIdx_;
//         MOSH_ASSERT(bufLen_ >= bufIdx_);
//         const int sizeDiff = reqSize - readSize;
//         MOSH_ASSERT(readSize >= readSize);
//         // we found datum in buffer
//         if (bufDiff >= sizeDiff) {
//             memcpy(dest + readSize, buffer_ + bufIdx_, sizeDiff);
//             bufIdx_ += sizeDiff;
//             readSize += sizeDiff;
//         } else {
//             memcpy(dest + readSize, buffer_ + bufIdx_, bufDiff);
//             readSize += bufDiff;
//             if (!fillBuffer()) {
//                 MOSH_FATAL("todo");
//                 return EOF;
//             }
//             if (bufLen_ == 0) { // EOF
//                 break;
//             }
//         }
//     }
//     return readSize;
}

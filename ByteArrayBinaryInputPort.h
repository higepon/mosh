/*
 * ByteArrayBinaryInputPort.h -
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
 *  $Id: ByteArrayBinaryInputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_BYTE_ARRAY_BINARY_INPUT_PORT_
#define SCHEME_BYTE_ARRAY_BINARY_INPUT_PORT_

#include "BinaryInputPort.h"

namespace scheme {

class ByteArrayBinaryInputPort : public BinaryInputPort
{
public:
    ByteArrayBinaryInputPort(const uint8_t* buf, int size);
    ~ByteArrayBinaryInputPort();

    // profiler tells that this should be inlined
    inline int getU8()
    {
        if (index_ >= size_) return EOF;
        return buf_[index_++];
    }

    inline int lookaheadU8()
    {
        if (index_ >= size_) return EOF;
        return buf_[index_];
    }

    ucs4string toString();
    int readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured);
    int readSome(uint8_t** buf, bool& isErrorOccured);
    int readAll(uint8_t** buf, bool& isErrorOccured);
    int open();
    int close();
    bool isClosed() const;
    int fileNo() const;
    bool hasPosition() const { return true; }
    bool hasSetPosition() const { return true; }
    Object position() const;
    bool setPosition(int position)
    {
        if (position >= size_) {
            return false;
        } else {
            index_ = position;
            return true;
        }
    }

private:
    const uint8_t* const buf_;
    const int size_;
    int index_;
    bool isClosed_;
};

}; // namespace scheme

#endif // SCHEME_BYTE_ARRAY_BINARY_INPUT_PORT_

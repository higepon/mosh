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

#include <stdio.h> // EOF
#include "BinaryInputPort.h"

namespace scheme {

class ByteArrayBinaryInputPort : public BinaryInputPort
{
public:
    ByteArrayBinaryInputPort(const uint8_t* buf, int64_t size);
    ~ByteArrayBinaryInputPort() override;

    // profiler tells that this should be inlined
    inline int getU8() override
    {
        if (index_ >= size_) return EOF;
        return buf_[index_++];
    }

    inline int lookaheadU8() override
    {
        if (index_ >= size_) return EOF;
        return buf_[index_];
    }

    ucs4string toString() override;
    int64_t readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured) override;
    int64_t readSome(uint8_t** buf, bool& isErrorOccured) override;
    int64_t readAll(uint8_t** buf, bool& isErrorOccured) override;
    int open() override;
    int close() override;
    bool isClosed() const override;
    int pseudoClose() override;
    bool hasPosition() const override { return true; }
    bool hasSetPosition() const override { return true; }
    Object position() const override;
    File* getFile() override;
    bool setPosition(int64_t position) override
    {
        if (size_ == 0 && position == 0) {
            return true;
        } else if (position > size_) {
            return false;
        } else {
            index_ = position;
            return true;
        }
    }
    ucs4string getLastErrorMessage() override
    {
        return UC("");
    }


private:
    const uint8_t* const buf_;
    const int64_t size_;
    int64_t index_;
    bool isClosed_;
    bool isPseudoClosed_;
};

} // namespace scheme

#endif // SCHEME_BYTE_ARRAY_BINARY_INPUT_PORT_

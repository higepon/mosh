/*
 * CustomBinaryInputOutputPort.h - 
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
 *  $Id: CustomBinaryInputOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_CUSTOM_BINARY_INPUT_OUTPUT_PORT_
#define SCHEME_CUSTOM_BINARY_INPUT_OUTPUT_PORT_

#include "BinaryInputOutputPort.h"

namespace scheme {

class CustomBinaryInputOutputPort : public BinaryInputOutputPort
{
public:
    CustomBinaryInputOutputPort(VM* theVM,
                                const ucs4string& id,
                                Object readProc,
                                Object writeProc,
                                Object getPositionProc,
                                Object setPositionProc,
                                Object closeProc);
    ~CustomBinaryInputOutputPort() override;

    int getU8() override;
    int lookaheadU8() override;
    int64_t readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured) override;
    int64_t readSome(uint8_t** buf, bool& isErrorOccured) override;
    int64_t readAll(uint8_t** buf, bool& isErrorOccured) override;
    ucs4string toString() override;
    int open() override;
    int close() override;
    int pseudoClose() override;
    bool isClosed() const override;

    // out
    int putU8(uint8_t v) override;
    int64_t putU8(uint8_t* v, int64_t size) override;
    int64_t putByteVector(ByteVector* bv, int64_t start = 0) override;
    int64_t putByteVector(ByteVector* bv, int64_t start, int64_t count) override;
    void flush() override;

    bool hasPosition() const override;
    bool hasSetPosition() const override;
    Object position() const override;
    bool setPosition(int64_t position) override;
    File* getFile() override;
    ucs4string getLastErrorMessage() override
    {
        return UC("");
    }


private:
    bool hasAheadU8() const;

    VM* theVM_;
    const ucs4string id_;
    const Object readProc_;
    const Object writeProc_;
    const Object getPositionProc_;
    const Object setPositionProc_;
    const Object closeProc_;

    bool isClosed_{false};
    bool isPseudoClosed_{false};
    int aheadU8_{EOF};
};

} // namespace scheme

#endif // SCHEME_CUSTOM_BINARY_INPUT_OUTPUT_PORT_

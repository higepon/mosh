/*
 * CustomBinaryOutputPort.h -
 *
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

#ifndef SCHEME_CUSTOM_BINARY_OUTPUT_PORT_
#define SCHEME_CUSTOM_BINARY_OUTPUT_PORT_

#include "BinaryOutputPort.h"

namespace scheme {

class CustomBinaryOutputPort : public BinaryOutputPort
{
public:
    CustomBinaryOutputPort(VM* theVM, const ucs4string& id, Object writeProc, Object getPositionProc, Object setPositionDProc, Object closeProc);
    ~CustomBinaryOutputPort() override;

    int putU8(uint8_t v) override;
    int64_t putU8(uint8_t* v, int64_t size) override;
    int64_t putByteVector(ByteVector* bv, int64_t start = 0) override;
    int64_t putByteVector(ByteVector* bv, int64_t start, int64_t count) override;
    int open() override;
    int close() override;
    int pseudoClose() override;
    bool isClosed() const override;
    void flush() override;
    bool hasPosition() const override;
    bool hasSetPosition() const override;
    Object position() const override;
    bool setPosition(int64_t position) override;
    ucs4string toString() override;
    File* getFile() override;
    ucs4string getLastErrorMessage() override
    {
        return ucs4string(UC(""));
    }

private:
    VM* theVM_;
    const ucs4string id_;
    const Object writeDProc_;
    const Object getPositionProc_;
    const Object setPositionDProc_;
    const Object closeProc_;
    bool isClosed_{false};
    bool isPseudoClosed_{false};
};

} // namespace scheme

#endif // SCHEME_CUSTOM_BINARY_OUTPUT_PORT_

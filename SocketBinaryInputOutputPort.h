/*
 * SocketBinaryInputOutputPort.h - <socket binary input/output port>
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
 *  $Id: SocketBinaryInputOutputPort.h 1227 2009-02-21 03:01:29Z higepon $
 */

#ifndef SCHEME__SOCKET_BINARY_INPUT_OUTPUT_PORT_
#define SCHEME__SOCKET_BINARY_INPUT_OUTPUT_PORT_

#include "BinaryInputOutputPort.h"

namespace scheme {

class Socket;
class SocketBinaryInputOutputPort : public BinaryInputOutputPort
{
public:
    SocketBinaryInputOutputPort(Socket* socket);
    virtual ~SocketBinaryInputOutputPort();

    // port interfaces
    bool hasPosition() const;
    bool hasSetPosition() const;
    Object position() const;
    int close();
    int pseudoClose();
    bool setPosition(int64_t position) ;
    ucs4string toString();

    // binary port interfaces
    int open();
    bool isClosed() const;

    // input interfaces
    int getU8();
    int lookaheadU8();
    int64_t readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured);
    int64_t readSome(uint8_t** buf, bool& isErrorOccured);
    int64_t readAll(uint8_t** buf, bool& isErrorOccured);

    // output interfaces
    int putU8(uint8_t v);
    int64_t putU8(uint8_t* v, int64_t size);
    int64_t putByteVector(ByteVector* bv, int64_t start = 0);
    int64_t putByteVector(ByteVector* bv, int64_t start, int64_t count);
    void flush();
    File* getFile();
    ucs4string getLastErrorMessage();

private:
    uint8_t getLastU8()
    {
        MOSH_ASSERT(hasLastU8());
        const uint8_t ret = lastU8_;
        lastU8_ = 0xffff;
        return ret;
    }
    bool hasLastU8() const
    {
        return lastU8_ != 0xffff;
    }

    void setLastU8(uint8_t c)
    {
        lastU8_ = c;
    }

    Socket* socket_;
    bool isClosed_;
    bool isPseudoClosed_;
    uint16_t lastU8_;
};

} // namespace scheme

#endif // SCHEME__FILE_BINARY_INPUT_OUTPUT_PORT_

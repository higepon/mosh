/*
 * ByteArrayBinaryOutputPort.h - <byte-array-binary-input-port>
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
 *  $Id: ByteArrayBinaryOutputPort.h 1301 2009-03-09 14:59:07Z higepon $
 */

#ifndef __SCHEME_BYTE_ARRAY_BINARY_OUTPUT_PORT__
#define __SCHEME_BYTE_ARRAY_BINARY_OUTPUT_PORT__

#include "ucs4string.h"
#include "BinaryOutputPort.h"

namespace scheme {

class ByteArrayBinaryOutputPort : public BinaryOutputPort
{
public:
    ByteArrayBinaryOutputPort();
    virtual ~ByteArrayBinaryOutputPort();

    int putU8(uint8_t v);
    int putU8(uint8_t* v, int size);
    int putByteVector(ByteVector* bv, int start = 0);
    int putByteVector(ByteVector* bv, int start, int count);
    int open();
    int close();
    bool isClosed() const;
    void flush();
    int fileNo() const { return INVALID_FILENO; }
    ucs4string toString();
    bool hasPosition() const;
    bool hasSetPosition() const;
    Object position() const;
    bool setPosition(int position);
    ByteVector* toByteVector() const;

protected:
    gc_vector<uint8_t> buffer_;
};

}; // namespace scheme

#endif // __SCHEME_BYTE_ARRAY_BINARY_OUTPUT_PORT__

/*
 * BufferedFileBinaryOutputPort.h - <file binary output port>
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

#ifndef SCHEME_BUFFERED_FILE_BINARY_OUTPUT_PORT_
#define SCHEME_BUFFERED_FILE_BINARY_OUTPUT_PORT_

#include "BinaryOutputPort.h"

namespace scheme {

class BufferedFileBinaryOutputPort : public BinaryOutputPort
{
public:
    BufferedFileBinaryOutputPort(File* file);
    BufferedFileBinaryOutputPort(const ucs4string& file);
    BufferedFileBinaryOutputPort(const ucs4string& file, int openFlags);
    virtual ~BufferedFileBinaryOutputPort();

    int putU8(uint8_t v);
    int putU8(uint8_t* v, int size);
    int putByteVector(ByteVector* bv, int start = 0);
    int putByteVector(ByteVector* bv, int start, int count);
    int open();
    virtual int close();
    int pseudoClose();
    bool isClosed() const;
    bool isPseudoClosed() const;
    void flush();
    ucs4string toString();
    virtual bool hasPosition() const;
    virtual bool hasSetPosition() const;
    virtual Object position() const;
    virtual bool setPosition(int position);
    File* getFile();

protected:
    enum {
        BUF_SIZE = 8192,
    };

    void initializeBuffer();
    virtual int writeToBuffer(uint8_t* data, int reqSize) = 0;

    File* file_;
    ucs4string fileName_;
    bool isClosed_;
    bool isPseudoClosed_;
    uint8_t* buffer_;
    int bufIdx_;
    int position_;
};

} // namespace scheme

#endif // SCHEME_BUFFERED_FILE_BINARY_OUTPUT_PORT_

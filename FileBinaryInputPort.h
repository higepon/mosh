/*
 * FileBinaryInputPort.h - <file binary input port>
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

#ifndef __SCHEME_FILE_BINARY_INPUT_PORT__
#define __SCHEME_FILE_BINARY_INPUT_PORT__

#include "BinaryInputPort.h"

namespace scheme {

class FileBinaryInputPort : public BinaryInputPort
{
public:
    enum {
        BUF_SIZE = 4096,
    };

    FileBinaryInputPort(int fd);
    FileBinaryInputPort(ucs4string file);
    FileBinaryInputPort(const char* file);
    FileBinaryInputPort(ucs4string file, Object fileOptions, Object bufferMode);
    virtual ~FileBinaryInputPort();

    ucs4string toString();
    int getU8();
    int lookaheadU8();
    ByteVector* getByteVector(uint32_t size);
    int open();
    int close();
    int fileno() const;
    virtual bool isClosed() const;

private:
    int fd_;
    ucs4string fileName_;
    bool isClosed_;
    int u8Buf_;
    enum bufferMode bufferMode_;
    uint8_t* buffer_;
    int bufLen_;
    int bufIdx_;

    void bufFill();
    int bufRead(uint8_t* data, int reqSize);
};

}; // namespace scheme

#endif // __SCHEME_FILE_BINARY_INPUT_PORT__

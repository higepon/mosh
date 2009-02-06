/*
 * FileBinaryOutputPort.h - <file binary output port>
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

#ifndef __SCHEME_FILE_BINARY_OUTPUT_PORT__
#define __SCHEME_FILE_BINARY_OUTPUT_PORT__

#include "BinaryOutputPort.h"

namespace scheme {

class FileBinaryOutputPort : public BinaryOutputPort
{
public:
    enum {
        NO_CREATE   = 1 << 0,
        NO_FAIL     = 1 << 1,
        NO_TRUNCATE = 1 << 2,

        BUF_SIZE    = 8192,
    };

    FileBinaryOutputPort(int fd);
    FileBinaryOutputPort(ucs4string file);
    FileBinaryOutputPort(ucs4string file, Object fileOptions, Object bufferMode);
    virtual ~FileBinaryOutputPort();

    int putU8(uint8_t v);
    int putU8(uint8_t* v, int size);
    int putByteVector(ByteVector* bv, int start = 0);
    int putByteVector(ByteVector* bv, int start, int count);
    int open();
    int close();
    bool isClosed() const;
    int fileno() const;
    int bufFlush();

protected:
    int fd_;
    bool isClosed_;
    enum bufferMode bufferMode_;
    uint8_t* buffer_;
    int bufIdx_;

    int bufWrite(uint8_t* data, int reqSize);
};

}; // namespace scheme

#endif // __SCHEME_FILE_BINARY_OUTPUT_PORT__

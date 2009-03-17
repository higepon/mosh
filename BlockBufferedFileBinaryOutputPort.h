/*
 * BlockBufferedFileBinaryOutputPort.h - <file binary output port>
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

#ifndef SCHEME_BLOCK_BUFFERED_FILE_BINARY_OUTPUT_PORT_
#define SCHEME_BLOCK_BUFFERED_FILE_BINARY_OUTPUT_PORT_

#include "BufferedFileBinaryOutputPort.h"

namespace scheme {

class BlockBufferedFileBinaryOutputPort : public BufferedFileBinaryOutputPort
{
public:
    BlockBufferedFileBinaryOutputPort(int fd) : BufferedFileBinaryOutputPort(fd) {}
    BlockBufferedFileBinaryOutputPort(ucs4string file) : BufferedFileBinaryOutputPort(file) {}
    BlockBufferedFileBinaryOutputPort(ucs4string file, int openFlags) : BufferedFileBinaryOutputPort(file, openFlags) {}
    virtual ~BlockBufferedFileBinaryOutputPort() {}

    virtual enum bufferMode bufferMode() const
    {
        return BLOCK;
    }

protected:
    int writeToBuffer(uint8_t* data, int reqSize)
    {
        int writeSize = 0;
        while (writeSize < reqSize) {
            MOSH_ASSERT(BUF_SIZE >= bufIdx_);
            const int bufDiff = BUF_SIZE - bufIdx_;
            MOSH_ASSERT(reqSize > writeSize);
            const int sizeDiff = reqSize - writeSize;
            if (bufDiff >= sizeDiff) {
                memcpy(buffer_+bufIdx_, data+writeSize, sizeDiff);
                bufIdx_ += sizeDiff;
                writeSize += sizeDiff;
            } else {
                memcpy(buffer_+bufIdx_, data+writeSize, bufDiff);
                bufIdx_ += bufDiff;
                writeSize += bufDiff;
                flush();
            }
        }
        return writeSize;
    }
};

}; // namespace scheme

#endif // SCHEME_LINE_BUFFERED_FILE_BINARY_OUTPUT_PORT_

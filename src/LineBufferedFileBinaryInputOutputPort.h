/*
 * LineBufferedFileBinaryInputOutputPort.h - 
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
 *  $Id: LineBufferedFileBinaryInputOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_LINEBUFFERED_FILE_BINARY_INPUT_OUTPUT_PORT_
#define SCHEME_LINEBUFFERED_FILE_BINARY_INPUT_OUTPUT_PORT_

#include "BufferedFileBinaryInputOutputPort.h"

namespace scheme {

class LineBufferedFileBinaryInputOutputPort : public BufferedFileBinaryInputOutputPort
{
public:
    LineBufferedFileBinaryInputOutputPort(const ucs4string& filename, int openFlags) :
        BufferedFileBinaryInputOutputPort(filename, openFlags) {}
    virtual ~LineBufferedFileBinaryInputOutputPort() {}

protected:
    // N.B. writeToFile doesn't change the fd's position.
    int64_t writeToBuffer(uint8_t* data, int64_t reqSize)
    {
        if (reqSize > 0) {
            isDirty_ = true;
        }
        const int64_t origPositon = file_->seek(0, File::Current);
        bool needUnwind = false;

        int64_t writeSize = 0;
        while (writeSize < reqSize) {
            const int64_t bufferRestSize = BUF_SIZE - bufferIndex_;
            if (0 == bufferRestSize) {
                internalFlush();
                needUnwind = true;
            }
            *(buffer_+bufferIndex_) = *(data+writeSize);
            bufferIndex_++;
            writeSize++;
            if (buffer_[bufferIndex_-1] == '\n') {
                internalFlush();
                needUnwind = true;
            }
        }
        if (needUnwind) {
            file_->seek(origPositon);
        }
        return writeSize;
    }
};

} // namespace scheme

#endif // SCHEME_LINEBUFFERED_FILE_BINARY_INPUT_OUTPUT_PORT_

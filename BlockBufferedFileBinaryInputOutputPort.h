/*
 * BlockBufferedFileBinaryInputOutputPort.h - 
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
 *  $Id: BlockBufferedFileBinaryInputOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_BLOCKBUFFERED_FILE_BINARY_INPUT_OUTPUT_PORT_
#define SCHEME_BLOCKBUFFERED_FILE_BINARY_INPUT_OUTPUT_PORT_

#include "OSCompat.h"
#include "BufferedFileBinaryInputOutputPort.h"

namespace scheme {

class BlockBufferedFileBinaryInputOutputPort : public BufferedFileBinaryInputOutputPort
{
public:
    BlockBufferedFileBinaryInputOutputPort(const ucs4string& filename, int openFlags) :
        BufferedFileBinaryInputOutputPort(filename, openFlags) {}
    virtual ~BlockBufferedFileBinaryInputOutputPort() {}

protected:
    // N.B. writeToFile doesn't change the fd's position.
    int writeToBuffer(uint8_t* data, size_t reqSize)
    {
        if (reqSize > 0) {
            isDirty_ = true;
        }
        size_t writeSize = 0;
        const int origPositon = file_->seek(0, File::Current);
        MOSH_ASSERT(origPositon >= 0);
        bool needUnwind = false;

        while (writeSize < reqSize) {
            MOSH_ASSERT(BUF_SIZE >= bufferIndex_);
            const int bufferRestSize = BUF_SIZE - bufferIndex_;
            MOSH_ASSERT(reqSize > writeSize);
            const int restSize = reqSize - writeSize;
            if (bufferRestSize >= restSize) {
                memcpy(buffer_ + bufferIndex_, data + writeSize, restSize);
                bufferIndex_ += restSize;
                writeSize += restSize;
            } else {
                memcpy(buffer_ + bufferIndex_, data + writeSize, bufferRestSize);
                bufferIndex_ += bufferRestSize;
                writeSize += bufferRestSize;
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

#endif // SCHEME_BLOCKBUFFERED_FILE_BINARY_INPUT_OUTPUT_PORT_

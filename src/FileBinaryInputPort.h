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

#ifndef SCHEME_FILE_BINARY_INPUT_PORT_
#define SCHEME_FILE_BINARY_INPUT_PORT_

#include "BinaryInputPort.h"

namespace scheme {

class File;

// None buffering <file-binary-port>
class FileBinaryInputPort : public BinaryInputPort
{
public:
    FileBinaryInputPort(File* file);
    FileBinaryInputPort(const ucs4string& file);
    FileBinaryInputPort(const char* file);
    virtual ~FileBinaryInputPort();

    ucs4string toString();
    int getU8();
    int lookaheadU8();
    int64_t readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured);
    int64_t readSome(uint8_t** buf, bool& isErrorOccured);
    int64_t readAll(uint8_t** buf, bool& isErrorOccured);
    int open();
    int close();
    int pseudoClose();
    bool isClosed() const;
    bool hasPosition() const;
    bool hasSetPosition() const;
    Object position() const;
    bool setPosition(int64_t position);
    File* getFile() { return file_; }
    ucs4string getLastErrorMessage()
    {
        return file_->getLastErrorMessage();
    }

private:
    bool hasAheadU8() const;

    File* file_;
    ucs4string fileName_;
    bool isClosed_;
    bool isPseudoClosed_;
    int aheadU8_;
    int64_t position_;
};

} // namespace scheme

#endif // SCHEME_FILE_BINARY_INPUT_PORT_

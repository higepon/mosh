/*
 * FileBinaryInputPort.cpp - None buffering <file-binary-input-port>
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

// #ifdef _WIN32
//     #include <io.h>
// #else
// #include <unistd.h>
// #endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h> // memcpy
#include "Object.h"
#include "Object-inl.h"
#include "HeapObject.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "OSCompat.h"
#include "FileBinaryInputPort.h"
#include "Bignum.h"
#include "Symbol.h"
#include "PortProcedures.h"


using namespace scheme;

FileBinaryInputPort::FileBinaryInputPort(File* file) : file_(file), fileName_(UC("<unknown file>")), isClosed_(false), isPseudoClosed_(false), aheadU8_(EOF), position_(0)
{
}

FileBinaryInputPort::FileBinaryInputPort(const ucs4string& file) : file_(new File), fileName_(file), isClosed_(false), isPseudoClosed_(false), aheadU8_(EOF), position_(0)
{
    file_->open(file, File::Read);
}

FileBinaryInputPort::FileBinaryInputPort(const char* file) : file_(new File), isClosed_(false), isPseudoClosed_(false), aheadU8_(EOF), position_(0)
{
    fileName_ = ucs4string::from_c_str(file);
    file_->open(fileName_, File::Read);
}

FileBinaryInputPort::~FileBinaryInputPort()
{
    close();
}

int FileBinaryInputPort::open()
{
    if (file_->isOpen()) {
        return MOSH_SUCCESS;
    } else {
        return MOSH_FAILURE;
    }
}

ucs4string FileBinaryInputPort::toString()
{
    ucs4string ret = UC("<binary-input-port ");
    ret += fileName_;
    ret += UC(">");
    return ret;
}

int FileBinaryInputPort::getU8()
{
    if (hasAheadU8()) {
        const uint8_t c = aheadU8_;
        aheadU8_ = EOF;
        position_++;
        return c;
    }

    uint8_t c;
    const int64_t result = file_->read(&c, 1);
    MOSH_ASSERT(result >= 0); // error will be raised by longjmp
    if (0 == result) {
        position_++;
        return EOF;
    } else {
        position_++;
        return c;
    }
}

int FileBinaryInputPort::lookaheadU8()
{
    if (hasAheadU8()) {
        return aheadU8_;
    }

    uint8_t c;
    const int64_t result = file_->read(&c, 1);
    MOSH_ASSERT(result >= 0); // error will be raised by longjmp
    if (0 == result) {
        return EOF;
    } else {
        aheadU8_ = c;
        return c;
    }
}

int64_t FileBinaryInputPort::readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured)
{
    int64_t ret;
    if (hasAheadU8()) {
        buf[0] = aheadU8_;
        aheadU8_ = EOF;
        ret = file_->read(buf + 1, reqSize - 1);
    } else {
        ret = file_->read(buf, reqSize);
    }
    MOSH_ASSERT(ret >= 0); // error will be raised by longjmp
    position_ += ret;
    return ret;
}

int64_t FileBinaryInputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    const int64_t restSize = file_->size() - position_;
    MOSH_ASSERT(restSize >= 0);
    if (restSize == 0) {
        return 0;
    }

    uint8_t* dest = allocatePointerFreeU8Array(restSize);
    if (hasAheadU8()) {
        dest[0] = aheadU8_;
        aheadU8_ = EOF;
        *buf = dest;
        position_++;
        const int64_t ret = file_->read(dest + 1, restSize - 1);
        MOSH_ASSERT(ret >= 0); // should never happen
        return ret;
    } else {
        const int64_t ret = file_->read(dest, restSize);
        MOSH_ASSERT(ret >= 0); // should never happen
        *buf = dest;
        position_ += ret;
        return ret;
    }
}

int64_t FileBinaryInputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    uint8_t* dest = allocatePointerFreeU8Array(1);
    if (hasAheadU8()) {
        dest[0] = aheadU8_;
        aheadU8_ = EOF;
        *buf = dest;
        position_++;
        return 1;
    } else {
        const int64_t ret = file_->read(dest, 1);
        *buf = dest;
        position_ += ret;
        return ret;
    }
}

bool FileBinaryInputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

int FileBinaryInputPort::close()
{
    if (!isClosed_) {
        isClosed_ = true;
        file_->close();
    }
    return MOSH_SUCCESS;
}

int FileBinaryInputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

// binary-ports should support position.
bool FileBinaryInputPort::hasPosition() const
{
    return true;
}

bool FileBinaryInputPort::hasSetPosition() const
{
    return true;
}

Object FileBinaryInputPort::position() const
{
    return Bignum::makeIntegerFromS64(position_);
}

bool FileBinaryInputPort::setPosition(int64_t position)
{
    const int64_t ret = file_->seek(position);
    if (ret >= 0 && position == ret) {
        position_ =  position;
        return true;
    } else {
        return false;
    }
}

bool FileBinaryInputPort::hasAheadU8() const
{
    return aheadU8_ != EOF;
}

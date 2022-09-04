/*
 * FileBinaryOutputPort.cpp -
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
#include "Pair.h"
#include "Pair-inl.h"
#include "FileBinaryOutputPort.h"
#include "ByteVector.h"
#include "Symbol.h"
#include "Bignum.h"
#include "ErrorProcedures.h"
#include "OSCompat.h"

using namespace scheme;

FileBinaryOutputPort::FileBinaryOutputPort(File* file) : file_(file), fileName_(UC("<unknown file>")), isClosed_(false), isPseudoClosed_(false), position_(0)
{
}

FileBinaryOutputPort::FileBinaryOutputPort(const ucs4string& file) : file_(new File), fileName_(file), isClosed_(false), isPseudoClosed_(false), position_(0)
{
    file_->open(file, File::Read | File::Write | File::Create);
}

FileBinaryOutputPort::FileBinaryOutputPort(const ucs4string& file, int openFlags) : file_(new File), fileName_(file), isClosed_(false), isPseudoClosed_(), position_(0)
{
    file_->open(file, File::Read | File::Write | File::Create | openFlags);
}

FileBinaryOutputPort::~FileBinaryOutputPort()
{
    close();
}

bool FileBinaryOutputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

int FileBinaryOutputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

int FileBinaryOutputPort::putU8(uint8_t v)
{
    return static_cast<int>(putU8(&v, 1));
}

int64_t FileBinaryOutputPort::putU8(uint8_t* v, int64_t size)
{
    return file_->write(v, size);
}

int64_t FileBinaryOutputPort::putByteVector(ByteVector* bv, int64_t start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int64_t FileBinaryOutputPort::putByteVector(ByteVector* bv, int64_t start, int64_t count)
{
    uint8_t* buf = bv->data();
    return file_->write(&buf[start], count);
}

int FileBinaryOutputPort::open()
{
    if (file_->isOpen()) {
        return MOSH_SUCCESS;
    } else {
        return MOSH_FAILURE;
    }
}

int FileBinaryOutputPort::close()
{
    if (!isClosed_) {
        isClosed_ = true;
        file_->close();
    }
    return MOSH_SUCCESS;
}

File* FileBinaryOutputPort::getFile()
{
    return file_;
}

void FileBinaryOutputPort::flush()
{
}

ucs4string FileBinaryOutputPort::toString()
{
    ucs4string ret(UC("<binary-output-port "));
    ret += fileName_;
    ret += UC(">");
    return ret;
}

bool FileBinaryOutputPort::hasPosition() const
{
    return true;
}

bool FileBinaryOutputPort::hasSetPosition() const
{
    return true;
}

Object FileBinaryOutputPort::position() const
{
    return Bignum::makeIntegerFromS64(position_);
}

bool FileBinaryOutputPort::setPosition(int64_t position)
{
    const int64_t ret = file_->seek(position);
    if (position == ret) {
        position_ = position;
        return true;
    } else {
        return false;
    }
}



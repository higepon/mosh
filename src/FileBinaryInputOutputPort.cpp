/*
 * FileBinaryInputOutputPort.cpp -
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
 *  $Id: FileBinaryInputOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
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
#include "Symbol.h"
#include "Bignum.h"
#include "FileBinaryInputOutputPort.h"
#include "ErrorProcedures.h"
#include "OSCompat.h"
#include "PortProcedures.h"

using namespace scheme;

FileBinaryInputOutputPort::FileBinaryInputOutputPort(const ucs4string& file, int openFlags) :
    file_(new File),
    fileName_(file)
    
{
    file_->open(file, File::Read | File::Write | File::Create | openFlags);
}

FileBinaryInputOutputPort::~FileBinaryInputOutputPort()
= default;

// port interfaces
bool FileBinaryInputOutputPort::hasPosition() const
{
    return true;
}

bool FileBinaryInputOutputPort::hasSetPosition() const
{
    return true;
}

Object FileBinaryInputOutputPort::position() const
{
    const int64_t pos = file_->seek(0, File::Current);
    return Bignum::makeIntegerFromS64(pos);
}

int FileBinaryInputOutputPort::close()
{
    if (!isClosed_) {
        isClosed_ = true;
        file_->close();
    }
    return MOSH_SUCCESS;
}

int FileBinaryInputOutputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

bool FileBinaryInputOutputPort::setPosition(int64_t position)
{
    const int64_t currentOffset = file_->seek(position);
    if (position == currentOffset) {
        return true;
    } else {
        return false;
    }
}

ucs4string FileBinaryInputOutputPort::toString()
{
    ucs4string ret(UC("<binary-input/output-port "));
    ret += fileName_;
    ret += UC(">");
    return ret;
}

// binary port interfaces
int FileBinaryInputOutputPort::open()
{
    if (file_->isOpen()) {
        return MOSH_SUCCESS;
    } else {
        return MOSH_FAILURE;
    }
}

bool FileBinaryInputOutputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

// input interfaces
int FileBinaryInputOutputPort::getU8()
{
    uint8_t c;
    if (0 == file_->read(&c, 1)) {
        return EOF;
    } else {
        return c;
    }
}

int FileBinaryInputOutputPort::lookaheadU8()
{
    uint8_t c;
    const int64_t origPositon = file_->seek(0, File::Current);
    MOSH_ASSERT(origPositon >= 0);
    if (0 == file_->read(&c, 1)) {
#ifdef DEBUG_VERSION
        const int64_t result = file_->seek(origPositon);
        MOSH_ASSERT(result >= 0);
#else
        file_->seek(origPositon);
#endif
        return EOF;
    } else {
#ifdef DEBUG_VERSION
        const int64_t result = file_->seek(origPositon);
        MOSH_ASSERT(result >= 0);
#else
        file_->seek(origPositon);
#endif
        return c;
    }
}

int64_t FileBinaryInputOutputPort::readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured)
{
    const int64_t readSize = file_->read(buf, reqSize);
    return readSize;
}

int64_t FileBinaryInputOutputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    const int64_t currentOffset = file_->seek(0, File::Current);
    MOSH_ASSERT(currentOffset >= 0);
    const int64_t restSize = file_->size() - currentOffset;
    MOSH_ASSERT(restSize >= 0);
    if (restSize == 0) {
        return 0;
    }
    uint8_t* dest = allocatePointerFreeU8Array(restSize);
    const int64_t readSize = file_->read(dest, restSize);
    *buf = dest;
    return readSize;
}

int64_t FileBinaryInputOutputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    return readAll(buf, isErrorOccured);
}

// output interfaces
int FileBinaryInputOutputPort::putU8(uint8_t v)
{
    return static_cast<int>(putU8(&v, 1));
}

int64_t FileBinaryInputOutputPort::putU8(uint8_t* v, int64_t size)
{
    const int64_t writtenSize = file_->write(v, size);
    return writtenSize;
}

int64_t FileBinaryInputOutputPort::putByteVector(ByteVector* bv, int64_t start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int64_t FileBinaryInputOutputPort::putByteVector(ByteVector* bv, int64_t start, int64_t count)
{
    uint8_t* buf = bv->data();
    const int64_t writtenSize = file_->write(&buf[start], count);
    return writtenSize;
}

void FileBinaryInputOutputPort::flush()
{
}


File* FileBinaryInputOutputPort::getFile()
{
    return nullptr;
}


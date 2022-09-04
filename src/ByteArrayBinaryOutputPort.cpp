/*
 * ByteArrayBinaryOutputPort.cpp -
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
 *  $Id: ByteArrayBinaryOutputPort.cpp 1331 2009-03-13 08:27:20Z higepon $
 */

#include <string.h> // memcpy
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteArrayBinaryOutputPort.h"
#include "ByteVector.h"
#include "Symbol.h"
#include "Bignum.h"
#include "ErrorProcedures.h"
#include "PortProcedures.h"

using namespace scheme;

ByteArrayBinaryOutputPort::ByteArrayBinaryOutputPort() 
    
= default;

ByteArrayBinaryOutputPort::~ByteArrayBinaryOutputPort()
= default;

bool ByteArrayBinaryOutputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

int ByteArrayBinaryOutputPort::putU8(uint8_t v)
{
    buffer_.resize(position_ + 1, 0);
    buffer_[position_++] = v;
    return 1;
}

int64_t ByteArrayBinaryOutputPort::putU8(uint8_t* v, int64_t _size)
{
    MOSH_ASSERT(isInSize_t(_size));
    const size_t size = static_cast<size_t>(_size);
    buffer_.resize(position_ + size, 0);
    for (size_t i = 0; i < size; i++) {
        buffer_[position_ + i] = v[i];
    }
    position_ += size;
    return size;
}

int64_t ByteArrayBinaryOutputPort::putByteVector(ByteVector* bv, int64_t start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int64_t ByteArrayBinaryOutputPort::putByteVector(ByteVector* bv, int64_t start, int64_t count)
{
    uint8_t* buf = bv->data();
    return putU8(&buf[start], count);
}

int ByteArrayBinaryOutputPort::open()
{
    return MOSH_SUCCESS;
}

int ByteArrayBinaryOutputPort::close()
{
    isClosed_ = true;
    return MOSH_SUCCESS;
}

int ByteArrayBinaryOutputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

void ByteArrayBinaryOutputPort::flush()
{
}

ucs4string ByteArrayBinaryOutputPort::toString()
{
    return ucs4string(UC("<bytevector-output-port>"));
}

bool ByteArrayBinaryOutputPort::hasPosition() const
{
    return true;
}

bool ByteArrayBinaryOutputPort::hasSetPosition() const
{
    return true;
}

Object ByteArrayBinaryOutputPort::position() const
{
    return Bignum::makeInteger(position_);
}

bool ByteArrayBinaryOutputPort::setPosition(int64_t position)
{
    if (position >= 0) {
        MOSH_ASSERT(isInSize_t(position));
        position_ = static_cast<uintptr_t>(position);
        return true;
    } else {
        return false;
    }
}

ByteVector* ByteArrayBinaryOutputPort::toByteVector()
{
    ByteVector* ret = new ByteVector(buffer_);
    buffer_.clear();
    position_ = 0;
    return ret;
}

File* ByteArrayBinaryOutputPort::getFile()
{
    return nullptr;
}

/*
 * ByteArrayBinaryInputPort.cpp - 
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: ByteArrayBinaryInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "ByteArrayBinaryInputPort.h"
#include "Fixnum.h"
#include "Bignum.h"


using namespace scheme;

ByteArrayBinaryInputPort::ByteArrayBinaryInputPort(const uint8_t* buf, int size) : buf_(buf), size_(size), index_(0), isClosed_(false), isPseudoClosed_(false)
{
}

ByteArrayBinaryInputPort::~ByteArrayBinaryInputPort()
{
    close();
}

ucs4string ByteArrayBinaryInputPort::toString() {
    return UC("<byte-array-input-port>");
}

int ByteArrayBinaryInputPort::readBytes(uint8_t* buf, int reqSize, bool& isErrorOccured)
{
    const int restSize = size_ - index_;
    const int sizeToRead = (reqSize > restSize) ? restSize : reqSize;
    memcpy(buf, &(buf_[index_]), sizeToRead);
    index_ += sizeToRead;
    return sizeToRead;
}

int ByteArrayBinaryInputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    return readAll(buf, isErrorOccured);
}

int ByteArrayBinaryInputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    const int restSize = size_ - index_;
    uint8_t* dest = allocatePointerFreeU8Array(restSize);
    memcpy(dest, &buf_[index_], restSize);
    *buf = dest;
    return restSize;
}

int ByteArrayBinaryInputPort::open()
{
    return MOSH_SUCCESS;
}

bool ByteArrayBinaryInputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

int ByteArrayBinaryInputPort::close()
{
    isClosed_ = true;
    return MOSH_SUCCESS;
}

int ByteArrayBinaryInputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

int ByteArrayBinaryInputPort::fileNo() const
{
    return BinaryInputPort::INVALID_FILENO;
}

Object ByteArrayBinaryInputPort::position() const
{
    return Bignum::makeInteger(index_);
}

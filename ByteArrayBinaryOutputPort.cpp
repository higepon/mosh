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
{
}

ByteArrayBinaryOutputPort::~ByteArrayBinaryOutputPort()
{
}

bool ByteArrayBinaryOutputPort::isClosed() const
{
    return false;
}

int ByteArrayBinaryOutputPort::putU8(uint8_t v)
{
    return putU8(&v, 1);
}

int ByteArrayBinaryOutputPort::putU8(uint8_t* v, int size)
{
    for (int i = 0; i < size; i++) {
        buffer_.push_back(v[i]);
    }
    return size;
}

int ByteArrayBinaryOutputPort::putByteVector(ByteVector* bv, int start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int ByteArrayBinaryOutputPort::putByteVector(ByteVector* bv, int start, int count)
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
    return MOSH_SUCCESS;
}

void ByteArrayBinaryOutputPort::flush()
{
}

ucs4string ByteArrayBinaryOutputPort::toString()
{
    return UC("<byte-array-output-port>");
}

bool ByteArrayBinaryOutputPort::hasPosition() const
{
    return false;
}

bool ByteArrayBinaryOutputPort::hasSetPosition() const
{
    return false;
}

Object ByteArrayBinaryOutputPort::position() const
{
    return Object::Undef;
}

bool ByteArrayBinaryOutputPort::setPosition(int position)
{
    return false;
}

ByteVector* ByteArrayBinaryOutputPort::toByteVector() const
{
    return new ByteVector(buffer_);
}

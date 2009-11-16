/*
 * StringTextualOutputPort.cpp -
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
 *  $Id: StringTextualOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Bignum.h"
#include "StringTextualOutputPort.h"
#include "Transcoder.h"
#include "OSCompat.h"

using namespace scheme;

StringTextualOutputPort::StringTextualOutputPort() : isClosed_(false), index_(0)
{
}

StringTextualOutputPort::~StringTextualOutputPort()
{
    close();
}

void StringTextualOutputPort::putChar(ucs4char c)
{
    if (buffer_.size() > index_) {
        buffer_[index_] = c;
    } else {
        buffer_ += c;
    }
    index_++;
}

ucs4string StringTextualOutputPort::getString()
{
    return buffer_;
}

void StringTextualOutputPort::reset()
{
    buffer_ = UC("");
    index_ = 0;
}

int StringTextualOutputPort::close()
{
    isClosed_ = true;
    return 0;
}

bool StringTextualOutputPort::isClosed() const
{
    return isClosed_;
}

void StringTextualOutputPort::flush()
{
    return;
}

ucs4string StringTextualOutputPort::toString()
{
    return UC("<string-output-port>");
}

bool StringTextualOutputPort::hasPosition() const
{
    return true;
}

bool StringTextualOutputPort::hasSetPosition() const
{
    return true;
}

Object StringTextualOutputPort::position() const
{
    return Bignum::makeInteger(index_);
}

bool StringTextualOutputPort::setPosition(int64_t _position)
{
    MOSH_ASSERT(isInSize_t(_position));
    const size_t position = static_cast<size_t>(_position);
    if (position > index_) {
        buffer_.resize(position, ' ');
    }
    index_ = position;
    return true;
}

enum OutputPort::bufferMode StringTextualOutputPort::bufferMode() const
{
    return NONE;
}

Transcoder* StringTextualOutputPort::transcoder() const
{
    return createNativeTranscoder();
}

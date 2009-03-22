/*
 * StringTextualInputPort.cpp - 
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
 *  $Id: StringTextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "StringTextualInputPort.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "Bignum.h"
#include "ByteArrayBinaryInputPort.h"

using namespace scheme;


StringTextualInputPort::StringTextualInputPort(const ucs4string& str) :
    TextualInputPort(),
    isClosed_(false),
    buffer_(str),
    index_(0),
    lineNo_(1)
{
}

StringTextualInputPort::~StringTextualInputPort()
{
}

int StringTextualInputPort::getLineNo() const
{
    return lineNo_;
}

ucs4char StringTextualInputPort::getChar()
{
    if (buffer_.size() == index_)
    {
        return EOF;
    }
    const ucs4char ch = buffer_[index_++];
    if (ch == '\n') {
        lineNo_++;
    }
    return ch;
}

void StringTextualInputPort::unGetChar(ucs4char c)
{
    if (EOF == c) return;
    index_--;
}

ucs4string StringTextualInputPort::toString()
{
    return UC("<string-input-port>");
}

int StringTextualInputPort::close()
{
    isClosed_ = true;
    return 0;
}

bool StringTextualInputPort::isClosed() const
{
    return isClosed_;
}

bool StringTextualInputPort::hasPosition() const
{
    return true;
}

bool StringTextualInputPort::hasSetPosition() const
{
    return true;
}

Object StringTextualInputPort::position() const
{
    return Bignum::makeInteger(index_);
}

bool StringTextualInputPort::setPosition(int position)
{
    if (position >= (int)buffer_.size()) {
        return false;
    } else {
        index_ = position;
        return true;
    }
}

Transcoder* StringTextualInputPort::transcoder() const
{
    return Transcoder::nativeTranscoder();
}


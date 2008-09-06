/*
 * TextualInputPort.cpp - 
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
 *  $Id: TextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Codec.h"
#include "Transcoder.h"
#include "BinaryInputPort.h"
#include "TextualInputPort.h"
#include "reader.h"

using namespace scheme;


TextualInputPort::TextualInputPort(BinaryInputPort* port, Transcoder* coder) : port_(port), codec_(coder->getCodec()), coder_(coder), line_(1)
{
}

TextualInputPort::TextualInputPort()
{
}

TextualInputPort::~TextualInputPort()
{
}

ucs4char TextualInputPort::getChar()
{
    ucs4char c;
    if (buffer_.empty()) {
        c= codec_->in(port_);
    } else {
        c = buffer_[buffer_.size() - 1];
        buffer_.erase(buffer_.size() - 1, 1);
    }
    if (c == '\n') ++line_;
    return c;
}

int TextualInputPort::getLine() const
{
    return line_;
}

void TextualInputPort::unGetChar(ucs4char c)
{
    if (EOF == c) return;
    buffer_ += c;
}

ucs4string TextualInputPort::toString() {
    return port_->toString();
}

void TextualInputPort::setError(Object error)
{
    error_ = error;
}

Object TextualInputPort::error() const
{
    return error_;
}

Object TextualInputPort::getDatum(bool& errorOccured)
{
    return read(this, errorOccured);
}

Object TextualInputPort::getDatum2()
{
    return read2(this);
}

int TextualInputPort::close()
{
    return port_->close();
}

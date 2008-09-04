/*
 * TextualByteVectorOutputPort.cpp - 
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
 *  $Id: TextualByteVectorOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "TextualByteVectorOutputPort.h"

using namespace scheme;

TextualByteVectorOutputPort::TextualByteVectorOutputPort(Transcoder* transcoder) : transcoder_(transcoder), codec_(transcoder->getCodec())
{
}

~TextualByteVectorOutputPort::TextualByteVectorOutputPort()
{
}

void TextualByteVectorOutputPort::putChar(ucs4char c)
{
    uint8_t buf[4];
    const int size = codec_->out(buf, c);
    for (int i = 0; i < size; i++) {
        v_.push_back(buf[i]);
    }
}

const gc_vector<uint8_t>& TextualByteVectorOutputPort::getByteVector() const
{
    return v_;
}

int TextualByteVectorOutputPort::close()
{
    return 0;
}

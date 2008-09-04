/*
 * TextualOutputPort.cpp - 
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
 *  $Id: TextualOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "TextualOutputPort.h"

using namespace scheme;

TextualOutputPort::TextualOutputPort()
{
}

TextualOutputPort::TextualOutputPort(BinaryOutputPort* port, Transcoder* coder) : port_(port),
                                                                                  codec_(coder->getCodec()),
                                                                                  coder_(coder),
                                                                                  isErrorOccured_(false),
                                                                                  errorMessage_(Object::Nil),
                                                                                  irritants_(Object::Nil)
{
}

TextualOutputPort::~TextualOutputPort()
{
}

int TextualOutputPort::close()
{
    return port_->close();
}

void TextualOutputPort::putString(String* str)
{
    putString(str->data());
}

void TextualOutputPort::putString(const ucs4string& s)
{
    for (ucs4string::size_type i = 0; i < s.size(); i++) {
        putChar(s[i]);
    }
}

void TextualOutputPort::putString(const char* s)
{
    const int len = strlen(s);
    for (int i = 0; i < len; i++) {
        putChar(s[i]);
    }
}

void TextualOutputPort::putChar(ucs4char c)
{
    codec_->out(port_, c);
}

BinaryOutputPort* TextualOutputPort::binaryPort() const
{
    return port_;
}

bool TextualOutputPort::isErrorOccured() const
{
    return isErrorOccured_;
}

Object TextualOutputPort::errorMessage() const
{
    return errorMessage_;
}

Object TextualOutputPort::irritants() const
{
    return irritants_;
}

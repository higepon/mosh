/*
 * TranscodedTextualInputPort.cpp - 
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
 *  $Id: TranscodedTextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Codec.h"
#include "Transcoder.h"
#include "BinaryInputPort.h"
#include "TextualInputPort.h"
#include "Reader.h"
#include "Scanner.h"
#include "NumberScanner.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "TextualOutputPort.h"
#include "ProcedureMacro.h"
#include "TranscodedTextualInputPort.h"

using namespace scheme;

TranscodedTextualInputPort::TranscodedTextualInputPort(BinaryInputPort* port, Transcoder* coder)
    : TextualInputPort(),
      port_(port),
      transcoder_(coder)
{
}

int TranscodedTextualInputPort::getLineNo() const
{
    return transcoder_->getLineNo();
}


TranscodedTextualInputPort::~TranscodedTextualInputPort()
{
    close();
}

ucs4char TranscodedTextualInputPort::getChar()
{
    return transcoder_->getChar(port_);
}

void TranscodedTextualInputPort::unGetChar(ucs4char c)
{
    return transcoder_->unGetChar(c);
}

ucs4string TranscodedTextualInputPort::toString()
{
    return port_->toString();
}

int TranscodedTextualInputPort::close()
{
    if (NULL != port_) {
        return port_->close();
    } else {
        return 0;
    }
}

Transcoder* TranscodedTextualInputPort::transcoder() const
{
    return transcoder_;
}


/*
 * TranscodedTextualOutputPort.cpp -
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
 *  $Id: TranscodedTextualOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */


#include "Object.h"
#include "Object-inl.h"
#include "Transcoder.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "Vector.h"
#include "Symbol.h"
#include "Regexp.h"
#include "ByteVector.h"
#include "Record.h"
#include "Codec.h"
#include "Transcoder.h"
#include "BinaryInputPort.h"
#include "BinaryOutputPort.h"
#include "TextualOutputPort.h"
#include "TextualInputPort.h"
#include "ProcedureMacro.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Compnum.h"
#include "Arithmetic.h"
#include "CompoundCondition.h"
#include "BinaryOutputPort.h"
#include "BinaryInputOutputPort.h"
#include "TranscodedTextualOutputPort.h"

using namespace scheme;

TranscodedTextualOutputPort::TranscodedTextualOutputPort(BinaryOutputPort* port, Transcoder* coder)
  : port_(port),
    transcoder_(coder),
    eolStyle_(coder->eolStyle())
{
}

TranscodedTextualOutputPort::~TranscodedTextualOutputPort()
{
    // close automatically by gc().
    close();
}

enum OutputPort::bufferMode TranscodedTextualOutputPort::bufferMode() const
{
    return port_->bufferMode();
}

int TranscodedTextualOutputPort::close()
{
    MOSH_ASSERT(port_ != NULL);
    return port_->close();
}

void TranscodedTextualOutputPort::putChar(ucs4char c)
{
    transcoder_->putChar(port_, c);
}

BinaryOutputPort* TranscodedTextualOutputPort::binaryPort() const
{
    return port_;
}

Transcoder* TranscodedTextualOutputPort::transcoder() const
{
    return transcoder_;
}

void TranscodedTextualOutputPort::flush()
{
    MOSH_ASSERT(port_ != NULL);
    port_->flush();
}

ucs4string TranscodedTextualOutputPort::toString()
{
    return port_->toString();
}

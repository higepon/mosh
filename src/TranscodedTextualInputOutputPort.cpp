/*
 * TranscodedTextualInputOutputPort.cpp -
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
 *  $Id: TranscodedTextualInputOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
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
#include "Codec.h"
#include "Transcoder.h"
#include "ProcedureMacro.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Compnum.h"
#include "Arithmetic.h"
#include "TranscodedTextualInputOutputPort.h"

using namespace scheme;

TranscodedTextualInputOutputPort::TranscodedTextualInputOutputPort(BinaryInputOutputPort* port, Transcoder* coder)
  : port_(port),
    transcoder_(coder)
    
{
}

TranscodedTextualInputOutputPort::~TranscodedTextualInputOutputPort()
{
    // close automatically by gc().
    close();
}

int TranscodedTextualInputOutputPort::close()
{
    isClosed_ = true;
    MOSH_ASSERT(port_ != NULL);
    return port_->close();
}

bool TranscodedTextualInputOutputPort::isClosed() const
{
    return isClosed_;
}

Object TranscodedTextualInputOutputPort::position() const
{
    return Object::Undef;
}

bool TranscodedTextualInputOutputPort::setPosition(int64_t position)
{
    return false;
}

bool TranscodedTextualInputOutputPort::hasPosition() const
{
    return false;
}

bool TranscodedTextualInputOutputPort::hasSetPosition() const
{
    return false;
}

// TextualInputPort interfaces
ucs4char TranscodedTextualInputOutputPort::getChar()
{
    return transcoder_->getChar(port_);
}

int TranscodedTextualInputOutputPort::getLineNo() const
{
    return line_;
}

void TranscodedTextualInputOutputPort::unGetChar(ucs4char c)
{
    return transcoder_->unGetChar(c);
}

Transcoder* TranscodedTextualInputOutputPort::transcoder() const
{
    return transcoder_;
}

// TextualOutputPort interfaces
void TranscodedTextualInputOutputPort::putChar(ucs4char c)
{
    transcoder_->putChar(port_, c);
}

void TranscodedTextualInputOutputPort::flush()
{
    MOSH_ASSERT(port_ != NULL);
    port_->flush();
}

ucs4string TranscodedTextualInputOutputPort::toString()
{
    ucs4string ret = UC("<transcoded-textual-input/output-port ");
    ret += port_->toString();
    ret += UC(">");
    return ret;
}


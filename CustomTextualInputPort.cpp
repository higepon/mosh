/*
 * CustomTextualInputPort.cpp -
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
 *  $Id: CustomTextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */


#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "CustomTextualInputPort.h"
#include "Bignum.h"
#include "SString.h"
#include "VM.h"
#include "Transcoder.h"

using namespace scheme;

CustomTextualInputPort::CustomTextualInputPort(VM* theVM, const ucs4string& id, Object readProc, Object getPositionProc, Object setPositionProc, Object closeProc)
    : theVM_(theVM),
      id_(id),
      readProc_(readProc),
      getPositionProc_(getPositionProc),
      setPositionProc_(setPositionProc),
      closeProc_(closeProc),
      buffer_(UC("")),
      line_(1)
{
    MOSH_ASSERT(readProc_.isProcedure());
    MOSH_ASSERT(getPositionProc_.isProcedure() || getPositionProc_.isFalse());
    MOSH_ASSERT(setPositionProc_.isProcedure() || setPositionProc_.isFalse());
    MOSH_ASSERT(closeProc_.isProcedure() || closeProc_.isFalse());

}

CustomTextualInputPort::~CustomTextualInputPort()
{
}

ucs4char CustomTextualInputPort::getChar()
{
    ucs4char c;
    if (buffer_.empty()) {
        const Object text = UC(" ");
        const Object start = Object::makeFixnum(0);
        const Object count = Object::makeFixnum(1);
        const Object result = theVM_->callClosure3(readProc_, text, start, count);
        MOSH_ASSERT(result.isFixnum());
        if (0 == result.toFixnum()) {
            return EOF;
        }
        c = text.toString()->data()[0];
    } else {
        c = buffer_[buffer_.size() - 1];
        buffer_.erase(buffer_.size() - 1, 1);
    }
    if (c == '\n') ++line_;
    return c;
}

int CustomTextualInputPort::getLineNo() const
{
    return line_;
}

void CustomTextualInputPort::unGetChar(ucs4char c)
{
    if (EOF == c) return;
    buffer_ += c;
}

Transcoder* CustomTextualInputPort::transcoder() const
{
    return Transcoder::nativeTranscoder();
}

bool CustomTextualInputPort::hasPosition() const
{
    return !getPositionProc_.isFalse();
}

bool CustomTextualInputPort::hasSetPosition() const
{
    return !setPositionProc_.isFalse();
}

Object CustomTextualInputPort::position() const
{
    // hasPosition() should be checked by the user of this class.
    MOSH_ASSERT(hasPosition());
    const Object position = theVM_->callClosure0(getPositionProc_);
    if (position.isFixnum() && !buffer_.empty()) {
        return Object::makeFixnum(position.toFixnum() - buffer_.size());
    } else {
        return position;
    }
}

bool CustomTextualInputPort::setPosition(int position)
{
    MOSH_ASSERT(hasSetPosition());
    // we need to reset the cache
    buffer_.clear();
    theVM_->callClosure1(setPositionProc_, Bignum::makeInteger(position));
    return true;
}

ucs4string CustomTextualInputPort::toString()
{
    ucs4string ret = UC("<custom textual input port ");
    ret += id_;
    ret += UC(">");
    return ret;
}

int CustomTextualInputPort::close()
{
    if (closeProc_.isCallable()) {
        theVM_->callClosure0(closeProc_);
    }
    return 0;
}

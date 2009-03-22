/*
 * CustomTextualInputPort.cpp -
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
 *  $Id: CustomTextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ByteVector.h"
#include "CustomTextualOutputPort.h"
#include "Fixnum.h"
#include "Bignum.h"
#include "VM.h"
#include "Transcoder.h"

using namespace scheme;

CustomTextualOutputPort::CustomTextualOutputPort(VM* theVM,
                                                 const ucs4string& id,
                                                 Object writeDProc,
                                                 Object getPositionProc,
                                                 Object setPositionDProc,
                                                 Object closeProc)
    : theVM_(theVM),
      id_(id),
      writeDProc_(writeDProc),
      getPositionProc_(getPositionProc),
      setPositionDProc_(setPositionDProc),
      closeProc_(closeProc),
      isClosed_(false)
{
    MOSH_ASSERT(writeDProc_.isProcedure());
    MOSH_ASSERT(getPositionProc_.isProcedure() || getPositionProc_.isFalse());
    MOSH_ASSERT(setPositionDProc_.isProcedure() || setPositionDProc_.isFalse());
    MOSH_ASSERT(closeProc_.isProcedure() || closeProc_.isFalse());
}

CustomTextualOutputPort::~CustomTextualOutputPort()
{
}

ucs4string CustomTextualOutputPort::toString()
{
    ucs4string ret = UC("<custom-textual-output-port ");
    ret += id_;
    ret += UC(">");
    return ret;
}

int CustomTextualOutputPort::close()
{
    if (closeProc_.isCallable()) {
        theVM_->callClosure0(closeProc_);
    }
    isClosed_ = true;
    return 0;
}

bool CustomTextualOutputPort::isClosed() const
{
    return isClosed_;
}

void CustomTextualOutputPort::flush()
{
}

enum OutputPort::bufferMode CustomTextualOutputPort::bufferMode() const
{
    return OutputPort::NONE;
}

void CustomTextualOutputPort::putChar(ucs4char c)
{
    const Object text = UC(" ");
    const Object start = Object::makeFixnum(0);
    const Object count = Object::makeFixnum(1);
    text.toString()->data()[0] = c;
    const Object result = theVM_->callClosure3(writeDProc_, text, start, count);
    MOSH_ASSERT(result.isFixnum());
    return;
}

Transcoder* CustomTextualOutputPort::transcoder() const
{
    return Transcoder::nativeTranscoder();
}

bool CustomTextualOutputPort::hasPosition() const
{
    return !setPositionDProc_.isFalse();
}

bool CustomTextualOutputPort::hasSetPosition() const
{
    return !setPositionDProc_.isFalse();
}

Object CustomTextualOutputPort::position() const
{
    // hasPosition() should be checked by the user of this class.
    MOSH_ASSERT(hasPosition());
    const Object position = theVM_->callClosure0(getPositionProc_);
    return position;
}

bool CustomTextualOutputPort::setPosition(int position)
{
    MOSH_ASSERT(hasSetPosition());
    // we need to reset the cache
    theVM_->callClosure1(setPositionDProc_, Bignum::makeInteger(position));
    return true;
}

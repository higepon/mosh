/*
 * CustomBinaryInputPort.cpp -
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
 *  $Id: CustomBinaryInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */


#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "CustomBinaryInputPort.h"
#include "Bignum.h"
#include "VM.h"

using namespace scheme;

CustomBinaryInputPort::CustomBinaryInputPort(VM* theVM, const ucs4string& id, Object readProc, Object getPositionProc, Object setPositionProc, Object closeProc)
    : theVM_(theVM),
      id_(id),
      readProc_(readProc),
      getPositionProc_(getPositionProc),
      setPositionProc_(setPositionProc),
      closeProc_(closeProc),
      isClosed_(false),
      isPseudoClosed_(false),
      aheadU8_(EOF)
{
    MOSH_ASSERT(readProc_.isProcedure());
    MOSH_ASSERT(getPositionProc_.isProcedure() || getPositionProc_.isFalse());
    MOSH_ASSERT(setPositionProc_.isProcedure() || setPositionProc_.isFalse());
    MOSH_ASSERT(closeProc_.isProcedure() || closeProc_.isFalse());

}

CustomBinaryInputPort::~CustomBinaryInputPort()
{
}

ucs4string CustomBinaryInputPort::toString()
{
    ucs4string ret = UC("<custom-input-port ");
    ret += id_;
    ret += UC(">");
    return ret;
}

int CustomBinaryInputPort::open()
{
    return 0;
}

int CustomBinaryInputPort::close()
{
    if (closeProc_.isCallable()) {
        theVM_->callClosure0(closeProc_);
    }
    isClosed_ = true;
    return MOSH_SUCCESS;
}

int CustomBinaryInputPort::pseudoClose()
{
    isPseudoClosed_ = true;
    return MOSH_SUCCESS;
}

bool CustomBinaryInputPort::isClosed() const
{
    return isClosed_ || isPseudoClosed_;
}

#include "SString.h"
#include "StringProcedures.h"
int CustomBinaryInputPort::getU8()
{
    if (hasAheadU8()) {
        int c = aheadU8_;
        aheadU8_ = EOF;
        return c;
    }

    const Object bv = Object::makeByteVector(1);
    const Object start = Object::makeFixnum(0);
    const Object count = Object::makeFixnum(1);
    const Object result = theVM_->callClosure3(readProc_, bv, start, count);
    MOSH_ASSERT(result.isFixnum());
    if (0 == result.toFixnum()) {
        return EOF;
    }
    return bv.toByteVector()->u8Ref(0);
}

int CustomBinaryInputPort::lookaheadU8()
{
    if (hasAheadU8()) {
        return aheadU8_;
    }

    const Object bv = Object::makeByteVector(1);
    const Object start = Object::makeFixnum(0);
    const Object count = Object::makeFixnum(1);
    const Object result = theVM_->callClosure3(readProc_, bv, start, count);
    MOSH_ASSERT(result.isFixnum());
    if (0 == result.toFixnum()) {
        return EOF;
    }
    aheadU8_ = bv.toByteVector()->u8Ref(0);
    return aheadU8_;
}


int64_t CustomBinaryInputPort::readBytes(uint8_t* buf, int64_t reqSize, bool& isErrorOccured)
{
    size_t readSize;
    for (readSize = 0; readSize < reqSize; readSize++) {
        const int v = getU8();
        if (EOF == v) {
            break;
        }
        buf[readSize] = v;
    }
    return readSize;
}

int64_t CustomBinaryInputPort::readSome(uint8_t** buf, bool& isErrorOccured)
{
    const int v = getU8();
    if (EOF == v) {
        return 0;
    } else {
        uint8_t* dest = allocatePointerFreeU8Array(1);
        dest[0] = v;
        *buf = dest;
        return 1;
    }
}

int64_t CustomBinaryInputPort::readAll(uint8_t** buf, bool& isErrorOccured)
{
    gc_vector<uint8_t> accum;
    for (;;) {
        const int v = getU8();
        if (EOF == v) {
            break;
        }
        accum.push_back(v);
    }
    uint8_t* dest = allocatePointerFreeU8Array(accum.size());
    for (size_t i = 0; i < accum.size(); i++) {
        dest[i] = accum[i];
    }
    *buf = dest;
    return accum.size();
}

bool CustomBinaryInputPort::hasPosition() const
{
    return !getPositionProc_.isFalse();
}

bool CustomBinaryInputPort::hasSetPosition() const
{
    return !setPositionProc_.isFalse();
}

bool CustomBinaryInputPort::hasAheadU8() const
{
    return aheadU8_ != EOF;
}

Object CustomBinaryInputPort::position() const
{
    // hasPosition() should be checked by the user of this class.
    MOSH_ASSERT(hasPosition());
    const Object position = theVM_->callClosure0(getPositionProc_);
    if (position.isFixnum() && hasAheadU8()) {
        return Object::makeFixnum(position.toFixnum() - 1);
    } else {
        return position;
    }
}

bool CustomBinaryInputPort::setPosition(int64_t position)
{
    MOSH_ASSERT(hasSetPosition());
    // we need to reset the aheadU8_
    aheadU8_ = EOF;
    theVM_->callClosure1(setPositionProc_, Bignum::makeIntegerFromS64(position));
    return true;
}

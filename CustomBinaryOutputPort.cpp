/*
 * CustomBinaryOutputPort.cpp -
 *
 *   Copyright (c) 2009  Kokosabu(MIURA Yasuyuki)  <kokosabu@gmail.com>
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
 *  $Id$
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "CustomBinaryOutputPort.h"
#include "Fixnum.h"
#include "Bignum.h"
#include "VM.h"

using namespace scheme;

CustomBinaryOutputPort::CustomBinaryOutputPort(VM* theVM, const ucs4string& id, Object writeDProc, Object getPositionProc, Object setPositionDProc, Object closeProc)
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

CustomBinaryOutputPort::~CustomBinaryOutputPort()
{
}

ucs4string CustomBinaryOutputPort::toString()
{
    ucs4string ret = UC("<custom output port ");
    ret += id_;
    ret += UC(">");
    return ret;
}

int CustomBinaryOutputPort::putU8(uint8_t v)
{
    const Object bv = Object::makeByteVector(1, v);
    const Object start = Object::makeFixnum(0);
    const Object count = Object::makeFixnum(1);
    const Object result = theVM_->callClosure3(writeDProc_, bv, start, count);
    MOSH_ASSERT(result.isFixnum());
    return result.toFixnum();
}

int CustomBinaryOutputPort::putU8(uint8_t* v, int size)
{
    const Object bv = Object::makeByteVector(new ByteVector(size, v));
    const Object start = Object::makeFixnum(0);
    if (Fixnum::canFit(size)) {
        const Object count = Object::makeFixnum(size);
        const Object result = theVM_->callClosure3(writeDProc_, bv, start, count);
        MOSH_ASSERT(result.isFixnum());
        return result.toFixnum();
    } else {
        const Object count = Object::makeBignum(size);
        const Object result = theVM_->callClosure3(writeDProc_, bv, start, count);
        MOSH_ASSERT(result.isBignum());
        return result.toBignum()->toS32();
    }
}

int CustomBinaryOutputPort::putByteVector(ByteVector* bv, int start /* = 0 */)
{
    return putByteVector(bv, start, bv->length() - start);
}

int CustomBinaryOutputPort::putByteVector(ByteVector* bv, int start, int count)
{
    Object startObj;
    if (Fixnum::canFit(start)) {
        startObj = Object::makeFixnum(start);
    } else {
        startObj = Object::makeBignum(start);
    }
    Object countObj;
    if (Fixnum::canFit(count)) {
        countObj = Object::makeFixnum(count);
    } else {
        countObj = Object::makeBignum(count);
    }
    const Object result = theVM_->callClosure3(writeDProc_, Object::makeByteVector(bv), startObj, countObj);
    MOSH_ASSERT(result.isFixnum() || result.isBignum());
    if (result.isFixnum()) {
        return result.toFixnum();
    } else {
        return result.toBignum()->toS32();
    }
}

int CustomBinaryOutputPort::open()
{
    return 0;
}

int CustomBinaryOutputPort::close()
{
    if (closeProc_.isCallable()) {
        theVM_->callClosure0(closeProc_);
    }
    isClosed_ = true;
    return 0;
}

bool CustomBinaryOutputPort::isClosed() const
{
    return isClosed_;
}

int CustomBinaryOutputPort::fileNo() const
{
    return BinaryPort::INVALID_FILENO;
}

void CustomBinaryOutputPort::bufFlush()
{
    // No Operation
}

bool CustomBinaryOutputPort::hasPosition() const
{
    return !getPositionProc_.isFalse();
}

bool CustomBinaryOutputPort::hasSetPosition() const
{
    return !setPositionDProc_.isFalse();
}

Object CustomBinaryOutputPort::position() const
{
    // hasPosition() should be checked by the user of this class.
    MOSH_ASSERT(hasPosition());
    return theVM_->callClosure0(getPositionProc_);
}

bool CustomBinaryOutputPort::setPosition(int position)
{
    MOSH_ASSERT(hasSetPosition());
    theVM_->callClosure1(setPositionDProc_, Bignum::makeInteger(position));
    return true;
}

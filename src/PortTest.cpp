/*
 * TestPort.cpp - Test for Port.
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
 *  $Id: TestVector.cpp 183 2008-07-04 06:19:28Z higepon $
 */
#include <gtest/gtest.h>

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "StringTextualOutputPort.h"
#include "TextualInputPort.h"
#include "PortProcedures.h"
#include "FileBinaryOutputPort.h"
#include "TestingFileBinaryOutputPort.h"
#include "FileBinaryInputPort.h"
#include "TestingFileBinaryInputPort.h"
#include "Vector.h"
#include "SString.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "FileBinaryInputPort.h"
#include "FileBinaryOutputPort.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Symbol.h"
#include "TestingVM.h"
#include "StringTextualOutputPort.h"
#include "PortProcedures.h"
#include "EqHashTable.h"
#include "Closure.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "BlockBufferedFileBinaryOutputPort.h"
#include "BufferedFileBinaryInputPort.h"
#include "OSCompatThread.h"
#include "VMFactory.h"
#include "MultiVMProcedures.h"

bool scheme::portIsClosed = false;

using namespace scheme;

#ifdef WITH_NMOSH_DEFAULTS
extern "C" const uint8_t* nmosh_image_ptr;
extern "C" const unsigned int nmosh_image_size;
#else
extern "C" const uint8_t* psyntax_mosh_image_ptr;
extern "C" unsigned int psyntax_mosh_image_size;
#endif
Object activateR6RSMode(VM* theVM, bool isDebugExpand)
{
#ifdef WITH_NMOSH_DEFAULTS
    return theVM->activateR6RSMode(nmosh_image_ptr, nmosh_image_size, isDebugExpand);
#else
    return theVM->activateR6RSMode(psyntax_mosh_image_ptr, psyntax_mosh_image_size, isDebugExpand);
#endif
}

class PortTest : public testing::Test {
protected:
    VM* theVM_;
    virtual void SetUp() {
        mosh_init();
        VMFactory factory;
        theVM_ = factory.create(1000, false);
        setCurrentVM(theVM_);
    }
};

TEST_F(PortTest, PortPredicate) {
    const Object stringPort = Object::makeStringOutputPort();
    EXPECT_TRUE(stringPort.isTextualOutputPort());
    EXPECT_FALSE(stringPort.isTextualInputPort());
    EXPECT_FALSE(stringPort.isBinaryInputPort());
    EXPECT_FALSE(stringPort.isBinaryOutputPort());
}

TEST_F(PortTest, StringInputPort) {
    bool isErrorOccured = false;
    EXPECT_TRUE(Object::makeStringInputPort(ucs4string(UC(""))).toTextualInputPort()->getDatum(isErrorOccured).isEof());
    EXPECT_FALSE(isErrorOccured);

    const Object obj = Object::makeStringInputPort(ucs4string(UC("3"))).toTextualInputPort()->getDatum(isErrorOccured);
    EXPECT_TRUE(obj.isFixnum());
    EXPECT_EQ(3, obj.toFixnum());
    EXPECT_FALSE(isErrorOccured);
}

TEST_F(PortTest, StandardPort) {
    const Object in = standardInputPortEx(theVM_, 0, NULL);
    EXPECT_TRUE(in.isBinaryInputPort());

    const Object out = standardOutputPortEx(theVM_, 0, NULL);
    EXPECT_TRUE(out.isBinaryOutputPort());

    const Object err = standardErrorPortEx(theVM_, 0, NULL);
    EXPECT_TRUE(err.isBinaryOutputPort());
}

TEST_F(PortTest, FileBinary) {
    FileBinaryOutputPort* out = new FileBinaryOutputPort(ucs4string(UC("/tmp/hoge.log")));
    out->putU8(1);
    out->putU8(0);
    out->close();

    FileBinaryInputPort* in = new FileBinaryInputPort(ucs4string(UC("/tmp/hoge.log")));
    EXPECT_EQ(1, in->lookaheadU8());
    EXPECT_EQ(1, in->getU8());
    EXPECT_EQ(0, in->lookaheadU8());
    EXPECT_EQ(0, in->getU8());
    EXPECT_EQ(EOF, in->lookaheadU8());
    EXPECT_EQ(EOF, in->getU8());
    in->close();
}

TEST_F(PortTest, BufferedFileBinary) {
    BlockBufferedFileBinaryOutputPort* out = new BlockBufferedFileBinaryOutputPort(ucs4string(UC("/tmp/hoge.log")));
    out->putU8(1);
    out->putU8(0);
    out->close();

    BufferedFileBinaryInputPort* in = new BufferedFileBinaryInputPort(ucs4string(UC("/tmp/hoge.log")));
    EXPECT_EQ(1, in->lookaheadU8());
    EXPECT_EQ(1, in->getU8());
    EXPECT_EQ(0, in->lookaheadU8());
    EXPECT_EQ(0, in->getU8());
    EXPECT_EQ(EOF, in->lookaheadU8());
    EXPECT_EQ(EOF, in->getU8());
    in->close();
}

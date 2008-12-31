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

using namespace scheme;

void signal_handler(int signo) {}

class PortTest : public testing::Test {
protected:
    virtual void SetUp() {
        mosh_init();
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
    EXPECT_TRUE(Object::makeStringInputPort(UC("")).toTextualInputPort()->getDatum(isErrorOccured).isEof());
    EXPECT_FALSE(isErrorOccured);

    const Object obj = Object::makeStringInputPort(UC("3")).toTextualInputPort()->getDatum(isErrorOccured);
    EXPECT_TRUE(obj.isFixnum());
    EXPECT_EQ(3, obj.toFixnum());
    EXPECT_FALSE(isErrorOccured);
}


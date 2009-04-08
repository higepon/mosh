/*
 * OSCompatTest - Test for OS compatibility functions
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
 *  $Id: TestVector.cpp 183 2008-07-04 06:19:28Z higepon $
 */
#include <gtest/gtest.h>

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "OSCompat.h"
#include "PortProcedures.h"

using namespace scheme;

class MoshTest : public testing::Test {
protected:
    virtual void SetUp() {
        mosh_init();
    }
};

TEST_F(MoshTest, getEnv) {
    EXPECT_EQ(NULL, getEnv(UC("MOSH_NOT_EXIST_ENV")));
    EXPECT_TRUE(NULL != getEnv(UC("PATH")));
}

TEST_F(MoshTest, getEnvAlist) {
    ASSERT_TRUE(getEnvAlist().isList());
}

TEST_F(MoshTest, stringError) {
    ASSERT_TRUE(stringError(1).size() > 0);
}

TEST_F(MoshTest, FileAccess) {
    EXPECT_TRUE(fileExistsP(UC("lib")));
    EXPECT_TRUE(fileWritableP(UC("lib")));
    EXPECT_TRUE(fileReadableP(UC("lib")));
}

TEST_F(MoshTest, utf32toUtf8) {
    ByteVector* bv = utf32toUtf8(UC("abc"));
    ASSERT_EQ(4, bv->length());
    EXPECT_EQ('a', bv->u8Ref(0));
    EXPECT_EQ('b', bv->u8Ref(1));
    EXPECT_EQ('c', bv->u8Ref(2));
    EXPECT_EQ('\0', bv->u8Ref(3));
}

TEST_F(MoshTest, readDirectory) {
    const Object directories = readDirectory(UC("."));
    ASSERT_TRUE(directories.isList());
}


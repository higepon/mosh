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
#include "OSCompat.h"
#include "ByteVector.h"
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

TEST_F(MoshTest, FileAccess) {
    EXPECT_TRUE(File::isExist(UC("lib")));
    EXPECT_TRUE(File::isWritable(UC("lib")));
    EXPECT_TRUE(File::isReadable(UC("lib")));
}

TEST_F(MoshTest, utf32toUtf8) {
    EXPECT_STREQ("abc", utf32toUtf8(UC("abc")));
}

TEST_F(MoshTest, readDirectory) {
    const Object directories = readDirectory(UC("."));
    ASSERT_TRUE(directories.isList());
}

TEST_F(MoshTest, createDirectory) {
    const ucs4char* dir = UC("OSCompatTestDir");
    ucs4string subdir = dir;
    subdir += UC("/hige");
    ASSERT_TRUE(createDirectory(dir));
    ASSERT_TRUE(createDirectory(subdir));
    const Object directories = readDirectory(dir);
    ASSERT_TRUE(directories.isList());
    EXPECT_EQ(3, Pair::length(directories));
    ASSERT_TRUE(File::deleteFileOrDirectory(subdir));
    ASSERT_TRUE(File::deleteFileOrDirectory(dir));
    EXPECT_FALSE(File::isExist(dir));
    EXPECT_FALSE(File::isExist(subdir));
}

TEST_F(MoshTest, renameFile) {
    const ucs4char* dir = UC("OSCompatTestDir");
    const ucs4char* dir2 = UC("OSCompatTestDir.bak");
    ASSERT_TRUE(createDirectory(dir));
    EXPECT_TRUE(File::isExist(dir));
    ASSERT_TRUE(File::rename(dir, dir2));
    EXPECT_FALSE(File::isExist(dir));
    EXPECT_TRUE(File::isExist(dir2));
    ASSERT_TRUE(File::deleteFileOrDirectory(dir2));
    EXPECT_FALSE(File::isExist(dir2));
}

TEST_F(MoshTest, isDirectory) {
    const ucs4char* dir = UC("OSCompatTestDir");
    ASSERT_TRUE(createDirectory(dir));
    EXPECT_TRUE(isDirectory(dir));
    ASSERT_TRUE(File::deleteFileOrDirectory(dir));
}

TEST_F(MoshTest, isDirectory2) {
    EXPECT_FALSE(isDirectory(UC("main.cpp")));
}

TEST_F(MoshTest, SymbolicLink) {
    const ucs4char* file = UC("hige");
    ASSERT_TRUE(File::createSymbolicLink(UC("."), file));
    EXPECT_FALSE(File::isRegular(file));
    EXPECT_TRUE(File::isSymbolicLink(file));
    ASSERT_TRUE(File::deleteFileOrDirectory(file));
}

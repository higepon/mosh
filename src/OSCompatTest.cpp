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
#include "VM.h"

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

class MoshTest : public testing::Test {
protected:
    virtual void SetUp() {
        mosh_init();
    }
};

TEST_F(MoshTest, getEnv) {
    EXPECT_EQ(NULL, getEnv(ucs4string(UC("MOSH_NOT_EXIST_ENV"))));
    EXPECT_TRUE(NULL != getEnv(ucs4string(UC("PATH"))));
}

TEST_F(MoshTest, getEnvAlist) {
    ASSERT_TRUE(getEnvAlist().isList());
}

TEST_F(MoshTest, FileAccess) {
    EXPECT_TRUE(File::isExist(ucs4string(UC("lib"))));
    EXPECT_TRUE(File::isWritable(ucs4string(UC("lib"))));
    EXPECT_TRUE(File::isReadable(ucs4string(UC("lib"))));
}

TEST_F(MoshTest, utf32toUtf8) {
    EXPECT_STREQ("abc", utf32toUtf8(ucs4string(UC("abc"))));
}

TEST_F(MoshTest, readDirectory) {
    const Object directories = readDirectory(ucs4string(UC(".")));
    ASSERT_TRUE(directories.isList());
}

TEST_F(MoshTest, createDirectory) {
    ucs4string dir(UC("OSCompatTestDir"));
    ucs4string subdir = dir;
    subdir += ucs4string(UC("/hige"));
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
    ucs4string dir(UC("OSCompatTestDir"));
    ucs4string dir2(UC("OSCompatTestDir.bak"));
    ASSERT_TRUE(createDirectory(dir));
    EXPECT_TRUE(File::isExist(dir));
    ASSERT_TRUE(File::rename(dir, dir2));
    EXPECT_FALSE(File::isExist(dir));
    EXPECT_TRUE(File::isExist(dir2));
    ASSERT_TRUE(File::deleteFileOrDirectory(dir2));
    EXPECT_FALSE(File::isExist(dir2));
}

TEST_F(MoshTest, isDirectory) {
    ucs4string dir(UC("OSCompatTestDir"));
    ASSERT_TRUE(createDirectory(dir));
    EXPECT_TRUE(isDirectory(dir));
    ASSERT_TRUE(File::deleteFileOrDirectory(dir));
}

TEST_F(MoshTest, isDirectory2) {
    EXPECT_FALSE(isDirectory(ucs4string(UC("main.cpp"))));
}

TEST_F(MoshTest, SymbolicLink) {
    ucs4string file(UC("hige"));
    ASSERT_TRUE(File::createSymbolicLink(ucs4string(UC(".")), file));
    EXPECT_FALSE(File::isRegular(file));
    EXPECT_TRUE(File::isSymbolicLink(file));
    ASSERT_TRUE(File::deleteFileOrDirectory(file));
}


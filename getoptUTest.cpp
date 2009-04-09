/*
 * getoptUTest.cpp - 
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
 *  $Id: getoptUTest.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <gtest/gtest.h>

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "getoptU.h"

using namespace scheme;

class MoshTest : public testing::Test {
protected:
    virtual void SetUp() {
        mosh_init();
        optindU = 1;
        opterrU = 1;
    }
};

TEST_F(MoshTest, getopt_longU) {
    struct optionU long_options[] = {
       {UC("help"), 0, 0, 'h'},
       {0, 0, 0, 0}
    };

    const int argc = 2;
    ucs4char* argv[] = {(ucs4char*)UC("mosh"), (ucs4char*)UC("-h") };
    int optionIndex = 0;
    ASSERT_EQ('h', getopt_longU(argc, argv, UC("h"), long_options, &optionIndex));
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("h"), long_options, &optionIndex));
}

TEST_F(MoshTest, getopt_longU_2) {
    struct optionU long_options[] = {
       {UC("help"), 0, 0, 'h'},
       {0, 0, 0, 0}
    };

    const int argc = 2;
    ucs4char* argv[] = {(ucs4char*)UC("mosh"), (ucs4char*)UC("-t") };
    int optionIndex = 0;
    ASSERT_EQ('t', getopt_longU(argc, argv, UC("htvpVcl:5rze"), long_options, &optionIndex));
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("htvpVcl:5rze"), long_options, &optionIndex));
}

TEST_F(MoshTest, getopt_longU_3) {
    struct optionU long_options[] = {
       {UC("help"), 0, 0, 'h'},
       {0, 0, 0, 0}
    };

    const int argc = 2;
    ucs4char* argv[] = {(ucs4char*)UC("mosh"), (ucs4char*)UC("-あ") };
    int optionIndex = 0;
    ASSERT_EQ(0x3042, getopt_longU(argc, argv, UC("hあvpVcl:5rze"), long_options, &optionIndex));
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:5rze"), long_options, &optionIndex));
}

TEST_F(MoshTest, getopt_longU_4) {
    struct optionU long_options[] = {
        {UC("loadpath"), optional_argument, 0, 'L'},
        {UC("help"), 0, 0, 'h'},
        {0, 0, 0, 0}
    };

    ucs4string argv1 = UC("mosh");
    ucs4string argv2 = UC("--loadpath=my-library");

    const int argc = 2;
    ucs4char* argv[] = { argv1.strdup(), argv2.strdup() };
    int optionIndex = 0;
    ASSERT_EQ('L', getopt_longU(argc, argv, UC("hあvpVcl:5rze"), long_options, &optionIndex));
    ucs4string path = optargU;
    EXPECT_TRUE(path == UC("my-library"));
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:5rze"), long_options, &optionIndex));
}

TEST_F(MoshTest, getopt_longU_5) {
    struct optionU long_options[] = {
        {UC("loadpath"), optional_argument, 0, 'L'},
        {UC("help"), 0, 0, 'h'},
        {0, 0, 0, 0}
    };

    const int argc = 3;
    ucs4string argv1 = UC("mosh");
    ucs4string argv2 = UC("-L");
    ucs4string argv3 = UC("my-library");
    ucs4char* argv[] = { argv1.strdup(), argv2.strdup(), argv3.strdup() };
    int optionIndex = 0;
    ASSERT_EQ('L', getopt_longU(argc, argv, UC("hあvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU != NULL);
    ucs4string path = optargU;
    EXPECT_TRUE(path == argv3);
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:5L:rze"), long_options, &optionIndex));
}

TEST_F(MoshTest, getopt_longU_6) {
    struct optionU long_options[] = {
        {UC("loadpath"), optional_argument, 0, 'L'},
        {UC("help"), 0, 0, 'h'},
        {0, 0, 0, 0}
    };

    const int argc = 2;
    ucs4char* argv[] = {(ucs4char*)UC("mosh"), (ucs4char*)UC("--loadpath=my-library") };
    int optionIndex = 0;
    ASSERT_EQ('L', getopt_longU(argc, argv, UC("hあvpVcl:L:5rze"), long_options, &optionIndex));
    ucs4string path = optargU;
    EXPECT_TRUE(path == UC("my-library"));
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:L:5rze"), long_options, &optionIndex));
}

TEST_F(MoshTest, getopt_longU_7) {
    struct optionU long_options[] = {
        {UC("loadpath"), optional_argument, 0, 'L'},
        {UC("help"), 0, 0, 'h'},
        {0, 0, 0, 0}
    };

    const int argc = 3;
    ucs4string argv1 = UC("mosh");
    ucs4string argv2 = UC("-p");
    ucs4string argv3 = UC("test.scm");
    ucs4char* argv[] = { argv1.strdup(), argv2.strdup(), argv3.strdup() };
    int optionIndex = 0;
    ASSERT_EQ('p', getopt_longU(argc, argv, UC("hあvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ucs4string script = argv[optindU];
    EXPECT_TRUE(script == argv3);
}

TEST_F(MoshTest, getopt_longU_8) {
    struct optionU long_options[] = {
        {UC("loadpath"), optional_argument, 0, 'L'},
        {UC("help"), 0, 0, 'h'},
        {0, 0, 0, 0}
    };

    const int argc = 3;
    ucs4string argv1 = UC("mosh");
    ucs4string argv2 = UC("-p");
    ucs4string argv3 = UC("あいう.scm");
    ucs4char* argv[] = { argv1.strdup(), argv2.strdup(), argv3.strdup() };
    int optionIndex = 0;
    ASSERT_EQ('p', getopt_longU(argc, argv, UC("hあvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ucs4string script = argv[optindU];
    EXPECT_TRUE(script == argv3);
}

TEST_F(MoshTest, getopt_longU_9) {
    struct optionU long_options[] = {
        {UC("loadpath"), optional_argument, 0, 'L'},
        {UC("help"), 0, 0, 'h'},
        {0, 0, 0, 0}
    };

    const int argc = 3;
    ucs4string argv1 = UC("mosh");
    ucs4string argv2 = UC("-あ");
    ucs4string argv3 = UC("あいう.scm");
    ucs4char* argv[] = { argv1.strdup(), argv2.strdup(), argv3.strdup() };
    int optionIndex = 0;
    ASSERT_EQ(0x3042, getopt_longU(argc, argv, UC("hあvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ucs4string script = argv[optindU];
    EXPECT_TRUE(script == argv3);
}

TEST_F(MoshTest, getopt_longU_10) {
    struct optionU long_options[] = {
        {UC("loadpath"), optional_argument, 0, 'L'},
        {UC("help"), 0, 0, 'h'},
        {0, 0, 0, 0}
    };

    const int argc = 3;
    ucs4string argv1 = UC("mosh");
    ucs4string argv2 = UC("-5");
    ucs4string argv3 = UC("あいう.scm");
    ucs4char* argv[] = { argv1.strdup(), argv2.strdup(), argv3.strdup() };
    int optionIndex = 0;
    ASSERT_EQ('5', getopt_longU(argc, argv, UC("hあvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ASSERT_EQ(-1, getopt_longU(argc, argv, UC("hあtvpVcl:5L:rze"), long_options, &optionIndex));
    ASSERT_TRUE(optargU == NULL);
    ucs4string script = argv[optindU];
    EXPECT_TRUE(script == argv3);
}

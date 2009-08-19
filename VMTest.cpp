/*
 * TestVM.cpp - Test for VM
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
#include "Vector.h"
#include "SString.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "OSCompat.h"
#include "OSCompatThread.h"
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
#include "StandardInputPort.h"
#include "StandardOutputPort.h"
#include "BlockBufferedFileBinaryOutputPort.h"
#include "BlockBufferedFileBinaryInputOutputPort.h"
#include "MultiVMProcedures.h"

using namespace scheme;

class VMTest : public testing::Test {
protected:
    TestingVM* theVM_;
    virtual void SetUp() {
        mosh_init();
        Transcoder* transcoder = nativeTranscoder();
        Object inPort    = Object::makeTextualInputPort(new StandardInputPort(), transcoder);
        Object outPort   = Object::makeTextualOutputPort(new StandardOutputPort(), transcoder);
        Object errorPort = Object::makeTextualOutputPort(new FileBinaryOutputPort(UC("/dev/null")), transcoder);
        theVM_ = new TestingVM(10000, outPort, errorPort, inPort, false /* isProfiler */);
        setCurrentVM(theVM_);
        theVM_->loadCompiler();
        theVM_->setValueString(UC("%loadpath"), Object::False);
        theVM_->setValueString(UC("%disable-acc"), Object::makeBool(true));
        theVM_->setValueString(UC("%clean-acc"), Object::makeBool(false));

    }
};

class VMErrorPortTest : public testing::Test {
protected:
    VM* theVM_;
    virtual void SetUp() {
        mosh_init();
        Transcoder* transcoder = nativeTranscoder();
        Object inPort    = Object::makeTextualInputPort(new StandardInputPort(), transcoder);
        Object outPort   = Object::makeTextualOutputPort(new StandardOutputPort(), transcoder);
        errorPort_ = Object::makeStringOutputPort();
        theVM_ = new TestingVM(10000, outPort, errorPort_, inPort, false /* isProfiler */);
        setCurrentVM(theVM_);
        theVM_->loadCompiler();
        theVM_->setValueString(UC("%loadpath"), Object::False);
    }
    Object errorPort_;
};


TEST_F(VMTest, StackTrace1) {
    theVM_->loadFileWithGuard(UC("./test/stack-trace1.scm"));
    EXPECT_STREQ("    error in raise: unhandled exception has occurred\n"
                 "\n"
                 " Condition components:\n"
                 "    1. &assertion\n"
                 "    2. &who: display\n"
                 "    3. &message: \"textual-output-port required, but got 3\"\n"
                 "    4. &irritants: ()\n"
                 "\n"
                 "\n"
                 " Stack trace:\n"
                 "    1. throw: <subr>\n"
                 "    2. sys-display: <subr>\n"
                 "    3. (a):  <transcoded-textual-input-port <binary-input-port ./test/stack-trace1.scm>>:7\n"
                 "    4. (b):  <transcoded-textual-input-port <binary-input-port ./test/stack-trace1.scm>>:12\n\n",
                 theVM_->getLastError().toString()->data().ascii_c_str());
}

TEST_F(VMTest, StackTrace2) {
    theVM_->setValueString(UC("*command-line-args*"), Pair::list1("./test/stack-trace2.scm"));
    theVM_->activateR6RSMode(false);
    EXPECT_STREQ("     error in raise: returned from non-continuable exception\n"
                 "\n"
                 " Stack trace:\n"
                 "    1. throw: <subr>\n"
                 "    2. (raise c):  compiler-with-library.scm:941\n"
                 "    3. sys-display: <subr>\n"
                 "    4. (a): <unknown location>\n"
                 "    5. (b): <unknown location>\n"
                 "    6. eval: <subr>\n"
                 "    7. (dynamic-wind in body out):  compiler-with-library.scm:776\n"
                 "    8. (dynamic-wind in body out):  compiler-with-library.scm:776\n"
                 "    9. (dynamic-wind in body out):  compiler-with-library.scm:776\n"
                 "    10. (<top-level>): <unknown location>\n\n",
                 theVM_->getLastError().toString()->data().ascii_c_str());
}


TEST_F(VMTest, BinaryOutputPortFlush) {
    Object outPort = Object::makeBinaryOutputPort(new BlockBufferedFileBinaryOutputPort(UC("/tmp/binary-output.txt")));
    theVM_->registerPort(outPort);
    theVM_->flushAllPorts();
    EXPECT_EQ(theVM_->lastFlushType_, TestingVM::FLUSH_BINARY_OUTPUT_PORT);
}


TEST_F(VMTest, BinaryInputOutputPortFlush) {
    Object outPort = Object::makeBinaryInputOutputPort(new BlockBufferedFileBinaryInputOutputPort(UC("/tmp/binary-input-output.txt"), 0));
    theVM_->registerPort(outPort);
    theVM_->flushAllPorts();
    EXPECT_EQ(theVM_->lastFlushType_, TestingVM::FLUSH_BINARY_INPUT_OUTPUT_PORT);
}


TEST_F(VMTest, TextualOutputPortFlush) {
    Transcoder* transcoder = nativeTranscoder();
    Object outPort = Object::makeTextualOutputPort(new BlockBufferedFileBinaryOutputPort(UC("/tmp/textual-output.txt")), transcoder);
    theVM_->registerPort(outPort);
    theVM_->flushAllPorts();
    EXPECT_EQ(theVM_->lastFlushType_, TestingVM::FLUSH_TEXTUAL_OUTPUT_PORT);
}


TEST_F(VMTest, TextualInputOutputPortFlush) {
    Transcoder* transcoder = nativeTranscoder();
    Object outPort = Object::makeTextualInputOutputPort(new BlockBufferedFileBinaryInputOutputPort(UC("/tmp/textual-input-output.txt"), 0), transcoder);
    theVM_->registerPort(outPort);
    theVM_->flushAllPorts();
    EXPECT_EQ(theVM_->lastFlushType_, TestingVM::FLUSH_TEXTUAL_INPUT_OUTPUT_PORT);
}


// todo
// TEST_F(VMErrorPortTest, StackTrace3) {
//     theVM_->setValueString(UC("*command-line-args*"), Pair::list1("./test/stack-trace3.scm"));
//     theVM_->activateR6RSMode(false);
//     EXPECT_STREQ(" Condition components:\n"
//                  "   1. &who: let\n"
//                  "   2. &message: \"invalid syntax\"\n"
//                  "   3. &syntax:\n"
//                  "       form: (let a 3 3)\n"
//                  "       subform: #f\n"
//                  "   4. &source-information:\n"
//                  "       file-name: \"./test/stack-trace3.scm\"\n"
//                  "       character: (5)\n"
//                  "\n"
//                  " Exception:\n"
//                  "     error in raise: returned from non-continuable exception\n"
//                  "\n"
//                  " Stack trace:\n"
//                  "    1. throw: <subr>\n"
//                  "    2. (raise c):  compiler-with-library.scm:889\n"
//                  "    3. apply: <subr>\n"
//                  "\n"
//                  "\n"
//                  , getOutputStringEx(theVM_, 1, &errorPort_).toString()->data().ascii_c_str());
// }

// TODO
// TEST_F(VMErrorPortTest, CompatPrefix) {
//     setenv("MOSH_LOADPATH", "./test", 1);
//     theVM_->setValueString(UC("*command-line-args*"), Pair::list1("./test/use-foo.scm"));
//     theVM_->activateR6RSMode(false);
//     EXPECT_STREQ("compat-mosh", getOutputStringEx(theVM_, 1, &errorPort_).toString()->data().ascii_c_str());
// }

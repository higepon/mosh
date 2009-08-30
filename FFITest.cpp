/*
 * FFITest.cpp
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

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
#include "FFI.h"
#include "FFIProcedures.h"
#include "ByteVector.h"
#include "StandardOutputPort.h"
#include "StandardInputPort.h"
#include "ExecutableMemory.h"

using namespace scheme;

#if defined(ARCH_IA32) || defined(ARCH_X86_64)

class FFITest : public testing::Test {
protected:
    virtual void SetUp() {
        mosh_init();
    }
};

class VMErrorPortTest : public testing::Test {
protected:
    VM* theVM_;
    virtual void SetUp() {
        mosh_init();
        Transcoder* transcoder = nativeTranscoder();
        const Object inPort    = Object::makeTextualInputPort(new StandardInputPort(), transcoder);
        const Object outPort   = Object::makeTextualOutputPort(new StandardOutputPort(), transcoder);
        errorPort_ = Object::makeStringOutputPort();
        theVM_ = new TestingVM(10000, outPort, errorPort_, inPort, false /* isProfiler */);
        theVM_->loadCompiler();
        theVM_->setValueString(UC("%loadpath"), Object::False);
    }
    Object errorPort_;
};

TEST_F(FFITest, loadAndlookup) {

    void* handle = FFI::open("./libffitest.so.1.0");
    ASSERT_TRUE(handle != NULL);
    int (*sub)(int, int) = (int (*)(int, int))FFI::lookup(handle, "sub");
    ASSERT_TRUE(sub != NULL);
    EXPECT_EQ(5, sub(7, 2));
    const int ret = FFI::close(handle);
    EXPECT_EQ(0, ret);
}

TEST_F(VMErrorPortTest, loadAndlookupScheme) {

    // open
    const Object name = "./libffitest.so.1.0";
    const Object handle = internalFfiOpenEx(theVM_, 1, &name);
    ASSERT_TRUE(handle.isPointer());

    // lookup
    Object args[2];
    args[0] = handle;
    args[1] = Symbol::intern(UC("sub"));
    const Object sym = internalFfiLookupEx(theVM_, 2, args);
    ASSERT_TRUE(sym.isPointer());

    // lookup
    args[0] = handle;
    args[1] = Symbol::intern(UC("sub-not-found"));
    const Object notFound = internalFfiLookupEx(theVM_, 2, args);
    ASSERT_TRUE(notFound.isFalse());
}


#ifdef ARCH_IA32

TEST_F(FFITest, CStackWithFlonum) {
    CStack cstack;
    EXPECT_TRUE(cstack.push(Object::makeFlonum(3.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(2.0)));
    double* p = (double*)cstack.frame();
    EXPECT_EQ(4, cstack.count());
    EXPECT_DOUBLE_EQ(3.0, p[0]);
    EXPECT_DOUBLE_EQ(2.0, p[1]);
}

TEST_F(FFITest, CStackWithFixnum) {
    CStack cstack;
    EXPECT_TRUE(cstack.push(Object::makeFixnum(3)));
    EXPECT_TRUE(cstack.push(Object::makeFixnum(4)));
    EXPECT_TRUE(cstack.push(Object::makeFixnum(-5)));
    EXPECT_TRUE(cstack.push(Bignum::makeIntegerFromU32(0xffffffff)));
    intptr_t* p = cstack.frame();
    EXPECT_EQ(4, cstack.count());
    EXPECT_EQ(3, p[0]);
    EXPECT_EQ(4, p[1]);
    EXPECT_EQ(-5, p[2]);
    const uint32_t x = 0xffffffff;
    const uint32_t y = p[3];
    EXPECT_EQ(x, y);
}

TEST_F(FFITest, CStackWithString) {
    CStack cstack;
    cstack.push(Object::makeFixnum(3));
    cstack.push("hige");
    intptr_t* p = cstack.frame();
    EXPECT_EQ(2, cstack.count());
    EXPECT_EQ(3, p[0]);
    EXPECT_STREQ("hige", (char*)p[1]);
}

TEST_F(FFITest, CStackWithByteVector) {
    CStack cstack;
    Object b = Object::makeByteVector(2);
    ByteVector* const bv = b.toByteVector();
    bv->u8Set(0, 1);
    bv->u8Set(1, 2);
    cstack.push(b);
    cstack.push("hige");
    intptr_t* p = cstack.frame();
    EXPECT_EQ(2, cstack.count());
    EXPECT_EQ(1, ((uint8_t*)(p[0]))[0]);
    EXPECT_EQ(2, ((uint8_t*)(p[0]))[1]);
    EXPECT_STREQ("hige", (char*)p[1]);
}

TEST_F(FFITest, CStackTooManyArgument) {
    // we assume this
    ASSERT_TRUE(sizeof(uint64_t) >= sizeof(intptr_t));

    CStack cstack;
    for (int i = 0; i < CStack::MAX_ARGC; i++) {
        EXPECT_TRUE(cstack.push(Object::makeFixnum(3)));
    }
    const bool result = cstack.push(Object::makeFixnum(3));
    EXPECT_FALSE(result);
    ucs4string err = cstack.getLastError();
    EXPECT_STREQ("too many ffi arguments", err.ascii_c_str());
}

#elif ARCH_X86_64
TEST_F(FFITest, CStackWithFixnum) {
    CStack cstack;
    EXPECT_TRUE(cstack.push(Object::makeFixnum(3)));
    EXPECT_TRUE(cstack.push(Object::makeFixnum(4)));
    EXPECT_TRUE(cstack.push(Object::makeFixnum(-5)));
    EXPECT_TRUE(cstack.push(Bignum::makeIntegerFromU32(0xffffffff)));
    intptr_t* p = cstack.reg();
    EXPECT_EQ(4, cstack.regCount());
    EXPECT_EQ(3, p[0]);
    EXPECT_EQ(4, p[1]);
    EXPECT_EQ(-5, p[2]);
    const uint32_t x = 0xffffffff;
    const uint32_t y = p[3];
    EXPECT_EQ(x, y);
}

TEST_F(FFITest, CStackWithString) {
    CStack cstack;
    cstack.push(Object::makeFixnum(3));
    cstack.push("hige");
    intptr_t* p = cstack.reg();
    EXPECT_EQ(2, cstack.regCount());
    EXPECT_EQ(3, p[0]);
    EXPECT_STREQ("hige", (char*)p[1]);
}

TEST_F(FFITest, CStackWithByteVector) {
    CStack cstack;
    Object b = Object::makeByteVector(2);
    ByteVector* const bv = b.toByteVector();
    bv->u8Set(0, 1);
    bv->u8Set(1, 2);
    cstack.push(b);
    cstack.push("hige");
    intptr_t* p = cstack.reg();
    EXPECT_EQ(2, cstack.regCount());
    EXPECT_EQ(1, ((uint8_t*)(p[0]))[0]);
    EXPECT_EQ(2, ((uint8_t*)(p[0]))[1]);
    EXPECT_STREQ("hige", (char*)p[1]);
}

TEST_F(FFITest, CStackTooManyArgument) {
    // we assume this
    ASSERT_TRUE(sizeof(uint64_t) >= sizeof(intptr_t));

    CStack cstack;
    for (int i = 0; i < CStack::MAX_ARGC + CStack::MAX_REG; i++) {
        EXPECT_TRUE(cstack.push(Object::makeFixnum(3)));
    }
    const bool result = cstack.push(Object::makeFixnum(3));
    EXPECT_FALSE(result);
    ucs4string err = cstack.getLastError();
    EXPECT_STREQ("too many ffi arguments", err.ascii_c_str());
}

TEST_F(FFITest, CStackWithFlonum) {
    CStack cstack;
    // use sse registers
    EXPECT_TRUE(cstack.push(Object::makeFlonum(1.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(2.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(3.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(4.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(5.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(6.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(7.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(8.0)));

    // use stack
    EXPECT_TRUE(cstack.push(Object::makeFlonum(9.0)));
    EXPECT_TRUE(cstack.push(Object::makeFlonum(10.0)));

    EXPECT_EQ(2, cstack.count());
    double* p = (double*)cstack.frame();
    EXPECT_DOUBLE_EQ(9.0, p[0]);
    EXPECT_DOUBLE_EQ(10.0, p[1]);
}

#endif

TEST_F(FFITest, CStackUnsupportedArgument) {
    // we assume this
    ASSERT_TRUE(sizeof(uint64_t) >= sizeof(intptr_t));
    CStack cstack;
    const bool result = cstack.push(Object::makeEqHashTable());
    EXPECT_FALSE(result);
    ucs4string err = cstack.getLastError();
    EXPECT_STREQ("unsupported ffi argument", err.ascii_c_str());
}

TEST_F(FFITest, refChar) {
    char c = 'z';
    Pointer p(&c);
    EXPECT_EQ('z', p.ref<char>(0));
}

TEST_F(FFITest, refUint64_t) {
    uint64_t c = 0xffffffffeeeeeeeeLL;
    Pointer p(&c);
    EXPECT_EQ(0xffffffffeeeeeeeeLL, p.ref<uint64_t>(0));
}

#if defined(ARCH_IA32) || defined(ARCH_X86_64)
TEST_F(FFITest, ExecutableMemory) {
    ExecutableMemory mem(32);
    ASSERT_TRUE(mem.allocate());

    /*
    int return3()
    {
        return 3;
    }
    */
#ifdef ARCH_IA32
    ASSERT_TRUE(mem.push(0x55)); // push   %ebp
    ASSERT_TRUE(mem.push(0x89)); // mov    %esp,%ebp
    ASSERT_TRUE(mem.push(0xe5));
    ASSERT_TRUE(mem.push(0x83)); // sub    $0x8,%esp
    ASSERT_TRUE(mem.push(0xec));
    ASSERT_TRUE(mem.push(0x08));
    ASSERT_TRUE(mem.push(0xb8)); // mov    $0x3,%eax
    ASSERT_TRUE(mem.push(0x03));
    ASSERT_TRUE(mem.push(0x00));
    ASSERT_TRUE(mem.push(0x00));
    ASSERT_TRUE(mem.push(0x00));
    ASSERT_TRUE(mem.push(0xc9)); // leave
    ASSERT_TRUE(mem.push(0xc3)); // ret
#else
    ASSERT_TRUE(mem.push(0x55)); // push   %rbp
    ASSERT_TRUE(mem.push(0x48)); // mov    %rsp,%rbp
    ASSERT_TRUE(mem.push(0x89));
    ASSERT_TRUE(mem.push(0xe5));
    ASSERT_TRUE(mem.push(0xb8)); // mov    $0x3,%eax
    ASSERT_TRUE(mem.push(0x03));
    ASSERT_TRUE(mem.push(0x00));
    ASSERT_TRUE(mem.push(0x00));
    ASSERT_TRUE(mem.push(0x00));
    ASSERT_TRUE(mem.push(0xc9)); // leaveq
    ASSERT_TRUE(mem.push(0xc3)); // retq
#endif

    int (*return3) () = (int (*) ())mem.address();
    EXPECT_EQ(3, return3());
}

static int a = 0;

static void set_a_4()
{
    a = 4;
}

TEST_F(FFITest, CallFromExecutableMemory) {
    ExecutableMemory mem(64);
    ASSERT_TRUE(mem.allocate());
    EXPECT_EQ(0, a);
#ifdef ARCH_IA32
#else
    ASSERT_TRUE(mem.push(0x55)); // push   %rbp
    ASSERT_TRUE(mem.push(0x48)); // mov    %rsp,%rbp
    ASSERT_TRUE(mem.push(0x89));
    ASSERT_TRUE(mem.push(0xe5));
    ASSERT_TRUE(mem.push(0x48)); // mov    $imm64,%rax
    ASSERT_TRUE(mem.push(0xb8));

    // set pointer address of set_a_4
    intptr_t p = reinterpret_cast<intptr_t>(set_a_4);
    for (int i = 0; i < 8; i++) {
        ASSERT_TRUE(mem.push((p >> (i * 8)) & 0xff));
    }
    ASSERT_TRUE(mem.push(0xff)); // callq *%rax
    ASSERT_TRUE(mem.push(0xd0));
    ASSERT_TRUE(mem.push(0xc9)); // leaveq
    ASSERT_TRUE(mem.push(0xc3)); // retq
#endif
    void (*func) () = (void (*) ())mem.address();
    func();
    EXPECT_EQ(4, a);
}

static int64_t arg1 = 0;

void getArgumentsStub()
{
#ifdef ARCH_IA32
#else
    asm volatile("movq %%rdi, %0;" : "=m" (arg1) : :);
#endif
}

TEST_F(FFITest, getArguments) {
    void (*func)(int) = (void (*)(int))getArgumentsStub;
    func(1234);
    EXPECT_EQ(1234, arg1);
}

static int64_t args[8];

void getArgumentsStub2()
{
// this breaks stack
//     char buf[32];
//     sprintf(buf, "hige");
//     printf(buf);
#ifdef ARCH_IA32
#else
    //RDI, RSI, RDX, RCX, R8, R9 and in stack.
    asm volatile("movq %%rdi, %0;"
                 "movq %%rsi, %1;"
                 "movq %%rdx, %2;"
                 "movq %%rcx, %3;"
                 "movq %%r8,  %4;"
                 "movq %%r9,  %5;"
                 "movq 8(%%rsp), %6;"
                 "movq 16(%%rsp), %7;"
                 : "=m" (args[0]),
                   "=m" (args[1]),
                   "=m" (args[2]),
                   "=m" (args[3]),
                   "=m" (args[4]),
                   "=m" (args[5]),
                   "=r" (args[6]),
                   "=r" (args[7]): :);
#endif
}

TEST_F(FFITest, getArguments2) {
    void (*func)(int, int, int, int, int, int, int, int) = (void (*)(int, int, int, int, int, int, int, int))getArgumentsStub2;
    func(1, 2, 3, 4, 5, 6, 7, 8);
    EXPECT_EQ(1, args[0]);
    EXPECT_EQ(2, args[1]);
    EXPECT_EQ(3, args[2]);
    EXPECT_EQ(4, args[3]);
    EXPECT_EQ(5, args[4]);
    EXPECT_EQ(6, args[5]);
    EXPECT_EQ(7, args[6]);
    EXPECT_EQ(8, args[7]);

}


#endif
#endif

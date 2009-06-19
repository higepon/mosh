/*
 * OSCompatThreadTest - Test for OS compatibility thread functions
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
#include "OSCompatThread.h"

using namespace scheme;

class MoshTest : public testing::Test {
protected:
    virtual void SetUp() {
        mosh_init();
    }
};

static void* return1Func(void* param)
{
    static int ret = 1;
    return &ret;
}

static void* return2Func(void* param)
{
    static int ret = 2;
    return &ret;
}

static void* returnFixnum(void* param)
{
    static Object ret = Object::makeFixnum(*(int*)param + 1234);
    return &ret;
}


TEST_F(MoshTest, simple) {
    Thread thread;
    ASSERT_TRUE(thread.create(return1Func, NULL));
    int* ret;
    ASSERT_TRUE(thread.join((void**)&ret));
    EXPECT_EQ(1, *ret);
}

TEST_F(MoshTest, simpleObject) {
    Thread thread;
    int arg = 2;
    ASSERT_TRUE(thread.create(returnFixnum, &arg));
    Object* ret;
    ASSERT_TRUE(thread.join((void**)&ret));
    EXPECT_EQ(1236, (*ret).toFixnum());
}


TEST_F(MoshTest, twoThreads) {
    Thread thread1;
    ASSERT_TRUE(thread1.create(return1Func, NULL));
    int* ret;
    ASSERT_TRUE(thread1.join((void**)&ret));

    Thread thread2;
    ASSERT_TRUE(thread2.create(return2Func, NULL));
    ASSERT_TRUE(thread2.join((void**)&ret));
    EXPECT_EQ(2, *ret);
}

TEST_F(MoshTest, twoThreads2) {
    Thread thread1;
    ASSERT_TRUE(thread1.create(return1Func, NULL));
    int* ret;

    Thread thread2;
    ASSERT_TRUE(thread2.create(return2Func, NULL));
    ASSERT_TRUE(thread2.join((void**)&ret));
    EXPECT_EQ(2, *ret);
    ASSERT_TRUE(thread1.join((void**)&ret));
    EXPECT_EQ(1, *ret);
}

static void* checkSelf(void* param)
{
    while (Thread::self() == NULL) {
    }
    EXPECT_EQ((Thread*)param, Thread::self());
    return NULL;
}

TEST_F(MoshTest, self) {
    Thread thread;
    ASSERT_TRUE(thread.create(checkSelf, &thread));
    thread.join(NULL);
}

// static void* checkYield(void* param)
// {
//     Thread::yield();
//     return NULL;
// }

// TEST_F(MoshTest, yield) {
//     Thread thread;
//     ASSERT_TRUE(thread.create(checkYield, &thread));
//     thread.join(NULL);
// }

static void* checkExit(void* param)
{
    static int ret = -1;
    Thread::exit(&ret);
    return NULL;
}

TEST_F(MoshTest, exit) {
    Thread thread;
    ASSERT_TRUE(thread.create(checkExit, &thread));
    int* ret = 0;
    thread.join((void**)&ret);
    EXPECT_EQ(-1, *ret);
}

TEST_F(MoshTest, checkSpecific) {
    Thread thread;
    ThreadSpecificKey* key = new ThreadSpecificKey;
    int ret = 1234;
    EXPECT_TRUE(Thread::setSpecific(key, &ret));
    int* value = (int*)Thread::getSpecific(key);
    ASSERT_TRUE(value != NULL);
    EXPECT_EQ(1234, *value);
}

TEST_F(MoshTest, checkSpecific2) {
    Thread thread;
    ThreadSpecificKey* key1 = new ThreadSpecificKey;
    ThreadSpecificKey* key2 = new ThreadSpecificKey;
    int ret1 = 1234;
    int ret2 = 1235;
    EXPECT_TRUE(Thread::setSpecific(key1, &ret1));
    EXPECT_TRUE(Thread::setSpecific(key2, &ret2));
    int* value1 = (int*)Thread::getSpecific(key1);
    ASSERT_TRUE(value1 != NULL);
    EXPECT_EQ(1234, *value1);
    int* value2 = (int*)Thread::getSpecific(key2);
    ASSERT_TRUE(value2 != NULL);
    EXPECT_EQ(1235, *value2);
}

Mutex* mutex = NULL;

void* checkMutex(void* param)
{
    EXPECT_FALSE(mutex->tryLock());
    return NULL;
}

TEST_F(MoshTest, checkMutex) {
    Thread thread;
    mutex = new Mutex;
    mutex->lock();
    ASSERT_TRUE(thread.create(checkMutex, &thread));
    thread.join(NULL);
    mutex->unlock();
}

Mutex* mutex2 = NULL;

void* checkMutex2(void* param)
{
    EXPECT_TRUE(mutex->tryLock());
    return NULL;
}

TEST_F(MoshTest, checkMutex2) {
    Thread thread;
    mutex2 = new Mutex;
    ASSERT_TRUE(thread.create(checkMutex2, &thread));
    thread.join(NULL);
}

static int value = 0;
static bool valueOK = false;

static Mutex* mutex1;

void* checkConditionVariable(void* param)
{
    ConditionVariable* conditionVariable = (ConditionVariable*)param;
    value = 2;
    valueOK = true;
    mutex1->lock();
    EXPECT_TRUE(conditionVariable->notify());
    mutex1->unlock();
    return NULL;
}

TEST_F(MoshTest, conditionVariable) {
    ConditionVariable conditionVariable;
    Thread thread;
    mutex1 = new Mutex;
    ASSERT_TRUE(thread.create(checkConditionVariable, &conditionVariable));
    mutex1->lock();
    while (!valueOK) {
        ASSERT_TRUE(conditionVariable.wait(mutex1));
    }
    mutex1->unlock();
    EXPECT_EQ(2, value);
}

static int value2 = 0;
static bool valueOK2 = false;
static Mutex* mutex3;
void* checkConditionVariable2(void* param)
{
    ConditionVariable* conditionVariable = (ConditionVariable*)param;
    value2 = 2;
    valueOK2 = true;
    mutex3->lock();
    EXPECT_TRUE(conditionVariable->notifyAll());
    mutex3->unlock();
    return NULL;
}

TEST_F(MoshTest, conditon2) {
    ConditionVariable conditionVariable;
    Thread thread;
    mutex3 = new Mutex;
    ASSERT_TRUE(thread.create(checkConditionVariable2, &conditionVariable));
    mutex3->lock();
    while (!valueOK2) {
        ASSERT_TRUE(conditionVariable.wait(mutex3));
    }
    mutex3->unlock();
    EXPECT_EQ(2, value2);
}

TEST_F(MoshTest, ConditionVariableName) {
    ConditionVariable conditionVariable;
    EXPECT_TRUE(UC("#<condition-variable>") == conditionVariable.toString());
}

TEST_F(MoshTest, ConditionVariableName2) {
    ConditionVariable conditionVariable(UC("hige"));
    EXPECT_TRUE(UC("#<condition-variable hige>") == conditionVariable.toString());
}




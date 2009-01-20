/*
 * Object.cpp - Test for base Object System.
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
#include "Symbol.h"

using namespace scheme;

class ObjectTest : public testing::Test {
protected:
    virtual void SetUp() {
        mosh_init();
    }
};

TEST_F(ObjectTest, ucs4string) {
    ucs4string text = UC("hige");
    EXPECT_EQ('h', text[0]);
    EXPECT_EQ('i', text[1]);
    EXPECT_EQ('g', text[2]);
    EXPECT_EQ('e', text[3]);

    EXPECT_STREQ("hige", text.ascii_c_str());
    EXPECT_EQ(strlen("hige"), text.size());



}

TEST_F(ObjectTest, Nil) {
    const Object nil = Object::Nil;
    EXPECT_TRUE(nil.isNil());
    EXPECT_FALSE(nil.isPair());
    EXPECT_FALSE(nil.isFixnum());
    EXPECT_FALSE(nil.isFalse());
    EXPECT_FALSE(nil.isTrue());
    EXPECT_FALSE(nil.isEof());
    EXPECT_FALSE(nil.isUndef());
    EXPECT_FALSE(nil.isUnbound());
    EXPECT_FALSE(nil.isChar());
}

TEST_F(ObjectTest, Boolean) {
    const Object t = Object::True;
    const Object f = Object::False;
    EXPECT_TRUE(t.isTrue());
    EXPECT_TRUE(f.isFalse());
    EXPECT_FALSE(t.isPair());
    EXPECT_FALSE(t.isFixnum());
    EXPECT_FALSE(t.isFalse());
    EXPECT_FALSE(t.isEof());
    EXPECT_FALSE(t.isUndef());
    EXPECT_FALSE(t.isUnbound());
    EXPECT_FALSE(f.isPair());
    EXPECT_FALSE(f.isFixnum());
    EXPECT_FALSE(f.isTrue());
    EXPECT_FALSE(f.isEof());
    EXPECT_FALSE(f.isUndef());
    EXPECT_FALSE(f.isUnbound());
    EXPECT_FALSE(f.isChar());
}

TEST_F(ObjectTest, Pair) {
    const Object p1 = Object::cons(Object::True, Object::False);
    EXPECT_TRUE(p1.isPair());
    EXPECT_TRUE(p1.car().isTrue());
    EXPECT_TRUE(p1.cdr().isFalse());
    EXPECT_FALSE(p1.isFixnum());
    EXPECT_FALSE(p1.isFalse());
    EXPECT_FALSE(p1.isTrue());
    EXPECT_FALSE(p1.isEof());
    EXPECT_FALSE(p1.isUndef());
    EXPECT_FALSE(p1.isUnbound());
    EXPECT_FALSE(p1.isByteVector());

    const Object p2 = Object::cons(Object::makeByteVector(0), Object::False);
    EXPECT_TRUE(p2.isPair());
    EXPECT_TRUE(p2.car().isByteVector());
    EXPECT_TRUE(p2.cdr().isFalse());
    EXPECT_FALSE(p2.isFixnum());
    EXPECT_FALSE(p2.isFalse());
    EXPECT_FALSE(p2.isTrue());
    EXPECT_FALSE(p2.isEof());
    EXPECT_FALSE(p2.isUndef());
    EXPECT_FALSE(p2.isUnbound());
    EXPECT_FALSE(p2.isByteVector());

    const Object p3 = Object::cons(p1, Object::Nil);
    EXPECT_TRUE(p3.isPair());
    EXPECT_TRUE(p3.car().isPair());
    EXPECT_TRUE(p3.cdr().isNil());
    EXPECT_FALSE(p3.isFixnum());
    EXPECT_FALSE(p3.isFalse());
    EXPECT_FALSE(p3.isTrue());
    EXPECT_FALSE(p3.isEof());
    EXPECT_FALSE(p3.isUndef());
    EXPECT_FALSE(p3.isUnbound());
    EXPECT_FALSE(p3.isByteVector());
    EXPECT_FALSE(p3.isChar());
}

TEST_F(ObjectTest, Fixnums) {
    const Object one = Object::makeFixnum(1);
    EXPECT_TRUE(one.isFixnum());
    EXPECT_FALSE(one.isFalse());
    EXPECT_FALSE(one.isTrue());
    EXPECT_FALSE(one.isEof());
    EXPECT_FALSE(one.isUndef());
    EXPECT_FALSE(one.isUnbound());
    EXPECT_FALSE(one.isByteVector());
    EXPECT_FALSE(one.isChar());

    EXPECT_EQ(1, Object::makeFixnum(1).toFixnum());
    EXPECT_EQ(-1, Object::makeFixnum(-1).toFixnum());
    EXPECT_EQ(-536870912, Object::makeFixnum(-536870912).toFixnum());
    EXPECT_EQ(536870911, Object::makeFixnum(536870911).toFixnum());
}

TEST_F(ObjectTest, Char) {
    const Object z = Object::makeChar('Z');
    EXPECT_TRUE(z.isChar());
    EXPECT_FALSE(z.isFalse());
    EXPECT_FALSE(z.isTrue());
    EXPECT_FALSE(z.isEof());
    EXPECT_FALSE(z.isUndef());
    EXPECT_FALSE(z.isUnbound());
    EXPECT_FALSE(z.isByteVector());
    EXPECT_EQ('Z', z.toChar());
}


TEST_F(ObjectTest, Vector) {
    const Object v = Object::makeVector(3, Object::Undef);
    EXPECT_TRUE(v.isVector());
    EXPECT_FALSE(v.isFalse());
    EXPECT_FALSE(v.isTrue());
    EXPECT_FALSE(v.isEof());
    EXPECT_FALSE(v.isUndef());
    EXPECT_FALSE(v.isUnbound());
    EXPECT_FALSE(v.isByteVector());

    EXPECT_TRUE(v.toVector()->ref(0).isUndef());
    EXPECT_TRUE(v.toVector()->ref(1).isUndef());
    EXPECT_TRUE(v.toVector()->ref(2).isUndef());
}


TEST_F(ObjectTest, Symbol) {
    const Object hoge = Symbol::intern(UC("hoge"));
    const Object hige = Symbol::intern(UC("hige"));
    const Object hige2 = Symbol::intern(UC("hige"));
    EXPECT_TRUE(hoge.isSymbol());
    EXPECT_TRUE(hige.isSymbol());
    EXPECT_TRUE(hige2.isSymbol());

    ucs4string text = hoge.toSymbol()->c_str();
    EXPECT_STREQ("hoge", text.ascii_c_str());

    EXPECT_FALSE(hoge.isFalse());
    EXPECT_FALSE(hoge.isTrue());
    EXPECT_FALSE(hoge.isEof());
    EXPECT_FALSE(hoge.isUndef());
    EXPECT_FALSE(hoge.isUnbound());
    EXPECT_FALSE(hoge.isByteVector());

    EXPECT_TRUE(hige == hige2);
    EXPECT_TRUE(hoge != hige2);
    EXPECT_TRUE(hoge != hige);
}

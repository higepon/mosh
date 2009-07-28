/*
 * FaslTest.cpp -
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
 *  $Id: FaslTest.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <gtest/gtest.h>
#include <stdio.h>
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
#include "FileBinaryInputPort.h"
#include "FileBinaryOutputPort.h"
#include "Bignum.h"
#include "Ratnum.h"
#include "Record.h"
#include "Flonum.h"
#include "Symbol.h"
#include "TestingVM.h"
#include "StringTextualOutputPort.h"
#include "PortProcedures.h"
#include "EqHashTable.h"
#include "Closure.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "RecordProcedures.h"
#include "StringProcedures.h"
#include "SimpleStruct.h"
#include "Fasl.h"
#include "ErrorProcedures.h"
#include "Record.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Equivalent.h"
#include "BufferedFileBinaryInputPort.h"
#include "BlockBufferedFileBinaryOutputPort.h"
#include "StandardInputPort.h"
#include "StandardOutputPort.h"


using namespace scheme;

class FaslTest : public testing::Test {
protected:
    VM* theVM_;
    virtual void SetUp() {
        mosh_init();
        Transcoder* transcoder = nativeTranscoder();
        Object inPort    = Object::makeTextualInputPort(new StandardInputPort(), transcoder);
        Object outPort   = Object::makeTextualOutputPort(new StandardOutputPort(), transcoder);
        Object errorPort = Object::makeTextualOutputPort(new BlockBufferedFileBinaryOutputPort(UC("/dev/null")), transcoder);
        theVM_ = new TestingVM(10000, outPort, errorPort, inPort, false /* isProfiler */);
        theVM_->loadCompiler();
    }

    virtual void Store(Object obj)
    {
        BinaryOutputPort* const out = new BlockBufferedFileBinaryOutputPort(UC("/tmp/fasl-test.tmp"));
        FaslWriter writer(out);
        writer.put(obj);
        out->close();
    }

    virtual Object Restore()
    {
        BinaryInputPort* const in = new BufferedFileBinaryInputPort(UC("/tmp/fasl-test.tmp"));
        FaslReader reader(theVM_, in);
        return reader.get();
    }

    virtual Object StoreAndRestore(Object obj)
    {
        Store(obj);
        return Restore();
    }
};

TEST_F(FaslTest, Fixnum) {
    const Object restored = StoreAndRestore(Object::makeFixnum(123456));
    ASSERT_TRUE(restored.isFixnum());
    EXPECT_EQ(123456, restored.toFixnum());
}

TEST_F(FaslTest, Symbol) {
    const Object restored = StoreAndRestore(Symbol::intern(UC("hige")));
    ASSERT_TRUE(restored.isSymbol());
    EXPECT_TRUE(restored == Symbol::intern(UC("hige")));
    ucs4string text = restored.toSymbol()->c_str();
    EXPECT_STREQ("hige", text.ascii_c_str());
}


// make EqHashTable and write/read with Fasl
TEST_F(FaslTest, EqHashTable) {
    const Object table = Object::makeEqHashTable();
    EqHashTable* ht = table.toEqHashTable();
    ht->set(Symbol::intern(UC("hage")), Pair::list2(Object::makeFixnum(1), Symbol::intern(UC("hige"))));
    ht->set(Symbol::intern(UC("mobe")), Pair::list2(Object::makeFixnum(2), Symbol::intern(UC("hage"))));

    // Write
    BinaryOutputPort* const out = new BlockBufferedFileBinaryOutputPort(UC("/tmp/fasl-test4.dat"));
    FaslWriter writer(out);
    writer.put(table);
    out->close();

    // Read
    BinaryInputPort* const in = new FileBinaryInputPort(UC("/tmp/fasl-test4.dat"));
    FaslReader reader(theVM_, in);
    const Object restored = reader.get();
    ASSERT_TRUE(restored.isEqHashTable());
    EqHashTable* const ht2 = restored.toEqHashTable();

    Vector* const keys = ht->keys().toVector();
    for (int i = 0; i < keys->length(); i++) {
        const Object key = keys->ref(i);
        const Object val1 = ht->ref(key, Object::False);
        const Object val2 = ht2->ref(key, Object::False);
        EXPECT_FALSE(val1.isFalse());
        EXPECT_FALSE(val2.isFalse());
        EXPECT_TRUE(eqv(val1, val1));
    }
}


// make RecordTypeDescriptor and write/read with Fasl
TEST_F(FaslTest, RecordTypeDescriptor) {
    const Object fields = Object::makeVector(2);
    Vector* const vfields = fields.toVector();
    vfields->set(0, Pair::list2(Symbol::intern(UC("mutable")), Symbol::intern(UC("x"))));
    vfields->set(1, Pair::list2(Symbol::intern(UC("mutable")), Symbol::intern(UC("y"))));
    const Object args1[] = {
        Symbol::intern(UC("point")),
        Object::False,
        Object::False,
        Object::False,
        Object::False,
        fields};

    const Object rtd = makeRecordTypeDescriptorEx(theVM_, sizeof(args1) / sizeof(Object), args1);
    EXPECT_TRUE(rtd.isRecordTypeDescriptor());
    // Write
    BinaryOutputPort* const out = new BlockBufferedFileBinaryOutputPort(UC("/tmp/fasl-test0.dat"));
    FaslWriter writer(out);
    writer.put(rtd);
    out->close();

    // Read
    BinaryInputPort* const in = new FileBinaryInputPort(UC("/tmp/fasl-test0.dat"));
    FaslReader reader(theVM_, in);
    const Object restored = reader.get();
    EXPECT_TRUE(rtd.eq(restored));
}

// make Point Record and write/read with Fasl.
TEST_F(FaslTest, Record) {
    const Object fields = Object::makeVector(2);
    Vector* const vfields = fields.toVector();
    vfields->set(0, Pair::list2(Symbol::intern(UC("mutable")), Symbol::intern(UC("x"))));
    vfields->set(1, Pair::list2(Symbol::intern(UC("mutable")), Symbol::intern(UC("y"))));
    const Object args1[] = {
        Symbol::intern(UC("point")),
        Object::False,
        Object::False,
        Object::False,
        Object::False,
        fields};

    const Object rtd = makeRecordTypeDescriptorEx(theVM_, sizeof(args1) / sizeof(Object), args1);
    EXPECT_TRUE(rtd.isRecordTypeDescriptor());
    const Object args2[] = { rtd, Object::False, Object::False };
    const Object rcd = makeRecordConstructorDescriptorEx(theVM_, sizeof(args2)/ sizeof(Object), args2);
    EXPECT_TRUE(rcd.isRecordConstructorDescriptor());
    const Object makePoint = recordConstructorEx(theVM_, 1, &rcd);
    EXPECT_TRUE(makePoint.isCallable());

    // Record
    const Object point = theVM_->callClosure2(makePoint, Object::makeFixnum(1), Object::makeFixnum(2));
    EXPECT_TRUE(point.isRecord());

    // Predicaete
    const Object predicate = recordPredicateEx(theVM_, 1, &rtd);
    EXPECT_TRUE(predicate.isCallable());
    EXPECT_TRUE(theVM_->callClosure1(predicate, point).isTrue());

    // Accessor
    Object args4[] = { rtd, Object::makeFixnum(0) };
    const Object pointX = recordAccessorEx(theVM_, sizeof(args4) / sizeof(Object), args4);
    Object args5[] = { rtd, Object::makeFixnum(1) };
    const Object pointY = recordAccessorEx(theVM_, sizeof(args5) / sizeof(Object), args5);
    EXPECT_EQ(1, theVM_->callClosure1(pointX, point).toFixnum());
    EXPECT_EQ(2, theVM_->callClosure1(pointY, point).toFixnum());

    // Write
    BinaryOutputPort* const out = new BlockBufferedFileBinaryOutputPort(UC("/tmp/fasl-test1.dat"));
    FaslWriter writer(out);
    writer.put(point);
    out->close();

    // Read
    BinaryInputPort* const in = new FileBinaryInputPort(UC("/tmp/fasl-test1.dat"));
    FaslReader reader(theVM_, in);
    const Object restored = reader.get();
    EXPECT_TRUE(eqv(point, restored));
    EXPECT_TRUE(theVM_->callClosure1(predicate, point).isTrue());
    EXPECT_EQ(1, theVM_->callClosure1(pointX, point).toFixnum());
    EXPECT_EQ(2, theVM_->callClosure1(pointY, point).toFixnum());
}

// make Point Record and write/read with Fasl.
TEST_F(FaslTest, RecordWithPair) {
    const Object fields = Object::makeVector(2);
    Vector* const vfields = fields.toVector();
    vfields->set(0, Pair::list2(Symbol::intern(UC("mutable")), Symbol::intern(UC("x"))));
    vfields->set(1, Pair::list2(Symbol::intern(UC("mutable")), Symbol::intern(UC("y"))));
    const Object args1[] = {
        Symbol::intern(UC("point")),
        Object::False,
        Object::False,
        Object::False,
        Object::False,
        fields};

    const Object rtd = makeRecordTypeDescriptorEx(theVM_, sizeof(args1) / sizeof(Object), args1);
    EXPECT_TRUE(rtd.isRecordTypeDescriptor());
    const Object args2[] = { rtd, Object::False, Object::False };
    const Object rcd = makeRecordConstructorDescriptorEx(theVM_, sizeof(args2)/ sizeof(Object), args2);
    EXPECT_TRUE(rcd.isRecordConstructorDescriptor());
    const Object makePoint = recordConstructorEx(theVM_, 1, &rcd);
    EXPECT_TRUE(makePoint.isCallable());

    // Record
    const Object point = theVM_->callClosure2(makePoint, Pair::list2(Symbol::intern(UC("hage")), Object::makeFixnum(2)), Object::makeString("hoge"));
    EXPECT_TRUE(point.isRecord());

    // Predicaete
    const Object predicate = recordPredicateEx(theVM_, 1, &rtd);
    EXPECT_TRUE(predicate.isCallable());
    EXPECT_TRUE(theVM_->callClosure1(predicate, point).isTrue());

    // Accessor
    Object args4[] = { rtd, Object::makeFixnum(0) };
    const Object pointX = recordAccessorEx(theVM_, sizeof(args4) / sizeof(Object), args4);
    Object args5[] = { rtd, Object::makeFixnum(1) };
    const Object pointY = recordAccessorEx(theVM_, sizeof(args5) / sizeof(Object), args5);
    EXPECT_TRUE(theVM_->callClosure1(pointX, point).isPair());
    EXPECT_TRUE(theVM_->callClosure1(pointY, point).isString());

    // Write
    BinaryOutputPort* const out = new BlockBufferedFileBinaryOutputPort(UC("/tmp/fasl-test2.dat"));
    FaslWriter writer(out);
    writer.put(point);
    out->close();

    // Read
    BinaryInputPort* const in = new FileBinaryInputPort(UC("/tmp/fasl-test2.dat"));
    FaslReader reader(theVM_, in);
    const Object restored = reader.get();
    EXPECT_TRUE(eqv(point, restored));
    EXPECT_TRUE(theVM_->callClosure1(predicate, point).isTrue());
    const Object p = theVM_->callClosure1(pointX, point);
    ASSERT_TRUE(p.isPair());
    EXPECT_TRUE(p.car().eq(Symbol::intern(UC("hage"))));
    ASSERT_TRUE(theVM_->callClosure1(pointY, point).isString());
}

TEST_F(FaslTest, SimpleStruct) {
    Object st = Object::makeSimpleStruct(Symbol::intern(UC("struct1")), 1);
    SimpleStruct* pst = st.toSimpleStruct();
    pst->set(0, Object::makeFixnum(1234));
    const Object restored = StoreAndRestore(st);
    ASSERT_TRUE(restored.isSimpleStruct());
    ASSERT_TRUE(restored.toSimpleStruct()->name() == Symbol::intern(UC("struct1")));
    EXPECT_TRUE(restored.toSimpleStruct()->ref(0) == Object::makeFixnum(1234));
}

TEST_F(FaslTest, SerializeBignum1) {
    const Object num = Arithmetic::expt(Object::makeFixnum(10), Object::makeFixnum(10));
    ASSERT_TRUE(num.isBignum());
    size_t size = 0;
    uint8_t* data = num.toBignum()->serialize(&size);
    const Object restored = Bignum::deserialize(data, size);
    EXPECT_TRUE(eqv(num, restored));
}

TEST_F(FaslTest, SerializeBignum2) {
    const Object num = Arithmetic::expt(Object::makeFixnum(-10), Object::makeFixnum(10));
    ASSERT_TRUE(num.isBignum());
    size_t size = 0;
    uint8_t* data = num.toBignum()->serialize(&size);
    const Object restored = Bignum::deserialize(data, size);
    EXPECT_TRUE(eqv(num, restored));
}

TEST_F(FaslTest, Bignum) {
    const Object num = Arithmetic::expt(Object::makeFixnum(-10), Object::makeFixnum(100));
    const Object restored = StoreAndRestore(num);
    ASSERT_TRUE(restored.isBignum());
    EXPECT_TRUE(eqv(restored, num));
}

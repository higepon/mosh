/*
 * Object-inl.h - inline functions for Object.
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
 *  $Id: Object-inl.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_OBJECT_INL__
#define __SCHEME_OBJECT_INL__

namespace scheme {

inline Object::Object()
{
}

inline Object::Object(const Object& o)
{
    val = o.val;
}

// not virtual
inline Object::~Object()
{
}

inline bool Object::isFixnum() const
{
    return tag() == 1;
}

inline bool Object::isBoolean() const
{
    return isFalse() || isTrue();
}

inline bool Object::isInstruction() const
{
    return (static_cast<word>(val) & 31) == 14;
}

inline bool Object::isCompilerInstruction() const
{
    return (static_cast<word>(val) & 31) == 30;
}


inline bool Object::isChar() const
{
    return (static_cast<word>(val) & 0x07L) == 2;
}

inline ucs4char Object::toChar() const
{
    MOSH_ASSERT(isChar());
    return static_cast<ucs4char>(val) >> 3;
}

inline signed long int Object::toFixnum() const
{
    MOSH_ASSERT(isFixnum());
    return static_cast<signed long int>(val) >> 2;
}

inline int Object::toInstruction() const
{
    MOSH_ASSERT(isInstruction());
    return static_cast<int>(val) >> 5;
}

inline int Object::toCompilerInstruction() const
{
    MOSH_ASSERT(isCompilerInstruction());
    return static_cast<int>(val) >> 5;
}

inline bool Object::isNil() const
{
    return *this == MAKE_CONST(CONST_NIL);
}

inline bool Object::isEof() const
{
    return *this == MAKE_CONST(CONST_EOF);
}

inline bool Object::isUndef() const
{
    return *this == MAKE_CONST(CONST_UNDEF);
}

inline bool Object::isUnbound() const
{
    return *this == MAKE_CONST(CONST_UNBOUND);
}

inline bool Object::isTrue() const
{
    return *this == MAKE_CONST(CONST_TRUE);
}

inline bool Object::isFalse() const
{
    return *this == MAKE_CONST(CONST_FALSE);
}

inline bool Object::isProcedure() const
{
    return isClosure() || isCProcedure() || isRegexp() || isRegMatch() || isCallable();
}

inline bool Object::isHashTable() const
{
    return isEqHashTable() || isGenericHashTable() || isEqvHashTable();
}

inline bool Object::operator==(Object o) const
{
    return val == o.val;
}

inline bool Object::operator!=(Object o) const
{
    return val != o.val;
}

inline Object Object::operator=(const Object& o)
{
    val = o.val;
    return *this;
}

inline HashTable* Object::toHashTable() const
{
    return reinterpret_cast<HashTable*>(reinterpret_cast<HeapObject*>(val)->obj);
}

inline Object* Object::toObjectPointer() const
{
#ifdef DEBUG_VERSION
    MOSH_ASSERT(isObjectPointer());
    return reinterpret_cast<Object*>(reinterpret_cast<HeapObject*>(val)->obj);
#else
    return reinterpret_cast<Object*>(val);
#endif
}

inline bool Object::isObjectPointer() const
{
#ifdef DEBUG_VERSION
    return isHeapObject()
        && reinterpret_cast<HeapObject*>(val)->type == HeapObject::ObjectPointer;
#else
    return false;
#endif
}

inline bool Object::isPointer() const
{
    return (val & 0x03) == 0;
}

inline bool Object::eq(Object o) const
{
    return operator==(o);
}

inline Object Object::makeFixnum(signed long int n)
{
    return Object((n << 2) + 1);
}

inline Object Object::makeRaw(int n)
{
    return Object(n);
}

inline Object Object::makeRaw(void* n)
{
    return Object(reinterpret_cast<word>(n));
}

inline Object Object::makeInstruction(int n)
{
    return Object((n << 5) + 14);
}

inline Object Object::makeCompilerInstruction(int n)
{
    return Object((n << 5) + 30);
}

inline Object Object::makeChar(ucs4char ch)
{
    return Object((ch << 3) + 2);
}

inline Object Object::makeConst(int n)
{
    return Object(MAKE_CONST(n));
}

inline Object Object::makeVector(int n, Object o)
{
    return Object(n, o);
}

inline Object Object::makeBool(bool a)
{
    return a ? Object::True : Object::False;
}

inline bool Object::isHeapObject() const
{
    return isPointer() && ((reinterpret_cast<HeapObject*>(val)->type & 0x03) == 0x03);
}

inline Pair* Object::toPair() const
{
    return reinterpret_cast<Pair*>(val);
}

inline Object Object::makeObjectPointer(Object* p)
{
#ifdef DEBUG_VERSION
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ObjectPointer,
                                                        reinterpret_cast<word>(p))));
#else
    return Object(reinterpret_cast<word>(p));
#endif
}

inline bool Object::isNumber() const
{
    return isReal() || isCompnum();
}

inline bool Object::isComplex() const
{
    return isReal() || isCompnum();
}

inline bool Object::isExactInteger() const
{
    return isFixnum() || isBignum();
}

inline bool Object::isInputPort() const
{
    return isTextualInputPort() || isBinaryInputPort() || isBinaryInputOutputPort() || isTextualInputOutputPort();
}

inline bool Object::isOutputPort() const
{
    return isTextualOutputPort() || isBinaryOutputPort() || isBinaryInputOutputPort();
}

inline bool Object::isBinaryPort() const
{
    return isBinaryInputPort() || isBinaryOutputPort();
}

inline bool Object::isTextualPort() const
{
    return isTextualInputPort() || isTextualOutputPort() || isTextualInputOutputPort();
}

inline bool Object::isPort() const
{
    return isInputPort() || isOutputPort();
}

inline Object* Object::makeObjectArray(int size)
{
#ifdef USE_BOEHM_GC
    return new(GC) Object[size];
#else
    return new Object[size];
#endif
}

// private
inline uint8_t Object::tag() const
{
    return (static_cast<word>(val)) & 0x03;
}

}; // namespace scheme

#endif // __SCHEME_OBJECT_INL__

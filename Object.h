/*
 * Object.h -
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
 *  $Id$
 */

#ifndef SCHEME_OBJECT_
#define SCHEME_OBJECT_

#include <setjmp.h>
#include "HeapObject.h"
#include "oniguruma.h"

namespace scheme {

struct Pair;
class Vector;
class String;
class Closure;
class Stack;
class HashTable;
class EqHashTable;
class EqvHashTable;
class GenericHashTable;
class CProcedure;
class Symbol;
class Box;
class ByteVector;
class TextualInputPort;
class TextualOutputPort;
class Regexp;
class RegMatch;
class OutputPort;
class BinaryInputPort;
class BinaryOutputPort;
class Transcoder;
class Codec;
class CodeBuilder;
class Callable;
class Record;
class RecordTypeDescriptor;
class RecordConstructorDescriptor;
class CompoundCondition;
class Ratnum;
class Flonum;
class Bignum;
class Compnum;
class VM;
class Port;
class Gloc;
class BinaryInputOutputPort;
class TextualInputOutputPort;

enum {
    CONST_NIL     = 0,
    CONST_EOF     = 1,
    CONST_UNDEF   = 2,
    CONST_UNBOUND = 3,
    CONST_TRUE    = 4,
    CONST_FALSE   = 5,
    CONST_IGNORE  = 6
};

enum EolStyle
{
    LF = 0x0a,
    CR = 0x0d,
    NEL = 0x85,
    LS = 0x2028,
    CRNEL,
    CRLF,
    E_NONE
};

enum ErrorHandlingMode
{
    IGNORE_ERROR,
    RAISE_ERROR,
    REPLACE_ERROR
};



#define MAKE_CONST(n) ((n << 4) + 6)

class Object
{
public:
    Object()
    {
    }
    ~Object()
    {
    }
    Object(const Object& o)
    {
        val = o.val;
    }
    Object(const ucs4char* str);
    Object(const ucs4string& str);
    Object(const char* str);

    bool isInputPort() const;
    bool isOutputPort() const;
    bool isBinaryPort() const;
    bool isTextualPort() const;
    bool isPort() const;
    bool isComplex() const;
    bool isReal() const;
    bool isRational() const;
    bool isFixnum() const;
    bool isOctet() const;
    bool isNumber() const;
    bool isInteger() const;
    bool isIntegerValued() const;
    bool isExactInteger() const;
    bool isBoolean() const;
    bool isInstruction() const;
    bool isCompilerInstruction() const;
    bool isChar() const;
    bool isNil() const;
    bool isEof() const;
    bool isUndef() const;
    bool isUnbound() const;
    bool isTrue() const;
    bool isFalse() const;
    bool isProcedure() const;
    bool isHashTable() const;
    bool isPair() const;
    bool isList() const;
    bool operator==(Object o) const;
    bool operator!=(Object o) const;
    bool isObjectPointer() const;
    bool isPointer() const;
    bool isHeapObject() const;

    ucs4char toChar() const;
    fixedint toFixnum() const;
    int toInstruction() const;
    int toCompilerInstruction() const;
    Pair* toPair() const;
    Object* toObjectPointer() const;
    Object operator=(const Object& o)
    {
        val = o.val;
        return *this;
    }
    HashTable* toHashTable() const;
    Object& car() const;
    Object& cdr() const;
    Object& sourceInfo() const;
    Object& first() const;
    Object& second() const;
    Object& third() const;
    Object& fourth() const;
    Object& fifth() const;

    bool eq(Object o) const;
    bool equal(VM* theVM, Object o) const;
    bool eqv(VM* theVM, Object o) const;

    static Object makeGloc(Object value);
    static Object* makeObjectArray(int size);
    static Object makeFixnum(fixedint n);
    static Object makeBignum(signed long int n);
    static Object makeBignum(Bignum* b);
    static Object makeBignum(const mpz_t v);
    static Object makeFlonum(double value);
    static Object makeRaw(int n);
    static Object makeRaw(void* n);
    static Object makeInstruction(int n);
    static Object makeCompilerInstruction(int n);
    static Object makeChar(ucs4char ch);
    static Object makeConst(int n);
    static Object cons(Object car, Object cdr, Object sourceInfo = Object::False);
    static Object makeVector(int n, Object o = Object::Undef);
    static Object makeVector(int n, Object* objects);
    static Object makeVector(Object pair);
    static Object makeString(const ucs4char* str);
    static Object makeString(const ucs4string& str);
    static Object makeString(const char* str);
    static Object makeObjectPointer(Object* p);
    static Object makeBinaryInputPort(int fd);
    static Object makeBinaryOutputPort(int fd);
    static Object makeBinaryInputOutputPort(BinaryInputOutputPort* port);
    static Object makeBinaryInputPort(BinaryInputPort* port);
    static Object makeBinaryOutputPort(BinaryOutputPort* port);
    static Object makeTextualInputFilePort(const ucs4char* str);
    static Object makeTextualInputFilePort(const char* str);
    static Object makeTextualInputPort(BinaryInputPort* port, Transcoder* coder);
    static Object makeTextualInputPort(TextualInputPort* port);
    static Object makeTextualInputOutputPort(BinaryInputOutputPort* port, Transcoder* coder);
    static Object makeTextualOutputPort(BinaryOutputPort* port, Transcoder* coder);
    static Object makeStringInputPort(const ucs4string& str);
    static Object makeStringInputPort(const uint8_t* buf, int size);
    static Object makeStringOutputPort();
    static Object makeCustomTextualInputPort(VM* theVM,
                                             const ucs4string& id,
                                             Object readProc,
                                             Object getPositionProc,
                                             Object setPositionProc,
                                             Object closeProc);
    static Object makeCustomTextualOutputPort(VM* theVM,
                                              const ucs4string& id,
                                              Object writeProc,
                                              Object getPositionProc,
                                              Object setPositionProc,
                                              Object closeProc);
    static Object makeCustomTextualInputOutputPort(VM* theVM,
                                                   const ucs4string& id,
                                                   Object readProc,
                                                   Object writeProc,
                                                   Object getPositionProc,
                                                   Object setPositionProc,
                                                   Object closeProc);

    static Object makeCustomBinaryInputPort(VM* theVM, const ucs4string& id, Object readProc, Object getPositionProc, Object setPositionProc, Object closeProc);
    static Object makeCustomBinaryInputOutputPort(VM* theVM, const ucs4string& id, Object readProc, Object writeProc, Object getPositionProc, Object setPositionProc, Object closeProc);
    static Object makeCustomBinaryOutputPort(VM* theVM, const ucs4string& id, Object writeDProc, Object getPositionProc, Object setPositionDProc, Object closeProc);
    static Object makeString(int n, ucs4char c = ' ');
    static Object makeClosure(Object* pc,
                              int size,
                              int argLength,
                              bool isOptionalArg,
                              const Object* freeVars,
                              int freeVariablesNum,
                              int maxStack,
                              Object sourceInfo);
    static Object makeClosure(const Closure* closure);
    static Object makeSymbol(const ucs4char* str);
    static Object makeInputFilePort(const ucs4char* str);
    static Object makeStack(Object* src, int size);
    static Object makeEqHashTable();
    static Object makeEqvHashTable(VM* theVM);
    static Object makeCProcedure(Object (*proc)(VM*, int, const Object*));
    static Object makeBox(Object o);
    static Object makeByteVector(int n);
    static Object makeByteVector(int n, int8_t v);
    static Object makeByteVector(ByteVector* b);
    static Object makeByteVector(Object pair);
    static Object makeByteVector(const gc_vector<uint8_t>& v);
    static Object makeBool(bool a);
    static Object makeRegexp(const ucs4string& pattern, bool caseFold = false);
    static Object makeRegMatch(OnigRegion* region, const ucs4string& text);
    static Object makeCodec(Codec* codec);
    static Object makeTranscoder(Transcoder* transcoder);
    static Object makeTranscoder(Codec* codec);
    static Object makeTranscoder(Codec* codec, enum EolStyle eolStyle);
    static Object makeTranscoder(Codec* codec, enum EolStyle eolStyle, enum ErrorHandlingMode errorHandlingMode);
    static Object makeCodeBuilder();
    static Object makeGenericHashTable(VM* theVM, Object hashFunction, Object equivalenceFunction);
    static Object makeCallable(Callable* callable);
    static Object makeRecord(Object rtd, const Object* fields, int fieldsLength);
    static Object makeRatnum(int numerator, int denominator);
    static Object makeRatnum(mpq_t r);
    static Object makeRatnum(Ratnum* r);
    static Object makeCompnum(Object real, Object imag);
    static Object makeRecordTypeDescriptor(Object name,
                                           Object parent,
                                           Object uid,
                                           Object isSealed,
                                           Object isOpaque,
                                           Object fields);

    static Object makeRecordConstructorDescriptor(VM* theVM,
                                                  Object rtd,
                                                  Object parentRcd,
                                                  Object protocol);

    static Object makeCompoundCondition(int conditionCounts, const Object* conditions);
    static Object makeCompoundCondition(Object conditions);
#include "Object-accessors.h"
    static const Object Nil;
    static const Object Eof;
    static const Object Undef;
    static const Object UnBound;
    static const Object True;
    static const Object False;
    static const Object Ignore;

private:
    Object(intptr_t n) : val(n) {}
    Object(int n, Object o); // for vector
    uint8_t tag() const;

public:
    intptr_t val; // public for performancs. val() const { return val; } is not be inlined with -O2

    friend bool operator <(const Object& o1, const Object& o2)
    {
            return o1.val < o2.val;
    }
};

} // namespace scheme

#endif // SCHEME_OBJECT_

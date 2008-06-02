/*
 * scheme.h - Scheme system header
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
 *  $Id: scheme.h 5323 2008-05-09 09:06:26Z higepon $
 */

#ifndef __SCHEME_SCHEME_H__
#define __SCHEME_SCHEME_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <iostream>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <map>
#include <vector>
#include "dirent.h"
#include <setjmp.h>
#include <sys/time.h>
#include <stdio.h>
#include <signal.h>

//#define DUMP_ALL_INSTRUCTIONS
//#define TRACE_INSN
#define INSN_LOG_FILE "/tmp/mosh-insn.log"

#ifdef USE_BOEHM_GC
#define EXTEND_GC : public gc
#include <gc_cpp.h>
#include <gc_allocator.h>
template <class T1, class T2>
class gc_map : public std::map<T1, T2, std::less<T1>, gc_allocator<std::pair<const T1, T2> > >, public gc { };
template <class T1>
class gc_vector : public std::vector<T1, gc_allocator<T1> >, public gc { };
#else
#define EXTEND_GC
template <class T1, class T2>
class gc_map : public std::map<T1, T2> {};

template <class T1>
class gc_vector : public std::vector<T1> {};
#endif

#define MAKE_BOOL(a) ((a) ? Object::True : Object::False)
#define RETURN_BOOL(a) return ((a) ? Object::True : Object::False)
#define SCHEME_ASSERT(condition) { if (!(condition)) { fprintf(stderr, "ASSERT failure %s:%d: %s\n", __FILE__, __LINE__, #condition); }}
//#define RAISE_ERROR(fmt, ...) //{ format(Object::True, UC(fmt), __VA_ARGS__); exit(-1); }
#define UC(a) (reinterpret_cast<const ucs4char*>(L##""a))

typedef intptr_t word;
typedef word ucs4char;

#ifdef __GNUC__
#define ALWAYS_INLINE  __attribute__((always_inline))
#define USE_DIRECT_THREADED_CODE
#else
#define ALWAYS_INLINE
#endif


#include "ucs4string.h"
#include "oniguruma.h"

#define PRFILER_TEMP_FILE "/tmp/mosh-profiler.log"

namespace scheme {

class HeapObject EXTEND_GC
{
    template <int N>
    class Type
    {
    public:
        static const int VALUE = ((N << 2) | 0x03);
    };

public:
    HeapObject(word type, word obj) : type(type), obj(obj) {}
    ~HeapObject() {} // not virtual
    word type;
    word obj;
    enum {
        Vector            = Type<0>::VALUE,
        String            = Type<1>::VALUE,
        Symbol            = Type<2>::VALUE,
        InputFilePort     = Type<3>::VALUE,
        Closure           = Type<4>::VALUE,
        Stack             = Type<5>::VALUE,
        EqHashTable       = Type<6>::VALUE,
        CProcedure        = Type<7>::VALUE,
        Box               = Type<8>::VALUE,
        ByteVector        = Type<9>::VALUE,
        TextualInputPort  = Type<10>::VALUE,
        Regexp            = Type<11>::VALUE,
        RegMatch          = Type<12>::VALUE,
        Values            = Type<13>::VALUE,
        TextualOutputPort = Type<14>::VALUE,
        BinaryInputPort   = Type<15>::VALUE,
        BinaryOutputPort  = Type<16>::VALUE,
        Codec             = Type<17>::VALUE,
        Transcoder        = Type<18>::VALUE,
        TypedVectorDesc   = Type<20>::VALUE,
        TypedVector       = Type<21>::VALUE,
        forbidden_comma
    };
};

struct Pair;
class Vector;
class String;
class InputFilePort;
class Closure;
class Stack;
class EqHashTable;
class CProcedure;
class Symbol;
class Box;
class ByteVector;
class TextualInputPort;
class TextualOutputPort;
class Regexp;
class RegMatch;
class Values;
class BinaryInputPort;
class BinaryOutputPort;
class Transcoder;
class Codec;
class TypedVectorDesc;
class TypedVector;

class Object
{
public:
    Object() {}
    ~Object() {} // not virtual

    Object(const Object& o)
    {
        val = o.val;
    }

    Object(const ucs4char* str);
    Object(const char* str);

    bool isInt() const
    {
        return tag() == 1;
    }

    bool isInstruction() const
    {
        return (static_cast<word>(val) & 31) == 14;
    }

    bool isCompilerInstruction() const
    {
        return (static_cast<word>(val) & 31) == 30;
    }


    bool isChar() const
    {
        return (static_cast<word>(val) & 0x07L) == 2;
    }

    ucs4char toChar() const
    {
        return static_cast<ucs4char>(val) >> 3;
    }

    signed long int toInt() const
    {
        return static_cast<signed long int>(val) >> 2;
    }

    int toInstruction() const
    {
        return static_cast<int>(val) >> 5;
    }

    int toCompilerInstruction() const
    {
        return static_cast<int>(val) >> 5;
    }

    bool isNil() const
    {
        return Nil == *this;
    }

    bool isEof() const
    {
        return Eof == *this;
    }

    bool isUndef() const
    {
        return Undef == *this;
    }

    bool isUnbound() const
    {
        return Undef == *this;
    }

    bool isTrue() const
    {
        return True == *this;
    }

    bool isFalse() const
    {
        return False == *this;
    }

    // Pair
    Pair* toPair() const;
    bool isPair() const;

    bool operator==(Object o) const
    {
        return val == o.val;
    }

    bool operator!=(Object o) const // 引数は参照の方良い？
    {
        return val != o.val;
    }

    Object operator=(const Object& o)
    {
        val = o.val;
        return *this;
    }

//    word val() const { return val; }

    Object& car() const;
    Object& cdr() const;
    Object& sourceInfo() const;
    Object& first() const;
    Object& second() const;
    Object& third() const;
    Object& fourth() const;
    Object& fifth() const;

    Object eq(Object o) const { RETURN_BOOL(operator==(o)); }
    Object equal(Object o) const;
    Object eqv(Object o) const;

public:

    static Object makeInt(signed long int n)
    {
        return Object((n << 2) + 1);
    }

    static Object makeRaw(int n)
    {
        return Object(n);
    }

    static Object makeRaw(void* n)
    {
        return Object(reinterpret_cast<word>(n));
    }


    static Object makeInstruction(int n)
    {
        return Object((n << 5) + 14);
    }

    static Object makeCompilerInstruction(int n)
    {
        return Object((n << 5) + 30);
    }

    static Object makeChar(ucs4char ch)
    {
        return Object((ch << 3) + 2);
    }

    static Object makeConst(int n)
    {
        return Object((n << 4) + 6);
    }

    static Object cons(Object car, Object cdr, Object sourceInfo = Object::False);

    static Object makeVector(int n, Object o = Object::Undef)
    {
        return Object(n, o);
    }

    static Object makeVector(int n, Object* objects);

    static Object makeVector(Object pair);

    static Object makeString(const ucs4char* str)
    {
        return Object(str);
    }

    static Object makeString(const ucs4string& str)
    {
        return Object(str.c_str());
    }


    static Object makeString(const char* str)
    {
        return Object(str);
    }

    static Object makeObjectPointer(Object* p)
    {
        return Object((word)p);
    }

    static Object makeBinaryInputPort(FILE* file);
    static Object makeTextualInputFilePort(const ucs4char* str);
    static Object makeTextualInputFilePort(const char* str);
    static Object makeTextualInputPort(BinaryInputPort* port, Transcoder* coder);
    static Object makeTextualOutputPort(BinaryOutputPort* port, Transcoder* coder);
    static Object makeStringInputPort(const ucs4string& str);
    static Object makeStringInputPort(const uint8_t* buf, int size);
    static Object makeStringOutputPort();
    static Object makeTextualByteVectorOuputPort(Transcoder* transcoder);

    static Object makeCustomBinaryInputPort(Object readProc);

    static Object makeString(int n, ucs4char c = ' ');

    static Object makeClosure(Object* pc, int argLength, bool isOptionalArg, const Object* freeVars, int freeVariablesNum, int maxStack, Object sourceInfo);
    static Object makeSymbol(const ucs4char* str);
    static Object makeInputFilePort(const ucs4char* str);
    static Object makeStack(Object* src, int size);
    static Object makeEqHashTable();
    static Object makeCProcedure(Object (*proc)(Object));
    static Object makeBox(Object o);
    static Object makeByteVector(int n, int8_t v = 0);
    static Object makeByteVector(ByteVector* b);
    static Object makeByteVector(const gc_vector<uint8_t>& v);
    static Object makeBool(bool a)
    {
        return a ? Object::True : Object::False;
    }
    static Object makeRegexp(const ucs4string& pattern, bool caseFold = false);
    static Object makeRegMatch(OnigRegion* region, const ucs4string& text);
    static Object makeValues(Object values);
    static Object makeUTF8Codec();
    static Object makeTranscoder(Codec* codec);
    static Object makeTypedVectorDesc(Object name, Object supertype, Object data, Object fieldMutability);
    static Object makeTypedVector(Object desc, Object fieldsList);

#define DECL_TO(type)                                                           \
type* to##type() const                                                  \
{                                                                               \
    return reinterpret_cast<type*>(reinterpret_cast<HeapObject*>(val)->obj);   \
}


#define DECL_IS(tp)                                                      \
bool is##tp() const                                               \
{                                                                         \
    return isHeapObject()                                                 \
        && reinterpret_cast<HeapObject*>(val)->type == HeapObject::tp;   \
}

#define DECL_ACCESSOR(type) \
DECL_IS(type) \
DECL_TO(type)

DECL_ACCESSOR(Vector)
DECL_ACCESSOR(InputFilePort)
DECL_ACCESSOR(Closure)
DECL_ACCESSOR(CProcedure)
DECL_ACCESSOR(String)
DECL_ACCESSOR(Symbol)
DECL_ACCESSOR(Stack)
DECL_ACCESSOR(Box)
DECL_ACCESSOR(EqHashTable)
DECL_ACCESSOR(ByteVector)
DECL_ACCESSOR(TextualInputPort)
DECL_ACCESSOR(TextualOutputPort)
DECL_ACCESSOR(Regexp)
DECL_ACCESSOR(RegMatch)
DECL_ACCESSOR(Values)
DECL_ACCESSOR(BinaryInputPort)
DECL_ACCESSOR(BinaryOutputPort)
DECL_ACCESSOR(Codec)
DECL_ACCESSOR(Transcoder)
DECL_ACCESSOR(TypedVector)
DECL_ACCESSOR(TypedVectorDesc)

Object* toObjectPointer() const { return reinterpret_cast<Object*>(val); }

    bool isPointer() const
    {
        return (val & 0x03) == 0;
    }


    static const Object Nil;
    static const Object Eof;
    static const Object Undef;
    static const Object UnBound;
    static const Object True;
    static const Object False;

    bool isHeapObject() const
    {
        return isPointer() && (reinterpret_cast<HeapObject*>(val)->type & 0x03 == 0x03);
    }

private:
    Object(word n) : val(n) {}
    Object(int n, Object o); // for vector

    uint8_t tag() const
    {
        return (static_cast<word>(val)) & 0x03;
    }


public:
    word val; // public for performancs. val() const { return val; } is not be inlined with -O2

    // todo これあっているか。
    // for gc_map<Object, Object>
    friend bool operator <(const Object& o1, const Object& o2)
    {
        return o1.val < o2.val;
    }
};

Object read(Object port);
Object read(TextualInputPort* port);
Object write(Object port, Object o, bool inList = false);
Object portOutputPair(Object port, Object obj, bool inList = false);

//Object format(Object port, const ucs4char* fmt, ...);




}; // namespace scheme

scheme::Object getCompiler();
void test();

#include "Pair.h"

// force inline
namespace scheme {


};

#include "Vector.h"
#include "SString.h"
#include "Symbol.h"
#include "ByteVector.h"
#include "Closure.h"
#include "Stack.h"
#include "EqHashTable.h"
#include "CProcedure.h"
#include "Box.h"
#include "Port.h"
#include "Regexp.h"
#include "Values.h"
#include "freeproc.h"
#include "TypedVector.h"
#endif // __SCHEME_SCHEME_H__

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
 *  $Id$
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

#ifdef DEBUG_VERSION
#define MOSH_ASSERT(condition) { if (!(condition)) { fprintf(stderr, "ASSERT failure %s:%d: %s\n", __FILE__, __LINE__, #condition); exit(-1);}}
#else
#define MOSH_ASSERT(condition) /* */
#endif

enum {
    MOSH_SUCCESS,
    MOSH_FAILURE,
    forbidden_comma
};

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


#ifdef USE_BOEHM_GC
class Object;
typedef std::vector<Object, gc_allocator<Object> > ObjectVector;
#else
class Object;
typedef std::vector<Object> ObjectVector;
#endif


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
        Vector                      = Type<0>::VALUE,
        String                      = Type<1>::VALUE,
        Symbol                      = Type<2>::VALUE,
        InputFilePort               = Type<3>::VALUE,
        Closure                     = Type<4>::VALUE,
        Stack                       = Type<5>::VALUE,
        EqHashTable                 = Type<6>::VALUE,
        CProcedure                  = Type<7>::VALUE,
        Box                         = Type<8>::VALUE,
        ByteVector                  = Type<9>::VALUE,
        TextualInputPort            = Type<10>::VALUE,
        Regexp                      = Type<11>::VALUE,
        RegMatch                    = Type<12>::VALUE,
        TextualOutputPort           = Type<13>::VALUE,
        BinaryInputPort             = Type<14>::VALUE,
        BinaryOutputPort            = Type<15>::VALUE,
        Codec                       = Type<16>::VALUE,
        Transcoder                  = Type<17>::VALUE,
        CodeBuilder                 = Type<20>::VALUE,
        GenericHashTable            = Type<21>::VALUE,
        EqvHashTable                = Type<22>::VALUE,
        Callable                    = Type<23>::VALUE,
        Record                      = Type<24>::VALUE,
        RecordTypeDescriptor        = Type<25>::VALUE,
        RecordConstructorDescriptor = Type<26>::VALUE,
        CompoundCondition           = Type<27>::VALUE,
        ObjectPointer               = Type<28>::VALUE, // used only debug mode
        forbidden_comma
    };
};

struct Pair;
class Vector;
class String;
class InputFilePort;
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

};

#include "Object.h"
#include "Object-inl.h"

namespace scheme {


Object read(Object port);
Object read(TextualInputPort* port, bool& errorOccured);
Object read2(TextualInputPort* port);
Object write(Object port, Object o, bool inList = false);
Object portOutputPair(Object port, Object obj, bool inList = false);

//Object format(Object port, const ucs4char* fmt, ...);




}; // namespace scheme

void test();

#include "Pair.h"

// force inline
// If you inline car() and cdr(), Mosh becomes xx% faster.
// use -pg for finding inline target functions.
#include "Pair-inl.h"
namespace scheme {


};

//#include "Vector.h"
//#include "SString.h"
#include "Symbol.h"
#include "ByteVector.h"
#include "Closure.h"
#include "Stack.h"
#include "EqHashTable.h"
#include "EqvHashTable.h"
#include "GenericHashTable.h"
#include "CProcedure.h"
#include "Box.h"
#include "Regexp.h"
#include "freeproc.h"
#include "Callable.h"
#include "Record.h"
#include "RecordTypeDescriptor.h"
#include "RecordConstructorDescriptor.h"
#include "CompoundCondition.h"

namespace scheme {


//     inline Object::Object(const ucs4string& str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str.data())))))
// {
// }


inline Object::Object(const char* str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str)))))
{
}

inline Object Object::makeVector(int n, Object* objects)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>
                                                        (new Vector(n, objects)))));
}

inline Object Object::makeVector(Object pair)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>
                                                        (new Vector(pair)))));
}


inline Object Object::makeString(int n, ucs4char c /* = ' ' */)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(n, c)))));
}




inline Object Object::makeByteVector(const gc_vector<uint8_t>& v)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(new ByteVector(v)))));
}

inline Object Object::makeByteVector(Object pair)
{
    MOSH_ASSERT(pair.isPair() || pair.isNil());
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(new ByteVector(pair)))));
}


inline Object Object::makeByteVector(ByteVector* b)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(b))));
}




};

#endif // __SCHEME_SCHEME_H__

/*
 * scheme.cpp - Scheme system objects and functions.
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
 *  $Id: scheme.cpp 5307 2008-05-06 14:43:00Z higepon $
 */

#include "scheme.h"

using namespace scheme;

const Object Object::Nil     = Object::makeConst(0);
const Object Object::Eof     = Object::makeConst(1);
const Object Object::Undef   = Object::makeConst(2);
const Object Object::UnBound = Object::makeConst(3);
const Object Object::True    = Object::makeConst(4);
const Object Object::False   = Object::makeConst(5);

Object Object::eqv(Object o) const
{
    return eq(o);
//     const int taga = tag();
//     const int tagb = o.tag();
//     if (taga && tagb) {
//         return eq(o);
//     } else if (taga && 0 == tagb) {
//         return Object::False;
//     } else if (0 == taga && tagb) {
//         return Object::False;
//     } else if (isPair() && o.isPair()) {
//         RETURN_BOOL(car().equal(o.car()) != Object::False &&
//                     cdr().equal(o.cdr()) != Object::False);
//     } else if (isHeapObject() && o.isHeapObject()) {
//         if (isString() && o.isString()) {
//             RETURN_BOOL(toString()->data() == o.toString()->data());
//         } else if (isSymbol() && o.isSymbol()) {
//             return eq(o);
//         } else if (isVector() && o.isVector()) {
//             Vector* av = toVector();
//             Vector* bv = o.toVector();
//             const int aLength = av->length();
//             const int bLength = bv->length();
//             if (aLength == bLength) {
//                 for (int i = 0; i < aLength; i++) {
//                     if (av->ref(i).equal(bv->ref(i)).isFalse()) return Object::False;
//                 }
//                 return Object::True;
//             } else {
//                 return Object::False;
//             }
//         } else if (isRegexp() && o.isRegexp()) {
//             RETURN_BOOL(toRegexp()->pattern() == o.toRegexp()->pattern());
//         } else if (isPointer() && o.isPointer()) {
//             // address of instruction label is pointer
//             RETURN_BOOL(o.val == val);
//         }
//     }

// if (isPointer() && o.isPointer()) {
//             // address of instruction label is pointer
//             RETURN_BOOL(o.val == val);
// }

//     // todo
//     return Object::False;
}


Object Object::equal(Object o) const
{
    const int taga = tag();
    const int tagb = o.tag();
    if (taga && tagb) {
        return eq(o);
    } else if (taga && 0 == tagb) {
        return Object::False;
    } else if (0 == taga && tagb) {
        return Object::False;
    } else if (isPair() && o.isPair()) {
        RETURN_BOOL(car().equal(o.car()) != Object::False &&
                    cdr().equal(o.cdr()) != Object::False);
    } else if (isHeapObject() && o.isHeapObject()) {
        if (isString() && o.isString()) {
            RETURN_BOOL(toString()->data() == o.toString()->data());
        } else if (isSymbol() && o.isSymbol()) {
            return eq(o);
        } else if (isVector() && o.isVector()) {
            Vector* av = toVector();
            Vector* bv = o.toVector();
            const int aLength = av->length();
            const int bLength = bv->length();
            if (aLength == bLength) {
                for (int i = 0; i < aLength; i++) {
                    if (av->ref(i).equal(bv->ref(i)).isFalse()) return Object::False;
                }
                return Object::True;
            } else {
                return Object::False;
            }
        } else if (isRegexp() && o.isRegexp()) {
            RETURN_BOOL(toRegexp()->pattern() == o.toRegexp()->pattern());
        }
// todo
//         } else if (isPointer() && o.isPointer()) {
//             // address of instruction label is pointer
//             RETURN_BOOL(o.val == val);
//         }

    }

    // todo
    return Object::False;
}

Object& Object::car() const
{
    return toPair()->car;
}

Object& Object::sourceInfo() const
{
    return toPair()->sourceInfo;
}

Object& Object::first() const
{
    return car();
}

Object& Object::second() const
{
    return cdr().car();
}

Object& Object::third() const
{
    return cdr().cdr().car();
}

Object& Object::fourth() const
{
    return cdr().cdr().cdr().car();
}

Object& Object::fifth() const
{
    return cdr().cdr().cdr().cdr().car();
}


Pair* Object::toPair() const
{
    return reinterpret_cast<Pair*>(val);
}

Object Object::cons(Object car, Object cdr, Object sourceInfo /* = Object::False */)
{
// this makes Mosh very slow.
// don't use this.
//     if (sourceInfo.isFalse() && car.isPair()) {
//         sourceInfo = car.sourceInfo();
//     }
    return Object(reinterpret_cast<word>(new Pair(car, cdr, sourceInfo)));
}

Object& Object::cdr() const
{
    return toPair()->cdr;
}

bool Object::isPair() const
{
    return isPointer() && ((toPair()->car.val & 0x03) != 0x03);
}

Object::Object(int n, Object o) : val(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>(new Vector(n, o)))))
{
}

Object::Object(const ucs4char* str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str)))))
{
}

Object::Object(const char* str) : val(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(str)))))
{
}

Object Object::makeVector(int n, Object* objects)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>
                                                        (new Vector(n, objects)))));
}

Object Object::makeVector(Object pair)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Vector, reinterpret_cast<word>
                                                        (new Vector(pair)))));
}


Object Object::makeSymbol(const ucs4char* str)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Symbol, reinterpret_cast<word>(new Symbol(str)))));
}

Object Object::makeBox(Object o)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Box, reinterpret_cast<word>(new Box(o)))));
}

Object Object::makeByteVector(int n, int8_t v)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector, reinterpret_cast<word>(new ByteVector(n, v)))));
}

Object Object::makeStack(Object* src, int size)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Stack, reinterpret_cast<word>(new Stack(src, size)))));
}

Object Object::makeClosure(Object* pc, int argLength, bool isOptionalArg,
                           const Object* freeVars, int freeVariablesNum, int maxStack, Object sourceInfo)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Closure,
                                                        reinterpret_cast<word>(new Closure(pc, argLength, isOptionalArg, freeVars, freeVariablesNum, maxStack, sourceInfo)))));
}

Object Object::makeEqHashTable()
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::EqHashTable, reinterpret_cast<word>(new EqHashTable()))));
}

Object Object::makeCProcedure(Object (*proc)(int, const Object*))
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::CProcedure, reinterpret_cast<word>(new CProcedure(proc)))));
}


Object Object::makeString(int n, ucs4char c /* = ' ' */)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::String, reinterpret_cast<word>(new String(n, c)))));
}

Object Object::makeTextualInputFilePort(const ucs4char* file)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualInputPort,
                                                        reinterpret_cast<word>(new TextualInputPort(new FileBinaryInputPort(file)
                                                                                                    , new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR))))));
}

Object Object::makeTextualInputFilePort(const char* file)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualInputPort,
                                                        reinterpret_cast<word>(new TextualInputPort(new FileBinaryInputPort(file)
                                                                                                    , new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR))))));
}


Object Object::makeStringInputPort(const ucs4string& str)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualInputPort,
                                                        reinterpret_cast<word>(new StringTextualInputPort(str)))));
}

Object Object::makeStringOutputPort()
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualOutputPort,
                                                        reinterpret_cast<word>(new StringTextualOutputPort()))));
}

Object Object::makeStringInputPort(const uint8_t* buf, int size)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualInputPort,
                                                        reinterpret_cast<word>(new TextualInputPort(new ByteArrayBinaryInputPort(buf, size)
                                                                                                    , new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR))))));
}


Object Object::makeRegexp(const ucs4string& pattern, bool caseFold /* = false */)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Regexp,
                                                        reinterpret_cast<word>(new Regexp(pattern, caseFold)))));
}

Object Object::makeRegMatch(OnigRegion* region, const ucs4string& text)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::RegMatch,
                                                        reinterpret_cast<word>(new RegMatch(region, text)))));
}

Object Object::makeCustomBinaryInputPort(Object readProc)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::BinaryInputPort,
                                                        reinterpret_cast<word>(new CustomBinaryInputPort(readProc)))));
}

Object Object::makeUTF8Codec()
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Codec,
                                                        reinterpret_cast<word>(new UTF8Codec()))));
}

Object Object::makeTranscoder(Codec* codec)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::Transcoder,
                                                        reinterpret_cast<word>(new Transcoder(codec)))));
}

Object Object::makeTextualInputPort(BinaryInputPort* port, Transcoder* transcoder)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualInputPort,
                                                        reinterpret_cast<word>(new TextualInputPort(port, transcoder)))));
}

Object Object::makeTextualByteVectorOuputPort(Transcoder* transcoder)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualOutputPort,
                                                        reinterpret_cast<word>(new TextualByteVectorOutputPort(transcoder)))));
}

Object Object::makeByteVector(const gc_vector<uint8_t>& v)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(new ByteVector(v)))));
}

Object Object::makeByteVector(ByteVector* b)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::ByteVector,
                                                        reinterpret_cast<word>(b))));
}

Object Object::makeBinaryInputPort(FILE* in)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::BinaryInputPort,
                                                        reinterpret_cast<word>(new FileBinaryInputPort(in)))));
}

Object Object::makeTextualOutputPort(BinaryOutputPort* port, Transcoder* transcoder)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TextualOutputPort,
                                                        reinterpret_cast<word>(new TextualOutputPort(port, transcoder)))));
}

Object Object::makeTypedVectorDesc(Object name, Object supertype, Object data, Object fieldMutability)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TypedVectorDesc,
                                                        reinterpret_cast<word>(new TypedVectorDesc(name, supertype, data, fieldMutability)))));
}

Object Object::makeTypedVector(Object desc, Object fieldsList)
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::TypedVector,
                                                        reinterpret_cast<word>(new TypedVector(desc, fieldsList)))));
}

Object Object::makeCodeBuilder()
{
    return Object(reinterpret_cast<word>(new HeapObject(HeapObject::CodeBuilder,
                                                        reinterpret_cast<word>(new CodeBuilder()))));
}

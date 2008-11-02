/*
 * HeapObject.h - 
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
 *  $Id: HeapObject.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_HEAP_OBJECT__
#define __SCHEME_HEAP_OBJECT__

#include "scheme.h"

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
        Ratnum                      = Type<29>::VALUE,
        Flonum                      = Type<30>::VALUE,
        Bignum                      = Type<31>::VALUE,
        forbidden_comma
    };
};

}; // namespace scheme

#endif // __SCHEME_HEAP_OBJECT__

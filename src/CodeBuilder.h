/*
 * CodeBuilder.h - CodeBuilder for compilation.
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

#ifndef SCHEME_CODE_BUILDER_H_
#define SCHEME_CODE_BUILDER_H_

#include "scheme.h"

namespace scheme {

class CodePacket EXTEND_GC
{
public:
    enum Type
    {
        EMPTY,
        ARGUMENT0,
        ARGUMENT1,
        ARGUMENT2,
        ARGUMENT3,
        EXTRA
    };

    CodePacket();
    CodePacket(Type packetType, Object instruction, Object argument1, Object argument2, Object argument3);

    // accessors
    Type type() const { return packetType_; }
    void setType(Type packetType) { packetType_ = packetType; }
    void setInstructionImmediate(int instruction) { instruction_ = Object::makeRaw(instruction); }
    void setArgument1(Object argument) { argument1_ = argument; }
    void setArgument2(Object argument) { argument2_ = argument; }
    void setArgument3(Object argument) { argument3_ = argument; }
    Object instruction() const { return instruction_; }
    intptr_t instructionImmediate() const { return instruction_.val; }
    Object argument1() const { return argument1_; }
    Object argument2() const { return argument2_; }
    Object argument3() const { return argument3_; }

private:
    Type packetType_;
    Object instruction_;
    Object argument1_;
    Object argument2_;
    Object argument3_;
};

class CodeBuilder EXTEND_GC
{
public:
    CodeBuilder();
    void putExtra(Object object);
    void putInstructionArgument2(Object instruction, Object argument1, Object argument2);
    void putInstructionArgument1(Object instruction, Object argument1);
    void putInstructionArgument0(Object instruction);
    Object emit();
    void append(CodeBuilder* codeBuilder);

    // accessors
    ObjectVector& code()
    {
//        printf("[%x] code called\n", this);
        flush();
        return code_;
    }

    // debug
    void printPrevious();

private:
    void flush();
    void put(CodePacket codePacket);
    void combineInstructionsArgument2(CodePacket codePacket);
    void combineInstructionsArgument1(CodePacket codePacket);
    void combineInstructionsArgument0(CodePacket codePacket);

    CodePacket previousCodePacket_;
    Object labelDefinitions_;
    Object labelReferences_;
    ObjectVector code_;
};

} // namespace scheme

#endif // SCHEME_CODE_BUILDER_H_

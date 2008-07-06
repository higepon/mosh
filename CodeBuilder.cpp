/*
 * CodeBuilder.cpp - CodeBuilder for compilation.
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
 *  $Id: CodeBuilder.h 183 2008-07-04 06:19:28Z higepon $
 */

#include "CodeBuilder.h"
#include "VM.h"

extern scheme::VM* theVM;
using namespace scheme;


CodePacket::CodePacket() : packetType_(EMPTY),
                           instruction_(Object::Undef),
                           argument1_(Object::Undef),
                           argument2_(Object::Undef),
                           operand_(Object::Undef)
{
}

CodePacket::CodePacket(Type packetType, Object instruction, Object argument1, Object argument2, Object operand)
    : packetType_(packetType),
      instruction_(instruction),
      argument1_(argument1),
      argument2_(argument2),
      operand_(operand)
{
}



CodeBuilder::CodeBuilder() : previousCodePacket_(CodePacket()),
                               labelDefinitions_(Object::Nil),
                               labelReferences_(Object::Nil),
                               code_(ObjectVector())
{
}

void CodeBuilder::putExtra(Object object)
{
    put(CodePacket(CodePacket::EXTRA, object, Object::Undef, Object::Undef, Object::Undef));
}

void CodeBuilder::put(CodePacket packet)
{
    if (packet.type() == CodePacket::EXTRA) {
        flush();
        previousCodePacket_ = packet;
    } else {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        SCHEME_ASSERT("not reached now! all packet should be EXTRA");
        // do nothing now!
    }
}

void CodeBuilder::flush()
{
    if (previousCodePacket_.type() == CodePacket::EMPTY) return;
    if (previousCodePacket_.type() == CodePacket::EXTRA) {
        VM_LOG1("previousCodePacket_.instruction()=~a\n", previousCodePacket_.instruction());
        code_.push_back(previousCodePacket_.instruction());
        previousCodePacket_.setType(CodePacket::EMPTY);
    } else {
        // do nothing now!
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug

        SCHEME_ASSERT("not reached now! all packet should be EXTRA");
    }
}

Object CodeBuilder::emit()
{
    flush();
    const Object ret = Pair::objectVectorToList(code_);
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    return ret;
}

void CodeBuilder::append(CodeBuilder* sourcCodeBuilder)
{
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    ObjectVector& sourceCode = sourcCodeBuilder->code();
    code_.insert(code_.end(), sourceCode.begin(), sourceCode.end());
}

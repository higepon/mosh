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

void CodeBuilder::putInstructionArgument1(Object instruction, Object argument)
{
    put(CodePacket(CodePacket::ARGUMENT1, instruction, argument, Object::Undef, Object::Undef));
}

void CodeBuilder::combineInstructionsArgument1(CodePacket codePacket)
{
    const Object argument1 = codePacket.argument1();
    switch(codePacket.instructionImmediate()) {
    case Instruction::REFER_LOCAL:
    {
        flush();
        const int index = argument1.toInt();
        if (0 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::REFER_LOCAL0));
        } else if (1 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::REFER_LOCAL1));
        } else if (2 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::REFER_LOCAL2));
        } else {
            // do nothing
        }
        previousCodePacket_ = codePacket;
        break;
    }
    case Instruction::REFER_FREE:
    {
        flush();
        const int index = argument1.toInt();
        if (0 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::REFER_FREE0));
        } else if (1 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::REFER_FREE1));
        } else if (2 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::REFER_FREE2));
        } else {
            // do nothing
        }
        previousCodePacket_ = codePacket;
        break;
    }
    case Instruction::RETURN:
        flush();
        const int index = argument1.toInt();
        if (1 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::RETURN1));
        } else if (2 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::RETURN2));
        } else if (3 == index) {
            codePacket.setType(CodePacket::EXTRA);
            codePacket.setInstruction(Object::makeRaw(Instruction::RETURN3));
        } else {
            // do nothing
        }
        previousCodePacket_ = codePacket;
        break;
    default:
        flush();
        previousCodePacket_ = codePacket;
        break;
    }
}

void CodeBuilder::combineInstructionsArgument0(CodePacket codePacket)
{
       switch(codePacket.instructionImmediate()) {
        case Instruction::PUSH:
            switch(previousCodePacket_.instructionImmediate()) {
            case Instruction::REFER_LOCAL0:
                previousCodePacket_.setType(CodePacket::EXTRA);
                previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_LOCAL0_PUSH));
                break;
            case Instruction::REFER_LOCAL1:
                previousCodePacket_.setType(CodePacket::EXTRA);
                previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_LOCAL1_PUSH));
                break;
            case Instruction::REFER_LOCAL2:
                previousCodePacket_.setType(CodePacket::EXTRA);
                previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_LOCAL2_PUSH));
                break;
            case Instruction::REFER_FREE0:
                previousCodePacket_.setType(CodePacket::EXTRA);
                previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_FREE0_PUSH));
                break;
            case Instruction::REFER_FREE1:
                previousCodePacket_.setType(CodePacket::EXTRA);
                previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_FREE1_PUSH));
                break;
            case Instruction::REFER_FREE2:
                previousCodePacket_.setType(CodePacket::EXTRA);
                previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_FREE2_PUSH));
                break;
// 100msec遅くなるので後回し
            case Instruction::REFER_FREE:
                previousCodePacket_.setType(CodePacket::ARGUMENT1);
                previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_FREE_PUSH));
                break;
            default:
                flush();
                previousCodePacket_ = codePacket;
                break;
            }
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
        }
}

void CodeBuilder::put(CodePacket codePacket)
{
    switch(codePacket.type()) {
#if 1
    case CodePacket::EXTRA:
 //        switch(codePacket.instructionImmediate()) {
//         case Instruction::PUSH:
//             if (previousCodePacket_.instructionImmediate() == Instruction::REFER_LOCAL0) {
//                 previousCodePacket_.setType(CodePacket::EXTRA);
//                 previousCodePacket_.setInstruction(Object::makeRaw(Instruction::REFER_LOCAL0_PUSH));
//             } else {
//                 flush();
//                 previousCodePacket_ = codePacket;
//             }
//             break;
//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//         }
        combineInstructionsArgument0(codePacket);
        break;
    case CodePacket::ARGUMENT1:
        combineInstructionsArgument1(codePacket);
        break;
#endif
    default:
        flush();
        previousCodePacket_ = codePacket;
        break;
    }
//     if (packet.type() == CodePacket::EXTRA) {
//         flush();
//         previousCodePacket_ = packet;
//     } else {
//         printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//         SCHEME_ASSERT("not reached now! all packet should be EXTRA");
//         // do nothing now!
//     }
}

void CodeBuilder::flush()
{
    switch(previousCodePacket_.type()) {
    case CodePacket::EMPTY:
        // just do nothing
        break;
    case CodePacket::EXTRA:
        code_.push_back(previousCodePacket_.instruction());
        previousCodePacket_.setType(CodePacket::EMPTY);
        break;
    case CodePacket::ARGUMENT1:
        code_.push_back(previousCodePacket_.instruction());
        code_.push_back(previousCodePacket_.argument1());
        previousCodePacket_.setType(CodePacket::EMPTY);
        break;
    default:
        // do nothing now!
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug

        SCHEME_ASSERT("not reached now! all packet should be EXTRA");
        break;
    }
//    if (previousCodePacket_.type() == CodePacket::EMPTY) return;
//     if (previousCodePacket_.type() == CodePacket::EXTRA) {
//         code_.push_back(previousCodePacket_.instruction());
//         previousCodePacket_.setType(CodePacket::EMPTY);
//     } else if (previousCodePacket_.type() == CodePacket::ARGUMENT1) {
//         code_.push_back(previousCodePacket_.instruction());
//         code_.push_back(previousCodePacket_.argument1());
//     } else {
//         // do nothing now!
//         printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug

//         SCHEME_ASSERT("not reached now! all packet should be EXTRA");
//     }
}

Object CodeBuilder::emit()
{
    flush();
    const Object ret = Pair::objectVectorToList(code_);
    return ret;
}

void CodeBuilder::append(CodeBuilder* sourcCodeBuilder)
{
    flush();
    ObjectVector& sourceCode = sourcCodeBuilder->code();
    code_.insert(code_.end(), sourceCode.begin(), sourceCode.end());
}

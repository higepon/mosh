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

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "CodeBuilder.h"
#include "TextualOutputPort.h"
#include "VM.h"

extern scheme::VM* theVM;
using namespace scheme;

CodePacket::CodePacket() : packetType_(EMPTY),
                           instruction_(Object::Undef),
                           argument1_(Object::Undef),
                           argument2_(Object::Undef),
                           argument3_(Object::Undef)
{
}

CodePacket::CodePacket(Type packetType, Object instruction, Object argument1, Object argument2, Object argument3)
    : packetType_(packetType),
      instruction_(instruction),
      argument1_(argument1),
      argument2_(argument2),
      argument3_(argument3)
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

void CodeBuilder::putInstructionArgument0(Object instruction)
{
    put(CodePacket(CodePacket::ARGUMENT0, instruction, Object::Undef, Object::Undef, Object::Undef));
}

void CodeBuilder::putInstructionArgument1(Object instruction, Object argument)
{
    put(CodePacket(CodePacket::ARGUMENT1, instruction, argument, Object::Undef, Object::Undef));
}

void CodeBuilder::putInstructionArgument2(Object instruction, Object argument1, Object argument2)
{
    put(CodePacket(CodePacket::ARGUMENT2, instruction, argument1, argument2, Object::Undef));
}

void CodeBuilder::combineInstructionsArgument0(CodePacket codePacket)
{
    switch(codePacket.instructionImmediate()) {
//     case Instruction::VECTOR_REF:
//     {
//         switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::REFER_LOCAL:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_VECTOR_REF);
//             break;
//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//             break;
//         }
//         break;
//     }
//     case Instruction::VECTOR_SET:
//     {
//         switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::REFER_LOCAL:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_VECTOR_SET);
//             break;
//         case Instruction::PUSH_CONSTANT:
//             previousCodePacket_.setInstructionImmediate(Instruction::PUSH_CONSTANT_VECTOR_SET);
//             break;

//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//             break;
//         }
//         break;
//     }
//     case Instruction::CAR:
//         switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::REFER_LOCAL:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_CAR);
//             break;
//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//             break;
//         }
//         break;
//     case Instruction::CDR:
//         switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::REFER_LOCAL:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_CDR);
//             break;
//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//             break;
//         }
//         break;
//     case Instruction::CONS:
//         switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::REFER_LOCAL:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_CONS);
//             break;
//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//             break;
//         }
//         break;
    case Instruction::PUSH:
        switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::VECTOR_REF:
//             previousCodePacket_.setInstructionImmediate(Instruction::VECTOR_REF_PUSH);
//             break;
        case Instruction::NUMBER_ADD:
            previousCodePacket_.setInstructionImmediate(Instruction::NUMBER_ADD_PUSH);
            break;
        case Instruction::NUMBER_SUB:
            previousCodePacket_.setInstructionImmediate(Instruction::NUMBER_SUB_PUSH);
            break;
        case Instruction::CAR:
            previousCodePacket_.setInstructionImmediate(Instruction::CAR_PUSH);
            break;
        case Instruction::CDR:
            previousCodePacket_.setInstructionImmediate(Instruction::CDR_PUSH);
            break;
        case Instruction::CONSTANT:
            previousCodePacket_.setInstructionImmediate(Instruction::CONSTANT_PUSH);
            break;
        case Instruction::REFER_LOCAL:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_PUSH);
            break;
        case Instruction::REFER_GLOBAL:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_GLOBAL_PUSH);
            break;

//         case Instruction::REFER_LOCAL0:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL0_PUSH);
//             break;
//         case Instruction::REFER_LOCAL1:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL1_PUSH);
//             break;
//         case Instruction::REFER_LOCAL2:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL2_PUSH);
//             break;
//         case Instruction::REFER_FREE0:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_FREE0_PUSH);
//             break;
//         case Instruction::REFER_FREE1:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_FREE1_PUSH);
//             break;
//         case Instruction::REFER_FREE2:
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_FREE2_PUSH);
//             break;
        case Instruction::REFER_FREE:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_FREE_PUSH);
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

void CodeBuilder::combineInstructionsArgument1(CodePacket codePacket)
{
    const Object argument1 = codePacket.argument1();
    switch(codePacket.instructionImmediate()) {
    case Instruction::TEST:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::NOT:
            previousCodePacket_.setInstructionImmediate(Instruction::NOT_TEST);
            previousCodePacket_.setType(CodePacket::ARGUMENT1);
            previousCodePacket_.setArgument1(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::BRANCH_NOT_NULL:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::REFER_LOCAL:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_BRANCH_NOT_NULL);
            previousCodePacket_.setType(CodePacket::ARGUMENT2);
            previousCodePacket_.setArgument2(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::BRANCH_NOT_LT:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::REFER_LOCAL:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_BRANCH_NOT_LT);
            previousCodePacket_.setType(CodePacket::ARGUMENT2);
            previousCodePacket_.setArgument2(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::BRANCH_NOT_LE:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::REFER_LOCAL_PUSH_CONSTANT:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE);
            previousCodePacket_.setType(CodePacket::ARGUMENT3);
            previousCodePacket_.setArgument3(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::BRANCH_NOT_NUMBER_EQUAL:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::REFER_LOCAL_PUSH_CONSTANT:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL);
            previousCodePacket_.setType(CodePacket::ARGUMENT3);
            previousCodePacket_.setArgument3(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::BRANCH_NOT_GE:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::REFER_LOCAL_PUSH_CONSTANT:
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE);
            previousCodePacket_.setType(CodePacket::ARGUMENT3);
            previousCodePacket_.setArgument3(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::FRAME:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::PUSH:
            previousCodePacket_.setType(CodePacket::ARGUMENT1);
            previousCodePacket_.setInstructionImmediate(Instruction::PUSH_FRAME);
            previousCodePacket_.setArgument1(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::ENTER:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::PUSH:
            previousCodePacket_.setType(CodePacket::ARGUMENT1);
            previousCodePacket_.setInstructionImmediate(Instruction::PUSH_ENTER);
            previousCodePacket_.setArgument1(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
    case Instruction::CONSTANT:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::REFER_LOCAL_PUSH:
            previousCodePacket_.setType(CodePacket::ARGUMENT2);
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_PUSH_CONSTANT);
            previousCodePacket_.setArgument2(codePacket.argument1());
            break;
//         case Instruction::REFER_LOCAL0_PUSH:
//             previousCodePacket_.setType(CodePacket::ARGUMENT1);
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL0_PUSH_CONSTANT);
//             previousCodePacket_.setArgument1(codePacket.argument1());
//             break;
//         case Instruction::REFER_LOCAL1_PUSH:
//             previousCodePacket_.setType(CodePacket::ARGUMENT1);
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL1_PUSH_CONSTANT);
//             previousCodePacket_.setArgument1(codePacket.argument1());
//             break;
//         case Instruction::REFER_LOCAL2_PUSH:
//             previousCodePacket_.setType(CodePacket::ARGUMENT1);
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL2_PUSH_CONSTANT);
//             previousCodePacket_.setArgument1(codePacket.argument1());
//             break;
        case Instruction::PUSH:
            previousCodePacket_.setType(CodePacket::ARGUMENT1);
            previousCodePacket_.setInstructionImmediate(Instruction::PUSH_CONSTANT);
            previousCodePacket_.setArgument1(codePacket.argument1());
            break;
        default:
            flush();
            previousCodePacket_ = codePacket;
            break;
        }
        break;
    }
//     case Instruction::LEAVE:
//     {
//         flush();
//         MOSH_ASSERT(argument1.isFixnum());
//         const int index = argument1.toFixnum();
//         if (1 == index) {
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::LEAVE1);
//         } else {
//             // do nothing
//         }
//         previousCodePacket_ = codePacket;
//         break;
//     }
//     case Instruction::REFER_LOCAL:
//     {
//         flush();
//         MOSH_ASSERT(argument1.isFixnum());
//         const int index = argument1.toFixnum();
//         switch(index) {
//         case 0:
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_LOCAL0);
//             break;
//         case 1:
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_LOCAL1);
//             break;
//         case 2:
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_LOCAL2);
//             break;
//         case 3:
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_LOCAL3);
//             break;
//         default:
//             break;
//         }
//         previousCodePacket_ = codePacket;
//         break;
//     }
    case Instruction::CALL:
    {
        switch(previousCodePacket_.instructionImmediate()) {
        case Instruction::REFER_GLOBAL:
            previousCodePacket_.setType(CodePacket::ARGUMENT2);
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_GLOBAL_CALL);
            previousCodePacket_.setArgument2(argument1);
            break;
        case Instruction::REFER_LOCAL:
            previousCodePacket_.setType(CodePacket::ARGUMENT2);
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_LOCAL_CALL);
            previousCodePacket_.setArgument2(argument1);
            break;
        case Instruction::REFER_FREE:
            previousCodePacket_.setType(CodePacket::ARGUMENT2);
            previousCodePacket_.setInstructionImmediate(Instruction::REFER_FREE_CALL);
            previousCodePacket_.setArgument2(argument1);
            break;
        case Instruction::SHIFT:
            previousCodePacket_.setType(CodePacket::ARGUMENT3);
            previousCodePacket_.setInstructionImmediate(Instruction::SHIFT_CALL);
            previousCodePacket_.setArgument3(argument1);
            break;

        default:
//         if (previousCodePacket_.instructionImmediate() == Instruction::REFER_GLOBAL) {
//             previousCodePacket_.setInstructionImmediate(Instruction::REFER_GLOBAL_CALL);
//             previousCodePacket_.setType(CodePacket::ARGUMENT2);
//             previousCodePacket_.setArgument2(codePacket.argument1());
//         } else if (previousCodePacket_.instructionImmediate() == Instruction::SHIFT) {
//             previousCodePacket_.setInstructionImmediate(Instruction::SHIFT_CALL);
//             previousCodePacket_.setType(CodePacket::ARGUMENT3);
//             previousCodePacket_.setArgument3(codePacket.argument1());
//         } else {
//             const int index = argument1.toFixnum();
//             if (index <= 3) {
//                 flush();
//                 switch(index) {
//                 case 1:
//                     codePacket.setType(CodePacket::ARGUMENT0);
//                     codePacket.setInstructionImmediate(Instruction::CALL1);
//                     break;
//                 case 2:
//                     codePacket.setType(CodePacket::ARGUMENT0);
//                     codePacket.setInstructionImmediate(Instruction::CALL2);
//                     break;
//                 case 3:
//                     codePacket.setType(CodePacket::ARGUMENT0);
//                     codePacket.setInstructionImmediate(Instruction::CALL3);
//                     break;
//                 default:
//                     break;
//                 }
//                 previousCodePacket_ = codePacket;
//             } else {
                flush();
                previousCodePacket_ = codePacket;
                break;
//            }
        }
        break;
    }
// N.B. This doesn't work. Because of code-builder-append!
//     case Instruction::RETURN:
//     {
//         switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::NUMBER_ADD:
//             previousCodePacket_.setType(CodePacket::ARGUMENT1);
//             previousCodePacket_.setInstructionImmediate(Instruction::NUMBER_ADD_RETURN);
//             previousCodePacket_.setArgument1(argument1);
//             break;
//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//             break;
//         }
//         break;
//     }

//     case Instruction::REFER_FREE:
//     {
//         flush();
//         MOSH_ASSERT(argument1.isFixnum());
//         const int index = argument1.toFixnum();
//         if (0 == index) {
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_FREE0);
//         } else if (1 == index) {
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_FREE1);
//         } else if (2 == index) {
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_FREE2);
//         } else if (3 == index) {
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::REFER_FREE3);

//         } else {
//             // do nothing
//         }
//         previousCodePacket_ = codePacket;
//         break;
//     }
//     case Instruction::RETURN:
//     {
//         flush();
//         MOSH_ASSERT(argument1.isFixnum());
//         const int index = argument1.toFixnum();
//         switch(index) {
//         case 1:
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::RETURN1);
//             break;
//         case 2:
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::RETURN2);
//             break;
//         case 3:
//             codePacket.setType(CodePacket::ARGUMENT0);
//             codePacket.setInstructionImmediate(Instruction::RETURN3);
//             break;
//         default:
//             break;
//         }
//         previousCodePacket_ = codePacket;
//         break;
//     }
    default:
        flush();
        previousCodePacket_ = codePacket;
        break;
    }
}

void CodeBuilder::combineInstructionsArgument2(CodePacket codePacket)
{
    const Object argument1 = codePacket.argument1();
    const Object argument2 = codePacket.argument2();
    switch(codePacket.instructionImmediate()) {
//     case Instruction::REFER_LOCAL_PUSH_CONSTANT:
//     {
//         switch(previousCodePacket_.instructionImmediate()) {
//         case Instruction::FRAME:
//             previousCodePacket_.setInstructionImmediate(Instruction::FRAME_REFER_LOCAL_PUSH_CONSTANT);
//             previousCodePacket_.setType(CodePacket::ARGUMENT3);
//             previousCodePacket_.setArgument2(codePacket.argument1());
//             previousCodePacket_.setArgument3(codePacket.argument2());
//             break;
//         default:
//             flush();
//             previousCodePacket_ = codePacket;
//             break;
//         }
//         break;
//     }
    default:
        flush();
        previousCodePacket_ = codePacket;
        break;
    }
}


void CodeBuilder::put(CodePacket codePacket)
{
    switch(codePacket.type()) {
    case CodePacket::ARGUMENT0:
        combineInstructionsArgument0(codePacket);
        break;
    case CodePacket::ARGUMENT1:
        combineInstructionsArgument1(codePacket);
        break;
    case CodePacket::ARGUMENT2:
        combineInstructionsArgument2(codePacket);
        break;
    case CodePacket::ARGUMENT3:
        flush();
        previousCodePacket_ = codePacket;
        break;
    case CodePacket::EXTRA:
        flush();
        previousCodePacket_ = codePacket;
        break;
    default:
        flush();
        previousCodePacket_ = codePacket;
        break;
    }
}

void CodeBuilder::flush()
{
    switch(previousCodePacket_.type()) {
    case CodePacket::EMPTY:
        return;
    case CodePacket::EXTRA:
        code_.push_back(previousCodePacket_.instruction());
        break;
    case CodePacket::ARGUMENT0:
        code_.push_back(previousCodePacket_.instruction());
        break;
    case CodePacket::ARGUMENT1:
        code_.push_back(previousCodePacket_.instruction());
        code_.push_back(previousCodePacket_.argument1());
        break;
    case CodePacket::ARGUMENT2:
        code_.push_back(previousCodePacket_.instruction());
        code_.push_back(previousCodePacket_.argument1());
        code_.push_back(previousCodePacket_.argument2());
        break;
    case CodePacket::ARGUMENT3:
        code_.push_back(previousCodePacket_.instruction());
        code_.push_back(previousCodePacket_.argument1());
        code_.push_back(previousCodePacket_.argument2());
        code_.push_back(previousCodePacket_.argument3());
        break;
    default:
        MOSH_ASSERT("not reached now! all packet should be EXTRA");
        break;
    }
    previousCodePacket_.setType(CodePacket::EMPTY);
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

void CodeBuilder::printPrevious()
{
    VM_LOG1("previous =~a\n", Instruction::toString(previousCodePacket_.instructionImmediate()));
}

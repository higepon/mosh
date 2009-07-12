/*
 * VM-Run.h -
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
 *  $Id: VM-Run.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_VM_RUN_
#define SCHEME_VM_RUN_

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ErrorProcedures.h"
#include "TextualOutputPort.h"
#include "EqHashTable.h"
#include "CProcedure.h"
#include "Arithmetic.h"
#include "Closure.h"
#include "Callable.h"
#include "RegexpProcedures.h"
#include "Bignum.h"
#include "Box.h"
#include "Vector.h"
#include "Stack.h"
#include "Record.h"
#include "UtilityProcedures.h"
#include "TextualInputPort.h"
#include "Codec.h"
#include "Equivalent.h"
#include "VM.h"
#include "Symbol.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "StringProcedures.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Gloc.h"
#include "TranscodedTextualInputOutputPort.h"
#include "SimpleStruct.h"


#ifdef ENABLE_PROFILER
#define COUNT_CALL(a) countCall(a)
#else
#define COUNT_CALL(a) /* */
#endif

#define BRANCH_ON_FALSE                         \
    if (ac_.isFalse()) {                        \
        const Object skipSize = fetchOperand(); \
        MOSH_ASSERT(skipSize.isFixnum());       \
        skip(skipSize.toFixnum() - 1);          \
    } else {                                    \
        pc_++;                                  \
    }

#define NUM_CMP_LOCAL(op, func)                                                             \
   const Object n = pop();                                                                  \
   if (n.isFixnum() && ac_.isFixnum()) {                                                    \
       ac_ = Object::makeBool(n.toFixnum() op ac_.toFixnum());                              \
   } else if (n.isFlonum() && ac_.isFlonum()) {                                             \
       ac_ = Object::makeBool(Flonum::func(n.toFlonum(), ac_.toFlonum()));                  \
   } else {                                                                                 \
       if (n.isReal() && ac_.isReal()) {                                                    \
           ac_ = Object::makeBool(Arithmetic::func(n, ac_));                                \
       } else {                                                                             \
           callWrongTypeOfArgumentViolationAfter(this, #op, "number required", L2(n, ac_)); \
           NEXT1;                                                                           \
       }                                                                                    \
   }

#define NUM_CMP(op, func, val)                                                              \
   const Object n = val;                                                                    \
   if (n.isFixnum() && ac_.isFixnum()) {                                                    \
       ac_ = Object::makeBool(n.toFixnum() op ac_.toFixnum());                              \
   } else if (n.isFlonum() && ac_.isFlonum()) {                                             \
       ac_ = Object::makeBool(Flonum::func(n.toFlonum(), ac_.toFlonum()));                  \
   } else {                                                                                 \
       if (n.isReal() && ac_.isReal()) {                                                    \
           ac_ = Object::makeBool(Arithmetic::func(n, ac_));                                \
       } else {                                                                             \
           callWrongTypeOfArgumentViolationAfter(this, #op, "number required", L2(n, ac_)); \
           NEXT;                                                                            \
       }                                                                                    \
   }

#ifdef USE_DIRECT_THREADED_CODE
#define SWITCH(val) goto *val;

#define CASE(insn)  LABEL_##insn : //printf("" #insn "\n");fflush(stdout);
#define NEXT                         \
{                                    \
    asm volatile(" \t # -- next start");   \
    goto *((*pc_++).val);            \
    asm volatile(" \t # -- next end");   \
}
#define NEXT1                       \
{                                   \
    numValues_ = 1;                 \
    goto *(*pc_++).val;             \
}

#define DEFAULT     LABEL_DEFAULT :
#define INSTRUCTION(insn) &&LABEL_ ## insn

#else /* !USE_DIRECT_THREADED_CODE */
#define SWITCH(val) switch (val)
#define CASE(insn)  case Instruction:: insn :
#define NEXT        break;
#define NEXT1        {numValues_ = 1; break;}
#define DEFAULT     default:
#define INSTRUCTION(insn) Instruction:: insn
#endif

#endif // SCHEME_VM_RUN_

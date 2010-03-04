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
#include "Ratnum.h"
#include "Flonum.h"
#include "Equivalent.h"
#include "VM.h"
#include "Symbol.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "StringProcedures.h"
#include "FFIProcedures.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Gloc.h"
#include "TranscodedTextualInputOutputPort.h"
#include "SimpleStruct.h"
#include "Continuation.h"
#include "Code.h"

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

#define NUM_CMP_LOCAL(op, opstring, func)                                                   \
   const Object n = pop();                                                                  \
   if (n.isFixnum() && ac_.isFixnum()) {                                                    \
       ac_ = Object::makeBool(n.toFixnum() op ac_.toFixnum());                              \
   } else if (n.isFlonum() && ac_.isFlonum()) {                                             \
       ac_ = Object::makeBool(Flonum::func(n.toFlonum(), ac_.toFlonum()));                  \
   } else {                                                                                 \
       if (n.isNumber() && ac_.isNumber()) {                                                \
           ac_ = Object::makeBool(Arithmetic::func(n, ac_));                                \
       } else {                                                                             \
           callWrongTypeOfArgumentViolationAfter(this, #opstring, "number", L2(n, ac_));    \
           NEXT1;                                                                           \
       }                                                                                    \
   }

#define NUM_CMP(op, opstring, func, val)                                 \
   const Object n = val;                                                                    \
   if (n.isFixnum() && ac_.isFixnum()) {                                                    \
       ac_ = Object::makeBool(n.toFixnum() op ac_.toFixnum());                              \
   } else if (n.isFlonum() && ac_.isFlonum()) {                                             \
       ac_ = Object::makeBool(Flonum::func(n.toFlonum(), ac_.toFlonum()));                  \
   } else {                                                                                 \
       if (n.isNumber() && ac_.isNumber()) {                                                  \
           ac_ = Object::makeBool(Arithmetic::func(n, ac_));                                \
       } else {                                                                             \
           callWrongTypeOfArgumentViolationAfter(this, #opstring, "number", L2(n, ac_)); \
           NEXT;                                                                            \
       }                                                                                    \
   }

#ifdef USE_DIRECT_THREADED_CODE
#define SWITCH(val) goto *val;

#define CASE(insn)  LABEL_##insn : asm volatile("# -- " #insn " start");
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

namespace scheme {
    // This function should be inlined on VM Run loop.
    // And should be callable from JIT.
    inline void VM::callOp(Object operand)
    {
        if (ac_.isCProcedure()) {
            COUNT_CALL(ac_);
            cl_ = ac_;
            if (ac_.toCProcedure()->proc == applyEx || ac_.toCProcedure()->proc == evalEx) {
                // don't share retCode with others.
                // because apply's arguments length is not constant.
                Object* const retCode = Object::makeObjectArray(2);
                retCode[0] = Object::makeRaw(labelReturn_);
                retCode[1] = operand;

                VM_ASSERT(operand.isFixnum());
                const int argc = operand.toFixnum();
                pc_  = retCode;
                ac_.toCProcedure()->call(this, argc, sp_ - argc);
            } else {
                CProcedure* const cprocedure = ac_.toCProcedure();
                // set pc_ before call() for pointing where to return.
                pc_  = cprocedure->returnCode;
                pc_[0] = Object::makeRaw(labelReturn_);
                pc_[1] = operand;
                VM_ASSERT(operand.isFixnum());
                const int argc = operand.toFixnum();
//                    LOG1("~a\n", getClosureName(ac_));
                cl_ = ac_;
                ac_ = ac_.toCProcedure()->call(this, argc, sp_ - argc);
            }
        } else if (ac_.isClosure()) {
            Closure* const c = ac_.toClosure();
            COUNT_CALL(ac_);
            dc_ = ac_;
            cl_ = ac_;

            VM_ASSERT(operand.isFixnum());
            const int argLength = operand.toFixnum();
            const int requiredLength = c->argLength;

            if (c->isOptionalArg) {
                const int extraLength = argLength - requiredLength;
                if (-1 == extraLength) {
                    Object* const sp = unShiftArgs(sp_, 1);
                    indexSet(sp, 0, Object::Nil);
                    sp_ = sp;
                    fp_ = sp - requiredLength;
                } else if (extraLength >= 0) {
                    indexSet(sp_, extraLength, stackToPairArgs(sp_, extraLength + 1));
                    Object* const sp = sp_ - extraLength;
                    fp_ = sp - requiredLength;
                    sp_ = sp;
                } else {
                    callWrongNumberOfArgumentsViolationAfter(this,
                                                             ac_.toClosure()->sourceInfoString(this),
                                                             requiredLength - 1,
                                                             operand.toFixnum());
                }
            } else if (requiredLength == argLength) {
                fp_ = sp_ - argLength;
            } else {
                Object args = Object::Nil;
                for (int i = 0; i < operand.toFixnum(); i++) {
                    args = Object::cons(index(sp_, i), args);
                }
                callWrongNumberOfArgumentsViolationAfter(this,
                                                         ac_.toClosure()->sourceInfoString(this),
                                                         requiredLength,
                                                         operand.toFixnum(),
                                                         args);
            }

            if (c->isJitCompiled()) {
                VM_LOG1("calling ~a\n", c->sourceInfo);
                CProcedure* const cprocedure = c->toCProcedure();
                VM_ASSERT(operand.isFixnum());
                // Since JIT compiled cproc refers to not argv[], but argc, we need to set up fp_.

                // Not same as Cproc, JIT-compile CProc issues RETURN.
                // So we don't use returnCode
                ac_ = cprocedure->call(this, argLength, sp_ - argLength);
                VM_LOG1("called ~a\n", c->sourceInfo);
            } else {
                if (c->maxStack + sp_ >= stackEnd_) {
                    expandStack(stackSize_ / 10);
                }

                if (enableJit_) {
                    tryJitCompile(ac_);
                }
                pc_ = c->pc;
            }
        } else if (ac_.isCallable()) {
            COUNT_CALL(ac_);
            cl_ = ac_;
            Callable* const callable = ac_.toCallable();
            VM_ASSERT(operand.isFixnum());
            const int argc = operand.toFixnum();
            // set pc_ before call() for pointing where to return.
            pc_  = callable->returnCode;
            pc_[0] = Object::makeRaw(labelReturn_);
            pc_[1] = operand;
            ac_ = ac_.toCallable()->call(this, argc, sp_ - argc);
        } else if (ac_.isRegexp()) {
            extern Object rxmatchEx(Object args);
            VM_ASSERT(operand.isFixnum());
            const int argc = operand.toFixnum();
            Object argv[2];
            argv[0] = ac_;
            argv[1] = sp_[-argc];
            const Object rxmatchProc = Object::makeCProcedure(scheme::rxmatchEx);
            CProcedure* const rxmatchCProc = rxmatchProc.toCProcedure();
            // set pc_ before call() for pointing where to return.
            pc_  = rxmatchCProc->returnCode;
            pc_[0] = Object::makeRaw(labelReturn_);
            pc_[1] = operand;
            ac_ = rxmatchCProc->call(this, argc + 1, argv);
        } else if (ac_.isRegMatch()) {
            extern Object regMatchProxy(Object args);
            VM_ASSERT(operand.isFixnum());
            const int argc = operand.toFixnum();
            Object argv[2];
            argv[0] = ac_;
            argv[1] = sp_[-argc];
            const Object regMatchProc = Object::makeCProcedure(scheme::regMatchProxy);
            CProcedure* const regmatchCProc = regMatchProc.toCProcedure();
            pc_  = regmatchCProc->returnCode;
            pc_[0] = Object::makeRaw(labelReturn_);
            pc_[1] = operand;
            ac_ = regmatchCProc->call(this, argc + 1, argv);
        } else if (ac_.isContinuation()) {
            Continuation* const c = ac_.toContinuation();
            const int codeSize = 18;

            // This memory allocation is not time consuming. (I've checked)
            Object* code = Object::makeObjectArray(codeSize);
            code[0] = Object::makeRaw(Instruction::CONSTANT_PUSH);
            code[1] = c->winders();
            code[2] = Object::makeRaw(Instruction::DYNAMIC_WINDERS);
            code[3] = Object::makeRaw(Instruction::BRANCH_NOT_EQ);// (if (not (eq? new (current-dynamic-winders))) perform-dynamic-wind)
            code[4] = Object::makeFixnum(3);
            code[5] = Object::makeRaw(Instruction::LOCAL_JMP);
            code[6] = Object::makeFixnum(8);
            code[7] = Object::makeRaw(Instruction::FRAME);
            code[8] = Object::makeFixnum(6);
            code[9] = Object::makeRaw(Instruction::CONSTANT_PUSH);
            code[10] = c->winders();
            code[11] = Object::makeRaw(Instruction::REFER_GLOBAL_CALL);
            code[12] = Symbol::intern(UC("perform-dynamic-wind"));
            code[13] = Object::makeFixnum(1);
            code[14] = Object::makeRaw(Instruction::RESTORE_CONTINUATION);
            code[15] = operand;         // length of arguments
            code[16] = c->stack();      // stack
            code[17] = c->shiftSize(); // shift size
            Object* nextPc = getDirectThreadedCode(code, codeSize);
            pc_ = nextPc;
        } else {
            callAssertionViolationAfter(this, "apply", "invalid application", L1(ac_));
        }
    }

};


#endif // SCHEME_VM_RUN_

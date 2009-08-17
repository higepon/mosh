/*
 * VM-inl.h -
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
 *  $Id: VM-inl.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_VM_INL_
#define SCHEME_VM_INL_

#include "config.h"
#include "Gloc.h"
#include <sys/time.h>

namespace scheme {

inline void VM::copyJmpBuf(jmp_buf dst, jmp_buf src)
{
    memcpy(dst, src, sizeof(jmp_buf));
}

inline Object VM::fetchOperand()
{
    return *pc_++;
}

inline void VM::skip(int n)
{
    pc_ = pc_ + n ;
}

inline void VM::push(Object obj)
{
    *sp_++ = obj;
}

inline Object VM::stackToPairArgs(Object* sp, int nArgs)
{
    Object args = Object::Nil;
    for (int i = 0; i < nArgs; i++) {
        args = Object::cons(index(sp, i), args);
    }
    return args;
}

inline void VM::setValueSymbol(Object id, Object val)
{
    nameSpace_.toEqHashTable()->set(id, Object::makeGloc(val));
}

inline void VM::setValueString(const ucs4char* id, Object val)
{
    setValueSymbol(Symbol::intern(id), val);
}


inline void VM::pairArgsToStack(Object* sp, int offset, Object args)
{
    if (args.isNil()) {
        indexSet(sp, offset, Object::Nil);
    } else {
        const int length = Pair::length(args);
        for (int i =  length - 1; !(args.isNil()); i--, args = args.cdr()) {
            indexSet(sp, i + offset, args.car());
        }
    }
}

inline void VM::indexSet(Object* sp, int i, Object v)
{
    *(sp - i - 1) = v;
}

inline Object* VM::shiftArgsToBottom(Object* sp, int depth, int diff)
{
    for (int i = depth - 1; i >= 0; i--) {
//          stack_[sp - i - diff - 1] = stack_[sp - i - 1];
        indexSet(sp, i + diff, index(sp, i)); // this is fastest
    }
//        memmove(&stack_[sp - diff - depth], &stack_[sp - depth], depth * sizeof(Object));
    return sp - diff;
}

// depth is not used
//    Object* unShiftArgs(Object* sp, int depth, int diff)
inline Object* VM::unShiftArgs(Object* sp, int diff)
{
    for (int i = 0; i < diff; i++) {
        indexSet(sp + diff - i, 0, index(sp, i));
    }
    return sp + diff;
}


inline Object VM::index(Object* sp, int n) const
{
    return *(sp - n - 1);
}

inline Object VM::pop()
{
    return *(--sp_);
}
inline Object VM::referLocal(int n) const
{
    return *(fp_ + n);
}

inline Object VM::referFree(Object n)
{
    MOSH_ASSERT(n.isFixnum());
    return dc_.toClosure()->referFree(n.toFixnum());
}

inline Object VM::referFree(int n)
{
    return dc_.toClosure()->referFree(n);
}

inline Object VM::makeContinuation(Object n)
{
//     struct timeval tv1, tv2;

//     static uintptr_t msec = 0;
//     static int i = 0;

//     gettimeofday(&tv1, NULL);

//     const int codeSize = 17;
//     Object* code = Object::makeObjectArray(codeSize);
//     code[0] = Object::makeRaw(Instruction::FRAME);
//     code[1] = Object::makeFixnum(6);
//     code[2] = Object::makeRaw(Instruction::CONSTANT_PUSH);
//     code[3] = dynamicWinders();
//     code[4] = Object::makeRaw(Instruction::REFER_GLOBAL_CALL);
//     code[5] = Symbol::intern(UC("perform-dynamic-wind"));
//     code[6] = Object::makeFixnum(1);
//     code[7] = Object::makeRaw(Instruction::REFER_LOCAL);
//     code[8] = Object::makeFixnum(0);
//     code[9] = Object::makeRaw(Instruction::CONTINUATION_VALUES);
//     code[10] = Object::makeRaw(Instruction::RESTORE_CONTINUATION);
//     code[11] = Object::makeStack(stack_, sp_ - stack_);
//     code[12] = Object::makeRaw(Instruction::SHIFT); // shift is issued just after restore.
//     code[13] = Object::makeFixnum(0);
//     code[14] = n;
//     code[15] = Object::makeRaw(Instruction::RETURN);
//     code[16] = Object::makeFixnum(0);
//     Object* c = getDirectThreadedCode(code, codeSize);
//     const Object closure = Object::makeClosure(c, codeSize, 1, true, sp_, 0, 1, Object::False);
//     gettimeofday(&tv2, NULL);
//     msec += (tv2.tv_sec - tv1.tv_sec) * 1000 * 1000 + (tv2.tv_usec - tv1.tv_usec);
//     if (i++ == 100000) {
//         printf("msec=%d\n", msec / 1000);
//     }


    return Object::makeContinuation(Object::makeStack(stack_, sp_ - stack_),
                                    n,
                                    dynamicWinders());
}

inline Object* VM::disasm(Closure* closure)
{
    return disasm(closure->pc - Closure::HEADER_SIZE, closure->size + Closure::HEADER_SIZE);
}

inline Object* VM::disasm(Object* code, int length)
{
#ifdef USE_DIRECT_THREADED_CODE
    Object* ret = Object::makeObjectArray(length);
    void** table = (void**)run(NULL, NULL, true).val;
    for (int i = 0; i < length; i++) {
        const Object c = code[i];
        bool isInsn = false;
        for (int j = 0; j < Instruction::INSTRUCTION_COUNT; j++) {
            if (c.val == (intptr_t)(table[Object::makeInstruction(j).val])) {
                isInsn = true;
                ret[i] = Object::makeInstruction(j);
            }
        }
        if (!isInsn) {
            ret[i] = c;
        }
    }
    return ret;
#else
    return code;
#endif
}

inline Object* VM::getDirectThreadedCode(Object* code, int length)
{
#ifdef USE_DIRECT_THREADED_CODE
    Object* direct = Object::makeObjectArray(length);
    void** table = (void**)run(NULL, NULL, true).val;
    for (int i = 0; i < length; i++) {
        // Direct threaded code
        // Be careful on using direct threaded code for following case.
        //   1. pre-compiled compiler has two types of instructions.
        //      (a) Instruction Object
        //          index for dispatch_table[].
        //          this object will be converted into the address of LABEL_XXX for direct jump.
        //      (b) CompilerInstruction Object
        //          Only pre-compiled compiler has this object.
        //          This object represents the instruction code which the compiler outputs.
        //      For example
        //          If compiler has a scheme code 'PUSH, it is pre-compiled into '(<Instruction::CONSTANT> <CompilerInstruction::PUSH>).
        if (code[i].isCompilerInstruction()) {
            direct[i] = Object::makeInstruction(code[i].toCompilerInstruction());
        } else if (code[i].isPair()) {
            // special case on compiler.scm
            // compiler.scm includes '(0 UNDEF) and UNDEF should be compiled as CompilerInstruction Object.
            for (Object p = code[i]; !p.isNil(); p = p.cdr()) {
                if (p.car().isCompilerInstruction()) {
                    p.toPair()->car = (Object::makeInstruction(p.car().toCompilerInstruction()));
                }
                // p can be dotted list.
                if (!p.cdr().isPair()) {
                    break;
                }
            }
            direct[i] = code[i];
        } else if (code[i].isInstruction()) {
            direct[i].val = (intptr_t)(table[code[i].val]);
        } else {
            direct[i] = code[i];
        }
    }
    return direct;
#else
    return code;
#endif
}

#ifdef ENABLE_PROFILER

inline void VM::countCall(Object proc)
{
    if (isProfiler_ && profilerRunning_) {
        static int i = 0;
        if (i >= SAMPLE_NUM) {
            stopTimer();
            storeCallSample();
            startTimer();
            i = 0;
        }
        callSamples_[i++] = proc;
    }
}

#endif // ENABLE_PROFILER

} // namespace scheme

#endif // SCHEME_VM_INL_

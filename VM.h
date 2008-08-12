/*
 * VM.h - Stack based virtual machine.
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

#ifndef __SCHEME_VM_H__
#define __SCHEME_VM_H__

#include "scheme.h"
#include "Instruction.h"

extern FILE* errOut;

namespace scheme {

#define RAISE0(msg)    raise(msg)
#define RAISE1(fmt, a)    raiseFormat(UC(fmt), L1(a))
#define RAISE2(fmt, a, b) raiseFormat(UC(fmt), L2(a, b))
#define RAISE3(fmt, a, b, c) raiseFormat(UC(fmt), L3(a, b, c))

#define VM_RAISE0(msg )    { if (theVM != NULL) {theVM->raise(Object::makeString(UC(msg)));} }
#define VM_RAISE1(fmt, a)  { if (theVM != NULL) {theVM->raiseFormat(UC(fmt), L1(a));} }
#define VM_RAISE2(fmt, a, b)  { if (theVM != NULL) {theVM->raiseFormat(UC(fmt), L2(a, b));} }
#define VM_RAISE3(fmt, a, b, c)  { if (theVM != NULL) {theVM->raiseFormat(UC(fmt), L3(a, b, c));} }

#define LOG1(fmt, a)       errorPort_.format(UC(fmt), L1(a))
#define LOG2(fmt, a, b)    errorPort_.format(UC(fmt), L2(a, b))

#define VM_LOG1(fmt, a)    theVM->getErrorPort().format(UC(fmt), L1(a))
#define VM_LOG2(fmt, a, b)    theVM->getErrorPort().format(UC(fmt), L2(a, b))

#ifdef TRACE_INSN
#define TRACE_INSN0(name) errorPort_.format(UC("=========================\n~a\n"), L1(name)), fflush(errOut)
#define TRACE_INSN1(name, fmt, a) TRACE_INSN0(name),errorPort_.format(UC(fmt), L1(a)), fflush(errOut)
#define TRACE_INSN2(name, fmt, a, b) TRACE_INSN0(name),errorPort_.format(UC(fmt), L2(a, b)), fflush(errOut)
#define TRACE_INSN3(name, fmt, a, b, c) TRACE_INSN0(name),errorPort_.format(UC(fmt), L3(a, b, c)), fflush(errOut)
#else
#define TRACE_INSN0(name) //
#define TRACE_INSN1(name, fmt, a) //
#define TRACE_INSN2(name, fmt, a, b) //
#define TRACE_INSN3(name, fmt, a, b, c) //
#endif

inline Object L1(Object a) { return Pair::list1(a); }
inline Object L2(Object a, Object b) { return Pair::list2(a, b); }
inline Object L3(Object a, Object b, Object c) { return Pair::list3(a, b, c); }
inline Object L4(Object a, Object b, Object c, Object d) { return Pair::list4(a, b, c, d); }

struct Values {
    Object* values;
    Object val;
    int num;
};

    class TextualOutputPort;

class VM EXTEND_GC
{
public:
    VM(int stackSize, TextualOutputPort& outPort, TextualOutputPort& errorPort, Object inputPort, bool isProfiler = false);
    ~VM();

    void importTopLevel();

    void copyJmpBuf(jmp_buf dst, jmp_buf src)
    {
        memcpy(dst, src, sizeof(jmp_buf));
    }

    void collectProfile();

    Object values(int num, const Object* v);
    Object run(Object* code, jmp_buf returnPoint, bool returnTable = false);
    Object evaluate(Object* o, int codeSize);
    Object evaluate(Object codeVector);
    Object eval(Object o, Object env);
    Object compile(Object o);
    Object callClosureByName(Object procSymbol, Object o);
    Object callClosure(Object closure, Object o);
    Object callClosure0(Object closure);
    Object callClosure2(Object closure, Object arg1, Object arg2);
    Object callClosure3(Object closure, Object arg1, Object arg2, Object arg3);
    Object applyClosure(Object closure, Object args);
    Object applyClosureDebug(Object closure, Object args, int returnSize);
    Object apply(Object proc, Object args);
    void load(const ucs4string& file);
    void loadFile(const ucs4string& file);
    Object withExceptionHandler(Object handler, Object thunk);
    void defaultExceptionHandler(Object error);

//     Values fetchValues()
//     {
//         Values v;
//         v.values = new Object[numValues_ - 1];
//         for (int i = 0; i < numValues_ - 1; i++) {
//             v.values[i] = values_[i];
//         }
//         v.val = ac_;
//         v.num = numValues_;
//         return v;
//     }
//     void restoreValues(Values v)
//     {
//         for (int i = 0; i < v.num; i++) {
//             values_[i] = v.values[i];
//         }
//         ac_ = v.val;
//         numValues_ = v.num;
//     }

    void initLibraryTable();
    void raiseFormat(const ucs4char* fmt, Object list);
    void raise(Object o);
    Object raiseContinuable(Object o);
    TextualOutputPort& getOutputPort() { return outputPort_; }
    TextualOutputPort& getErrorPort() { return errorPort_; }

    Object getStackTrace();

    void setOutputPort(TextualOutputPort& port);
    void setInputPort(Object port ) { inputPort_ = port; }
    Object standardInputPort() const { return stdinPort_; }

    Object currentInputPort() { return inputPort_; }


    void defineGlobal(Object id, Object val)
    {
        const Object found = nameSpace_.toEqHashTable()->ref(id, notFound_);
        if (found == notFound_) {
            nameSpace_.toEqHashTable()->set(id, val);
        } else {
            Object e = splitId(id);
            RAISE2("~a on library ~a : defined twice\n", e.cdr(), e.car());
        }
    }

    Object idToTopLevelSymbol(Object id)
    {
        ucs4string name(UC("top level :$:"));
        name += id.toSymbol()->c_str();
        // don't use name variable directly, it is temporary!
        return Symbol::intern(Object::makeString(name).toString()->data().c_str());
    }

    void setTopLevelGlobalValue(Object id, Object val)
    {
        const Object key = idToTopLevelSymbol(id);
        nameSpace_.toEqHashTable()->set(idToTopLevelSymbol(id), val);
    }


    Object getTopLevelGlobalValue(Object id)
    {
        const Object key = idToTopLevelSymbol(id);
        const Object val = nameSpace_.toEqHashTable()->ref(key, notFound_);
        if (val != notFound_) {
            return val;
        } else {
            RAISE1("unbound variable ~a on symbol-values", id);
            return Object::Undef;
        }
    }

    Object getTopLevelGlobalValueOrFalse(Object id)
    {
        const Object key = idToTopLevelSymbol(id);
        const Object val = nameSpace_.toEqHashTable()->ref(key, notFound_);
        if (val != notFound_) {
            return val;
        } else {
            return Object::False;
        }
    }


    Object getClosureName(Object closure);

#ifdef ENABLE_PROFILER
    // Profiler
    void initProfiler();
    void stopProfiler();
    void startTimer();
    void stopTimer();
    void collecProfile();
    Object getProfileResult();
    void storeCallSample();
    void storeCallSampleToFile();
    Object getCProcedureName(Object proc);
    void countCall(Object proc)
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
#endif

protected:



    void import(Object libname)
    {
        EqHashTable* const ht = instances_.toEqHashTable();
        if (ht->ref(libname, Object::False).isFalse()) {
            ht->set(libname, Object::makeEqHashTable());
        }
    }

    Object fetchOperand() { return *pc_++; }

    void skip(int n) { pc_ = pc_ + n ; }

    void push(Object obj)
    {
        *sp_++ = obj;
    }

    void pushWithCheck(Object obj)
    {
        if (sp_ > maxStack_) {
            maxStack_ = sp_;
        }
        *sp_++ = obj;
    }


    Object splitId(Object id);

    Object stackToPairArgs(Object* sp, int nArgs)
    {
        Object args = Object::Nil;
        for (int i = 0; i < nArgs; i++) {
            args = Object::cons(index(sp, i), args);
        }
        return args;
    }

    void pairArgsToStack(Object* sp, int offset, Object args)
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

    void indexSet(Object* sp, int i, Object v)
    {
        *(sp - i - 1) = v;
    }

    Object* shiftArgsToBottom(Object* sp, int depth, int diff)
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
    Object* unShiftArgs(Object* sp, int diff)
    {
        for (int i = 0; i < diff; i++) {
            indexSet(sp + diff - i, 0, index(sp, i));
        }
        return sp + diff;
    }


    Object index(Object* sp, int n)
    {
        return *(sp - n - 1);
    }

    // あとで
    Object referLocal(int n)
    {
        return index(fp_ + n + 1, 0);
    }

    Object referFree(Object n)
    {
        return dc_.toClosure()->referFree(n.toInt());
    }

    Object referFree(int n)
    {
        return dc_.toClosure()->referFree(n);
    }


    Object makeContinuation(Object n)
    {
        const int codeSize = 6;
        Object* code = Object::makeObjectArray(codeSize);
        code[0] = Object::makeRaw(Instruction::REFER_LOCAL);
        code[1] = Object::makeInt(0);
        code[2] = Object::makeRaw(Instruction::RESTORE_CONTINUATION);
        code[3] = Object::makeStack(stack_, sp_ - stack_);
        code[4] = Object::makeRaw(Instruction::RETURN);
        code[5] = n;
        return Object::makeClosure(getDirectThreadedCode(code, codeSize), 1, false, sp_, 0, 1, Object::False);
    }

    Object* getDirectThreadedCode(Object* code, int length)
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
                direct[i].val = (word)(table[code[i].val]);
            } else {
                direct[i] = code[i];
            }
        }
        return direct;
#else
        return code;
#endif
    }

    // $library structure accessor.
    // This structure is shared with compiler and VM.
    void setLibraryMacro(Object library, Object macro) { library.toVector()->set(5, macro); }
    Object getLibraryCompiledBody(Object library) { return library.toVector()->ref(7); }

public:
    Object ac_;  // accumulator     register
    Object dc_;  // display closure register, used for refer-free
    Object cl_;  // current closure register, used for profiler.
    Object* fp_; // frame pointer   register
    Object* sp_; // stack pointer   register
    Object* pc_; // program counter register
protected:
    const int stackSize_;
    Object* stack_;
    Object* stackEnd_;
    Object* maxStack_;
    Object topLevelInstance_;
    Object instances_;
    Object libraries_;
    Object nameSpace_;
    Object notFound_;
    TextualOutputPort& outputPort_;
    TextualOutputPort& errorPort_;
    Object inputPort_;
    Object stdinPort_;
    Object errorObj_;
    Object errorHandler_;
    Object returnCode_[2];
    Object outerSourceInfo_;
#ifdef ENABLE_PROFILER
    word labelReturn_;           // for profiler
    static const int SAMPLE_NUM; // for profiler
    Object* samples_;            // for profiler
    Object* callSamples_;        // for profiler
    Object callHash_;            // for profiler
    int totalSampleCount_;       // for profiler
    bool profilerRunning_;       // for profiler
#endif
    const bool isProfiler_;      // for profiler
    const int maxNumValues_;
    int numValues_;
    Object* values_;
    jmp_buf returnPoint_;
};

}; // namespace scheme

#endif // __SCHEME_VM_H__

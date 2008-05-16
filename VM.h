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
 *  $Id: VM.h 5322 2008-05-09 06:38:11Z higepon $
 */

#ifndef __SCHEME_VM_H__
#define __SCHEME_VM_H__

#include "scheme.h"
#include "Instruction.h"

namespace scheme {

#define RAISE1(fmt, a)    raise(UC(fmt), L1(a))
#define RAISE2(fmt, a, b) raise(UC(fmt), L2(a, b))
#define RAISE3(fmt, a, b, c) raise(UC(fmt), L3(a, b, c))

#define VM_RAISE1(fmt, a)  { if (theVM != NULL) {theVM->raise(UC(fmt), L1(a));} }
#define VM_RAISE2(fmt, a, b)  { if (theVM != NULL) {theVM->raise(UC(fmt), L2(a, b));} }
#define VM_RAISE3(fmt, a, b, c)  { if (theVM != NULL) {theVM->raise(UC(fmt), L3(a, b, c));} }

#define LOG1(fmt, a)       errorPort_.format(UC(fmt), L1(a))
#define LOG2(fmt, a, b)    errorPort_.format(UC(fmt), L2(a, b))

#define VM_LOG1(fmt, a)    theVM->getErrorPort().format(UC(fmt), L1(a))
#define VM_LOG2(fmt, a, b)    theVM->getErrorPort().format(UC(fmt), L2(a, b))

#if 0
#define TRACE_INSN0(name) errorPort_.format(UC("=========================\n~a\n"), L1(name))
#define TRACE_INSN1(name, fmt, a) TRACE_INSN0(name),errorPort_.format(UC(fmt), L1(a))
#define TRACE_INSN2(name, fmt, a, b) TRACE_INSN0(name),errorPort_.format(UC(fmt), L2(a, b))
#define TRACE_INSN3(name, fmt, a, b, c) TRACE_INSN0(name),errorPort_.format(UC(fmt), L3(a, b, c))
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

class VM EXTEND_GC
{
public:
    VM(int stackSize, TextualOutputPort& outPort, TextualOutputPort& errorPort, Object inputPort);
    ~VM();

    void importTopLevel();

    Object run(Object* code, bool returnTable = false);
    Object evaluate(Object* o, int codeSize);
    Object evaluate(Object codeVector);
    Object eval(Object o, Object env);
    Object compile(Object o);
    Object callClosureByName(Object procSymbol, Object o);
    Object callClosure(Object closure, Object o);
    Object applyClosure(Object closure, Object args);
    Object apply(Object proc, Object args);
    void load(const ucs4string& file);
    void loadFile(const ucs4string& file);

    void initLibraryTable();
    void raise(const ucs4char* fmt, Object list);
    TextualOutputPort& getOutputPort() { return outputPort_; }
    TextualOutputPort& getErrorPort() { return errorPort_; }

    void showStackTrace(Object errorMessage);

    void setOutputPort(TextualOutputPort& port) { outputPort_ = port; }

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

    Object splitId(Object id);

// (define (stack->pair-args stack sp num-args)
//   (let loop ([n (- num-args 1)])
//     (if (>= n 0)
//         (cons (index stack sp n) (loop (- n 1)) )
//         '())))


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
            for (int i = Pair::length(args) - 1; !(args.isNil()); i--, args = args.cdr()) {
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
        return cl_.toClosure()->referFree(n.toInt());
    }

    Object referFree(int n)
    {
        return cl_.toClosure()->referFree(n);
    }


    Object makeContinuation(Object n)
    {
        const int codeSize = 6;
#ifdef USE_BOEHM_GC
        Object* code = new(GC) Object[codeSize];
#else
        Object* code = new Object[codeSize];
#endif
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
        Object* direct = new(GC) Object[length];
        void** table = (void**)run(NULL, true).val;
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
    Object cl_;  // current closure register
    Object* fp_; // frame pointer   register
    Object* sp_; // stack pointer   register
    Object* pc_; // program counter register
protected:

    const int stackSize_;
    Object* stack_;
    Object* stackEnd_;
    Object topLevelInstance_;
    Object instances_;
    Object libraries_;
    Object nameSpace_;
    Object notFound_;
    TextualOutputPort& outputPort_;
    TextualOutputPort& errorPort_;
    Object inputPort_;
    Object stdinPort_;
    void* labelStart_;
    void* labelEnd_;
};

}; // namespace scheme

#endif // __SCHEME_VM_H__

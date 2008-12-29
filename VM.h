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
#include <setjmp.h>
#include "Instruction.h"
//#include "Pair.h"

extern FILE* errOut;

namespace scheme {

#define LOG1(fmt, a)       errorPort_.toTextualOutputPort()->format(UC(fmt), L1(a))
#define LOG2(fmt, a, b)    errorPort_.toTextualOutputPort()->format(UC(fmt), L2(a, b))
#define LOG3(fmt, a, b, c)    errorPort_.toTextualOutputPort()->format(UC(fmt), L3(a, b, c))

#define VM_LOG1(fmt, a)    theVM->getErrorPort().toTextualOutputPort()->format(UC(fmt), L1(a))
#define VM_LOG2(fmt, a, b)    theVM->getErrorPort().toTextualOutputPort()->format(UC(fmt), L2(a, b))
#define VM_LOG3(fmt, a, b, c)    theVM->getErrorPort().toTextualOutputPort()->format(UC(fmt), L3(a, b, c))

#define L1(a) Pair::list1(a)
#define L2(a, b) Pair::list2(a, b)
#define L3(a, b, c) Pair::list3(a, b, c)
#define L4(a, b, c, d) Pair::list4(a, b, c, d)

#define SAVE_REGISTERS()                       \
    const Object ac = ac_;                     \
    const Object dc = dc_;                     \
    const Object cl = cl_;                     \
    const Object errorHandler = errorHandler_; \
    Object* const pc = pc_;                    \
    Object* const fp = fp_;                    \
    Object* const sp = sp_;

#define RESTORE_REGISTERS()       \
    ac_ = ac;                     \
    cl_ = cl;                     \
    dc_ = dc;                     \
    errorHandler_ = errorHandler; \
    fp_ = fp;                     \
    pc_ = pc;                     \
    sp_ = sp;

#define FASL_GET(image) FaslReader(new ByteArrayBinaryInputPort(image, sizeof(image))).get()



#ifdef DEBUG_VERSION
#define VM_ASSERT(condition) { if (!(condition)) { \
            fprintf(stderr, "*** ASSERT failure %s:%d: %s\n", __FILE__, __LINE__, #condition); \
            LOG2("    dc_ = ~a\n    cl_=~a\n", \
                 dc_.toClosure()->sourceInfoString(), \
                 cl_.toClosure()->sourceInfoString()); \
                 ::exit(-1);}}
#else
#define VM_ASSERT(condition) /* */
#endif

class TextualOutputPort;

class VM EXTEND_GC
{
public:
    VM(int stackSize, Object outPort, Object errorPort, Object inputPort, bool isProfiler = false);
    virtual ~VM();

    Object getLastError() const { return errorObj_; }
    void loadCompiler();
    void importTopLevel();
    void dumpCompiledCode(Object code) const;
    void printStack() const;
    void copyJmpBuf(jmp_buf dst, jmp_buf src);
    void collectProfile();

    Object values(int num, const Object* v);
    Object run(Object* code, jmp_buf returnPoint, bool returnTable = false);
    Object evaluate(Object* o, int codeSize);
    Object evaluate(Object codeVector);
//    Object eval(Object o, Object env);
    Object compile(Object o);
    Object callClosureByName(Object procSymbol, Object o);
    Object callClosure1(Object closure, Object o);
    Object callClosure0(Object closure);
    Object callClosure2(Object closure, Object arg1, Object arg2);
    Object callClosure3(Object closure, Object arg1, Object arg2, Object arg3);
    Object setAfterTrigger1(Object closure, Object arg1);
    Object setAfterTrigger0(Object closure);
    Object evalAfter(Object sexp);
    void applyClosure(Object closure, Object args);
    Object apply(Object proc, Object args);
    void load(const ucs4string& file);
    void loadFile(const ucs4string& file);
    void defaultExceptionHandler(Object error);
    void showStack(int count, const char* file, int line);
#define SHOW_STACK(count) showStack(count, __FILE__, __LINE__)

    void throwException(Object exception);
    Object getOutputPort();
    Object getErrorPort();
    Object getStackTrace();
    void setOutputPort(Object port);
    void setInputPort(Object port);
    Object standardInputPort() const;
    Object currentInputPort();
    Object idToTopLevelSymbol(Object id);
    void setTopLevelGlobalValue(Object id, Object val);
    Object getTopLevelGlobalValue(Object id);
    Object getTopLevelGlobalValueOrFalse(Object id);
    Object getClosureName(Object closure);
    bool isR6RSMode() const;
    void activateR6RSMode(bool isDebugExpand);
    Object* disasm(Object* code, int length);
    Object* disasm(Closure* closure);
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
    void countCall(Object proc);
#endif

protected:
    virtual int exit(int status)
    {
        ::exit(status);
        return status;
    }
    void import(Object libname);
    Object fetchOperand();
    void skip(int n);
    void push(Object obj);
    void pushWithCheck(Object obj);
    Object stackToPairArgs(Object* sp, int nArgs);
    void pairArgsToStack(Object* sp, int offset, Object args);
    void indexSet(Object* sp, int i, Object v);
    Object* shiftArgsToBottom(Object* sp, int depth, int diff);
    Object* unShiftArgs(Object* sp, int diff);
    Object index(Object* sp, int n) const;
    Object pop();
    Object referLocal(int n) const;
    Object referFree(Object n);
    Object referFree(int n);
    Object makeContinuation(Object n);
    Object* getDirectThreadedCode(Object* code, int length);
    void expandStack(int plusSize);
    Object compileWithoutHalt(Object sexp);

public:
    Object ac_;  // accumulator     register
    Object dc_;  // display closure register, used for refer-free
    Object cl_;  // current closure register, used for profiler.
    Object* fp_; // frame pointer   register
    Object* sp_; // stack pointer   register
    Object* pc_; // program counter register
protected:
    int stackSize_;
    Object* stack_;
    Object* stackEnd_;
    Object* maxStack_;
    Object nameSpace_;
    Object notFound_;
    Object outputPort_;
    Object errorPort_;
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
    bool isR6RSMode_;
};

}; // namespace scheme

#endif // __SCHEME_VM_H__

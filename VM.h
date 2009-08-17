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

#ifndef SCHEME_VM_H_
#define SCHEME_VM_H_

#include "scheme.h"
#include <setjmp.h>
#include "Instruction.h"
#include "EqHashTable.h"

#ifdef _MSC_VER
#define snprintf _snprintf
#endif
namespace scheme {

typedef gc_vector<Object> Ports;

#define L1(a) Pair::list1(a)
#define L2(a, b) Pair::list2(a, b)
#define L3(a, b, c) Pair::list3(a, b, c)
#define L4(a, b, c, d) Pair::list4(a, b, c, d)

#define SAVE_REGISTERS()                       \
    const Object ac = ac_;                     \
    const Object dc = dc_;                     \
    const Object cl = cl_;                     \
    Object* const pc = pc_;                    \
    Object* const fp = fp_;                    \
    Object* const sp = sp_;

#define RESTORE_REGISTERS()       \
    ac_ = ac;                     \
    cl_ = cl;                     \
    dc_ = dc;                     \
    fp_ = fp;                     \
    pc_ = pc;                     \
    sp_ = sp;

#define FASL_GET(image) FaslReader(this, new ByteArrayBinaryInputPort(image, sizeof(image))).get()

#ifdef DEBUG_VERSION
#define VM_ASSERT(condition) { if (!(condition)) { \
            fprintf(stderr, "*** ASSERT failure %s:%d: %s\n", __FILE__, __LINE__, #condition); \
            VM_LOG2("    dc_ = ~a\n    cl_=~a\n", \
                 dc_.toClosure()->sourceInfoString(this), \
                 cl_.toClosure()->sourceInfoString(this)); \
                 ::exit(-1);}}

#else
#define VM_ASSERT(condition) /* */
#endif

class TextualOutputPort;
class Thread;
class ReaderContext;
class NumberReaderContext;

class VM EXTEND_GC
{
public:
    VM(int stackSize, Object outPort, Object errorPort, Object inputPort, bool isProfiler = false);
    virtual ~VM();

    bool isMainThread() const
    {
        return NULL == thread_;
    }

    ucs4string toString() const
    {
        ucs4string ret = UC("#<vm ");
        ret += name_;
        char buf[32];
        snprintf(buf, sizeof(buf), " %lx", (uintptr_t)this);
        ret += ucs4string::from_c_str(buf);
        ret += UC(">");
        return ret;
    }
    void setName(const ucs4string& name) { name_ = name; }
#ifdef _MSC_VER
    uint32_t getErrno() const { return errno_; }
    void setErrno(uint32_t e) { errno_ = e; }
#else
    int getErrno() const { return errno_; }
    void setErrno(int e) { errno_ = e; }
#endif
    Object getLastError() const { return errorObj_; }
    void loadCompiler();
    void dumpCompiledCode(Object code) const;
    void printStack() const;
    void copyJmpBuf(jmp_buf dst, jmp_buf src);
    void collectProfile();
    void setThread(Thread* thread);
    Thread* thread();

    Object values(int num, const Object* v);
    Object values2(Object obj1, Object obj2);
    Object values3(Object obj1, Object obj2, Object obj3);
    Object run(Object* code, jmp_buf returnPoint, bool returnTable = false);
    Object evaluate(Object* o, int codeSize);
    Object evaluateCodeVector(Object codeVector);
    Object compile(Object o);
    Object callClosureByName(Object procSymbol, Object o);
    Object callClosure1(Object closure, Object o);
    Object callClosure0(Object closure);
    Object callClosure2(Object closure, Object arg1, Object arg2);
    Object callClosure3(Object closure, Object arg1, Object arg2, Object arg3);
    Object setAfterTrigger1(Object closure, Object arg1);
    Object setAfterTrigger0(Object closure);
    Object evalAfter(Object sexp);
    Object evalCompiledAfter(Object code);
    void applyClosure(Object closure, Object args);
    Object apply(Object proc, Object args);
    void loadFileWithGuard(const ucs4string& file);
    void loadFileUnsafe(const ucs4string& file);

    void defaultExceptionHandler(Object error);
    void showStack(int count, const char* file, int line);
#define SHOW_STACK(count) showStack(count, __FILE__, __LINE__)

    void throwException(Object exception);
    Object currentOutputPort() const;
    Object currentErrorPort() const;
    Object currentInputPort() const;
    Object getStackTrace();
    void setCurrentOutputPort(Object port);
    void setCurrentInputPort(Object port);

    Object idToTopLevelSymbol(Object id);
    void setValueSymbol(Object id, Object val);
    void setValueString(const ucs4char* id, Object val);
    Object getTopLevelGlobalValue(Object id);
    Object getTopLevelGlobalValueOrFalse(Object id);
    bool isR6RSMode() const;
    Object activateR6RSMode(bool isDebugExpand);
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
    void countCall(Object proc);
#endif
    Object getClosureName(Object closure);
    Object getCProcedureName(Object proc) const;
    void registerPort(Object obj);
    void unregisterPort(Object obj);
    virtual void flushAllPorts();

    Object findGenerativeRtd(Object uid);
    void addGenerativeRtd(Object uid, Object rtd);

    ReaderContext* readerContext() { return readerContext_; }
    NumberReaderContext* numberReaderContext() { return numberReaderContext_; }

    void setDynamicWinders(Object winders)
    {
        dynamicWinders_ = winders;
    }

    Object dynamicWinders() const
    {
        return dynamicWinders_;
    }

protected:
    virtual int exit(int status)
    {
        flushAllPorts();
        ::exit(status);
        return status;
    }
    Object fetchOperand();
    void skip(int n);
    void push(Object obj);
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
    bool mayBeStackPointer(Object* obj) const;

private:
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
    Object currentOutputPort_;
    Object currentErrorPort_;
    Object currentInputPort_;
    Object errorObj_;
    Object returnCode_[2];
    Object outerSourceInfo_;
#ifdef ENABLE_PROFILER
    intptr_t labelReturn_;           // for profiler
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
    Ports activePorts_;
    ucs4string name_;
    Thread* thread_;
    ObjectMap generativeRtds_;
    Object* cProcs_;

    // on the fly instructions array.
    Object closureForEvaluate_;
    Object closureForApply_;

    Object* applyCodeForCallClosure0_;
    int applyCodeForCallClosure0Length_;

    Object* applyCodeForCallClosure1_;
    int applyCodeForCallClosure1Length_;

    Object* applyCodeForCallClosure2_;
    int applyCodeForCallClosure2Length_;

    Object* applyCodeForCallClosure3_;
    int applyCodeForCallClosure3Length_;

    Object* callCodeForSetAfterTrigger0_;
    int callCodeForSetAfterTrigger0Length_;

    Object* callCodeForSetAfterTrigger1_;
    int callCodeForSetAfterTrigger1Length_;

    Object* applyCodeForApplyClosure_;
    int applyCodeForApplyClosureLength_;

    Object* applyCodeForCallClosureByName_;
    int applyCodeForCallClosureByNameLength_;

    Object* callCode_;
    int callCodeLength_;
    ReaderContext* readerContext_;
    NumberReaderContext* numberReaderContext_;
#if _MSC_VER
    uint32_t errno_;
#else
    int errno_;
#endif
    Object dynamicWinders_;
};

} // namespace scheme

#endif // SCHEME_VM_H_

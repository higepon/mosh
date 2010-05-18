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
#include "JitStack.h"
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
class Code;

class VM EXTEND_GC
{
public:
    VM(int stackSize, Object outPort, Object errorPort, Object inputPort, bool isProfiler = false, bool enableJit = false);
    virtual ~VM();

    static void copyOptions(VM* destVM, VM* srcVM);


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
    void debugPrintState()
    {
        printf("sp=%p", sp_);
        printf("fp=%p", fp_);
    }

    Object values(int num, const Object* v);
    Object values2(Object obj1, Object obj2);
    Object values3(Object obj1, Object obj2, Object obj3);


    Object compile(Object o);
    Object callClosureByName(Object procSymbol, Object o);
    Object callClosure1(Object closure, Object o);
    Object callClosure0(Object closure);
    Object callClosure2(Object closure, Object arg1, Object arg2);
    Object callClosure3(Object closure, Object arg1, Object arg2, Object arg3);
    Object setAfterTrigger4(Object closure, Object arg1, Object arg2, Object arg3, Object arg4);
    Object setAfterTrigger3(Object closure, Object arg1, Object arg2, Object arg3);
    Object setAfterTrigger2(Object closure, Object arg1, Object arg2);
    Object setAfterTrigger1(Object closure, Object arg1);
    Object setAfterTrigger0(Object closure);
    Object evalAfter(Object sexp);
    Object evalCompiledAfter(Object code);
    void applyClosure(Object closure, Object args);
    Object vmapply(Object proc, Object args);
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
    Object getGlobalValue(Object id);
    Object getGlobalValueOrFalse(Object id);
    Object getGlobalValueOrFalse(const ucs4char* id);
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

    // returns uid
    uintptr_t registerCallBackTrampoline(Object closure)
    {
        uintptr_t uid = callBackTrampolinesUid_++;
        callBackTrampolines_->set(Object::makeFixnum(uid),
                                  closure);
        return uid;
    }

    void unregisterCallBackTrampoline(uintptr_t uid)
    {
        callBackTrampolines_->deleteD(Object::makeFixnum(uid));
    }


    Object getCallBackTrampoline(uintptr_t uid)
    {
        return callBackTrampolines_->ref(Object::makeFixnum(uid), Object::False);
    }

    JitStack* jitStack()
    {
        return &jitStack_;
    }

    void tryJitCompile(Object closure);
    void tryInvokeJitLibrary();
    void raiseNotPairErrorForJit(int op);
    void raiseVectorRequiredError(int op, Object obj);
    void raiseVectorInvalidIndexError(int op);
    void callOp(Object operand);
    void numberAddOp();
    void numberSubOp();
    Object call(Object n);
    Object tailCall(Object n, Object diff);

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
    void expandStack(int plusSize);
    Object compileWithoutHalt(Object sexp);
    bool mayBeStackPointer(Object* obj) const;
    void** getDispatchTable()
    {
        return (void**)runLoop(NULL, NULL, true).val;
    }

public:
    // DONT change the order or offset.
    // JIT compiler depends on them.
    Object ac_;  // accumulator     register
    Object dc_;  // display closure register, used for refer-free
    Object cl_;  // current closure register, used for profiler.
    Object* fp_; // frame pointer   register
    Object* sp_; // stack pointer   register
    Object* pc_; // program counter register
    int numValues_;
    Object* values_;

protected:

    int stackSize_;
    Object* stack_;
    Object* stackEnd_;
    Object nameSpace_;
    Object notFound_;
    Object currentOutputPort_;
    Object currentErrorPort_;
    Object currentInputPort_;
    Object errorObj_;
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
    jmp_buf returnPoint_;
    bool isR6RSMode_;
    Ports activePorts_;
    ucs4string name_;
    Thread* thread_;
    Object* cProcs_;

    // on the fly instructions array.
    Object closureForEvaluate_;
    Object closureForApply_;

    Code* applyCode_;
    Code* callClosure0Code_;
    Code* callClosure1Code_;
    Code* callClosure2Code_;
    Code* callClosure3Code_;
    Code* trigger0Code_;
    Code* trigger1Code_;
    Code* trigger2Code_;
    Code* trigger3Code_;
    Code* trigger4Code_;

    Code* applyClosureCode_;
    Code* callClosureByNameCode_;
    Code* callCode_;
    Code* haltCode_;

    ReaderContext* readerContext_;
    NumberReaderContext* numberReaderContext_;
#if _MSC_VER
    uint32_t errno_;
#else
    int errno_;
#endif
    Object dynamicWinders_;
    EqHashTable* callBackTrampolines_;
    uintptr_t callBackTrampolinesUid_;
    JitStack jitStack_;
    bool isJitLibraryLoading_;
    bool isJitCompiling_;
    bool enableJit_;
    Object jitCompiler_;

private:
    enum
    {
        SIZE_OF_FRAME = 4
    };
    typedef struct Registers
    {
        Object ac;
        Object dc;
        Object cl;
        Object* pc;
        int spOffset;
        int fpOffset;
    } Registers;

    void saveRegisters(Registers* r)
    {
        r->ac = ac_;
        r->dc = dc_;
        r->cl = cl_;
        r->pc = pc_;
        r->spOffset = sp_ - stack_;
        r->fpOffset = fp_ - stack_;
    }

    void restoreRegisters(Registers* r)
    {
        ac_ = r->ac;
        dc_ = r->dc;
        cl_ = r->cl;
        pc_ = r->pc;
        sp_ = stack_ + r->spOffset;
        fp_ = stack_ + r->fpOffset;
    }

    void initializeDynamicCode();
    Object evaluateSafe(Object* code, int codeSize, bool isCompiler = false);
    Object evaluateSafe(Code* code);
    Object evaluateSafe(Vector* code);
    Object evaluateUnsafe(Object* code, int codeSize, bool isCompiler = false);
    Object evaluateUnsafe(Vector* code, bool isCompiler = false);

    void makeCallFrame(Object* pc)
    {
        const Object p = Object::makeObjectPointer(pc);
        push(p);
        push(dc_);
        push(cl_);
        push(Object::makeObjectPointer(fp_));
    }

    Object* getDirectThreadedCode(const Object* code, int length, bool isCompiler = false);
    Object runLoop(Object* code, jmp_buf returnPoint, bool returnTable = false);

};

} // namespace scheme

#endif // SCHEME_VM_H_

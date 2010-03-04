/*
 * VM.cpp - Virtual stack based machine.
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

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "EqHashTable.h"
#include "VM.h"
#include "Closure.h"
#include "EqHashTable.h"
#include "Symbol.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "CompilerProcedures.h"
#include "HashTableProceduures.h"
#include "RecordProcedures.h"
#include "StringProcedures.h"
#include "PortProcedures.h"
#include "ConditionProcedures.h"
#include "ErrorProcedures.h"
#include "ListProcedures.h"
#include "ArithmeticProcedures.h"
#include "FlonumProcedures.h"
#include "BitwiseProcedures.h"
#include "ProcessProcedures.h"
#include "ByteVectorProcedures.h"
#include "FFIProcedures.h"
#include "Record.h"
#include "Codec.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Equivalent.h"
#include "TextualOutputPort.h"
#include "TextualInputPort.h"
#include "Vector.h"
#include "SString.h"
#include "CProcedure.h"
#include "Box.h"
#include "UtilityProcedures.h"
#include "RegexpProcedures.h"
#include "FixnumProcedures.h"
#include "SocketProcedures.h"
#include "MultiVMProcedures.h"
#include "CompilerProcedures.h"
#include "Bignum.h"
#include "ByteArrayBinaryInputPort.h"
#include "Symbol.h"
#include "SimpleStruct.h"
#include "FaslReader.h"
#include "Gloc.h"
#include "OSCompat.h"
#include "BinaryOutputPort.h"
#include "BinaryInputOutputPort.h"
#include "TranscodedTextualInputOutputPort.h"
#include "Scanner.h"
#include "Reader.h"
#include "NumberReader.h"
#include "Time.h"
#include "Code.h"

#define TRY_VM  sigjmp_buf org;                     \
                copyJmpBuf(org, returnPoint_);   \
                if (sigsetjmp(returnPoint_, 1) == 0)  \

#define CATCH_VM copyJmpBuf(returnPoint_, org); \
                 } else {

using namespace scheme;

#include "cprocedures.cpp"

VM::VM(int stackSize, Object outPort, Object errorPort, Object inputPort, bool isProfiler, bool enableJit) :
    ac_(Object::Nil),
    dc_(Object::Nil),
    cl_(Object::Nil),
    pc_(NULL),
    numValues_(0),
    stackSize_(stackSize),
    currentOutputPort_(outPort),
    currentErrorPort_(errorPort),
    currentInputPort_(inputPort),
    errorObj_(Object::Nil),
#ifdef ENABLE_PROFILER
    profilerRunning_(false),
#endif
    isProfiler_(isProfiler),
    maxNumValues_(256),
    isR6RSMode_(false),
    name_(UC("")),
    thread_(NULL),
    readerContext_(new ReaderContext),
    numberReaderContext_(new NumberReaderContext),
    errno_(0),
    dynamicWinders_(Object::Nil),
    callBackTrampolines_(new EqHashTable),
    callBackTrampolinesUid_(0),
    isJitLibraryLoading_(false),
    isJitCompiling_(false),
    enableJit_(enableJit),
    jitCompiler_(Object::False)
{
    stack_ = Object::makeObjectArray(stackSize);
    values_ = Object::makeObjectArray(maxNumValues_);
    stackEnd_ = stack_ + stackSize;
    sp_ = stack_;
    fp_ = stack_;
    nameSpace_ = Object::makeEqHashTable();
    outerSourceInfo_   = L2(Object::False, Symbol::intern(UC("<top-level>")));
    cProcs_ = Object::makeObjectArray(cProcNum);
    for (int i = 0; i < cProcNum; i++) {
        cProcs_[i] = Object::makeCProcedure(cProcFunctions[i]);
    }

    // initialize "On the fly" instructions array
    closureForEvaluate_ = Object::makeClosure(NULL, 0, 0, false, cProcs_, cProcNum, 0, outerSourceInfo_);
    closureForApply_ = Object::makeClosure(NULL, 0, 0, false, cProcs_, cProcNum, 1, outerSourceInfo_);;

    initializeDynamicCode();
}

VM::~VM() {}

void VM::initializeDynamicCode()
{
    applyCode_ = new Code(9);
    applyCode_->push(Object::makeRaw(Instruction::FRAME));
    applyCode_->push(Object::makeFixnum(7));
    applyCode_->push(Object::makeRaw(Instruction::CONSTANT));
    applyCode_->push(Object::Undef);
    applyCode_->push(Object::makeRaw(Instruction::PUSH));
    applyCode_->push(Object::makeRaw(Instruction::CONSTANT));
    applyCode_->push(Object::Undef);
    applyCode_->push(Object::makeRaw(Instruction::APPLY));
    applyCode_->push(Object::makeRaw(Instruction::HALT));

    callClosure0Code_ = new Code(7);
    callClosure0Code_->push(Object::makeRaw(Instruction::FRAME));
    callClosure0Code_->push(Object::makeFixnum(5));
    callClosure0Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure0Code_->push(Object::Undef);
    callClosure0Code_->push(Object::makeRaw(Instruction::CALL));
    callClosure0Code_->push(Object::makeFixnum(0));
    callClosure0Code_->push(Object::makeRaw(Instruction::HALT));

    callClosure1Code_ = new Code(10);
    callClosure1Code_->push(Object::makeRaw(Instruction::FRAME));
    callClosure1Code_->push(Object::makeFixnum(8));
    callClosure1Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure1Code_->push(Object::Undef);
    callClosure1Code_->push(Object::makeRaw(Instruction::PUSH));
    callClosure1Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure1Code_->push(Object::Undef);
    callClosure1Code_->push(Object::makeRaw(Instruction::CALL));
    callClosure1Code_->push(Object::makeFixnum(1));
    callClosure1Code_->push(Object::makeRaw(Instruction::HALT));

    callClosure2Code_ = new Code(13);
    callClosure2Code_->push(Object::makeRaw(Instruction::FRAME));
    callClosure2Code_->push(Object::makeFixnum(11));
    callClosure2Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure2Code_->push(Object::Undef);
    callClosure2Code_->push(Object::makeRaw(Instruction::PUSH));
    callClosure2Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure2Code_->push(Object::Undef);
    callClosure2Code_->push(Object::makeRaw(Instruction::PUSH));
    callClosure2Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure2Code_->push(Object::Undef);
    callClosure2Code_->push(Object::makeRaw(Instruction::CALL));
    callClosure2Code_->push(Object::makeFixnum(2));
    callClosure2Code_->push(Object::makeRaw(Instruction::HALT));

    callClosure3Code_ = new Code(16);
    callClosure3Code_->push(Object::makeRaw(Instruction::FRAME));
    callClosure3Code_->push(Object::makeFixnum(14));
    callClosure3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure3Code_->push(Object::Undef);
    callClosure3Code_->push(Object::makeRaw(Instruction::PUSH));
    callClosure3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure3Code_->push(Object::Undef);
    callClosure3Code_->push(Object::makeRaw(Instruction::PUSH));
    callClosure3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure3Code_->push(Object::Undef);
    callClosure3Code_->push(Object::makeRaw(Instruction::PUSH));
    callClosure3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosure3Code_->push(Object::Undef);
    callClosure3Code_->push(Object::makeRaw(Instruction::CALL));
    callClosure3Code_->push(Object::makeFixnum(3));
    callClosure3Code_->push(Object::makeRaw(Instruction::HALT));

    trigger0Code_ = new Code(7);
    trigger0Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger0Code_->push(Object::Undef);
    trigger0Code_->push(Object::makeRaw(Instruction::CALL));
    trigger0Code_->push(Object::makeFixnum(0));
    trigger0Code_->push(Object::makeRaw(Instruction::RETURN));
    trigger0Code_->push(Object::makeFixnum(0));
    trigger0Code_->push(Object::makeRaw(Instruction::HALT));

    trigger1Code_ = new Code(10);
    trigger1Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger1Code_->push(Object::Undef);
    trigger1Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger1Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger1Code_->push(Object::Undef);
    trigger1Code_->push(Object::makeRaw(Instruction::CALL));
    trigger1Code_->push(Object::makeFixnum(1));
    trigger1Code_->push(Object::makeRaw(Instruction::RETURN));
    trigger1Code_->push(Object::makeFixnum(0));
    trigger1Code_->push(Object::makeRaw(Instruction::HALT));

    applyClosureCode_ = new Code(5);
    applyClosureCode_->push(Object::makeRaw(Instruction::CALL));
    applyClosureCode_->push(Object::makeFixnum(0));
    applyClosureCode_->push(Object::makeRaw(Instruction::RETURN));
    applyClosureCode_->push(Object::makeFixnum(0));
    applyClosureCode_->push(Object::makeRaw(Instruction::HALT));

    callClosureByNameCode_ = new Code(10);
    callClosureByNameCode_->push(Object::makeRaw(Instruction::FRAME));
    callClosureByNameCode_->push(Object::makeFixnum(8));
    callClosureByNameCode_->push(Object::makeRaw(Instruction::CONSTANT));
    callClosureByNameCode_->push(Object::Undef);
    callClosureByNameCode_->push(Object::makeRaw(Instruction::PUSH));
    callClosureByNameCode_->push(Object::makeRaw(Instruction::REFER_GLOBAL));
    callClosureByNameCode_->push(Object::Undef);
    callClosureByNameCode_->push(Object::makeRaw(Instruction::CALL));
    callClosureByNameCode_->push(Object::makeFixnum(1));
    callClosureByNameCode_->push(Object::makeRaw(Instruction::HALT));

    void** dispatchTable = getDispatchTable();

    // callCode_ can be shared, since nested call will happen after fetching operand n.
    callCode_ = new Code(2);
    callCode_->push(Object::makeRaw(dispatchTable[Instruction::CALL]));
    callCode_->push(Object::makeFixnum(0));

    // haltCode_ is read only, so we can share this
    haltCode_ = new Code(1);
    haltCode_->push(Object::makeRaw(dispatchTable[Instruction::HALT]));
}

void VM::loadCompiler()
{
#   include "pmatch.h"
#   include "compiler-with-library.h"
    const Object libCompiler = FASL_GET(compiler_with_library_image);
#ifdef ENABLE_PROFILER
    if (isProfiler_) {
        initProfiler();
    }
#endif
    TRY_VM {
        evaluateUnsafe(libCompiler.toVector());
        const Object libMatch = FASL_GET(pmatch_image);
        evaluateUnsafe(libMatch.toVector());
        CATCH_VM
        // call default error handler
        defaultExceptionHandler(errorObj_);
        this->exit(-1);
    }
}


Object VM::getGlobalValue(Object id)
{
    const Object val = nameSpace_.toEqHashTable()->ref(id, notFound_);
    if (val != notFound_) {
        return val.toGloc()->value();
    } else {
        callAssertionViolationAfter(this, "symbol-value2", "unbound variable", L1(id));
        return Object::Undef;
    }
}

void VM::defaultExceptionHandler(Object error)
{
    currentErrorPort_.toTextualOutputPort()->format(this, UC("\n Exception:\n~a\n"), L1(error));
}

void VM::dumpCompiledCode(Object code) const
{
    MOSH_ASSERT(code.isVector());
    Vector* const v = code.toVector();
    for (int i = 0; i < v->length(); i++) {
        const Object c = v->ref(i);
        if (c.isInstruction()) {
            VM_LOG1("\n~a ", Instruction::toString(c.val));
        } else {
            VM_LOG1("~a ", c);
        }
    }
}


// N.B. If you call loadFileUnsafe, be sure that this code is inside the TRY_VM/CATCH_VM
void VM::loadFileUnsafe(const ucs4string& file)
{
    Registers r;
    saveRegisters(&r);
    const Object loadPort = Object::makeTextualInputFilePort(file.ascii_c_str());
    TextualInputPort* p = loadPort.toTextualInputPort();
    bool readErrorOccured = false;
    for (Object o = p->getDatum(readErrorOccured); !o.isEof(); o = p->getDatum(readErrorOccured)) {
        if (readErrorOccured) {
            callLexicalViolationImmidiaImmediately(this, "read", p->error());
        }
        evaluateUnsafe(compile(o).toVector());
    }
    restoreRegisters(&r);
}

void VM::loadFileWithGuard(const ucs4string& file)
{
    TRY_VM {
        ucs4string moshLibPath(UC(MOSH_LIB_PATH));
        moshLibPath += UC("/") + file;
        if (File::isExist(file)) {
            loadFileUnsafe(file);
        } else if (File::isExist(moshLibPath)) {
            loadFileUnsafe(moshLibPath);
        } else {
            callAssertionViolationImmidiaImmediately(this,
                                                     "load",
                                                     "cannot find file in load path",
                                                     L1(Object::makeString(file)));
        }
    CATCH_VM
        // call default error handler
        defaultExceptionHandler(errorObj_);
        this->exit(-1);
    }
}

// Faster than evaluateUnsafe, used to load compiler, which won't raise error.
Object VM::evaluateUnsafe(Object* code, int codeSize)
{
    closureForEvaluate_.toClosure()->pc = code;
    ac_ = closureForEvaluate_;
    dc_ = closureForEvaluate_;
    cl_ = closureForEvaluate_;
    fp_ = 0;
    Object* const direct = getDirectThreadedCode(code, codeSize);
    return runLoop(direct, NULL);
}

Object VM::evaluateUnsafe(Vector* code)
{
    return evaluateUnsafe(code->data(), code->length());
}

Object VM::evaluateSafe(Object* code, int codeSize)
{
    Registers r;
    saveRegisters(&r);
    Object ret = Object::Undef;
    TRY_VM {
        ret = evaluateUnsafe(code, codeSize);
    CATCH_VM
        defaultExceptionHandler(errorObj_);
        this->exit(-1);
    }
    restoreRegisters(&r);
    return ret;
}

Object VM::evaluateSafe(Vector* code)
{
    return evaluateSafe(code->data(), code->length());
}

Object VM::evaluateSafe(Code* code)
{
    return evaluateSafe(code->code(), code->size());
}

Object VM::callClosure0(Object closure)
{
    callClosure0Code_->set(3, closure);
    return evaluateSafe(callClosure0Code_);
}

Object VM::callClosure1(Object closure, Object arg)
{
    VM_LOG2("now calling  ~a ~a\n", closure.isClosure() ? closure.toClosure()->sourceInfo : Object::False, closure);
    callClosure1Code_->set(3, arg);
    callClosure1Code_->set(6, closure);
    Object ret = evaluateSafe(callClosure1Code_);
    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    return ret;
}

Object VM::callClosure2(Object closure, Object arg1, Object arg2)
{
    callClosure2Code_->set(3, arg1);
    callClosure2Code_->set(6, arg2);
    callClosure2Code_->set(9, closure);
    return evaluateSafe(callClosure2Code_);
}

Object VM::callClosure3(Object closure, Object arg1, Object arg2, Object arg3)
{
    callClosure3Code_->set(3, arg1);
    callClosure3Code_->set(6, arg2);
    callClosure3Code_->set(9, arg3);
    callClosure3Code_->set(12, closure);
    return evaluateSafe(callClosure3Code_);
}

Object VM::compileWithoutHalt(Object sexp)
{
    static Object compiler = Symbol::intern(UC("compile-w/o-halt"));
    return callClosureByName(compiler, sexp);
}

Object VM::evalCompiledAfter(Object code)
{
    VM_ASSERT(code.isVector());
    Vector* const vcode = code.toVector();

    // We need to append "RETURN 0" to the code.
    const int codeSize = vcode->length();
    const int bodySize = codeSize + 2;
    Object* body = Object::makeObjectArray(bodySize);
    for (int i = 0; i < codeSize; i++) {
        body[i] = (vcode->data())[i];
    }
    body[codeSize]   = Object::makeRaw(Instruction::RETURN);
    body[codeSize+ 1] = Object::makeFixnum(0);

    // make closure for save/restore current environment
    Closure* const closure = new Closure(getDirectThreadedCode(body, bodySize), // pc
                                         bodySize,                              // codeSize
                                         0,                                     // argLength
                                         false,                                 // isOptionalArg
                                         cProcs_,                                // freeVars
                                         cProcNum,                              // freeVariablesNum
                                         0,                                     // todo maxStack
                                         Object::False);                        // todo sourceInfo
    return setAfterTrigger0(Object::makeClosure(closure));
}

Object VM::evalAfter(Object sexp)
{
    const Object code = compileWithoutHalt(sexp);
    VM_ASSERT(code.isVector());
    Vector* const vcode = code.toVector();

    // We need to append "RETURN 0" to the code.
    const int codeSize = vcode->length();
    const int bodySize = codeSize + 2;
    Object* body = Object::makeObjectArray(bodySize);
    for (int i = 0; i < codeSize; i++) {
        body[i] = (vcode->data())[i];
    }
    body[codeSize]   = Object::makeRaw(Instruction::RETURN);
    body[codeSize+ 1] = Object::makeFixnum(0);

    // make closure for save/restore current environment
    Closure* const closure = new Closure(getDirectThreadedCode(body, bodySize), // pc
                                         bodySize,                              // codeSize
                                         0,                                     // argLength
                                         false,                                 // isOptionalArg
                                         cProcs_,                                // freeVars
                                         cProcNum,                              // freeVariablesNum
                                         0,                                     // todo maxStack
                                         Object::False);                        // todo sourceInfo
    return setAfterTrigger0(Object::makeClosure(closure));
}

Object VM::setAfterTrigger0(Object closure)
{
    makeCallFrame(pc_);
    pc_ = getDirectThreadedCode(trigger0Code_->code(), trigger0Code_->size());
    pc_[1]= closure;
    return ac_;
}


Object VM::setAfterTrigger1(Object closure, Object arg1)
{
    makeCallFrame(pc_);
    pc_ = getDirectThreadedCode(trigger1Code_->code(), trigger1Code_->size());
    pc_[4] = closure;
    pc_[1]= arg1;
    return ac_;
}

void VM::applyClosure(Object closure, Object args)
{
    applyClosureCode_->set(1, Object::makeFixnum(Pair::length(args)));
    makeCallFrame(pc_);
    for (Object obj = args; !obj.isNil(); obj = obj.cdr()) {
        push(obj.car());
    }
    ac_ = closure;
    pc_ = getDirectThreadedCode(applyClosureCode_->code(), applyClosureCode_->size());
}

Object VM::callClosureByName(Object procSymbol, Object arg)
{
    MOSH_ASSERT(procSymbol.isSymbol());
    callClosureByNameCode_->set(3, arg);
    callClosureByNameCode_->set(6, procSymbol);
    return evaluateSafe(callClosureByNameCode_);
}

Object VM::apply(Object proc, Object args)
{
    applyCode_->set(3, args);
    applyCode_->set(6, proc);
    return evaluateSafe(applyCode_);
}

// ToDo:We can optimize for cprocedure case.
// N.B.
// If you want to know how this works, see lib/mosh/jit/compiler.
Object VM::call(Object n)
{
    MOSH_ASSERT(n.isFixnum());

    // Insert HALT code to the FRAME
    Object* frame = sp_ - n.toFixnum() - SIZE_OF_FRAME;
    *(frame + 0) = Object::makeObjectPointer(haltCode_->code());

    // CALL! (No need to save/restore?)
    callCode_->set(1, n);
    const Object ret = runLoop(callCode_->code(), NULL);
    return ret;
}

Object VM::tailCall(Object n, Object diff)
{
    MOSH_ASSERT(n.isFixnum());
    MOSH_ASSERT(diff.isFixnum());
    Object* topFrame = sp_ - n.toFixnum() - diff.toFixnum() - SIZE_OF_FRAME;

    Object dc1 = *(topFrame + 1);
    Object cl1 = *(topFrame + 2);
    Object fp1 = *(topFrame + 3);

    // Shift args
    for (int i = 0; i < n.toFixnum(); i ++) {
        *(sp_ + SIZE_OF_FRAME - i - 1) = *(sp_ - i - 1);
    }

    // Make dummy frame
    sp_ = sp_ - n.toFixnum();
    push(Object::makeObjectPointer(haltCode_->code()));
    push(dc1);
    push(cl1);
    push(fp1);

    fp_ += SIZE_OF_FRAME;
    sp_ += n.toFixnum();
    callCode_->set(1, n);
    const Object ret = runLoop(callCode_->code(), NULL);
    return ret;
}


Object VM::vmapply(Object proc, Object args)
{
    const int procLength = Pair::length(proc);
    Code* code = new Code(procLength + 7);

    code->push(Object::makeRaw(Instruction::FRAME));
    code->push(Object::makeFixnum(procLength + 5));
    code->push(Object::makeRaw(Instruction::CONSTANT));
    code->push(args);
    code->push(Object::makeRaw(Instruction::PUSH));
    for (Object o = proc; !o.isNil(); o = o.cdr()) {
        code->push(o.car());
    }
    code->push(Object::makeRaw(Instruction::APPLY));
    code->push(Object::makeRaw(Instruction::HALT));
    return evaluateSafe(code->code(), code->size());
}

Object VM::compile(Object code)
{
    static Object proc = Symbol::intern(UC("compile"));
    const Object compiled = callClosureByName(proc, code);
    return compiled;
}

Object VM::getStackTrace()
{
    const int MAX_DEPTH = 20;
    const int FP_OFFSET_IN_FRAME = 1;
    const int CLOSURE_OFFSET_IN_FRAME = 2;

    const Object sport = Object::makeStringOutputPort();
    TextualOutputPort* port = sport.toTextualOutputPort();
    Object* fp = fp_;
    Object* cl = &cl_;
    for (int i = 1;;) {
        if (cl->isClosure()) {
            Object src = cl->toClosure()->sourceInfo;
            if (src.isPair()) {
                port->format(this, UC("    ~d. "), L1(Object::makeFixnum(i)));
                const Object procedure = src.cdr();
                const Object location  = src.car();
                if (location.isFalse()) {
                    port->format(this, UC("~a: <unknown location>\n"), L1(unGenSyms(procedure)));
                } else {
                    const Object lineno = location.cdr().car();
                    const Object file   = location.car();
                    const Object procedureName = procedure.car();

                    // anonymous procedure
                    if (procedure.car() == Symbol::intern(UC("lambda"))) {
                        // format source information to follwing style
                        // (lambda (arg1 arg2 arg3) ...)
                        Object args = unGenSyms(procedure.cdr());
                        const Object procedureSource = Pair::list3(procedureName, args, Symbol::intern(UC("...")));
                        port->format(this, UC("~a:  ~a:~a\n"), L3(procedureSource, file, lineno));
                    } else {
                        port->format(this, UC("~a:  ~a:~a\n"), L3(unGenSyms(procedure), file, lineno));
                    }
                }
                i++;
            }
        } else if (cl->isCProcedure()) {
            port->format(this, UC("    ~d. "), L1(Object::makeFixnum(i)));
            port->format(this, UC("~a: <subr>\n"), L1(getClosureName(*cl)));
            i++;
        } else if (cl->isRegMatch()) {
            port->format(this, UC("    ~d. "), L1(Object::makeFixnum(i)));
            port->format(this, UC("<reg-match>: ~a\n"), L1(*cl));
            i++;
        } else if (cl->isRegexp()) {
            port->format(this, UC("    ~d. "), L1(Object::makeFixnum(i)));
            port->format(this, UC("<regexp>: ~a\n"), L1(*cl));
            i++;
        } else {
            MOSH_ASSERT(false);
        }
        if (i > MAX_DEPTH) {
            port->display(this, UC("      ... (more stack dump truncated)\n"));
            break;
        }

        VM_ASSERT(!(*cl).isObjectPointer());
        VM_ASSERT((*cl).isClosure() || (*cl).isCProcedure() );
        if (fp > stack_) {
            cl = fp - CLOSURE_OFFSET_IN_FRAME;

            // N.B. We must check whether cl is Object pointer or not.
            // If so, we can't touch them. (touching may cause crash)
            if (mayBeStackPointer(cl)) {
                break;
            }
            if (!((*cl).isClosure()) && !((*cl).isCProcedure())) {
                break;
            }
            // next fp is Object pointer, so 4byte aligned.
            // if it is not Object pointer, may be tail call
            Object* nextFp = fp - FP_OFFSET_IN_FRAME;
            if (!(nextFp->isRawPointer())) {
                break;
            }

            if (!mayBeStackPointer(nextFp)) {
//                getOutputPort().toTextualOutputPort()->format(UC("[[[[~a]]]]"), *nextFp);
                break;
            }

            VM_ASSERT(nextFp->isObjectPointer());
            fp = nextFp->toObjectPointer();
        } else {
            break;
        }
    }
    return getOutputStringEx(this, 1, &sport);
}

bool VM::mayBeStackPointer(Object* obj) const
{
#ifdef DEBUG_VERSION
    // not heap object
    if (!obj->isHeapObject()) {
        return false;
    }
    Object* const p = reinterpret_cast<Object*>(reinterpret_cast<HeapObject*>(obj->val)->obj);
    return p >= stack_ && p <= stackEnd_;
#else
    Object* const p = reinterpret_cast<Object*>(obj->val);
    return p >= stack_ && p <= stackEnd_;
#endif
}

void VM::throwException(Object exception)
{
#ifdef DEBUG_VERSION
    VM_LOG1("error~a\n", exception);
    fflush(stderr);
    fflush(stdout);
#endif
    const Object stackTrace = getStackTrace();
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();
    textualOutputPort->format(this, UC("~a\n Stack trace:\n~a\n"), Pair::list2(exception, stackTrace));
    errorObj_ = getOutputStringEx(this, 1, &stringOutputPort);

    siglongjmp(returnPoint_, -1);
}

void VM::showStack(int count, const char* file, int line)
{
   printf("** STACK %s:%d\n", file, line);fflush(stdout);
#ifdef DEBUG_VERSION
    for (int i = count - 1; i >= 0; i--) {
        VM_LOG2("============================================\n~d: ~a\n", Object::makeFixnum(i), index(sp_, i));
    }
#else
    callAssertionViolationImmidiaImmediately(this, "vm", "don't use showStack");
#endif
}

bool VM::isR6RSMode() const
{
    return isR6RSMode_;
}

Object VM::activateR6RSMode(bool isDebugExpand)
{
#   include "psyntax.h"
    isR6RSMode_ = true;
    setValueString(UC("debug-expand"), Object::makeBool(isDebugExpand));
    const Object libPsyntax = FASL_GET(psyntax_image);
    return evaluateSafe(libPsyntax.toVector());
}

Object VM::getGlobalValueOrFalse(Object id)
{
    const Object val = nameSpace_.toEqHashTable()->ref(id, notFound_);
    if (val != notFound_) {
        return val.toGloc()->value();
    } else {
        return Object::False;
    }
}

Object VM::getGlobalValueOrFalse(const ucs4char* id)
{
    return getGlobalValueOrFalse(Symbol::intern(id));
}

Object VM::currentOutputPort() const
{
    return currentOutputPort_;
}

Object VM::currentErrorPort() const
{
    return currentErrorPort_;
}

Object VM::currentInputPort() const
{
    return currentInputPort_;
}

void VM::setCurrentInputPort(Object port)
{
    currentInputPort_ = port;
}

void VM::setCurrentOutputPort(Object port)
{
    currentOutputPort_ = port;
}

void VM::expandStack(int plusSize)
{
    fprintf(stderr, "Stack Expand stack=%p plusSize=%d\n", stack_, plusSize);

    const int nextStackSize = stackSize_ + plusSize;
    Object* nextStack = Object::makeObjectArray(nextStackSize);
    if (NULL == nextStack) {
        // todo
        // handle stack overflow with guard
        callAssertionViolationImmidiaImmediately(this, "#<closure>", "stack overflow", L1(Object::makeFixnum(sp_ - stack_)));
    }
    memcpy(nextStack, stack_, sizeof(Object) * stackSize_);
    fp_ = nextStack + (fp_ - stack_);
    sp_ = nextStack + (sp_ - stack_);
    stackEnd_ = nextStack + nextStackSize;
    stack_ = nextStack;
    stackSize_ = nextStackSize;
}

void VM::printStack() const
{
    VM_LOG2("==========dc=~a prev=~a \n", dc_, dc_.toClosure()->prev);
    for (int i = 1; i>= 0; i--) {
        if (fp_ + i >= stackEnd_) {
            break;
        }
        const Object obj = referLocal(i);

        if (!obj.isObjectPointer()) {
            VM_LOG2("~d: ~a\n", Object::makeFixnum(i), obj);
        }
        fflush(stderr);
    }
}

Object VM::values(int num, const Object* v)
{
    if (0 == num) {
        numValues_ = 0;
        return Object::Undef;
    }
    for (int i = 1; i < num; i++) {
        if (i >= maxNumValues_) {
            callAssertionViolationAfter(this, "values", "too many values", Pair::list1(Object::makeFixnum(i)));
            return Object::Undef;
        }
        values_[i - 1] = v[i];
    }
    numValues_ = num;
    return v[0]; // set to ac_ later.
}

Object VM::values2(Object obj1, Object obj2)
{
    values_[0] = obj2;
    numValues_ = 2;
    return obj1; // set to ac_ later.
}

Object VM::values3(Object obj1, Object obj2, Object obj3)
{
    values_[0] = obj2;
    values_[1] = obj3;
    numValues_ = 3;
    return obj1; // set to ac_ later.
}


Object VM::getCProcedureName(Object proc) const
{
    for (int k = 0; k < cProcNum; k++) {
        if (proc == cProcs_[k]) {
            return Symbol::intern(cProcNames[k]);
        }
    }
    return Symbol::intern(UC("<unknwon subr>"));
}

void VM::registerPort(Object obj)
{
    MOSH_ASSERT(obj.isOutputPort());
    activePorts_.push_back(obj);
}

void VM::unregisterPort(Object obj)
{
    MOSH_ASSERT(obj.isOutputPort());
    Ports::iterator it = activePorts_.begin();
    while (it != activePorts_.end()) {
        if (obj.eq(*it)) {
            activePorts_.erase(it);
            return;
        }
        it++;
    }
}

void VM::flushAllPorts()
{
    Ports::iterator it = activePorts_.begin();
    while (it != activePorts_.end()) {
        const Object outputPort = *it;
        if (outputPort.isBinaryOutputPort()) {
            outputPort.toBinaryOutputPort()->flush();
        } else if (outputPort.isBinaryInputOutputPort()) {
            outputPort.toBinaryInputOutputPort()->flush();
        } else if (outputPort.isTextualOutputPort()) {
            outputPort.toTextualOutputPort()->flush();
        } else if (outputPort.isTextualInputOutputPort()) {
            outputPort.toTextualInputOutputPort()->flush();
        } else {
            MOSH_ASSERT(false);
        }
        it = activePorts_.erase(it);
    }
}

void VM::setThread(Thread* thread)
{
    thread_ = thread;
}

Thread* VM::thread()
{
    return thread_;
}

Object VM::findGenerativeRtd(Object uid)
{
    ObjectMap::const_iterator found = generativeRtds_.find(uid);
    if (found == generativeRtds_.end()) {
        return Object::False;
    } else {
        return found->second;

    }
}

void VM::addGenerativeRtd(Object uid, Object rtd)
{
    generativeRtds_[uid] = rtd;
}

void VM::tryInvokeJitLibrary()
{
    const Object importSpec = Pair::list3(Symbol::intern(UC("mosh")), Symbol::intern(UC("jit")),  Symbol::intern(UC("compiler")));
    const Object invokeLibrary = getGlobalValueOrFalse(Symbol::intern(UC("invoke-library-by-name")));
    if (invokeLibrary.isClosure()) {
        callClosure1(invokeLibrary, importSpec);
        jitCompiler_ = getGlobalValueOrFalse(Symbol::intern(UC("jit-compile")));
        VM_ASSERT(jitCompiler_.isClosure());
    }
}

void VM::tryJitCompile(Object closure)
{
    const int CALL_COUNT_JIT_THRESHOLD = 10;
    const int MAX_CLOSURE_SIZE = 100;

    MOSH_ASSERT(closure.isClosure());
    Closure* c = closure.toClosure();

    c->incrementCalledCount();

    bool dontJitCompile =
        isJitCompiling_                                || // Other jit compilation runs
        isJitLibraryLoading_                           || // Invoking (mosh jit compiler)
        c->isJitError()                                || // This closure once compiled and causes error.
        c->getCalledCount() < CALL_COUNT_JIT_THRESHOLD || // Check closure size
        c->size > MAX_CLOSURE_SIZE;

    if (dontJitCompile) {
        return;
    }

    isJitCompiling_ = true;

    // (mosh jit compiler) should be invoked.
    if (jitCompiler_.isFalse()) {
        isJitLibraryLoading_ = true;
        tryInvokeJitLibrary();
        isJitLibraryLoading_ = false;
        isJitCompiling_ = false;
        return;
    } else {
//        VM_LOG2("now compiling ~a ~a\n", c->sourceInfo, closure);
        Time t1 = Time::now();
        Object compiled = callClosure1(jitCompiler_, closure);
        Time t2 = Time::now();
        if (compiled.isFalse()) {
//            LOG2("jit compile error ~a ~d usec\n", c->sourceInfo, Bignum::makeIntegerFromUintprt_t(Time::diffUsec(t2, t1)));
            c->setJitCompiledError();
        } else {
            LOG2("jit compile~a ~d usec\n", closure, Bignum::makeIntegerFromUintprt_t(Time::diffUsec(t2, t1)));
        }
        isJitCompiling_ = false;
        return;
    }
}

void VM::raiseNotPairErrorForJit(int op)
{
    const char* operation[2] = {"car", "cdr"};
    VM_ASSERT(op < (int)(sizeof(operation) / sizeof(const char*)));
    callAssertionViolationAfter(this, operation[op], "pair required", Pair::list1(ac_));
}

void VM::raiseVectorRequiredError(int op, Object obj)
{
    const char* operations[1] = {"vector-ref"};
    VM_ASSERT(op < (int)(sizeof(operations) / sizeof(const char*)));
    callAssertionViolationAfter(this,
                                operations[op],
                                "vector required",
                                L1(obj));
}

void VM::raiseVectorInvalidIndexError(int op)
{
    const char* operations[1] = {"vector-ref"};
    VM_ASSERT(op < (int)(sizeof(operations) / sizeof(const char*)));
    callAssertionViolationAfter(this,
                                operations[op],
                                "index out of range",
                                L1(ac_));
}

void VM::copyOptions(VM* destVM, VM* srcVM)
{
    const ucs4char* options[] = {UC("%loadpath"), UC("%verbose"), UC("*command-line-args*")};
    for (size_t i = 0; i < sizeof(options) / sizeof(ucs4char*); i ++) {
        destVM->setValueString(options[i], srcVM->getGlobalValueOrFalse(options[i]));
    }
}

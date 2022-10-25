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
#include "HashTableProcedures.h"
#include "StringProcedures.h"
#include "PortProcedures.h"
#include "ErrorProcedures.h"
#include "ListProcedures.h"
#include "ArithmeticProcedures.h"
#include "FlonumProcedures.h"
#include "BitwiseProcedures.h"
#include "ProcessProcedures.h"
#include "ByteVectorProcedures.h"
#include "FFIProcedures.h"
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
#include "ArrayProcedures.h"
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
#include "Code.h"

#define TRY_VM  jmp_buf org;                     \
                copyJmpBuf(org, returnPoint_);   \
                if (setjmp(returnPoint_) == 0)   \

#define CATCH_VM copyJmpBuf(returnPoint_, org); \
                 } else {

using namespace scheme;

#include "cprocedures.cpp"

VM::VM(int stackSize, Object outPort, Object errorPort, Object inputPort, bool isProfiler) :
    ac_(Object::Nil),
    dc_(Object::Nil),
    cl_(Object::Nil),
    
    stackSize_(stackSize),
    currentOutputPort_(outPort),
    currentErrorPort_(errorPort),
    currentInputPort_(inputPort),
    errorObj_(Object::Nil),
#ifdef ENABLE_PROFILER
    
#endif
    isProfiler_(isProfiler),
    
    name_(UC("")),
    
    readerContext_(new ReaderContext),
    numberReaderContext_(new NumberReaderContext),
    
    dynamicWinders_(Object::Nil),
    callBackTrampolines_(new EqHashTable)
    
{
    stack_ = Object::makeObjectArrayLocal(stackSize);
    values_ = Object::makeObjectArrayLocal(maxNumValues_);
    stackEnd_ = stack_ + stackSize;
    sp_ = stack_;
    fp_ = stack_;
    nameSpace_ = Object::makeEqHashTable();
    outerSourceInfo_   = L2(Object::False, Symbol::intern(UC("<top-level>")));
    cProcs_ = Object::makeObjectArrayLocal(cProcNum);
    for (int i = 0; i < cProcNum; i++) {
        cProcs_[i] = Object::makeCProcedure(cProcFunctions[i]);
    }

    // initialize "On the fly" instructions array
    closureForEvaluate_ = Object::makeClosure(nullptr, 0, 0, false, cProcs_, cProcNum, 0, outerSourceInfo_);
    closureForApply_ = Object::makeClosure(nullptr, 0, 0, false, cProcs_, cProcNum, 1, outerSourceInfo_);;

    initializeDynamicCode();
}

VM::~VM() = default;

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

    trigger2Code_ = new Code(13);
    trigger2Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger2Code_->push(Object::Undef);
    trigger2Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger2Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger2Code_->push(Object::Undef);
    trigger2Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger2Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger2Code_->push(Object::Undef);
    trigger2Code_->push(Object::makeRaw(Instruction::CALL));
    trigger2Code_->push(Object::makeFixnum(2));
    trigger2Code_->push(Object::makeRaw(Instruction::RETURN));
    trigger2Code_->push(Object::makeFixnum(0));
    trigger2Code_->push(Object::makeRaw(Instruction::HALT));

    trigger3Code_ = new Code(16);
    trigger3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger3Code_->push(Object::Undef);
    trigger3Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger3Code_->push(Object::Undef);
    trigger3Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger3Code_->push(Object::Undef);
    trigger3Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger3Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger3Code_->push(Object::Undef);
    trigger3Code_->push(Object::makeRaw(Instruction::CALL));
    trigger3Code_->push(Object::makeFixnum(3));
    trigger3Code_->push(Object::makeRaw(Instruction::RETURN));
    trigger3Code_->push(Object::makeFixnum(0));
    trigger3Code_->push(Object::makeRaw(Instruction::HALT));

    trigger4Code_ = new Code(19);
    trigger4Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger4Code_->push(Object::Undef);
    trigger4Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger4Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger4Code_->push(Object::Undef);
    trigger4Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger4Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger4Code_->push(Object::Undef);
    trigger4Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger4Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger4Code_->push(Object::Undef);
    trigger4Code_->push(Object::makeRaw(Instruction::PUSH));
    trigger4Code_->push(Object::makeRaw(Instruction::CONSTANT));
    trigger4Code_->push(Object::Undef);
    trigger4Code_->push(Object::makeRaw(Instruction::CALL));
    trigger4Code_->push(Object::makeFixnum(4));
    trigger4Code_->push(Object::makeRaw(Instruction::RETURN));
    trigger4Code_->push(Object::makeFixnum(0));
    trigger4Code_->push(Object::makeRaw(Instruction::HALT));

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

    callCode_ = new Code(3);
}

void VM::loadCompiler()
{
#   include "match.h"
#   include "baselib.h"
    const Object libCompiler = FASL_GET(baselib_image);
#ifdef ENABLE_PROFILER
    if (isProfiler_) {
        initProfiler();
    }
#endif
    TRY_VM {
        evaluateUnsafe(libCompiler.toVector(), true);
        const Object libMatch = FASL_GET(match_image);
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
        callAssertionViolationAfter(this, UC("symbol-value2"), UC("unbound variable"), L1(id));
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
    for (size_t i = 0; i < v->length(); i++) {
        const Object c = v->ref(i);
        if (c.isInstruction()) {
            VM_LOG1("\n~a ", Object(Instruction::toString(static_cast<int>(c.val))));
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
            callLexicalViolationImmidiaImmediately(this, UC("read"), p->error());
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
                                                     UC("load"),
                                                     UC("cannot find file in load path"),
                                                     L1(Object::makeString(file)));
        }
    CATCH_VM
        // call default error handler
        defaultExceptionHandler(errorObj_);
        this->exit(-1);
    }
}

// Faster than evaluateUnsafe, used to load compiler, which won't raise error.
Object VM::evaluateUnsafe(Object* code, size_t codeSize, bool isCompiler /* = false */)
{
    closureForEvaluate_.toClosure()->pc = code;
    ac_ = closureForEvaluate_;
    dc_ = closureForEvaluate_;
    cl_ = closureForEvaluate_;
    fp_ = nullptr;
    Object* const direct = getDirectThreadedCode(code, codeSize, isCompiler);
    return runLoop(direct, nullptr);
}

Object VM::evaluateUnsafe(Vector* code, bool isCompiler /* = false */)
{
    return evaluateUnsafe(code->data(), code->length(), isCompiler);
}

Object VM::evaluateSafe(Object* code, size_t codeSize, bool isCompiler /* = false */)
{
    Registers r;
    saveRegisters(&r);
    Object ret = Object::Undef;
    TRY_VM {
        ret = evaluateUnsafe(code, codeSize, isCompiler);
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
    callClosure1Code_->set(3, arg);
    callClosure1Code_->set(6, closure);
    return evaluateSafe(callClosure1Code_);
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
    const size_t codeSize = vcode->length();
    const size_t bodySize = codeSize + 2;
    Object* body = Object::makeObjectArray(bodySize);
    for (size_t i = 0; i < codeSize; i++) {
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
    const size_t codeSize = vcode->length();
    const size_t bodySize = codeSize + 2;
    Object* body = Object::makeObjectArray(bodySize);
    for (size_t i = 0; i < codeSize; i++) {
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

Object VM::setAfterTrigger2(Object closure, Object arg1, Object arg2)
{
    makeCallFrame(pc_);
    pc_ = getDirectThreadedCode(trigger2Code_->code(), trigger2Code_->size());
    pc_[7] = closure;
    pc_[4]= arg2;
    pc_[1]= arg1;
    return ac_;
}

Object VM::setAfterTrigger3(Object closure, Object arg1, Object arg2, Object arg3)
{
    makeCallFrame(pc_);
    pc_ = getDirectThreadedCode(trigger3Code_->code(), trigger3Code_->size());
    pc_[10] = closure;
    pc_[7]= arg3;
    pc_[4]= arg2;
    pc_[1]= arg1;
    return ac_;
}

Object VM::setAfterTrigger4(Object closure, Object arg1, Object arg2, Object arg3, Object arg4)
{
    makeCallFrame(pc_);
    pc_ = getDirectThreadedCode(trigger4Code_->code(), trigger4Code_->size());
    pc_[13] = closure;
    pc_[10]= arg4;
    pc_[7]= arg3;
    pc_[4]= arg2;
    pc_[1]= arg1;
    return ac_;
}

// we need to save registers.
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

Object VM::getStackTraceObj()
{
#if 0
    monapi_warn("stack trace is currently disabled, since it causes gc crash.");
    return Object::Nil;
#else
    //const int MAX_DEPTH = 20;
    const int FP_OFFSET_IN_FRAME = 1;
    const int CLOSURE_OFFSET_IN_FRAME = 2;

    Object r = Object::Nil;
    Object cur = Object::Nil;
    Object* fp = fp_;
    Object* cl = &cl_;
    for (int i = 0;;) {
        if (cl->isClosure()) {
            Object src = cl->toClosure()->sourceInfo;
            if (src.isPair()) {
                const Object procedure = src.cdr();
                const Object location  = src.car();
                r = L3(Symbol::intern(UC("*proc*")),procedure,location);
            }else{
                r = L1(Symbol::intern(UC("*unknown-proc*")));
            }
            i++;
        } else if (cl->isCProcedure()) {
            r = L2(Symbol::intern(UC("*cproc*")),getClosureName(*cl));
            i++;
        } else if (cl->isRegMatch()) {
            r = L2(Symbol::intern(UC("*reg-match*")),*cl);
            i++;
        } else if (cl->isRegexp()) {
            r = L2(Symbol::intern(UC("*regexp*")),*cl);
            i++;
        } else {
            MOSH_ASSERT(false);
        }
        cur = Object::cons(Object::cons(Object::makeFixnum(i),r),cur);
#if 0
        if (i > MAX_DEPTH) {
            port->display(this, UC("      ... (more stack dump truncated)\n"));
            break;
        }
#endif

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
    return cur;
#endif
}

Object VM::getStackTrace()
{
#if 0
    monapi_warn("stack trace is currently disabled, since it causes gc crash.");
    return Object::Nil;
#else
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
#endif
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

    longjmp(returnPoint_, -1);
}

void VM::showStack(int count, const char* file, int line)
{
   printf("** STACK %s:%d\n", file, line);fflush(stdout);
#ifdef DEBUG_VERSION
    for (int i = count - 1; i >= 0; i--) {
        VM_LOG2("============================================\n~d: ~a\n", Object::makeFixnum(i), index(sp_, i));
    }
#else
    callAssertionViolationImmidiaImmediately(this, UC("vm"), UC("don't use showStack"));
#endif
}

bool VM::isR6RSMode() const
{
    return isR6RSMode_;
}

Object VM::activateR6RSMode(const uint8_t* image, unsigned int image_size, bool isDebugExpand)
{
    isR6RSMode_ = true;
    setValueString(UC("debug-expand"), Object::makeBool(isDebugExpand));
    const Object code = FASL_GET_WITH_SIZE(image, image_size);
    TRY_VM {
        Vector* v = code.toVector();
        return evaluateSafe(v->data(), v->length());
    CATCH_VM
            // call default error handler
            defaultExceptionHandler(errorObj_);
            this->exit(-1);
            return Object::Undef;
    }
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

void VM::setCurrentErrorPort(Object port)
{
    currentErrorPort_ = port;
}

void VM::expandStack(int plusSize)
{

    const int nextStackSize = stackSize_ + plusSize;
    const int WARN_STACK_SIZE_IN_MB = 48;
    if  (nextStackSize * sizeof(intptr_t) > WARN_STACK_SIZE_IN_MB * 1024 * 1024) {
        fprintf(stderr, "Warning: Stack is growing to %ld MB\n", nextStackSize * sizeof(intptr_t) / 1024 / 1024);
    }

    Object* nextStack = Object::makeObjectArrayLocal(nextStackSize);
    if (nullptr == nextStack) {
        // todo
        // handle stack overflow with guard
        callAssertionViolationImmidiaImmediately(this, UC("#<closure>"), UC("stack overflow"), L1(Object::makeFixnum(static_cast<int>(sp_ - stack_))));
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
            callAssertionViolationAfter(this, UC("values"), UC("too many values"), Pair::list1(Object::makeFixnum(i)));
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

Object VM::values6(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6)
{
    values_[0] = obj2;
    values_[1] = obj3;
    values_[2] = obj4;
    values_[3] = obj5;
    values_[4] = obj6;
    numValues_ = 6;
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

void VM::copyOptions(VM* destVM, VM* srcVM)
{
    const ucs4char* options[] = {
        // mosh options
        UC("%loadpath"), UC("%verbose"), UC("*command-line-args*"), UC("%optimize?"),

        // nmosh options
        UC("%nmosh-portable-mode"), UC("%nmosh-prefixless-mode")
    };
    for (auto & option : options) {
        destVM->setValueString(option, srcVM->getGlobalValueOrFalse(option));
    }
}

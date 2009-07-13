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
#include "Fasl.h"
#include "Gloc.h"
#include "OSCompat.h"
#include "BinaryOutputPort.h"
#include "BinaryInputOutputPort.h"
#include "TranscodedTextualInputOutputPort.h"
#include "Scanner.h"
#include "Reader.h"
#include "NumberReader.h"

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
    pc_(NULL),
    stackSize_(stackSize),
    maxStack_(NULL),
    currentOutputPort_(outPort),
    currentErrorPort_(errorPort),
    currentInputPort_(inputPort),
    errorObj_(Object::Nil),
#ifdef ENABLE_PROFILER
    profilerRunning_(false),
#endif
    isProfiler_(isProfiler),
    maxNumValues_(256),
    numValues_(0),
    isR6RSMode_(false),
    name_(UC("")),
    thread_(NULL),
    readerContext_(new ReaderContext),
    numberReaderContext_(new NumberReaderContext),
    errno_(0)
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

    applyCodeForCallClosure0Length_ = 7;
    applyCodeForCallClosure0_ = Object::makeObjectArray(applyCodeForCallClosure0Length_);
    applyCodeForCallClosure0_[0] = Object::makeRaw(Instruction::FRAME);
    applyCodeForCallClosure0_[1] = Object::makeFixnum(5);
    applyCodeForCallClosure0_[2] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure0_[3] = Object::Undef;
    applyCodeForCallClosure0_[4] = Object::makeRaw(Instruction::CALL);
    applyCodeForCallClosure0_[5] = Object::makeFixnum(0);
    applyCodeForCallClosure0_[6] = Object::makeRaw(Instruction::HALT);

    applyCodeForCallClosure1Length_ = 10;
    applyCodeForCallClosure1_ = Object::makeObjectArray(applyCodeForCallClosure1Length_);
    applyCodeForCallClosure1_[0] = Object::makeRaw(Instruction::FRAME);
    applyCodeForCallClosure1_[1] = Object::makeFixnum(8);
    applyCodeForCallClosure1_[2] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure1_[3] = Object::Undef;
    applyCodeForCallClosure1_[4] = Object::makeRaw(Instruction::PUSH);
    applyCodeForCallClosure1_[5] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure1_[6] = Object::Undef;
    applyCodeForCallClosure1_[7] = Object::makeRaw(Instruction::CALL);
    applyCodeForCallClosure1_[8] = Object::makeFixnum(1);
    applyCodeForCallClosure1_[9] = Object::makeRaw(Instruction::HALT);

    applyCodeForCallClosure2Length_ = 13;
    applyCodeForCallClosure2_ = Object::makeObjectArray(applyCodeForCallClosure2Length_);
    applyCodeForCallClosure2_[0] = Object::makeRaw(Instruction::FRAME);
    applyCodeForCallClosure2_[1] = Object::makeFixnum(11);
    applyCodeForCallClosure2_[2] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure2_[3] = Object::Undef;
    applyCodeForCallClosure2_[4] = Object::makeRaw(Instruction::PUSH);
    applyCodeForCallClosure2_[5] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure2_[6] = Object::Undef;
    applyCodeForCallClosure2_[7] = Object::makeRaw(Instruction::PUSH);
    applyCodeForCallClosure2_[8] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure2_[9] = Object::Undef;
    applyCodeForCallClosure2_[10] = Object::makeRaw(Instruction::CALL);
    applyCodeForCallClosure2_[11] = Object::makeFixnum(2);
    applyCodeForCallClosure2_[12] = Object::makeRaw(Instruction::HALT);

    applyCodeForCallClosure3Length_ = 16;
    applyCodeForCallClosure3_ = Object::makeObjectArray(applyCodeForCallClosure3Length_);
    applyCodeForCallClosure3_[0] = Object::makeRaw(Instruction::FRAME);
    applyCodeForCallClosure3_[1] = Object::makeFixnum(14);
    applyCodeForCallClosure3_[2] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure3_[3] = Object::Undef;
    applyCodeForCallClosure3_[4] = Object::makeRaw(Instruction::PUSH);
    applyCodeForCallClosure3_[5] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure3_[6] = Object::Undef;
    applyCodeForCallClosure3_[7] = Object::makeRaw(Instruction::PUSH);
    applyCodeForCallClosure3_[8] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure3_[9] = Object::Undef;
    applyCodeForCallClosure3_[10] = Object::makeRaw(Instruction::PUSH);
    applyCodeForCallClosure3_[11] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosure3_[12] = Object::Undef;
    applyCodeForCallClosure3_[13] = Object::makeRaw(Instruction::CALL);
    applyCodeForCallClosure3_[14] = Object::makeFixnum(3);
    applyCodeForCallClosure3_[15] = Object::makeRaw(Instruction::HALT);

    callCodeForSetAfterTrigger0Length_ = 7;
    callCodeForSetAfterTrigger0_ = Object::makeObjectArray(callCodeForSetAfterTrigger0Length_);
    callCodeForSetAfterTrigger0_[0] = Object::makeRaw(Instruction::CONSTANT);
    callCodeForSetAfterTrigger0_[1] = Object::Undef;
    callCodeForSetAfterTrigger0_[2] = Object::makeRaw(Instruction::CALL);
    callCodeForSetAfterTrigger0_[3] = Object::makeFixnum(0);
    callCodeForSetAfterTrigger0_[4] = Object::makeRaw(Instruction::RETURN);
    callCodeForSetAfterTrigger0_[5] = Object::makeFixnum(0);
    callCodeForSetAfterTrigger0_[6] = Object::makeRaw(Instruction::HALT);

    callCodeForSetAfterTrigger1Length_ = 10;
    callCodeForSetAfterTrigger1_ = Object::makeObjectArray(callCodeForSetAfterTrigger1Length_);
    callCodeForSetAfterTrigger1_[0] = Object::makeRaw(Instruction::CONSTANT);
    callCodeForSetAfterTrigger1_[1] = Object::Undef;
    callCodeForSetAfterTrigger1_[2] = Object::makeRaw(Instruction::PUSH);
    callCodeForSetAfterTrigger1_[3] = Object::makeRaw(Instruction::CONSTANT);
    callCodeForSetAfterTrigger1_[4] = Object::Undef;
    callCodeForSetAfterTrigger1_[5] = Object::makeRaw(Instruction::CALL);
    callCodeForSetAfterTrigger1_[6] = Object::makeFixnum(1);
    callCodeForSetAfterTrigger1_[7] = Object::makeRaw(Instruction::RETURN);
    callCodeForSetAfterTrigger1_[8] = Object::makeFixnum(0);
    callCodeForSetAfterTrigger1_[9] = Object::makeRaw(Instruction::HALT);

    applyCodeForApplyClosureLength_ = 5;
    applyCodeForApplyClosure_ = Object::makeObjectArray(applyCodeForApplyClosureLength_);
    applyCodeForApplyClosure_[0] = Object::makeRaw(Instruction::CALL);
    applyCodeForApplyClosure_[1] = Object::makeFixnum(0);
    applyCodeForApplyClosure_[2] = Object::makeRaw(Instruction::RETURN);
    applyCodeForApplyClosure_[3] = Object::makeFixnum(0);
    applyCodeForApplyClosure_[4] = Object::makeRaw(Instruction::HALT);

    applyCodeForCallClosureByNameLength_ = 10;
    applyCodeForCallClosureByName_ = Object::makeObjectArray(applyCodeForCallClosureByNameLength_);
    applyCodeForCallClosureByName_[0] = Object::makeRaw(Instruction::FRAME);
    applyCodeForCallClosureByName_[1] = Object::makeFixnum(8);
    applyCodeForCallClosureByName_[2] = Object::makeRaw(Instruction::CONSTANT);
    applyCodeForCallClosureByName_[3] = Object::Undef;
    applyCodeForCallClosureByName_[4] = Object::makeRaw(Instruction::PUSH);
    applyCodeForCallClosureByName_[5] = Object::makeRaw(Instruction::REFER_GLOBAL);
    applyCodeForCallClosureByName_[6] = Object::Undef;
    applyCodeForCallClosureByName_[7] = Object::makeRaw(Instruction::CALL);
    applyCodeForCallClosureByName_[8] = Object::makeFixnum(1);
    applyCodeForCallClosureByName_[9] = Object::makeRaw(Instruction::HALT);

    int callCodeLength_ = 3;
    callCode_ = Object::makeObjectArray(callCodeLength_);
}

VM::~VM() {}

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
        evaluateCodeVector(libCompiler);
        const Object libMatch = FASL_GET(pmatch_image);
        evaluateCodeVector(libMatch);
        CATCH_VM
        // call default error handler
        defaultExceptionHandler(errorObj_);
        this->exit(-1);
    }
}


Object VM::getTopLevelGlobalValue(Object id)
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
    SAVE_REGISTERS();
    const Object loadPort = Object::makeTextualInputFilePort(file.ascii_c_str());
    TextualInputPort* p = loadPort.toTextualInputPort();
    bool readErrorOccured = false;
    for (Object o = p->getDatum(readErrorOccured); !o.isEof(); o = p->getDatum(readErrorOccured)) {
        if (readErrorOccured) {
            callLexicalViolationImmidiaImmediately(this, "read", p->error());
        }
        const Object compiled = compile(o);
//            dumpCompiledCode(compiled);
        evaluateCodeVector(compiled);
    }
    RESTORE_REGISTERS();
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

Object VM::evaluateCodeVector(Object codeVector)
{
    Vector* const v = codeVector.toVector();
    return evaluate(v->data(), v->length());
}

Object VM::evaluate(Object* code, int codeSize)
{
    closureForEvaluate_.toClosure()->pc = code;
    ac_ = closureForEvaluate_;
    dc_ = closureForEvaluate_;
    cl_ = closureForEvaluate_;
    fp_ = 0;
    Object* const direct = getDirectThreadedCode(code, codeSize);
    return run(direct, NULL);
}

Object VM::callClosure0(Object closure)
{
    applyCodeForCallClosure0_[3] = closure;
    SAVE_REGISTERS();
    const Object ret = evaluate(applyCodeForCallClosure0_, applyCodeForCallClosure0Length_);
    RESTORE_REGISTERS();
    return ret;
}

Object VM::callClosure1(Object closure, Object arg)
{
    applyCodeForCallClosure1_[3] = arg;
    applyCodeForCallClosure1_[6] = closure;

    SAVE_REGISTERS();
    const Object ret = evaluate(applyCodeForCallClosure1_, applyCodeForCallClosure1Length_);
    RESTORE_REGISTERS();
    return ret;
}

Object VM::callClosure2(Object closure, Object arg1, Object arg2)
{
    applyCodeForCallClosure2_[3] = arg1;
    applyCodeForCallClosure2_[6] = arg2;
    applyCodeForCallClosure2_[9] = closure;
    SAVE_REGISTERS();
    Object ret = Object::Undef;;
    TRY_VM {
        ret = evaluate(applyCodeForCallClosure2_, applyCodeForCallClosure2Length_);
    CATCH_VM
        // call default error handler
        defaultExceptionHandler(errorObj_);
        this->exit(-1);
    }

    RESTORE_REGISTERS();
    return ret;
}


Object VM::callClosure3(Object closure, Object arg1, Object arg2, Object arg3)
{
    applyCodeForCallClosure3_[3] = arg1;
    applyCodeForCallClosure3_[6] = arg2;
    applyCodeForCallClosure3_[9] = arg3;
    applyCodeForCallClosure3_[12] = closure;
    SAVE_REGISTERS();
    const Object ret = evaluate(applyCodeForCallClosure3_, applyCodeForCallClosure3Length_);
    RESTORE_REGISTERS();
    return ret;
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

#define MAKE_CALL_CODE(argumentCount)           \
        Object::makeRaw(Instruction::CONSTANT), \
        Object::Undef,                          \
        Object::makeRaw(Instruction::CALL),     \
        Object::makeFixnum(argumentCount),      \
        Object::makeRaw(Instruction::RETURN),   \
        Object::makeFixnum(0),                  \
        Object::makeRaw(Instruction::HALT)

#define MAKE_PUSH_FRAME(code, codeSize)           \
    push(Object::makeObjectPointer(pc_));         \
    pc_ = getDirectThreadedCode(code, codeSize);  \
    push(dc_);                                    \
    push(cl_);                                    \
    push(Object::makeObjectPointer(fp_));

Object VM::setAfterTrigger0(Object closure)
{
    MAKE_PUSH_FRAME(callCodeForSetAfterTrigger0_, callCodeForSetAfterTrigger0Length_);
    pc_[1]= closure;
    return ac_;
}


Object VM::setAfterTrigger1(Object closure, Object arg1)
{
    MAKE_PUSH_FRAME(callCodeForSetAfterTrigger1_, callCodeForSetAfterTrigger1Length_);
    pc_[4]= closure;
    pc_[1]= arg1;
    return ac_;
}

void VM::applyClosure(Object closure, Object args)
{
    applyCodeForApplyClosure_[1] =Object::makeFixnum(Pair::length(args));

    // Same as Frame.
    // pc_ points where to return after applyClosure
    push(Object::makeObjectPointer(pc_));
    push(dc_);
    push(cl_);

    // push arguments
    push(Object::makeObjectPointer(fp_));
    for (Object obj = args; !obj.isNil(); obj = obj.cdr()) {
        push(obj.car());
    }
    ac_ = closure;
    pc_ = getDirectThreadedCode(applyCodeForApplyClosure_, applyCodeForApplyClosureLength_);
}

// we need to save registers.
Object VM::callClosureByName(Object procSymbol, Object arg)
{
    MOSH_ASSERT(procSymbol.isSymbol());
    applyCodeForCallClosureByName_[3] = arg;
    applyCodeForCallClosureByName_[6] = procSymbol;

    SAVE_REGISTERS();
    const Object ret = evaluateCodeVector(Object::makeVector(applyCodeForCallClosureByNameLength_, applyCodeForCallClosureByName_));
    RESTORE_REGISTERS();
    return ret;
}

Object VM::apply(Object proc, Object args)
{
    const int procLength = Pair::length(proc);
    const int length  = procLength + 7;
    Object* code = Object::makeObjectArray(length);
    code[0] = Object::makeRaw(Instruction::FRAME);
    code[1] = Object::makeFixnum(procLength + 5);
    code[2] = Object::makeRaw(Instruction::CONSTANT);
    code[3] = args;
    code[4] = Object::makeRaw(Instruction::PUSH);
    int i = 0;
    for (Object o = proc; !o.isNil(); o = o.cdr()) {
        code[5 + i] = o.car();
        i++;
    }
    code[5 + i] = Object::makeRaw(Instruction::APPLY);
    code[6 + i] = Object::makeRaw(Instruction::HALT);

    closureForApply_.toClosure()->pc = code;
    SAVE_REGISTERS();
    Object* const direct = getDirectThreadedCode(code, length);
    dc_ = closureForApply_;
    cl_ = closureForApply_;
    const Object ret = run(direct, NULL);
    RESTORE_REGISTERS();
    return ret;
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
    TRY_VM {
        return evaluateCodeVector(libPsyntax);
    CATCH_VM
        // call default error handler
        defaultExceptionHandler(errorObj_);
        this->exit(-1);
    }
    return Object::Undef;
}

Object VM::getTopLevelGlobalValueOrFalse(Object id)
{
    const Object val = nameSpace_.toEqHashTable()->ref(id, notFound_);
    if (val != notFound_) {
        return val.toGloc()->value();
    } else {
        return Object::False;
    }
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


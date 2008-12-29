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
#include "VM.h"
#include "Closure.h"
#include "VM-inl.h"
#include "Symbol.h"
#include "EqHashTable.h"
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
#include "ByteVectorProcedures.h"
#include "Record.h"
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
#include "Bignum.h"
#include "ByteArrayBinaryInputPort.h"
#include "Fasl.h"

#define TRY     jmp_buf org;                     \
                copyJmpBuf(org, returnPoint_);   \
                if (setjmp(returnPoint_) == 0)   \

#define CATCH   copyJmpBuf(returnPoint_, org); \
                } else {



using namespace scheme;

VM::VM(int stackSize, Object outPort, Object errorPort, Object inputPort, bool isProfiler) :
    ac_(Object::Nil),
    dc_(Object::Nil),
    cl_(Object::Nil),
    pc_(NULL),
    stackSize_(stackSize),
    maxStack_(NULL),
    outputPort_(outPort),
    errorPort_(errorPort),
    inputPort_(inputPort),
    stdinPort_(Object::makeBinaryInputPort(stdin)),
    errorObj_(Object::Nil),
    profilerRunning_(false),
    isProfiler_(isProfiler),
    maxNumValues_(256),
    numValues_(0),
    isR6RSMode_(false)
{
    stack_ = Object::makeObjectArray(stackSize);
    values_ = Object::makeObjectArray(maxNumValues_);
    stackEnd_ = stack_ + stackSize;
    sp_ = stack_;
    fp_ = stack_;
    nameSpace_ = Object::makeEqHashTable();

    outerSourceInfo_   = L2(Object::False, Symbol::intern(UC("<top-level>")));
}

VM::~VM() {}

void VM::loadCompiler()
{
#   include "match.h"
#   include "compiler-with-library.h"
    const Object libCompiler = FASL_GET(compiler_with_library_image);
#ifdef ENABLE_PROFILER
    if (isProfiler_) {
        initProfiler();
    }
#endif
    evaluate(libCompiler);
    const Object libMatch = FASL_GET(match_image);
    evaluate(libMatch);
}


Object VM::getTopLevelGlobalValue(Object id)
{
    const Object val = nameSpace_.toEqHashTable()->ref(id, notFound_);
    if (val != notFound_) {
        return val;
    } else {
        callAssertionViolationAfter("symbol-value2", "unbound variable", L1(id));
        return Object::Undef;
    }
}

void VM::defaultExceptionHandler(Object error)
{
    errorPort_.toTextualOutputPort()->format(UC("\n Exception:\n~a\n"), L1(error));
}

void VM::dumpCompiledCode(Object code) const
{
    MOSH_ASSERT(code.isVector());
    Vector* const v = code.toVector();
    for (int i = 0; i < v->length(); i++) {
        const Object c = v->ref(i);
        if (c.isInstruction()) {
            LOG1("~a ", Instruction::toString(c.val));
        } else {
            LOG1("~a ", c);
        }
    }
}


// これはいずれ Scheme でおきかえる。
void VM::loadFile(const ucs4string& file)
{
    SAVE_REGISTERS();
    TRY {
        const Object loadPort = Object::makeTextualInputFilePort(file.ascii_c_str());
        TextualInputPort* p = loadPort.toTextualInputPort();
        bool readErrorOccured = false;
        for (Object o = p->getDatum(readErrorOccured); !o.isEof(); o = p->getDatum(readErrorOccured)) {
            if (readErrorOccured) {
                callLexicalViolationImmidiaImmediately("read", p->error());
            }
            const Object compiled = compile(o);
//            dumpCompiledCode(compiled);
            evaluate(compiled);
        }

        CATCH
            // call default error handler
            defaultExceptionHandler(errorObj_);
        exit(-1);
    }
    RESTORE_REGISTERS();
}

void VM::load(const ucs4string& file)
{
    TRY {
        ucs4string moshLibPath(UC(MOSH_LIB_PATH));
        moshLibPath += UC("/") + file;
        if (fileExistsP(file)) {
            loadFile(file);
        } else if (fileExistsP(moshLibPath)) {
            loadFile(moshLibPath);
        } else {
            callAssertionViolationImmidiaImmediately("load",
                                                     "cannot find file in load path",
                                                     L1(Object::makeString(file)));
        }
        copyJmpBuf(returnPoint_, org);
    CATCH
        // call default error handler
        defaultExceptionHandler(errorObj_);
        exit(-1);
    }
}

Object VM::evaluate(Object codeVector)
{
    Object ret = Object::Nil;
    TRY {
        Vector* const v = codeVector.toVector();
        ret = evaluate(v->data(), v->length());
    CATCH
        defaultExceptionHandler(errorObj_);
        exit(-1);
    }
    return ret;
}

#include "cprocedures.cpp"

Object VM::evaluate(Object* code, int codeSize)
{
    static Object closure = Object::Undef;
    if (Object::Undef == closure) {

        closure = Object::makeClosure(NULL, 0, 0, false, cProcs, cProcNum, 0, outerSourceInfo_);
    }
    closure.toClosure()->pc = code;
    ac_ = closure;
    dc_ = closure;
    cl_ = closure;
    fp_ = 0;
    Object* const direct = getDirectThreadedCode(code, codeSize);
    return run(direct, NULL);
}

Object VM::getCProcedureName(Object proc)
{
    for (int k = 0; k < cProcNum; k++) {
        if (proc == cProcs[k]) {
            return Symbol::intern(cProcNames[k]);
        }
    }
    return Symbol::intern(UC("<unknwon subr>"));
}

Object VM::callClosure0(Object closure)
{
    static Object applyCode[] = {
        Object::makeRaw(Instruction::FRAME),
        Object::makeFixnum(5),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::CALL),
        Object::makeFixnum(0),
        Object::makeRaw(Instruction::HALT),
    };

    applyCode[3] = closure;

    SAVE_REGISTERS();
    const Object ret = evaluate(applyCode, sizeof(applyCode) / sizeof(Object));
    RESTORE_REGISTERS();
    return ret;
}

Object VM::callClosure1(Object closure, Object arg)
{
    static Object applyCode[] = {
        Object::makeRaw(Instruction::FRAME),
        Object::makeFixnum(8),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::CALL),
        Object::makeFixnum(1),
        Object::makeRaw(Instruction::HALT),
    };

    applyCode[3] = arg;
    applyCode[6] = closure;

    SAVE_REGISTERS();
    const Object ret = evaluate(applyCode, sizeof(applyCode) / sizeof(Object));
    RESTORE_REGISTERS();
    return ret;
}

Object VM::callClosure2(Object closure, Object arg1, Object arg2)
{
    static Object applyCode[] = {
        Object::makeRaw(Instruction::FRAME),
        Object::makeFixnum(11),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::CALL),
        Object::makeFixnum(2),
        Object::makeRaw(Instruction::HALT),
    };
    applyCode[3] = arg1;
    applyCode[6] = arg2;
    applyCode[9] = closure;
    SAVE_REGISTERS();
    const Object ret = evaluate(applyCode, sizeof(applyCode) / sizeof(Object));
    RESTORE_REGISTERS();
    return ret;
}


Object VM::callClosure3(Object closure, Object arg1, Object arg2, Object arg3)
{
    static Object applyCode[] = {
        Object::makeRaw(Instruction::FRAME),
        Object::makeFixnum(14),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::CALL),
        Object::makeFixnum(3),
        Object::makeRaw(Instruction::HALT),
    };
    applyCode[3] = arg1;
    applyCode[6] = arg2;
    applyCode[9] = arg3;
    applyCode[12] = closure;
    SAVE_REGISTERS();
    const Object ret = evaluate(applyCode, sizeof(applyCode) / sizeof(Object));
    RESTORE_REGISTERS();
    return ret;
}

void VM::setOutputPort(Object port)
{
    outputPort_ = port;
}

Object VM::compileWithoutHalt(Object sexp)
{
    static Object compiler = Symbol::intern(UC("compile-w/o-halt"));
    return callClosureByName(compiler, sexp);
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
                                         cProcs,                                // freeVars
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
    static Object callCode[] = {
        MAKE_CALL_CODE(0)
    };
    MAKE_PUSH_FRAME(callCode, sizeof(callCode) / sizeof(Object));
    pc_[1]= closure;
    return ac_;
}


Object VM::setAfterTrigger1(Object closure, Object arg1)
{
    static Object callCode[] = {
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        MAKE_CALL_CODE(1)
    };
    MAKE_PUSH_FRAME(callCode, sizeof(callCode) / sizeof(Object));
    pc_[4]= closure;
    pc_[1]= arg1;

    return ac_;
}

void VM::applyClosure(Object closure, Object args)
{
    static Object applyCode[] = {
        Object::makeRaw(Instruction::CALL),
        Object::makeFixnum(0),
        Object::makeRaw(Instruction::RETURN), // return from applyClosure
        Object::makeFixnum(0),
        Object::makeRaw(Instruction::HALT)
    };

    applyCode[1] =Object::makeFixnum(Pair::length(args));

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
    pc_ = getDirectThreadedCode(applyCode, sizeof(applyCode) / sizeof(Object));
}

// we need to save registers.
Object VM::callClosureByName(Object procSymbol, Object arg)
{
    MOSH_ASSERT(procSymbol.isSymbol());
    static Object applyCode[] = {
        Object::makeRaw(Instruction::FRAME),
        Object::makeFixnum(8),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::REFER_GLOBAL),
        Object::Undef,
        Object::makeRaw(Instruction::CALL),
        Object::makeFixnum(1),
        Object::makeRaw(Instruction::HALT),
    };
    applyCode[3] = arg;
    applyCode[6] = procSymbol;

    SAVE_REGISTERS();
    const Object ret = evaluate(Object::makeVector(sizeof(applyCode) / sizeof(Object), applyCode));
    RESTORE_REGISTERS();
    return ret;
}

Object VM::apply(Object proc, Object args)
{
//     printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//     LOG1("<~a>", proc);
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

    static Object closure = Object::Undef;
    if (Object::Undef == closure) {
//#       include "cprocedures.cpp"
        closure = Object::makeClosure(NULL, 0, 0, false, cProcs, cProcNum, 1, outerSourceInfo_);
    }
    closure.toClosure()->pc = code;
    SAVE_REGISTERS();
    Object* const direct = getDirectThreadedCode(code, length);
    dc_ = closure;
    cl_ = closure;
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
                port->format(UC("    ~d. "), L1(Object::makeFixnum(i)));
                const Object procedure = src.cdr();
                const Object location  = src.car();
                if (location.isFalse()) {
                    port->format(UC("~a: <unknown location>\n"), L1(unGenSyms(procedure)));
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
                        port->format(UC("~a:  ~a:~a\n"), L3(procedureSource, file, lineno));
                    } else {
                        port->format(UC("~a:  ~a:~a\n"), L3(unGenSyms(procedure), file, lineno));
                    }
                }
                i++;
            }
        } else if (cl->isCProcedure()) {
            port->format(UC("    ~d. "), L1(Object::makeFixnum(i)));
            port->format(UC("~a: <subr>\n"), L1(getClosureName(*cl)));
            i++;
        } else if (cl->isRegMatch()) {
            port->format(UC("    ~d. "), L1(Object::makeFixnum(i)));
            port->format(UC("<reg-match>: ~a\n"), L1(*cl));
            i++;
        } else if (cl->isRegexp()) {
            port->format(UC("    ~d. "), L1(Object::makeFixnum(i)));
            port->format(UC("<regexp>: ~a\n"), L1(*cl));
            i++;
        } else {
            MOSH_ASSERT(false);
        }
        if (i > MAX_DEPTH) {
            port->display(UC("      ... (more stack dump truncated)\n"));
            break;
        }

        VM_ASSERT(!(*cl).isObjectPointer());
        VM_ASSERT((*cl).isClosure() || (*cl).isCProcedure() );
        if (fp > stack_) {
            cl = fp - CLOSURE_OFFSET_IN_FRAME;
            if (!((*cl).isClosure()) && !((*cl).isCProcedure())) {
                break;
            }
            // next fp is Object pointer, so 4byte aligned.
            // if it is not Object pointer, may be tail call
            Object* nextFp = fp - FP_OFFSET_IN_FRAME;
            if (!(nextFp->isPointer())) {
                break;
            }
            if (nextFp->isSymbol() ||
                nextFp->isString() ||
                nextFp->isClosure() ||
                nextFp->isCProcedure()) {
                break;
            }
            if (!nextFp->isObjectPointer()) {
                LOG1("val=~s", *nextFp);
            }
            VM_ASSERT(nextFp->isObjectPointer());
            fp = nextFp->toObjectPointer();
        } else {
            break;
        }
    }
    return sysGetOutputStringEx(1, &sport);
}

void VM::throwException(Object exception)
{
#ifdef DEBUG_VERSION
//    LOG1("error~a\n", exception);
    fflush(stderr);
    fflush(stdout);
#endif
    const Object stackTrace = getStackTrace();
    const Object stringOutputPort = Object::makeStringOutputPort();
    TextualOutputPort* const textualOutputPort = stringOutputPort.toTextualOutputPort();
    textualOutputPort->format(UC("~a\n Stack trace:\n~a\n"), Pair::list2(exception, stackTrace));
    errorObj_ = sysGetOutputStringEx(1, &stringOutputPort);

    longjmp(returnPoint_, -1);
}

void VM::showStack(int count, const char* file, int line)
{
    printf("** STACK %s:%d\n", file, line);fflush(stdout);
#ifdef DEBUG_VERSION
    for (int i = count - 1; i >= 0; i--) {
        LOG2("============================================\n~d: ~a\n", Object::makeFixnum(i), index(sp_, i));
    }
#else
    callAssertionViolationImmidiaImmediately("vm", "don't use showStack");
#endif
}

bool VM::isR6RSMode() const
{
    return isR6RSMode_;
}

void VM::activateR6RSMode(bool isDebugExpand)
{
#   include "psyntax.h"
    isR6RSMode_ = true;
    setTopLevelGlobalValue(Symbol::intern(UC("debug-expand")), Object::makeBool(isDebugExpand));
    const Object libPsyntax = FASL_GET(psyntax_image);
    evaluate(libPsyntax);
}


void VM::setTopLevelGlobalValue(Object id, Object val)
{
    nameSpace_.toEqHashTable()->set(id, val);
}

Object VM::getTopLevelGlobalValueOrFalse(Object id)
{
    const Object val = nameSpace_.toEqHashTable()->ref(id, notFound_);
    if (val != notFound_) {
        return val;
    } else {
        return Object::False;
    }
}

Object VM::getOutputPort()
{
    return outputPort_;
}

Object VM::getErrorPort() {
    return errorPort_;
}

void VM::setInputPort(Object port )
{
    inputPort_ = port;
}

Object VM::standardInputPort() const
{
    return stdinPort_;
}

Object VM::currentInputPort()
{
    return inputPort_;
}

void VM::expandStack(int plusSize)
{
    const int nextStackSize = stackSize_ + plusSize;
    Object* nextStack = Object::makeObjectArray(nextStackSize);
    if (NULL == nextStack) {
        // todo
        // handle stack overflow with guard
        callAssertionViolationImmidiaImmediately("#<closure>", "stack overflow", L1(Object::makeFixnum(sp_ - stack_)));
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
    LOG2("==========dc=~a prev=~a \n", dc_, dc_.toClosure()->prev);
    for (int i = 1; i>= 0; i--) {
        if (fp_ + i >= stackEnd_) {
            break;
        }
        const Object obj = referLocal(i);

        if (!obj.isObjectPointer()) {
            LOG2("~d: ~a\n", Object::makeFixnum(i), obj);
        }
        fflush(stderr);
    }
}

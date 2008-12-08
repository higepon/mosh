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

#include <sys/time.h>
#include <signal.h>
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
#include "RegexpProcedures.h"
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
#include "Stack.h"
#include "UtilityProcedures.h"
#include "Arithmetic.h"
#include "FixnumProcedures.h"

#ifdef DUMP_ALL_INSTRUCTIONS
    extern FILE* stream;
#endif

#ifdef ENABLE_PROFILER
#define COUNT_CALL(a) countCall(a)
#else
#define COUNT_CALL(a) /* */
#endif

#define TRY     jmp_buf org;                     \
                copyJmpBuf(org, returnPoint_);   \
                if (setjmp(returnPoint_) == 0)   \

#define CATCH   copyJmpBuf(returnPoint_, org); \
                } else {

using namespace scheme;

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
    libraries_ = Object::makeEqHashTable();
    instances_ = Object::makeEqHashTable();
    nameSpace_ = Object::makeEqHashTable();

    // Source info format (("compiler-with-library.scm" 8149) pass2/adjust-arglist reqargs optarg iargs name)
    // lineno = 0 is not appeared in stack trace.
    outerSourceInfo_   = L2(Object::False, Symbol::intern(UC("<top-level>")));
}

VM::~VM() {}


Object VM::getTopLevelGlobalValue(Object id)
{
    const Object key = idToTopLevelSymbol(id);
    const Object val = nameSpace_.toEqHashTable()->ref(key, notFound_);
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

// same as (eval ...)
Object VM::eval(Object obj, Object env)
{
    const Object code = compile(obj);

    // (CALL) -> evalEx -> eval -> evaluate
    // evaluate が pc_ を壊してしまい evalEx の呼び出し元に戻れない。
    SAVE_REGISTERS();
    const Object ret = evaluate(code);
    RESTORE_REGISTERS();
    return ret;
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


void VM::importTopLevel()
{
    notFound_  = Symbol::intern(UC("vm hash table not found"));
    topLevelInstance_ = Object::makeEqHashTable();
    EqHashTable* ht = instances_.toEqHashTable();
    ht->set(Symbol::intern(UC("top-level")), topLevelInstance_);
}

void VM::initLibraryTable()
{
    const Object toplevel = Symbol::intern(UC("top-level"));
    libraries_.toEqHashTable()->eraseAllExcept(toplevel);
    instances_.toEqHashTable()->eraseAllExcept(toplevel);
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

        closure = Object::makeClosure(NULL, 0, false, cProcs, cProcNum, 0, outerSourceInfo_);
    }
    closure.toClosure()->pc = code;
    ac_ = closure;
    dc_ = closure;
    cl_ = closure;
    fp_ = 0;
    Object* const direct = getDirectThreadedCode(code, codeSize);
    return run(direct, NULL);
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

Object VM::setAfterTrigger1(Object closure, Object arg1)
{
    static Object callCode[] = {
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::PUSH),
        Object::makeRaw(Instruction::CONSTANT),
        Object::Undef,
        Object::makeRaw(Instruction::CALL),
        Object::makeFixnum(1),
        Object::makeRaw(Instruction::RETURN),
        Object::makeFixnum(0),
        Object::makeRaw(Instruction::HALT)
    };
    push(Object::makeObjectPointer(pc_));
    pc_ = getDirectThreadedCode(callCode, sizeof(callCode) / sizeof(Object));

    push(dc_);
    push(cl_);
    push(Object::makeObjectPointer(fp_));
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
    //todo
    ucs4string proc(UC("top-level:$:"));
    proc += procSymbol.toSymbol()->c_str();
    applyCode[3] = arg;
    applyCode[6] = Symbol::intern(proc.c_str());

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
        closure = Object::makeClosure(NULL, 0, false, cProcs, cProcNum, 1, outerSourceInfo_);
    }
    closure.toClosure()->pc = code;
    SAVE_REGISTERS();
    Object* const direct = getDirectThreadedCode(code, length);
    dc_ = closure;
    cl_ = closure;
//    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    const Object ret = run(direct, NULL);
//    printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    RESTORE_REGISTERS();
    return ret;
}

Object VM::compile(Object code)
{
    static Object proc = Symbol::intern(UC("compile"));
    const Object compiled = callClosureByName(proc, code);
    return compiled;
}

//#define DUMP_ALL_INSTRUCTIONS

#ifdef USE_DIRECT_THREADED_CODE
#define SWITCH(val) goto *val;

#ifdef DUMP_ALL_INSTRUCTIONS
#include "DebugInstruction.h"
#define CASE(insn)  LABEL_##insn : logBuf[0] = DebugInstruction:: insn; logBuf[1] = 6;
#define NEXT                        \
{                                   \
    fwrite(logBuf, 1, 2, stream);   \
    goto *(*pc_++).val  ;           \
}
#define NEXT1                       \
{                                   \
    fwrite(logBuf, 1, 2, stream);   \
    numValues_ = 1;                 \
    goto *(*pc_++).val  ;           \
}

#else
#define CASE(insn)  LABEL_##insn :
#define NEXT                         \
{                                    \
    asm volatile(" \t # -- next start");   \
    goto *((*pc_++).val);            \
    asm volatile(" \t # -- next end");   \
}
#define NEXT1                       \
{                                   \
    numValues_ = 1;                 \
    goto *(*pc_++).val;             \
}


#endif // DUMP_ALL_INSTRUCTIONS

#define DEFAULT     LABEL_DEFAULT :
#define INSTRUCTION(insn) &&LABEL_ ## insn

#else /* !USE_DIRECT_THREADED_CODE */
#define SWITCH(val) switch (val)
#define CASE(insn)  case Instruction:: insn :
#define NEXT        break;
#define DEFAULT     default:
#define INSTRUCTION(insn) Instruction:: ## insn
#endif

Object VM::run(Object* code, jmp_buf returnPoint, bool returnTable /* = false */)
{
#ifdef DUMP_ALL_INSTRUCTIONS
    uint8_t logBuf[2];
    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    TextualOutputPort logPort(TextualOutputPort(new FileBinaryOutputPort(stream), transcoder));
#endif
#ifdef USE_DIRECT_THREADED_CODE
#include "labels.cpp"

    if (returnTable) {
#ifdef ENABLE_PROFILER
        labelReturn_ = reinterpret_cast<word>(&&LABEL_RETURN); // used for profiler
#endif
        return Object::makeRaw(dispatch_table);
    }
#endif

    returnCode_[0] = Object::makeRaw(INSTRUCTION(RETURN));
    returnCode_[1] = Object::makeFixnum(0);

    static Object callCode[] = {
        Object::makeRaw(INSTRUCTION(CALL)),
        Object::makeFixnum(0),
        Object::makeRaw(INSTRUCTION(HALT)),
    };

    Object operand = Object::Undef;

    // shourt cut pointers
    EqHashTable* const nameSpace = nameSpace_.toEqHashTable();

    pc_ = code;
    for (;;) {
        const Object insn = *pc_++;
        SWITCH((int)insn.val) {
        CASE(HALT)
        {
#ifdef DUMP_ALL_INSTRUCTIONS
            fwrite(logBuf, 1, 2, stream);
#endif
            return ac_;
        }
        CASE(CALL1)
        {
            operand = Object::makeFixnum(1);
            goto call_entry;
        }
        CASE(CALL2)
        {
            operand = Object::makeFixnum(2);
            goto call_entry;
        }
        CASE(CALL3)
        {
            operand = Object::makeFixnum(3);
            goto call_entry;
        }
        CASE(CALL)
        {
            operand = fetchOperand();
        call_entry:
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = operand.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif
            if (ac_.isCProcedure()) {
#ifdef DUMP_ALL_INSTRUCTIONS
                fwrite(logBuf, 1, 2, stream);
#endif
                COUNT_CALL(ac_);
                cl_ = ac_;
                if (ac_.toCProcedure()->proc == applyEx) {

                    // don't share retCode with others.
                    // because apply's arguments length is not constant.
                    Object* const retCode = Object::makeObjectArray(2);
                    retCode[0] = Object::makeRaw(INSTRUCTION(RETURN));
                    retCode[1] = operand;

                    MOSH_ASSERT(operand.isFixnum());
                    const int argc = operand.toFixnum();
                    pc_  = retCode;

                    ac_.toCProcedure()->call(argc, sp_ - argc);
                } else {
                    CProcedure* const cprocedure = ac_.toCProcedure();
                    // set pc_ before call() for pointing where to return.
                    pc_  = cprocedure->returnCode;
                    pc_[0] = Object::makeRaw(INSTRUCTION(RETURN));
                    pc_[1] = operand;
                    MOSH_ASSERT(operand.isFixnum());
                    const int argc = operand.toFixnum();
                    ac_ = ac_.toCProcedure()->call(argc, sp_ - argc);
                }
            } else if (ac_.isClosure()) {
                const Closure* const c = ac_.toClosure();
                if (c->maxStack + sp_ >= stackEnd_) {
                    printf("CALL: stack expansion\n");
                    expandStack(stackSize_ / 10);
                }
                COUNT_CALL(ac_);
                MOSH_ASSERT(operand.isFixnum());
                const int argLength = operand.toFixnum();
                const int requiredLength = c->argLength;
                dc_ = ac_;
                cl_ = ac_;
                pc_ = c->pc;
                if (c->isOptionalArg) {
                    const int extraLength = argLength - requiredLength;
                    if (-1 == extraLength) {
                        Object* const sp = unShiftArgs(sp_, 1);
                        indexSet(sp, 0, Object::Nil);
                        sp_ = sp;
                        fp_ = sp - requiredLength;
                    } else if (extraLength >= 0) {
                        indexSet(sp_, extraLength, stackToPairArgs(sp_, extraLength + 1));
                        Object* const sp = sp_ - extraLength;
                        fp_ = sp - requiredLength;
                        sp_ = sp;
                    } else {
                        callWrongNumberOfArgumentsViolationAfter(ac_.toClosure()->sourceInfoString(),
                                                                 requiredLength,
                                                                 operand.toFixnum());
                    }
                } else if (requiredLength == argLength) {
                    fp_ = sp_ - argLength;
                } else {
                    Object args = Object::Nil;
                    for (int i = 0; i < operand.toFixnum(); i++) {
                        args = Object::cons(index(sp_, i), args);
                    }
                    callWrongNumberOfArgumentsViolationAfter(ac_.toClosure()->sourceInfoString(),
                                                             requiredLength,
                                                             operand.toFixnum(),
                                                             args);
                }
            } else if (ac_.isCallable()) {
                COUNT_CALL(ac_);
                cl_ = ac_;
                Callable* const callable = ac_.toCallable();
                MOSH_ASSERT(operand.isFixnum());
                const int argc = operand.toFixnum();
                // set pc_ before call() for pointing where to return.
                pc_  = callable->returnCode;
                pc_[0] = Object::makeRaw(INSTRUCTION(RETURN));
                pc_[1] = operand;
                ac_ = ac_.toCallable()->call(this, argc, sp_ - argc);

//                 returnCode_[1] = operand;
//                pc_  = returnCode_;
//                goto return_entry;
            } else if (ac_.isRegexp()) {
                extern Object rxmatchEx(Object args);
                MOSH_ASSERT(operand.isFixnum());
                const int argc = operand.toFixnum();
                Object argv[2];
                argv[0] = ac_;
                argv[1] = sp_[-argc];
                const Object rxmatchProc = Object::makeCProcedure(scheme::rxmatchEx);
                CProcedure* const rxmatchCProc = rxmatchProc.toCProcedure();
                // set pc_ before call() for pointing where to return.
                pc_  = rxmatchCProc->returnCode;
                pc_[0] = Object::makeRaw(INSTRUCTION(RETURN));
                pc_[1] = operand;
                ac_ = rxmatchCProc->call(argc + 1, argv);
            } else if (ac_.isRegMatch()) {
                extern Object regMatchProxy(Object args);
                MOSH_ASSERT(operand.isFixnum());
                const int argc = operand.toFixnum();
                Object argv[2];
                argv[0] = ac_;
                argv[1] = sp_[-argc];
                ac_ = Object::makeCProcedure(scheme::regMatchProxy).toCProcedure()->call(argc + 1, argv);
//                 returnCode_[1] = operand;
//                 pc_  = returnCode_;
                goto return_entry;
            } else {
                callAssertionViolationAfter("apply", "invalid application", L1(ac_));
            }
            NEXT;
        }
        CASE(APPLY)
        {
            const Object args = index(sp_, 0);
            if (args.isNil()) {
                callCode[1] = Object::makeFixnum(0);
                pc_  = callCode;
                sp_--;
            } else {
                if (! args.isPair()) {
                    callAssertionViolationAfter("apply", "bug?", L1(ac_));
                    NEXT;
                }
                const int length = Pair::length(args);
                const int shiftLen = length > 1 ? length - 1 : 0;
                Object* const sp = sp_ + shiftLen; //unShiftArgs(sp_, 0, shiftLen);////
                pairArgsToStack(sp, 0, args);
                callCode[1] = Object::makeFixnum(length);
                pc_ = callCode;
                sp_ = sp;
            }
            NEXT;
        }
        CASE(PUSH)
        {
            TRACE_INSN0("PUSH");
            push(ac_);
            NEXT;
        }
        CASE(CONSTANT_PUSH)
        {
            const Object c = fetchOperand();
            ac_ = c;
            push(ac_);
            NEXT1;
        }
        CASE(ASSIGN_FREE)
        {
            const Object n = fetchOperand();
            MOSH_ASSERT(n.isFixnum());
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = n.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif
            TRACE_INSN1("ASSIGN_FREE", "(~d)\n", n);
            referFree(n).toBox()->set(ac_);
            NEXT;
        }
        CASE(ASSIGN_GLOBAL)
        {
            const Object id = fetchOperand();
            TRACE_INSN1("ASSIGN_GLOBAL", "(~a)\n", id);
//            const Object val = nameSpace_.toEqHashTable()->ref(id, notFound_);
            // "set! to unbound variable" is checked by psyntax.
//             if (val == notFound_) {
//                 Object e = splitId(id);
//                 callAssertionViolationAfter("set!", "can't set! to unbound variable", L1(e.cdr()));
//                 NEXT;
//             } else {
                nameSpace->set(id, ac_);
                ac_ = Object::Undef;
//             }
            NEXT1;
        }
        CASE(ASSIGN_LOCAL)
        {
            const Object n = fetchOperand();
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = n.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif
            TRACE_INSN1("ASSIGN_LOCAL", "(~d)\n", n);
//            index(fp_, n.toFixnum()).toBox()->set(ac_);
            MOSH_ASSERT(n.isFixnum());
            referLocal(n.toFixnum()).toBox()->set(ac_);
//            index(fp_ + , n.toFixnum()).toBox()->set(ac_);
            NEXT;
        }
        CASE(BOX)
        {
            const Object n = fetchOperand();
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = n.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif
            TRACE_INSN1("BOX", "(~a)\n", n);
            MOSH_ASSERT(n.isFixnum());
            indexSet(sp_, n.toFixnum(), Object::makeBox(index(sp_, n.toFixnum())));
            NEXT;
        }
        CASE(CAAR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.car();
                if (ac_.isPair()) {
                    ac_ = ac_.car();
                } else {
                    callAssertionViolationAfter("caar", "pair required", Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter("caar", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CADR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.cdr();
                if (ac_.isPair()) {
                    ac_ = ac_.car();
                } else {
                    callAssertionViolationAfter("cadr", "pair required", Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter("cadr", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CAR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.car();
            } else {
                callAssertionViolationAfter("car", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CAR_PUSH)
        {
            TRACE_INSN0("CAR_PUSH");
            if (ac_.isPair()) {
                push(ac_.car());
            } else {
                // todo エラーにこれを入れれば便利じゃ？
//                LOG1("cl=~a\n", dc_.toClosure()->sourceInfoString());
                callAssertionViolationAfter("car", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CDAR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.car();
                if (ac_.isPair()) {
                    ac_ = ac_.cdr();
                } else {
                    callAssertionViolationAfter("cdar", "pair required", Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter("cdar", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CDDR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.cdr();
                if (ac_.isPair()) {
                    ac_ = ac_.cdr();
                } else {
                    callAssertionViolationAfter("cddr", "pair required", Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter("cddr", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CDR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.cdr();
            } else {
                callAssertionViolationAfter("cdr", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CDR_PUSH)
        {
            TRACE_INSN0("CDR_PUSH");
            if (ac_.isPair()) {
                push(ac_.cdr());
            } else {
                callAssertionViolationAfter("cdr", "pair required", Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CLOSURE)
        {
//             const int skipSize         = fetchOperand().toFixnum();
//             const int argLength        = fetchOperand().toFixnum();
//             const bool isOptionalArg   = !fetchOperand().isFalse();
//             const int freeVariablesNum = fetchOperand().toFixnum();
//             const int maxStack         = fetchOperand().toFixnum();
//             const Object sourceInfo    = fetchOperand();
// //            LOG1("(CLOSURE) source=~a\n", sourceInfo);
//             TRACE_INSN1("CLOSURE", "(n => ~d)\n", Object::makeFixnum(freeVariablesNum));
//             ac_ = Object::makeClosure(pc_, argLength, isOptionalArg, (sp_ - freeVariablesNum), freeVariablesNum, maxStack, sourceInfo);
//             sp_ -= freeVariablesNum;
//             pc_ += skipSize - 6;
//             NEXT1;
            const Object skipSizeObject      = fetchOperand();
            const Object argLengthObject     = fetchOperand();
            const Object isOptionalArgObjecg   = fetchOperand();
            const Object freeVariablesNumObject = fetchOperand();
            const Object maxStackObject         = fetchOperand();
            const Object sourceInfo    = fetchOperand();

            MOSH_ASSERT(skipSizeObject.isFixnum());
            const int skipSize         = skipSizeObject.toFixnum();

            MOSH_ASSERT(argLengthObject.isFixnum());
            const int argLength        = argLengthObject.toFixnum();
            const bool isOptionalArg   = !isOptionalArgObjecg.isFalse();

            MOSH_ASSERT(freeVariablesNumObject.isFixnum());
            const int freeVariablesNum = freeVariablesNumObject.toFixnum();
            MOSH_ASSERT(maxStackObject.isFixnum());
            const int maxStack         =maxStackObject.toFixnum();

//            LOG1("(CLOSURE) source=~a\n", sourceInfo);

            TRACE_INSN1("CLOSURE", "(n => ~d)\n", Object::makeFixnum(freeVariablesNum));
            ac_ = Object::makeClosure(pc_, argLength, isOptionalArg, (sp_ - freeVariablesNum), freeVariablesNum, maxStack, sourceInfo);
            sp_ -= freeVariablesNum;
            pc_ += skipSize - 6;
            NEXT1;
        }
        CASE(CONS)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("CONS");
            ac_ = Object::cons(n, ac_);
            sp_--;
            NEXT1;
        }
        CASE(CONSTANT)
        {
            const Object c = fetchOperand();
            TRACE_INSN0("CONSTANT\n");
            ac_ = c;
            NEXT1;
        }
        CASE(PUSH_CONSTANT)
        {
            push(ac_);
            ac_ = fetchOperand();
            NEXT1;
        }
        CASE(DEFINE_GLOBAL)
        {
            const Object id = fetchOperand();
//            LOG1("define ~a\n", id);
            const Object found = nameSpace->ref(id, notFound_);
            if (found == notFound_) {
                nameSpace->set(id, ac_);
            } else {
                Object e = splitId(id);
                callErrorAfter("define", "defined twice", L1(e.cdr()));
            }
            NEXT;
        }
        CASE(DISPLAY)
        {
            const Object n = fetchOperand();
            MOSH_ASSERT(n.isFixnum());
            const int freeVariablesNum = n.toFixnum();

            // create display closure
            const Object display = Object::makeClosure(NULL, 0, false, sp_ - freeVariablesNum, freeVariablesNum, 0, Object::False);
            if (dc_.isClosure()) {
                dc_.toClosure()->child = display;
            }
            dc_ = display;
            TRACE_INSN0("DISPLAY");
            sp_ = sp_ - freeVariablesNum;
            NEXT;
        }
        CASE(ENTER)
        {
            TRACE_INSN0("ENTER");
            const Object n = fetchOperand(); // not used
            MOSH_ASSERT(n.isFixnum());
            fp_ = sp_ - n.toFixnum();
            NEXT;
        }
        CASE(PUSH_ENTER)
        {
            push(ac_);
            TRACE_INSN0("ENTER");
            const Object n = fetchOperand(); // not used
            MOSH_ASSERT(n.isFixnum());
            fp_ = sp_ - n.toFixnum();
            NEXT;
        }
        CASE(EQ)
        {
            const Object o = index(sp_, 0);
            TRACE_INSN0("EQ");
            ac_ = Object::makeBool(o.eq(ac_));
            sp_--;
            NEXT1;
        }
        CASE(EQV)
        {
            const Object o = index(sp_, 0);
            TRACE_INSN0("EQV");
            ac_ = Object::makeBool(eqv(o, ac_));
            sp_--;
            NEXT1;
        }
        CASE(EQUAL)
        {
            const Object o = index(sp_, 0);
            ac_ = Object::makeBool(equal(o, ac_, new EqHashTable()));
            sp_--;
            NEXT1;
        }
        CASE(PUSH_FRAME)
        {
            push(ac_);
            goto frame_entry;
        }
        CASE(FRAME)
        {
        frame_entry:
            const Object n = fetchOperand();
            TRACE_INSN1("FRAME", "(~d)\n", n);
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = n.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif
            MOSH_ASSERT(n.isFixnum());
            const int skipSize = n.toFixnum();
            push(Object::makeObjectPointer(pc_ + skipSize - 1));
            push(dc_);
            push(cl_);
            push(Object::makeObjectPointer(fp_));
            NEXT;
        }
        CASE(REFER_FREE0_INDIRECT)
        {
            ac_ = referFree(0);
            goto indirect_entry;
        }
        CASE(REFER_FREE1_INDIRECT)
        {
            ac_ = referFree(1);
            goto indirect_entry;
        }
        CASE(INDIRECT)
        {
            TRACE_INSN0("INDIRECT");
        indirect_entry:
            ac_ = ac_.toBox()->value();
            NEXT1;
        }
        CASE(LEAVE1)
        {
            operand = Object::makeFixnum(1);
            goto leave_entry;
        }
        CASE(LEAVE)
        {
            operand= fetchOperand();
        leave_entry:
#ifdef DUMP_ALL_INSTRUCTIONS
            if (operand.toFixnum() <= DebugInstruction::OPERAND_MAX) logBuf[1] = operand.toFixnum();
#endif
            TRACE_INSN1("LEAVE", "(~d)\n", operand);
            MOSH_ASSERT(operand.isFixnum());
            Object* const sp = sp_ - operand.toFixnum();

            const Object fpObject = index(sp, 0);
            MOSH_ASSERT(fpObject.isObjectPointer());
            fp_ = fpObject.toObjectPointer();

            dc_ = index(sp, 1);
            MOSH_ASSERT(dc_.isProcedure());

            sp_ = sp - 2;
            NEXT;
        }
        CASE(LET_FRAME)
        {
            TRACE_INSN0("LET_FRAME");
            const Object maxStack = fetchOperand();
            if (maxStack.toFixnum() + sp_ >= stackEnd_) {
                printf("LET_FRAME: stack expansion\n");
                expandStack(stackSize_ / 10);
            }
            push(dc_);
            push(Object::makeObjectPointer(fp_));
            NEXT;
        }
        CASE(LIST)
        {
            const Object numObject = fetchOperand();
            MOSH_ASSERT(numObject.isFixnum());
            const int num = numObject.toFixnum();
            Object list = Object::Nil;
            for (int i = 0; i < num; i++) {
                list = Object::cons(index(sp_, i), list);
            }
            ac_ = list;
            sp_ = sp_ - num;
            NEXT1;
        }
        CASE(LOCAL_JMP)
        {
            const Object n = fetchOperand();
            TRACE_INSN1("LOCAL_JMP", "(~d)\n", n);
            MOSH_ASSERT(n.isFixnum());
            pc_ += n.toFixnum() - 1;
            NEXT;
        }
        CASE(MAKE_CONTINUATION)
        {
            const Object n = fetchOperand();
            TRACE_INSN1("MAKE_CONTINUATION", "(~d)\n", n);
            ac_ = makeContinuation(n);
            NEXT1;
        }
        CASE(MAKE_VECTOR)
        {
            const Object n = index(sp_, 0);
            MOSH_ASSERT(n.isFixnum());
            ac_ = Object::makeVector(n.toFixnum(), ac_);
            sp_--;
            NEXT1;

        }
        CASE(NOP)
        {
            TRACE_INSN0("NOP");
            NEXT;
        }
        CASE(NOT)
        {
            TRACE_INSN0("NOT");
            ac_ = ac_.isFalse() ? Object::True : Object::False;
            NEXT1;
        }
        CASE(NULL_P)
        {
            TRACE_INSN0("NULL_P");
            ac_ = ac_.isNil() ? Object::True : Object::False;
            NEXT1;
        }
        CASE(APPEND2)
        {
            TRACE_INSN0("APPEND2");
            ac_ = Pair::append2(index(sp_, 0), ac_);
            sp_--;
            NEXT1;
        }
        CASE(NUMBER_ADD)
        {
            TRACE_INSN0("NUMBER_ADD");
            const Object n = index(sp_, 0);
            sp_--;
            ac_ = Arithmetic::add(n, ac_);
            NEXT1;
        }
        CASE(NUMBER_EQUAL)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_EQUAL");
            sp_--;
            ac_ = Object::makeBool(Arithmetic::eq(n, ac_));
            NEXT1;
        }
        CASE(NUMBER_GE)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_GE");
            sp_--;
            ac_ = Object::makeBool(Arithmetic::ge(n, ac_));
            NEXT1;
        }
        CASE(NUMBER_GT)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_GT");
            sp_--;
            ac_ = Object::makeBool(Arithmetic::gt(n, ac_));
            NEXT1;
        }
        CASE(NUMBER_LE)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_LE");
            sp_--;
            ac_ = Object::makeBool(Arithmetic::le(n, ac_));
            NEXT1;
        }
        CASE(NUMBER_LT)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_GE");
            sp_--;
            ac_ = Object::makeBool(Arithmetic::lt(n, ac_));
            NEXT1;
        }
        CASE(NUMBER_MUL)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_MUL");
            sp_--;
            ac_ = Arithmetic::mul(n, ac_);
            NEXT1;
        }
        CASE(NUMBER_DIV)
        {
            const Object n = index(sp_, 0);
            sp_--;
            ac_ = Arithmetic::div(n, ac_);
            NEXT1
        }
        CASE(NUMBER_SUB)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_SUB");
            sp_--;
            ac_ = Arithmetic::sub(n, ac_);
            NEXT1;
        }
        CASE(NUMBER_SUB_PUSH)
        {
            const Object n = index(sp_, 0);
            TRACE_INSN0("NUMBER_SUB");
            sp_--;
            ac_ = Arithmetic::sub(n, ac_);
            push(ac_);
            NEXT1;
        }
        CASE(REFER_LOCAL0_NUMBER_ADD_PUSH)
        {
            ac_ = referLocal(0);
            goto number_add_push_entry;
        }
        CASE(NUMBER_ADD_PUSH)
        {
        number_add_push_entry:
            const Object n = index(sp_, 0);
            sp_--;
            ac_ = Arithmetic::add(n, ac_);
            push(ac_);
            NEXT1;
        }
        CASE(PAIR_P)
        {
            TRACE_INSN0("PAIR_P");
            ac_ = Object::makeBool(ac_.isPair());
            NEXT1;
        }
        CASE(READ)
        {
            bool errorOccured = false;
            TextualInputPort* const inputPort = ac_.isNil() ? inputPort_.toTextualInputPort() : ac_.toTextualInputPort();
            ac_ = inputPort->getDatum(errorOccured);
            if (errorOccured) {
                callLexicalViolationAfter("read", inputPort->error());
            }
            NEXT1;
        }
        CASE(READ_CHAR)
        {
            TRACE_INSN0("READ_CHAR");
            const ucs4char c = ac_.isNil() ? inputPort_.toTextualInputPort()->getChar() : ac_.toTextualInputPort()->getChar();
            ac_= c == EOF ? Object::Eof : Object::makeChar(c);
            NEXT1;
        }
        CASE(REDUCE)
        {
            TRACE_INSN0("REDUCE");
            const Object n = fetchOperand();
            MOSH_ASSERT(n.isFixnum());
            sp_ = fp_ + n.toFixnum();;
            NEXT;
        }
        CASE(REFER_FREE)
        {
            operand = fetchOperand();
        refer_free_entry:
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = operand.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif

            MOSH_ASSERT(operand.isFixnum());
            ac_ = referFree(operand);
            TRACE_INSN1("REFER_FREE", "(~d)\n", operand);
            NEXT1;
        }
        CASE(REFER_FREE_PUSH)
        {
            push(referFree(fetchOperand()));
            NEXT;
        }
        CASE(REFER_FREE0_PUSH)
        {
            push(referFree(0));
            NEXT;
        }
        CASE(REFER_FREE1_PUSH)
        {
            push(referFree(1));
            NEXT;
        }
        CASE(REFER_FREE2_PUSH)
        {
            push(referFree(2));
            NEXT;
        }
        CASE(REFER_FREE0)
        {
            operand = Object::makeFixnum(0);
            goto refer_free_entry;
        }
        CASE(REFER_FREE1)
        {
            operand = Object::makeFixnum(1);
            goto refer_free_entry;
        }
        CASE(REFER_FREE2)
        {
            operand = Object::makeFixnum(2);
            goto refer_free_entry;
        }
        CASE(REFER_FREE3)
        {
            operand = Object::makeFixnum(3);
            goto refer_free_entry;
        }
        CASE(REFER_GLOBAL)
        {
            const Object id = fetchOperand();
            const Object val = nameSpace->ref(id, notFound_);
            if (val == notFound_) {
                Object e = splitId(id);
                callAssertionViolationAfter("eval",
                                            "unbound variable",
                                            // R6RS mode requires demangle of symbol.
                                            L1(unGenSym(e.cdr())));
            } else {
                ac_ = val;
            }
            NEXT1;
        }
        CASE(REFER_GLOBAL_CALL)
        {
            const Object id = fetchOperand();
            const Object val = nameSpace->ref(id, notFound_);
            if (val == notFound_) {
                Object e = splitId(id);
                callAssertionViolationAfter("eval",
                                            "unbound variable",
                                            L1(unGenSym(e.cdr())));
                NEXT1; // for error handling
            } else {
                ac_ = val;
            }
            operand = fetchOperand();
            goto call_entry;
        }
        CASE(REFER_LOCAL)
        {
            operand = fetchOperand();
        refer_local_entry:
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = operand.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif

            MOSH_ASSERT(operand.isFixnum());
            ac_ = referLocal(operand.toFixnum());
            TRACE_INSN1("REFER_LOCAL", "(~d)\n", operand);
            NEXT1;
        }
        CASE(REFER_LOCAL0)
        {
            operand = Object::makeFixnum(0);
            goto refer_local_entry;
        }
        CASE(REFER_LOCAL1)
        {
            operand = Object::makeFixnum(1);
            goto refer_local_entry;
        }
        CASE(REFER_LOCAL2)
        {
            operand = Object::makeFixnum(2);
            goto refer_local_entry;
        }
        CASE(REFER_LOCAL3)
        {
            operand = Object::makeFixnum(3);
            goto refer_local_entry;
        }
        CASE(REFER_LOCAL0_PUSH_CONSTANT)
        {
            push(referLocal(0));
            const Object n = fetchOperand();
            ac_ = n;
            TRACE_INSN0("REFER_LOCAL_PUSH0");
            NEXT1;
        }
        CASE(REFER_LOCAL1_PUSH_CONSTANT)
        {
            push(referLocal(1));
            const Object n = fetchOperand();
            ac_ = n;
            TRACE_INSN0("REFER_LOCAL_PUSH0");
            NEXT1;
        }
        CASE(REFER_LOCAL_PUSH)
        {
            const Object n = fetchOperand();
            MOSH_ASSERT(n.isFixnum());
            push(referLocal(n.toFixnum()));
            TRACE_INSN0("REFER_LOCAL_PUSH0");
            NEXT;
        }
        CASE(REFER_LOCAL0_PUSH)
        {
            push(referLocal(0));
            TRACE_INSN0("REFER_LOCAL_PUSH0");
            NEXT;
        }
        CASE(REFER_LOCAL1_PUSH)
        {
            push(referLocal(1));
            TRACE_INSN0("REFER_LOCAL_PUSH1");
            NEXT;
        }
        CASE(REFER_LOCAL2_PUSH)
        {
            push(referLocal(2));
            TRACE_INSN0("REFER_LOCAL_PUSH1");
            NEXT;
        }
        CASE(RESTORE_CONTINUATION)
        {
            const Object s = fetchOperand();
            sp_ = stack_ + s.toStack()->restore(stack_);
            NEXT;
        }
        CASE(RETURN1)
        {
            operand = Object::makeFixnum(1);
            goto return_entry;
        }
        CASE(RETURN2)
        {
            operand = Object::makeFixnum(2);
            goto return_entry;
        }
        CASE(RETURN3)
        {
            operand = Object::makeFixnum(3);
            goto return_entry;
        }
        CASE(RETURN)
        {
            operand = fetchOperand();
        return_entry:
#ifdef DUMP_ALL_INSTRUCTIONS
            const int m = operand.toFixnum();
            if (m <= DebugInstruction::OPERAND_MAX) logBuf[1] = m;
#endif

            MOSH_ASSERT(operand.isFixnum());
            Object* const sp = sp_ - operand.toFixnum();

            const Object fpObject = index(sp, 0);
            MOSH_ASSERT(fpObject.isObjectPointer());
            fp_ = fpObject.toObjectPointer();

            cl_ = index(sp, 1);
            if (!cl_.isProcedure()) {
                LOG1("proc = ~a\n", cl_);
            }
            MOSH_ASSERT(cl_.isProcedure());

            dc_ = index(sp, 2);
            MOSH_ASSERT(dc_.isProcedure());

            const Object pcObject = index(sp, 3);
            MOSH_ASSERT(pcObject.isObjectPointer());
            pc_ = pcObject.toObjectPointer();

            sp_ = sp - 4;
            NEXT;
        }
        CASE(SET_CAR)
        {
            TRACE_INSN0("SET_CAR");
            const Object p = index(sp_, 0);
            p.car() = ac_;
            ac_ = Object::Undef;
            sp_--;
            NEXT1;
        }
        CASE(SET_CDR)
        {
            TRACE_INSN0("SET_CDR");
            const Object p = index(sp_, 0);
            p.cdr() = ac_;
            ac_ = Object::Undef;
            sp_--;
            NEXT1;
        }
        //---------------------------- SHIFTJ -----------------------------
        //
        // SHIFT for embedded jump which appears in named let optimization.
        //   Two things happens.
        //   1. SHIFT the stack (same as SHIFT operation)
        //   2. Restore fp and c registers.
        //      This is necessary for jump which is across let or closure boundary.
        //      new-fp => new-sp - arg-length
        //
        CASE(SHIFTJ)
        {
            const Object depthObject = fetchOperand();
            MOSH_ASSERT(depthObject.isFixnum());

            const int depth = depthObject.toFixnum();

            const Object diffObject = fetchOperand();
            MOSH_ASSERT(diffObject.isFixnum());
            const int diff  = diffObject.toFixnum();
            sp_ = shiftArgsToBottom(sp_, depth, diff);

            fp_ = sp_ - depth;
            MOSH_ASSERT(index(fp_, 1).isClosure());
            dc_ = index(fp_, 1).toClosure()->child;
            NEXT;
        }
        CASE(SHIFT)
        {
            const Object depthObject = fetchOperand();
            MOSH_ASSERT(depthObject.isFixnum());

            const int depth = depthObject.toFixnum();

            const Object diffObject = fetchOperand();
            MOSH_ASSERT(diffObject.isFixnum());
            const int diff  = diffObject.toFixnum();
#ifdef DUMP_ALL_INSTRUCTIONS
            if (depth <= DebugInstruction::OPERAND_MAX) logBuf[1] = depth;
#endif
            sp_ = shiftArgsToBottom(sp_, depth, diff);
            TRACE_INSN3("SHIFT", "(~d, ~d, sp=>~a)\n", Object::makeFixnum(depth), Object::makeFixnum(diff), Object::makeFixnum(stackSize_ - (stackEnd_ - sp_)));
            NEXT;
        }
        CASE(SHIFT_CALL)
        {
            const Object depthObject = fetchOperand();
            const Object diffObject = fetchOperand();

            MOSH_ASSERT(depthObject.isFixnum());
            MOSH_ASSERT(diffObject.isFixnum());
            const int depth = depthObject.toFixnum();
            const int diff  = diffObject.toFixnum();
            sp_ = shiftArgsToBottom(sp_, depth, diff);
            operand = fetchOperand();
            goto call_entry;
        }
        CASE(SYMBOL_P)
        {
            TRACE_INSN0("SYMBOL_P");
            ac_ = Object::makeBool(ac_.isSymbol());
            NEXT1;
        }
        CASE(TEST)
        {
            TRACE_INSN0("TEST");
        test_entry:
            if (ac_.isFalse()) {
                const Object skipSize = fetchOperand();
                MOSH_ASSERT(skipSize.isFixnum());
                skip(skipSize.toFixnum() - 1);
            } else {
                pc_++;
            }
            NEXT;
        }
        CASE(NOT_TEST)
        {
            ac_ = ac_.isFalse() ? Object::True : Object::False;
            goto test_entry;
        }
        CASE(NUMBER_LE_TEST)
        {
            ac_ = Object::makeBool(Arithmetic::le(index(sp_, 0), ac_));
            sp_--;
            goto test_entry;
        }
        CASE(REFER_LOCAL0_EQV_TEST)
        {
            ac_ = Object::makeBool(eqv(index(sp_, 0), referLocal(0)));
            sp_--;
            goto test_entry;
        }
        CASE(UNDEF)
        {
            TRACE_INSN0("UNDEF");
            ac_ = Object::Undef;
            NEXT1;
        }
        CASE(VECTOR_LENGTH)
        {
            TRACE_INSN0("VECTOR_LENGTH");
            ac_ = Object::makeFixnum(ac_.toVector()->length());
            NEXT1;
        }
        CASE(VECTOR_P)
        {
            TRACE_INSN0("VECTOR_P");
            ac_ = Object::makeBool(ac_.isVector());
            NEXT1;
        }
        CASE(REFER_LOCAL0_VECTOR_REF)
        {
            ac_ = referLocal(0);
            goto vector_ref_entry;
        }
        CASE(REFER_LOCAL0_VECTOR_SET)
        {
            ac_ = referLocal(0);
            goto vector_set_entry;
        }
        CASE(VECTOR_REF)
        {
        vector_ref_entry:
            const Object v = index(sp_, 0);
            MOSH_ASSERT(ac_.isFixnum());
            if (v.isVector()) {
                ac_ = v.toVector()->ref(ac_.toFixnum());
                sp_--;
            } else {
                callAssertionViolationAfter("vector-ref",
                                            "vector required",
                                            L1(v));
            }
            NEXT1;
        }
        CASE(VECTOR_SET)
        {
        vector_set_entry:
            const Object v = index(sp_, 1);
            const Object n = index(sp_, 0);
            MOSH_ASSERT(n.isFixnum());
            if (v.isVector()) {
                v.toVector()->set(n.toFixnum(), ac_);
                ac_ = Object::Undef;
                sp_ -= 2;
            } else {
                callAssertionViolationAfter("vector-set",
                                            "vector required",
                                            L1(v));
            }
            NEXT1;
        }
        CASE(CONTINUATION_VALUES)
        {
            MOSH_ASSERT(ac_.isPair() || ac_.isNil());
            const int num = Pair::length(ac_);
            if (num > maxNumValues_ + 1) {
                callAssertionViolationAfter("values", "too many values", Pair::list1(Object::makeFixnum(num)));
            } else {
                numValues_ = num;
                if (ac_.isPair()) {
                    Object p = ac_.cdr();
                    if (num >= 0) {
                        for (int i = 0; i < num - 1; i++) {
                            values_[i] = p.car();
                            p = p.cdr();
                        }
                        ac_ = ac_.car();
                    }
                } else { // isNil()
                    /* do nothing */
                }
            }
            NEXT;
        }
        CASE(VALUES)
        {
            TRACE_INSN0("VALUES");
            //  values stack layout
            //    (value 'a 'b 'c 'd)
            //    ==>
            //    =====
            //      a
            //    =====
            //      b
            //    =====
            //      c    [ac_] = d
            //    =====
            //  values are stored in [valuez vector] and [a-reg] like following.
            //  #(b c d)
            //  [ac_] = a
            const Object numObject = fetchOperand();
            MOSH_ASSERT(numObject.isFixnum());
            const int num = numObject.toFixnum();
            if (num > maxNumValues_ + 1) {
                callAssertionViolationAfter("values", "too many values", Pair::list1(Object::makeFixnum(num)));
            } else {
                numValues_ = num;
                if (num >= 0) {
                    for (int i = num - 1; i > 0; i--) {
                        values_[i - 1] = ac_;
                        ac_ = index(sp_, num - i - 1);
                    }
                }

                if (numValues_ > 1) {
                    sp_ =  sp_ - (numValues_ - 1);
                } else {
                    // there's no need to push
                }
            }
            NEXT;
        }
        CASE(RECEIVE)
        {
            const Object reqargsObject = fetchOperand();
            const Object optargObject = fetchOperand();
            MOSH_ASSERT(reqargsObject.isFixnum());
            const int reqargs = reqargsObject.toFixnum();
            MOSH_ASSERT(optargObject.isFixnum());
            const int optarg  = optargObject.toFixnum();
            if (numValues_ < reqargs) {
                callAssertionViolationAfter("receive",
                                            "received fewer values than expected",
                                            L2(Object::makeFixnum(numValues_),
                                               Object::makeFixnum(reqargs)));
                NEXT;
            } else if (optarg == 0 && numValues_ > reqargs) {
                callAssertionViolationAfter("receive",
                                            "received more values than expected",
                                            L2(Object::makeFixnum(numValues_),
                                               Object::makeFixnum(reqargs)));
                NEXT;
            }
            // (receive (a b c) ...)
            if (optarg == 0) {
                if (reqargs > 0) {
                    push(ac_);
                }
                for (int i = 0; i < reqargs - 1; i++) {
                    push(values_[i]);
                }
            // (receive a ...)
            } else if (reqargs == 0) {

                Object ret = numValues_ == 0 ? Object::Nil : Pair::list1(ac_);
                for (int i = 0; i < numValues_ - 1; i++) {
                    ret = Pair::appendD(ret, Pair::list1(values_[i]));
                }
                push(ret);
            // (receive (a b . c) ...)
            } else {
                Object ret = Object::Nil;
                push(ac_);
                for (int i = 0; i < numValues_ - 1; i++) {
                    if (i < reqargs - 1) {
                        push(values_[i]);
                    } else {
                        ret = Pair::appendD(ret, Pair::list1(values_[i]));
                    }
                }
                push(ret);
            }
            NEXT1;
        }
        CASE(COMPILE_ERROR)
        {
            const Object who = fetchOperand();
            const Object message = fetchOperand();
            const Object irritants = fetchOperand();
            callAssertionViolationAfter(who, message, irritants);
            NEXT;
        }
        CASE(UNFIXED_JUMP)
        {
            callAssertionViolationAfter("UNFIXED_JUMP", "bug of VM");
            NEXT;
        }
        CASE(STOP)
        {
            printf("STOP for debug\n");
            exit(-1);
        }
        DEFAULT
        {
            callAssertionViolationAfter("VM", "unknown instruction, bug of VM");
            NEXT;
        }
        } // SWITCH
    }
}

// id is (format "~a:$:~a" libname symbolname)
Object VM::splitId(Object id)
{
    MOSH_ASSERT(id.isSymbol());
    const ucs4string text = id.toSymbol()->c_str();
    ucs4string::size_type i = text.find(UC(":$:"));
    if (i == ucs4string::npos) {
        return Object::Nil;
    }
    Object libname = Object::makeString(text.substr(0, i).c_str());
    Object symbol = Symbol::intern(text.substr(i + 3, text.size() - i - 3).c_str());
    return Object::cons(libname, symbol);
}


Object VM::getStackTrace()
{
    const int MAX_DEPTH = 20;

    const Object sport = Object::makeStringOutputPort();
    TextualOutputPort* port = sport.toTextualOutputPort();
    Object* fp = fp_;
    Object* cl = &dc_;
    for (int i = 1;;) {
        port->format(UC("    ~d. "), L1(Object::makeFixnum(i)));
        if (cl->isClosure()) {
            Object src = cl->toClosure()->sourceInfo;
            if (src.isPair()) {
                if (src.car().isFalse()) {
                    port->format(UC("<unknown location>: ~a \n"), L1(src.cdr()));
                } else {
                    const Object lineno = src.car().cdr().car();
                    port->format(UC("~a:~a: ~a \n"), L3(src.car().car(), lineno, src.cdr()));
                }
                i++;
            }
        } else if (cl->isCProcedure()) {
            port->format(UC("<subr>: ~a\n"), L1(getClosureName(*cl)));
            i++;
        } else if (cl->isRegMatch()) {
            port->format(UC("<reg-match>: ~a\n"), L1(*cl));
            i++;
        } else if (cl->isRegexp()) {
            port->format(UC("<regexp>: ~a\n"), L1(*cl));
            i++;
        } else {
            // this case is very rare and may be bug of VM.
//             LOG1("cl = ~a\n", *cl);
//             fprintf(stderr, "fatal: stack corrupt!\n");
            break;//            exit(-1);
        }
        if (i > MAX_DEPTH) {
            port->display(UC("      ... (more stack dump truncated)\n"));
            break;
        }

        if (fp->isString()) {
            // tail call?
            break;
        }
        cl = fp - 2;
        if (cl->isObjectPointer() && !cl->isClosure() && !cl->isCProcedure() && !cl->isRegexp() && !cl->isRegexp()) {
            break;
        }
        if (fp > stack_) {
            fp = (fp - 1)->toObjectPointer();
        } else {
            break;
        }
    }
    return sysGetOutputStringEx(1, &sport);
}

void VM::throwException(Object exception)
{
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

void VM::activateR6RSMode()
{
    isR6RSMode_ = true;
}


#ifdef ENABLE_PROFILER
//----------------------------------------------------------------------
//    Profiler
//----------------------------------------------------------------------
extern VM* theVM;
extern Object stringTosymbolEx(Object args);
const int VM::SAMPLE_NUM = 50000;

static void signal_handler(int signo)
{
    theVM->collectProfile();
}

void VM::initProfiler()
{
    samples_ = Object::makeObjectArray(SAMPLE_NUM);
    callSamples_ = Object::makeObjectArray(SAMPLE_NUM);
    callHash_ = Object::makeEqHashTable();
    totalSampleCount_ = 0;
    for (int i = 0; i < SAMPLE_NUM; i++) {
        samples_[i] = Object::Nil;
        callSamples_[i] = Object::Nil;
    }
    struct sigaction act;
    act.sa_handler = &signal_handler; // set signal_handler
    act.sa_flags = SA_RESTART;        // restart system call after signal handler

    if (sigaction(SIGPROF, &act, NULL) != 0) {
        callAssertionViolationImmidiaImmediately("profiler", "sigaction failed");
    }
    startTimer();
}

void VM::stopProfiler()
{
    stopTimer();
}

void VM::startTimer()
{
    const int INTERVAL_USEC = 10 * 1000;
    struct itimerval tval, oval;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = INTERVAL_USEC;
    tval.it_value.tv_sec = 0;
    tval.it_value.tv_usec = INTERVAL_USEC;
    setitimer(ITIMER_PROF, &tval, &oval);
    profilerRunning_ = true;
}

void VM::stopTimer()
{
    profilerRunning_ = false;
    struct itimerval tval, oval;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = 0;
    tval.it_value.tv_sec = 0;
    tval.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &tval, &oval);
}

void VM::collectProfile()
{
    static int i = 0;
    if (!profilerRunning_) return;
    if (i >= SAMPLE_NUM) {
        errorPort_.toTextualOutputPort()->display(UC("buffer full profiler stopped."));
        stopTimer();
    } else if ((*pc_).val == labelReturn_ && ac_.isCProcedure()) {
        samples_[i++] = ac_;
    } else {
        samples_[i++] = cl_;
    }
    totalSampleCount_++;
}

// this is slow, because it creates namespace hash for each call.
Object VM::getClosureName(Object closure)
{
    EqHashTable* nameSpace = nameSpace_.toEqHashTable()->swap().toEqHashTable();
    if (closure.isCProcedure()) {
        return getCProcedureName(closure);
    } else if (closure.isClosure()) {
        const Object name = nameSpace->ref(closure, notFound_);
        if (name == notFound_) {
            return Object::False;
        } else {
            return stringTosymbol(splitId(name.cdr()));
        }
    } else {
        return Object::False;
    }
}

void VM::storeCallSample()
{
    EqHashTable* callHash = callHash_.toEqHashTable();
    for (int i = 0; i < SAMPLE_NUM; i++) {
        const Object proc = callSamples_[i];
        if (proc.isNil()) continue;
        const Object count = callHash->ref(proc, Object::False);
        if (count.isNil()) {
            /* */
        } else if (count.isFixnum()) {
            callHash->set(proc, Object::makeFixnum(count.toFixnum() + 1));
        } else {
            callHash->set(proc, Object::makeFixnum(1));
        }
        callSamples_[i] = Object::Nil;
    }
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

Object VM::values(int num, const Object* v)
{
    if (0 == num) {
        numValues_ = 0;
        return Object::Undef;
    }
    for (int i = 1; i < num; i++) {
        if (i >= maxNumValues_) {
            callAssertionViolationAfter("values", "too many values", Pair::list1(Object::makeFixnum(i)));
            return Object::Undef;
        }
        values_[i - 1] = v[i];
    }
    numValues_ = num;
    return v[0]; // set to ac_ later.
}

Object VM::getProfileResult()
{
    profilerRunning_ = false;
    stopProfiler();
    Object ret = Object::Nil;
    for (int i = 0; i < SAMPLE_NUM; i++) {
        const Object o = samples_[i];
        if (o.isProcedure()) {
            ret = Pair::append2(ret, L1(o));
        }
    }

    storeCallSample();
    return Object::cons(Object::makeFixnum(totalSampleCount_), Object::cons(callHash_, ret));
}

#endif // ENABLE_PROFILER

void VM::setTopLevelGlobalValue(Object id, Object val)
{
    const Object key = idToTopLevelSymbol(id);
    nameSpace_.toEqHashTable()->set(idToTopLevelSymbol(id), val);
}

Object VM::idToTopLevelSymbol(Object id)
{
    if (!id.isSymbol()) {
        LOG1("id=~a", id);
    }
    MOSH_ASSERT(id.isSymbol());
    ucs4string name(UC("top-level:$:"));
    name += id.toSymbol()->c_str();
    // don't use name variable directly, it is temporary!
    return Symbol::intern(name.strdup());
}

// $library structure accessor.
// This structure is shared with compiler and VM.
void VM::setLibraryMacro(Object library, Object macro)
{
    library.toVector()->set(5, macro);
}
Object VM::getLibraryCompiledBody(Object library)
{
    return library.toVector()->ref(7);
}

Object VM::getTopLevelGlobalValueOrFalse(Object id)
{
    const Object key = idToTopLevelSymbol(id);
    const Object val = nameSpace_.toEqHashTable()->ref(key, notFound_);
    if (val != notFound_) {
        return val;
    } else {
        return Object::False;
    }
}

void VM::import(Object libname)
{
    EqHashTable* const ht = instances_.toEqHashTable();
    if (ht->ref(libname, Object::False).isFalse()) {
        ht->set(libname, Object::makeEqHashTable());
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

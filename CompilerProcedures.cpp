/*
 * CompilerProcedures.cpp - Procedures written in C++ for compiler.
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
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "CompilerProcedures.h"
#include "ProcedureMacro.h"
#include "ListProcedures.h"
#include "CodeBuilder.h"
#include "EqHashTable.h"
#include "Closure.h"
#include "TextualOutputPort.h"
#include "Symbol.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "SimpleStruct.h"
#include "ExecutableMemory.h"
#include "Bignum.h"

using namespace scheme;

static Object pass4FixupLabelCollect(Object vec);
static Object pass4FixupLabel(Object vec);
static Object findFree(VM* theVM, Object iform, Object locals, Object canFrees);
static Object findFreeRec(VM* theVM, Object i, Object l, Object canFrees, Object labelsSeen);
static Object findFreeRecMap(VM* theVM, Object l, Object canFrees, Object labelsSeen, Object list);
static Object findSetsRecMap(VM* theVM, Object lvars, Object list, Object labelsSeen);
static Object findSets(VM* theVM, Object iform, Object lvars);
static Object findSetsRec(VM* theVM, Object i, Object lvars, Object labelsSeen);

enum {
    TAG_CONST         = 0,
    TAG_LET           = 2,
    TAG_SEQ           = 3,
    TAG_LAMBDA        = 4,
    TAG_LOCAL_REF     = 5,
    TAG_LOCAL_ASSIGN  = 6,
    TAG_GLOBAL_REF    = 7,
    TAG_GLOBAL_ASSIGN = 8,
    TAG_UNDEF         = 9,
    TAG_IF            = 10,
    TAG_ASM           = 11,
    TAG_DEFINE        = 12,
    TAG_CALL_CC       = 13,
    TAG_CALL          = 14,
    TAG_LABEL         = 15,
    TAG_LIST          = 16,
    TAG_IT            = 17,
    TAG_RECEIVE       = 18
};

Object scheme::objTointegerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("obj->integer");
    checkArgumentLength(1);
    return Bignum::makeIntegerFromUintprt_t(argv[0].val);
}

// (u8-list->c-procedure u8-list)
// Used for Jit compilation.
Object scheme::u8ListTocProcedureEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("u8-list->c-procedure");
    checkArgumentLength(1);
    argumentCheckList(0, u8List);
    const int length = Pair::length(u8List);

    ExecutableMemory* mem = new ExecutableMemory(length);
    if (!mem->allocate()) {
        callAssertionViolationAfter(theVM, procedureName, "Memory allocation error");
        return Object::Undef;
    }
    uint8_t* address = mem->address();
    int i = 0;
    for (Object obj = u8List; !obj.isNil(); obj = obj.cdr()) {
        if (!obj.car().isFixnum()) {
            callAssertionViolationAfter(theVM, procedureName, "u8 list required", L1(u8List));
            return Object::Undef;
        }
        address[i++] = obj.car().toFixnum();
    }
    mem->flush();
    return Object::makeCProcedure(((Object (*)(VM* vm, int, const Object*))address));
}

static inline uintptr_t getClassMemberPointer(bool (Object::*func)() const)
{
    uintptr_t* p = reinterpret_cast<uintptr_t*>(&func);
    return *p;
}

// (get-c-address name)
Object scheme::getCAddressEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-c-address");
    checkArgumentLength(1);
    argumentAsSymbol(0, name);
    static const ucs4char* names[] = {UC("Object::isNumber"),
                                      UC("Object::True"),
                                      UC("Object::False")
    };
    static uintptr_t pointers[] = {getClassMemberPointer(&Object::isNumber),
                                   (uintptr_t)(&Object::True),
                                   (uintptr_t)(&Object::False)
    };
    MOSH_ASSERT(sizeof(names) == sizeof(pointers));
    for (size_t i = 0; i < sizeof(pointers) / sizeof(uintptr_t); i++) {
        if (ucs4string(names[i]) == ucs4string(name->c_str())) {
            return Bignum::makeIntegerFromUintprt_t(pointers[i]);
        }
    }
    callAssertionViolationAfter(theVM, procedureName, "unknown c function", L1(argv[0]));
    return Object::Undef;
}

Object scheme::labelEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("$label");
    checkArgumentLength(1);
    const Object label = Object::makeVector(3);
    Vector* const v = label.toVector();
    v->set(0, Object::makeFixnum(TAG_LABEL));
    v->set(1, argv[0]);
    v->set(2, Object::False);
    return label;
}

Object scheme::localRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("$localRefEx");
    checkArgumentLength(1);
    const Object label = Object::makeVector(2);
    const Object lvar = argv[0];
    label.toVector()->set(0, Object::makeFixnum(TAG_LOCAL_REF));
    label.toVector()->set(1, lvar);
    MOSH_ASSERT(lvar.isVector());
    Vector* const v = lvar.toVector();
    const Object refCount = v->ref(3);
    MOSH_ASSERT(refCount.isFixnum());
    v->set(3, Object::makeFixnum(refCount.toFixnum() + 1));
    return label;
}

Object scheme::pass1FindSymbolInLvarsEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pass1/find-symbol-in-lvars");
    checkArgumentLength(2);
    const Object symbol = argv[0];
    const Object lvars  = argv[1];
    for (Object p = lvars; p.isPair(); p = p.cdr()) {
        const Object lvar =p.car();
        if (symbol == lvarSym(lvar)) {
            return lvar;
        }
    }
    return Object::False;
}

Object scheme::pass3CompileReferEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pass3/compile-refer");
    checkArgumentLength(4);
    const Object codeBuilder        = argv[0];
    const Object variable           = argv[1];
    const Object localVariablesList = argv[2];
    int localsIndex = 0;
    for (Object p = localVariablesList; p.isPair(); p = p.cdr(), localsIndex++) {
        const Object localVariable = p.car();
        if (localVariable == variable) {
            codeBuilder.toCodeBuilder()->putInstructionArgument1(Object::makeRaw(Instruction::REFER_LOCAL), Object::makeFixnum(localsIndex));
            return Object::makeFixnum(0);
        }
    }

    const Object freeVariablesList = argv[3];
    int freesIndex = 0;
    for (Object p = freeVariablesList; p.isPair(); p = p.cdr(), freesIndex++) {
        const Object freeVariable = p.car();
        if (freeVariable == variable) {
            codeBuilder.toCodeBuilder()->putInstructionArgument1(Object::makeRaw(Instruction::REFER_FREE), Object::makeFixnum(freesIndex));
            return Object::makeFixnum(0);
        }
    }
    callAssertionViolationAfter(theVM, procedureName, "bug? Unknown lvar", L1(variable));
    return Object::Undef;
}


Object scheme::pass3FindFreeEx(VM* theVM, int argc, const Object* argv)
{
    return findFree(theVM, argv[0], argv[1], argv[2]);
}

Object scheme::pass3FindSetsEx(VM* theVM, int argc, const Object* argv)
{
    return findSets(theVM, argv[0], argv[1]);
}

Object findFree(VM* theVM, Object iform, Object locals, Object canFrees)
{
    const Object ret = findFreeRec(theVM, iform, locals, canFrees, Object::Nil);
    return uniq(ret);
}

bool existsInCanFrees(Object sym, Object canFrees)
{
    for (Object frees = canFrees; frees.isPair(); frees = frees.cdr()) {
        if (!memq(sym, frees.car()).isFalse()) {
            return true;
        }
    }
    return false;
}

Object findFreeLet(VM* theVM, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object letLvars = v->ref(2);
        const Object letInits = v->ref(3);
        const Object letBody = v->ref(4);
        return Pair::append2(findFreeRecMap(theVM, l, canFrees, labelsSeen, letInits),
                             findFreeRec(theVM, letBody, letLvars, canFrees, labelsSeen));
}

Object findFreeReceive(VM* theVM, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        const Object receiveLVars = v->ref(1);
        return Pair::append2(findFreeRec(theVM, receiveVals, l, canFrees, labelsSeen),
                             findFreeRec(theVM, receiveBody, receiveLVars, canFrees, labelsSeen));

}

Object findFreeSeq(VM* theVM, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object seqBody = v->ref(1);
        return findFreeRecMap(theVM, l, canFrees, labelsSeen, seqBody);
}

Object findFreeLambda(VM* theVM, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object lambdaBody = v->ref(6);
        const Object lambdaLvars = v->ref(5);
        return findFreeRec(theVM, lambdaBody, lambdaLvars, canFrees, labelsSeen);
}

Object findFreeLocalAssign(VM* theVM, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
    const Object sym = v->ref(1).toVector()->ref(1);
        const Object val = v->ref(2);
        if (existsInCanFrees(sym, canFrees)) {
            return Object::cons(sym, findFreeRec(theVM, val, l, canFrees, labelsSeen));
        } else {
            return findFreeRec(theVM, val, l, canFrees, labelsSeen);
        }
}

Object findFreeLocalRef(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object sym = v->ref(1).toVector()->ref(1);
        if (!memq(sym, l).isFalse()) {
            return Object::Nil;
        } else if (existsInCanFrees(sym, canFrees)) {
            return Pair::list1(sym);
        } else {
            return Object::Nil;
        }
}

Object findFreeGlobalRef(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object sym = v->ref(1);
        if (existsInCanFrees(sym, canFrees)) {
            return Pair::list1(sym);
        } else {
            return Object::Nil;
        }
}

Object findFreeIf(VM* theVM, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
    const Object testF = findFreeRec(theVM, v->ref(1), l, canFrees, labelsSeen);
    const Object thenF = findFreeRec(theVM, v->ref(2), l, canFrees, labelsSeen);
    const Object elseF = findFreeRec(theVM, v->ref(3), l, canFrees, labelsSeen);
    return Pair::append2(testF, Pair::append2(thenF, elseF));
}

Object findFreeLabel(VM* theVM, Object i, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object labelBody = v->ref(1);
        if (!memq(i, labelsSeen).isFalse()) {
            return Object::Nil;
        } else {
            return findFreeRec(theVM, labelBody, l, canFrees, Object::cons(i, labelsSeen));
        }
}

Object findFreeCall(VM* theVM, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findFreeRecMap(theVM, l, canFrees, labelsSeen, callArgs),
                             findFreeRec(theVM, callProc, l, canFrees, labelsSeen));
}


Object findFreeRec(VM* theVM, Object i, Object l, Object canFrees, Object labelsSeen)
{
    Vector* v = i.toVector();
    MOSH_ASSERT(v->ref(0).isFixnum());
    switch(v->ref(0).toFixnum()) {
    case TAG_CONST:
        return Object::Nil;
    case TAG_LET:
    {
        return findFreeLet(theVM, v, l, canFrees, labelsSeen);
    }
    case TAG_RECEIVE:
    {
        return findFreeReceive(theVM, v, l, canFrees, labelsSeen);
    }
    case TAG_SEQ:
    {
        return findFreeSeq(theVM, v, l, canFrees, labelsSeen);
    }
    case TAG_LAMBDA:
    {
        return findFreeLambda(theVM, v, l, canFrees, labelsSeen);
    }
    case TAG_LOCAL_ASSIGN:
    {
        return findFreeLocalAssign(theVM, v, l, canFrees, labelsSeen);
    }
    case TAG_LOCAL_REF:
    {
        return findFreeLocalRef(v, l, canFrees, labelsSeen);
    }
    case TAG_GLOBAL_REF:
    {
        return findFreeGlobalRef(v, l, canFrees, labelsSeen);
    }
    case TAG_UNDEF:
        return Object::Nil;
    case TAG_IF:
    {
        return findFreeIf(theVM, v, l, canFrees, labelsSeen);
    }
    case TAG_ASM:
    {
        const Object asmArgs = v->ref(2);
        return findFreeRecMap(theVM, l, canFrees, labelsSeen, asmArgs);
    }
    case TAG_DEFINE:
    {
        const Object defineVal = v->ref(2);
        return findFreeRec(theVM, defineVal, l, canFrees, labelsSeen);
    }
    case TAG_CALL:
    {
        return findFreeCall(theVM, v, l, canFrees, labelsSeen);
    }
    case TAG_CALL_CC:
    {
        const Object callccProc = v->ref(1);
        return findFreeRec(theVM, callccProc, l, canFrees, labelsSeen);
    }
    case TAG_GLOBAL_ASSIGN:
    {
        const Object globalAssignVal = v->ref(2);
        return findFreeRec(theVM, globalAssignVal, l, canFrees, labelsSeen);
    }
    case TAG_LIST:
    {
        const Object listArgs = v->ref(1);
        return findFreeRecMap(theVM, l, canFrees, labelsSeen, listArgs);
    }
    case TAG_LABEL:
    {
        return findFreeLabel(theVM, i, v, l, canFrees, labelsSeen);
    }
    case TAG_IT:
        return Object::Nil;
    default:
        callAssertionViolationAfter(theVM, "find-free", "unknown iform", L1(v->ref(0)));
        return Object::Undef;
    }
    return Object::Undef;
}


Object findFreeRecMap(VM* theVM, Object l, Object canFrees, Object labelsSeen, Object list)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        ret = Pair::append2(ret, findFreeRec(theVM, p.car(), l, canFrees, labelsSeen));
    }
    return ret;
}

Object findSetsRecMap(VM* theVM, Object lvars, Object list, Object labelsSeen)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        ret = Pair::append2(ret, findSetsRec(theVM, p.car(), lvars, labelsSeen));
    }
    return ret;
}

Object findSets(VM* theVM, Object iform, Object lvars)
{
    return uniq(findSetsRec(theVM, iform, lvars, Object::Nil));
}

Object findSetsLabel(VM* theVM, Object i, Object lvars, Object labelsSeen)
{
    Vector* v = i.toVector();
    const Object labelBody = v->ref(1);
    if (!memq(i, labelsSeen).isFalse()) {
        return Object::Nil;
    } else {
        return findSetsRec(theVM, labelBody, lvars, Object::cons(i, labelsSeen));
    }
}

Object findSetsRec(VM* theVM, Object i, Object lvars, Object labelsSeen)
{
    Vector* v = i.toVector();
    MOSH_ASSERT(v->ref(0).isFixnum());
    switch(v->ref(0).toFixnum()) {
    case TAG_CONST:
        return Object::Nil;
    case TAG_LET:
    {
        const Object letInits = v->ref(3);
        const Object letBody = v->ref(4);
        return Pair::append2(findSetsRecMap(theVM, lvars, letInits, labelsSeen),
                             findSetsRec(theVM, letBody, lvars, labelsSeen));
    }
    case TAG_RECEIVE:
    {
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        return Pair::append2(findSetsRec(theVM, receiveVals, lvars, labelsSeen),
                             findSetsRec(theVM, receiveBody, lvars, labelsSeen));
    }
    case TAG_SEQ:
    {
        const Object seqBody = v->ref(1);
        return findSetsRecMap(theVM, lvars, seqBody, labelsSeen);
    }
    case TAG_LAMBDA:
    {
        const Object lambdaBody = v->ref(6);
        return findSetsRec(theVM, lambdaBody, lvars, labelsSeen);
    }
    case TAG_LOCAL_ASSIGN:
    {
        const Object localAssignLvar = v->ref(1);
        const Object localAssignVal = v->ref(2);
        if (memq(localAssignLvar, lvars).isFalse()) {
            return findSetsRec(theVM, localAssignVal, lvars, labelsSeen);
        } else {
            return Object::cons(localAssignLvar, findSetsRec(theVM, localAssignVal, lvars, labelsSeen));
        }
    }
    case TAG_LOCAL_REF:
    {
        return Object::Nil;
    }
    case TAG_GLOBAL_REF:
    {
        return Object::Nil;
    }
    case TAG_UNDEF:
        return Object::Nil;
    case TAG_IF:
    {
        const Object testF = findSetsRec(theVM, v->ref(1), lvars, labelsSeen);
        const Object thenF = findSetsRec(theVM, v->ref(2), lvars, labelsSeen);
        const Object elseF = findSetsRec(theVM, v->ref(3), lvars, labelsSeen);
        return Pair::append2(testF, Pair::append2(thenF, elseF));
    }
    case TAG_ASM:
    {
        const Object asmArgs = v->ref(2);
        return findSetsRecMap(theVM, lvars, asmArgs, labelsSeen);
    }
    case TAG_DEFINE:
    {
        const Object defineVal = v->ref(3);
        return findSetsRec(theVM, defineVal, lvars, labelsSeen);
    }
    case TAG_CALL:
    {
        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findSetsRecMap(theVM, lvars, callArgs, labelsSeen),
                             findSetsRec(theVM, callProc, lvars, labelsSeen));
    }
    case TAG_CALL_CC:
    {
        const Object callccProc = v->ref(1);
        return findSetsRec(theVM, callccProc, lvars, labelsSeen);
    }
    case TAG_GLOBAL_ASSIGN:
    {
        const Object globalAssignVal = v->ref(2);
        return findSetsRec(theVM, globalAssignVal, lvars, labelsSeen);
    }
    case TAG_LIST:
    {
        const Object listArgs = v->ref(1);
        return findSetsRecMap(theVM, lvars, listArgs, labelsSeen);
    }
    case TAG_LABEL:
    {
        return findSetsLabel(theVM, i, lvars, labelsSeen);
    }
    case TAG_IT:
        return Object::Nil;
    default:
        callAssertionViolationAfter(theVM, "find-free", "unknown iform", L1(v->ref(0)));
        return Object::Undef;
    }
    return Object::Undef;
}


Object scheme::pass4FixupLabelsEx(VM* theVM, int argc, const Object* argv)
{
    return pass4FixupLabel(argv[0]);
}

Object scheme::makeCodeBuilderEx(VM* theVM, int argc, const Object* argv)
{
    return Object::makeCodeBuilder();
}

Object scheme::codeBuilderPutExtra1DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put1!");
    checkArgumentLength(2);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    return Object::Undef;
}


Object scheme::codeBuilderPutExtra2DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put2!");
    checkArgumentLength(3);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    codeBuilder->putExtra(argv[2]);
    return Object::Undef;
}

Object scheme::codeBuilderPutExtra3DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put3!");
    checkArgumentLength(4);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    codeBuilder->putExtra(argv[2]);
    codeBuilder->putExtra(argv[3]);
    return Object::Undef;
}

Object scheme::codeBuilderPutExtra4DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put4!");
    checkArgumentLength(5);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    codeBuilder->putExtra(argv[2]);
    codeBuilder->putExtra(argv[3]);
    codeBuilder->putExtra(argv[4]);
    return Object::Undef;
}


Object scheme::codeBuilderPutExtra5DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put5!");
    checkArgumentLength(6);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    codeBuilder->putExtra(argv[2]);
    codeBuilder->putExtra(argv[3]);
    codeBuilder->putExtra(argv[4]);
    codeBuilder->putExtra(argv[5]);
    return Object::Undef;
}


Object scheme::codeBuilderAppendDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-append!");
    checkArgumentLength(2);

    argumentAsCodeBuilder(0, destCodeBuilder);
    argumentAsCodeBuilder(1, sourceCodeBuilder);
    destCodeBuilder->append(sourceCodeBuilder);
    return Object::Undef;
}

Object scheme::codeBuilderEmitEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-emit2");
    checkArgumentLength(1);

    argumentAsCodeBuilder(0, codeBuilder);
    return codeBuilder->emit();
}

Object scheme::codeBuilderPutInsnArg2DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put-insn-arg2!");
    checkArgumentLength(4);

    argumentAsCodeBuilder(0, codeBuilder);
    const Object instruction = argv[1];
    const Object argument1 = argv[2];
    const Object argument2 = argv[3];
    codeBuilder->putInstructionArgument2(instruction, argument1, argument2);
    return Object::Undef;
}

Object scheme::codeBuilderPutInsnArg1DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put-insn-arg1!");
    checkArgumentLength(3);

    argumentAsCodeBuilder(0, codeBuilder);
    const Object instruction = argv[1];
    const Object argument = argv[2];
    codeBuilder->putInstructionArgument1(instruction, argument);
    return Object::Undef;
}

Object scheme::codeBuilderPutInsnArg0DEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put-insn-arg0!");
    checkArgumentLength(2);
    const Object cb = argv[0];

    argumentAsCodeBuilder(0, codeBuilder);
    const Object instruction = argv[1];

    codeBuilder->putInstructionArgument0(instruction);
    return Object::Undef;
}

Object pass4FixupLabelCollect(Object vec)
{
    static const Object NOP                     = Object::makeRaw(Instruction::NOP);
    static const Object UNFIXED_JUMP            = Object::makeRaw(Instruction::UNFIXED_JUMP);
    static const Object TEST                    = Object::makeRaw(Instruction::TEST);
    static const Object BRANCH_NOT_NUMBER_EQUAL = Object::makeRaw(Instruction::BRANCH_NOT_NUMBER_EQUAL);
    static const Object BRANCH_NOT_NULL         = Object::makeRaw(Instruction::BRANCH_NOT_NULL);
    static const Object BRANCH_NOT_LE           = Object::makeRaw(Instruction::BRANCH_NOT_LE);
    static const Object BRANCH_NOT_LT           = Object::makeRaw(Instruction::BRANCH_NOT_LT);
    static const Object BRANCH_NOT_GE           = Object::makeRaw(Instruction::BRANCH_NOT_GE);
    static const Object BRANCH_NOT_GT           = Object::makeRaw(Instruction::BRANCH_NOT_GT);
    static const Object BRANCH_NOT_EQ           = Object::makeRaw(Instruction::BRANCH_NOT_EQ);
    static const Object BRANCH_NOT_EQV          = Object::makeRaw(Instruction::BRANCH_NOT_EQV);
    static const Object BRANCH_NOT_EQUAL        = Object::makeRaw(Instruction::BRANCH_NOT_EQUAL);
    static const Object NOT_TEST                = Object::makeRaw(Instruction::NOT_TEST);
    static const Object FRAME                   = Object::makeRaw(Instruction::FRAME);
    static const Object PUSH_FRAME              = Object::makeRaw(Instruction::PUSH_FRAME);
    static const Object CLOSURE                 = Object::makeRaw(Instruction::CLOSURE);
    static const Object REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE =
        Object::makeRaw(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE);
    static const Object REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE =
        Object::makeRaw(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE);
    static const Object REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL =
        Object::makeRaw(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL);
    static const Object REFER_LOCAL_BRANCH_NOT_NULL =
        Object::makeRaw(Instruction::REFER_LOCAL_BRANCH_NOT_NULL);
    static const Object REFER_LOCAL_BRANCH_NOT_LT =
        Object::makeRaw(Instruction::REFER_LOCAL_BRANCH_NOT_LT);

    const Vector* const v = vec.toVector();
    const int length = v->length();
    const Object ret = Object::makeVector(length, NOP);
    Vector* const rv= ret.toVector();
    Object labels = Object::makeEqHashTable();
    EqHashTable* const table = labels.toEqHashTable();
    for (int i = 0, j = 0; i < length;) {
        const Object insn = v->ref(i);
        if (insn == UNFIXED_JUMP              ||
            insn == TEST                      ||
            insn == BRANCH_NOT_NULL           ||
            insn == BRANCH_NOT_NUMBER_EQUAL   ||
            insn == BRANCH_NOT_LE             ||
            insn == BRANCH_NOT_LT             ||
            insn == BRANCH_NOT_GE             ||
            insn == BRANCH_NOT_GT             ||
            insn == BRANCH_NOT_EQ             ||
            insn == BRANCH_NOT_EQV            ||
            insn == BRANCH_NOT_EQUAL          ||
            insn == NOT_TEST                  ||
            insn == FRAME                     ||
            insn == PUSH_FRAME                ||
            insn == CLOSURE) {
            rv->set(j, insn);
            rv->set(j + 1, v->ref(i + 1));
            i += 2;
            j += 2;
        } else if (insn == REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE ||
                   insn == REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE ||
                   insn == REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL) {
            rv->set(j, insn);
            rv->set(j + 1, v->ref(i + 1));
            rv->set(j + 2, v->ref(i + 2));
            rv->set(j + 3, v->ref(i + 3));
            i += 4;
            j += 4;
        } else if (insn == REFER_LOCAL_BRANCH_NOT_NULL ||
                   insn == REFER_LOCAL_BRANCH_NOT_LT) {
            rv->set(j, insn);
            rv->set(j + 1, v->ref(i + 1));
            rv->set(j + 2, v->ref(i + 2));
            i += 3;
            j += 3;
        } else if (insn.isVector() && insn.toVector()->length() > 0 && insn.toVector()->ref(0).isFixnum() &&
                   insn.toVector()->ref(0).toFixnum() == TAG_LABEL) {
            i++;
            table->set(insn, Object::makeFixnum(j));
        } else {
            rv->set(j, insn);
            i++;
            j++;
        }
    }
    return Object::cons(ret, labels);
}

Object pass4FixupLabel(Object vec)
{
    static const Object UNFIXED_JUMP            = Object::makeRaw(Instruction::UNFIXED_JUMP);
    static const Object TEST                    = Object::makeRaw(Instruction::TEST);
    static const Object BRANCH_NOT_NULL         = Object::makeRaw(Instruction::BRANCH_NOT_NULL);
    static const Object BRANCH_NOT_NUMBER_EQUAL = Object::makeRaw(Instruction::BRANCH_NOT_NUMBER_EQUAL);
    static const Object BRANCH_NOT_LE           = Object::makeRaw(Instruction::BRANCH_NOT_LE);
    static const Object BRANCH_NOT_LT           = Object::makeRaw(Instruction::BRANCH_NOT_LT);
    static const Object BRANCH_NOT_GE           = Object::makeRaw(Instruction::BRANCH_NOT_GE);
    static const Object BRANCH_NOT_GT           = Object::makeRaw(Instruction::BRANCH_NOT_GT);
    static const Object BRANCH_NOT_EQ           = Object::makeRaw(Instruction::BRANCH_NOT_EQ);
    static const Object BRANCH_NOT_EQV          = Object::makeRaw(Instruction::BRANCH_NOT_EQV);
    static const Object BRANCH_NOT_EQUAL        = Object::makeRaw(Instruction::BRANCH_NOT_EQUAL);
    static const Object NOT_TEST                = Object::makeRaw(Instruction::NOT_TEST);
    static const Object FRAME                   = Object::makeRaw(Instruction::FRAME);
    static const Object PUSH_FRAME              = Object::makeRaw(Instruction::PUSH_FRAME);
    static const Object CLOSURE                 = Object::makeRaw(Instruction::CLOSURE);
    static const Object LOCAL_JMP               = Object::makeRaw(Instruction::LOCAL_JMP);
    static const Object RETURN                  = Object::makeRaw(Instruction::RETURN);
    static const Object REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE =
        Object::makeRaw(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE);
    static const Object REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE =
        Object::makeRaw(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE);
    static const Object REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL =
        Object::makeRaw(Instruction::REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL);
    static const Object REFER_LOCAL_BRANCH_NOT_NULL =
        Object::makeRaw(Instruction::REFER_LOCAL_BRANCH_NOT_NULL);
    static const Object REFER_LOCAL_BRANCH_NOT_LT =
        Object::makeRaw(Instruction::REFER_LOCAL_BRANCH_NOT_LT);

    const Object collected = pass4FixupLabelCollect(vec);
    Vector* const code = collected.car().toVector();
    const Object labels = collected.cdr();
    EqHashTable* const table = labels.toEqHashTable();
    const int length = code->length();

    for (int i = 0; i < length;) {
        const Object insn = code->ref(i);
        if (insn == UNFIXED_JUMP) {
            const Object label = table->ref(code->ref(i + 1), Object::False);
            if (!labels.isFalse()) {
                code->set(i, LOCAL_JMP);
                code->set(i + 1, Object::makeFixnum(label.toFixnum() - i - 1));
                i += 2;
            } else {
                i++;
            }
        } else if (insn == TEST                           ||
                   insn == BRANCH_NOT_NUMBER_EQUAL        ||
                   insn == BRANCH_NOT_NULL                ||
                   insn == BRANCH_NOT_LE                  ||
                   insn == BRANCH_NOT_LT                  ||
                   insn == BRANCH_NOT_GE                  ||
                   insn == BRANCH_NOT_GT                  ||
                   insn == BRANCH_NOT_EQ                  ||
                   insn == BRANCH_NOT_EQV                 ||
                   insn == BRANCH_NOT_EQUAL               ||
                   insn == NOT_TEST                       ||
                   insn == FRAME                          ||
                   insn == PUSH_FRAME                     ||
                   insn == CLOSURE) {
            const Object label = table->ref(code->ref(i + 1), Object::False);
            if (!labels.isFalse()) {
                code->set(i, insn);
                code->set(i + 1, Object::makeFixnum(label.toFixnum() - i - 1));
                i += 2;
            } else {
                i++;
            }
        } else if (insn == REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE ||
                   insn == REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE ||
                   insn == REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL) {
            const Object label = table->ref(code->ref(i + 3), Object::False);
            if (!labels.isFalse()) {
                code->set(i, insn);
                code->set(i + 3, Object::makeFixnum(label.toFixnum() - i - 3));
                i += 4;
            } else {
                i++;
            }
        } else if (insn == REFER_LOCAL_BRANCH_NOT_NULL ||
                   insn == REFER_LOCAL_BRANCH_NOT_LT) {
            const Object label = table->ref(code->ref(i + 2), Object::False);
            if (!labels.isFalse()) {
                code->set(i, insn);
                code->set(i + 2, Object::makeFixnum(label.toFixnum() - i - 2));
                i += 3;
            } else {
                i++;
            }
        } else {
            i++;
        }
    }
    // Peephole optimization
    for (int i = 0; i < length; i++) {
        const Object insn = code->ref(i);

        // when jump destination is jump.
        if (insn == LOCAL_JMP || insn == FRAME) {
            MOSH_ASSERT(i + 1 < length);
            MOSH_ASSERT(code->ref(i + 1).isFixnum());
            const int offset = code->ref(i + 1).toFixnum() + 1;
            const int destinationIndex = i + offset;
            MOSH_ASSERT(i + offset < length);
            const Object dest = code->ref(destinationIndex);
            if (dest == LOCAL_JMP) {
                code->set(i + 1, Object::makeFixnum(offset + code->ref(destinationIndex + 1).toFixnum()));
            } else if (dest == RETURN) {
                //  jump LABEL
                //  ...
                // LABEL:
                //  return or leave
                // =>
                //  leave
                //  ...
                // LABEL:
                //  return or leave
                code->set(i, RETURN);
                code->set(i + 1, code->ref(destinationIndex + 1));
            }

        } else if (insn == TEST ||
                   insn == BRANCH_NOT_NULL ||
                   insn == BRANCH_NOT_LE ||
                   insn == BRANCH_NOT_LT ||
                   insn == BRANCH_NOT_GT ||
                   insn == BRANCH_NOT_GE ||
                   insn == BRANCH_NOT_NUMBER_EQUAL ||
                   insn == BRANCH_NOT_EQ ||
                   insn == BRANCH_NOT_EQV ||
                   insn == BRANCH_NOT_EQUAL
            ) {
            // when test destination is test
            // if ac_ == #f, test in destination is also #f.
            MOSH_ASSERT(i + 1 < length);
            MOSH_ASSERT(code->ref(i + 1).isFixnum());
            const int offset = code->ref(i + 1).toFixnum() + 1;
            const int destinationIndex = i + offset;
            MOSH_ASSERT(i + offset < length);
            if (code->ref(destinationIndex) == TEST ||
                code->ref(destinationIndex) == LOCAL_JMP) {
                code->set(i + 1, Object::makeFixnum(offset + code->ref(destinationIndex + 1).toFixnum()));
            }
        }
    }
    return collected.car();
}

Object scheme::disasmEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("disasm");
    checkArgumentLength(1);
    argumentAsClosure(0, closure);
    Object* code = theVM->disasm(closure);
    for (int i = 0; i < closure->size + Closure::HEADER_SIZE; i++) {
        const Object c = code[i];
        if (c.isInstruction()) {
            LOG1("\n~a ", Instruction::toString(c.val));
        } else {
            LOG1("~a ", c);
        }
    }
    return Object::Undef;
}

Object scheme::printStackEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("print-stack");
    checkArgumentLength(0);
    theVM->printStack();
    return Object::Undef;
}

/* psyntax/expander.ss
(define join-wraps
    (lambda (m1* s1* ae1* e)
      (define cancel
        (lambda (ls1 ls2)
          (let f ((x (car ls1)) (ls1 (cdr ls1)))
            (if (null? ls1)
                (cdr ls2)
                (cons x (f (car ls1) (cdr ls1)))))))
      (let ((m2* (stx-mark* e))
            (s2* (stx-subst* e))
            (ae2* (stx-ae* e)))
        (if (and (not (null? m1*))
                 (not (null? m2*))
                 (anti-mark? (car m2*)))
            ; cancel mark, anti-mark, and corresponding shifts
            (values (cancel m1* m2*) (cancel s1* s2*) (cancel ae1* ae2*))
            (values (append m1* m2*) (append s1* s2*) (append ae1* ae2*))))))
*/
static Object f(Object x, Object ls1, Object ls2)
{
    if (ls1.isNil()) {
        return ls2.cdr();
    } else {
        return Object::cons(x, f(ls1.car(), ls1.cdr(), ls2));
    }
}

static Object cancel(Object ls1, Object ls2)
{
    MOSH_ASSERT(ls1.isPair());
    return f(ls1.car(), ls1.cdr(), ls2);
}

Object scheme::joinWrapsEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("join-wraps");
    checkArgumentLength(4);
    Object m1Mul = argv[0];
    Object s1Mul = argv[1];
    Object ae1Mul = argv[2];
    Object e = argv[3];
    Object m2Mul = e.toSimpleStruct()->ref(1);
    Object s2Mul = e.toSimpleStruct()->ref(2);
    Object ae2Mul = e.toSimpleStruct()->ref(3);
    if (!m1Mul.isNil() && !m2Mul.isNil() && m2Mul.car().isFalse()) {
        return theVM->values3(cancel(m1Mul, m2Mul),
                              cancel(s1Mul, s2Mul),
                              cancel(ae1Mul, ae2Mul));
    } else {
        return theVM->values3(Pair::append2(m1Mul, m2Mul),
                       Pair::append2(s1Mul, s2Mul),
                       Pair::append2(ae1Mul, ae2Mul));

    }
}

/* psyntax/expander.ss
  ;;; Two lists of marks are considered the same if they have the
  ;;; same length and the corresponding marks on each are eq?.
  (define same-marks?
    (lambda (x y)
      (or (and (null? x) (null? y)) ;(eq? x y)
          (and (pair? x) (pair? y)
               (eq? (car x) (car y))
               (same-marks? (cdr x) (cdr y))))))
*/
bool isSameMarks(Object x, Object y)
{
    for (;;) {
        if (x.isNil() && y.isNil()) {
            return true;
        }
        if (x.isNil() && !y.isNil()) {
            return false;
        }
        if (!x.isNil() && y.isNil()) {
            return false;
        }
        if (x.isPair() && !y.isPair()) {
            return false;
        }
        if (!x.isPair() && y.isPair()) {
            return false;
        }
        if (x.car() != y.car()) {
            return false;
        }
        x = x.cdr();
        y = y.cdr();
    }
    return true;
}
Object scheme::sameMarksPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("same-marks*?");
    checkArgumentLength(2);
    return Object::makeBool(isSameMarks(argv[0], argv[1]));
}

/* psyntax/expander.ss
(define (same-marks*? mark* mark** si)
    (if (null? si)
        #f
        (if (same-marks? mark* (vector-ref mark** (car si)))
            (car si)
            (same-marks*? mark* mark** (cdr si)))))
*/
Object scheme::sameMarksMulPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("same-marks*?");
    checkArgumentLength(3);
    Object markMul = argv[0];
    Object markMulMul = argv[1];
    Object si = argv[2];
    for (;;) {
        if (si.isNil()) {
            return Object::False;
        }
        MOSH_ASSERT(markMulMul.isVector());
        MOSH_ASSERT(si.isPair());
        MOSH_ASSERT(si.car().isFixnum());
        if (isSameMarks(markMul, markMulMul.toVector()->ref(si.car().toFixnum()))) {
            return si.car();
        }
        si = si.cdr();
    }
    MOSH_ASSERT(false);
    return Object::Undef;
}

/* psyntax/expander.ss
(define id->real-label
    (lambda (id)
      (let ((sym (id->sym id)))
        (let search ((subst* (stx-subst* id)) (mark* (stx-mark* id)))
          (cond
            ((null? subst*) #f)
            ((eq? (car subst*) 'shift)
             ;;; a shift is inserted when a mark is added.
             ;;; so, we search the rest of the substitution
             ;;; without the mark.
             (search (cdr subst*) (cdr mark*)))
            (else
             (let ((rib (car subst*)))
               (cond
                 ((rib-sealed/freq rib) =>
                  (lambda (ht)
                    (let ((si (hashtable-ref ht sym #f)))
                      (let ((i (and si
                            (same-marks*? mark*
                              (rib-mark** rib) (reverse si)))))
                        (if i
                          (vector-ref (rib-label* rib) i)
                        (search (cdr subst*) mark*))))))
;                 ((find-label rib sym mark*))
                 (else
                  (let f ((sym* (rib-sym* rib))
                          (mark** (rib-mark** rib))
                          (label* (rib-label* rib)))
                    (cond
                      ((null? sym*) (search (cdr subst*) mark*))
                      ((and (eq? (car sym*) sym)
                            (same-marks? (car mark**) mark*))
                       (car label*))
                      (else (f (cdr sym*) (cdr mark**) (cdr label*))))))))))))))
*/

Object scheme::idTorealLabelEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("id->real-label");
    checkArgumentLength(1);
    Object id = argv[0];
    MOSH_ASSERT(id.isSimpleStruct());
    Object sym = id.toSimpleStruct()->ref(0);
    Object substMul = id.toSimpleStruct()->ref(2);
    Object markMul = id.toSimpleStruct()->ref(1);
    for (;;) {

        if (substMul.isNil()) {
            return Object::False;
        }
        MOSH_ASSERT(substMul.isPair());
        if (substMul.car() == Symbol::SHIFT) {
            substMul = substMul.cdr();
            MOSH_ASSERT(markMul.isPair());
            markMul = markMul.cdr();
            continue;
        } else {
            Object rib = substMul.car();
            MOSH_ASSERT(rib.isSimpleStruct());
            Object ribSealedFreq = rib.toSimpleStruct()->ref(3);
            if (!ribSealedFreq.isFalse()) {
                MOSH_ASSERT(ribSealedFreq.isEqHashTable());
                Object si = ribSealedFreq.toEqHashTable()->ref(sym, Object::False);
                Object i;
                if (si.isFalse()) {
                    i = Object::False;
                } else {
                    Object as[3];
                    as[0] = markMul;
                    as[1] = rib.toSimpleStruct()->ref(1);
                    as[2] = Pair::reverse(si);
                    i = sameMarksMulPEx(theVM, 3, as);
                }

                if (i.isFalse()) {
                    substMul = substMul.cdr();
                    continue;
                } else {
                    MOSH_ASSERT(i.isFixnum());
                    return rib.toSimpleStruct()->ref(2).toVector()->ref(i.toFixnum());
                }
            } else {
                Object symMul = rib.toSimpleStruct()->ref(0);
                Object markMulMul = rib.toSimpleStruct()->ref(1);
                Object labelMul = rib.toSimpleStruct()->ref(2);
                for (;;) {
                    if (symMul.isNil()) {
                        substMul = substMul.cdr();
                        break;
                    } else if (sym == symMul.car() && isSameMarks(markMulMul.car(), markMul)) {
                        return labelMul.car();
                    } else {
                        symMul = symMul.cdr();
                        markMulMul = markMulMul.cdr();
                        labelMul = labelMul.cdr();
                    }
                }
            }
        }
    }
}

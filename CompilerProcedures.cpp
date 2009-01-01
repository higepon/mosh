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
#include "VM-inl.h"

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
    CONST         = 0,
    LET           = 2,
    SEQ           = 3,
    LAMBDA        = 4,
    LOCAL_REF     = 5,
    LOCAL_ASSIGN  = 6,
    GLOBAL_REF    = 7,
    GLOBAL_ASSIGN = 8,
    UNDEF         = 9,
    IF            = 10,
    ASM           = 11,
    DEFINE        = 12,
    CALL_CC       = 13,
    CALL          = 14,
    LABEL         = 15,
    LIST          = 16,
    IT            = 17,
    RECEIVE       = 18
};

Object scheme::labelEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("$label");
    checkArgumentLength(1);
    const Object label = Object::makeVector(3);
    label.toVector()->set(0, Object::makeFixnum(LABEL));
    label.toVector()->set(1, argv[0]);
    label.toVector()->set(2, Object::False);
    return label;
}

Object scheme::localRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("$localRefEx");
    checkArgumentLength(1);
    const Object label = Object::makeVector(2);
    const Object lvar = argv[0];
    label.toVector()->set(0, Object::makeFixnum(LOCAL_REF));
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

static ObjectMap cache_;

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
    case CONST:
        return Object::Nil;
    case LET:
    {
        return findFreeLet(theVM, v, l, canFrees, labelsSeen);
    }
    case RECEIVE:
    {
        return findFreeReceive(theVM, v, l, canFrees, labelsSeen);
    }
    case SEQ:
    {
        return findFreeSeq(theVM, v, l, canFrees, labelsSeen);
    }
    case LAMBDA:
    {
        return findFreeLambda(theVM, v, l, canFrees, labelsSeen);
    }
    case LOCAL_ASSIGN:
    {
        return findFreeLocalAssign(theVM, v, l, canFrees, labelsSeen);
    }
    case LOCAL_REF:
    {
        return findFreeLocalRef(v, l, canFrees, labelsSeen);
    }
    case GLOBAL_REF:
    {
        return findFreeGlobalRef(v, l, canFrees, labelsSeen);
    }
    case UNDEF:
        return Object::Nil;
    case IF:
    {
        return findFreeIf(theVM, v, l, canFrees, labelsSeen);
    }
    case ASM:
    {
        const Object asmArgs = v->ref(2);
        return findFreeRecMap(theVM, l, canFrees, labelsSeen, asmArgs);
    }
    case DEFINE:
    {
        const Object defineVal = v->ref(2);
        return findFreeRec(theVM, defineVal, l, canFrees, labelsSeen);
    }
    case CALL:
    {
        return findFreeCall(theVM, v, l, canFrees, labelsSeen);
    }
    case CALL_CC:
    {
        const Object callccProc = v->ref(1);
        return findFreeRec(theVM, callccProc, l, canFrees, labelsSeen);
    }
    case GLOBAL_ASSIGN:
    {
        const Object globalAssignVal = v->ref(2);
        return findFreeRec(theVM, globalAssignVal, l, canFrees, labelsSeen);
    }
    case LIST:
    {
        const Object listArgs = v->ref(1);
        return findFreeRecMap(theVM, l, canFrees, labelsSeen, listArgs);
    }
    case LABEL:
    {
        return findFreeLabel(theVM, i, v, l, canFrees, labelsSeen);
    }
    case IT:
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
    case CONST:
        return Object::Nil;
    case LET:
    {
        const Object letInits = v->ref(3);
        const Object letBody = v->ref(4);
        return Pair::append2(findSetsRecMap(theVM, lvars, letInits, labelsSeen),
                             findSetsRec(theVM, letBody, lvars, labelsSeen));
    }
    case RECEIVE:
    {
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        return Pair::append2(findSetsRec(theVM, receiveVals, lvars, labelsSeen),
                             findSetsRec(theVM, receiveBody, lvars, labelsSeen));
    }
    case SEQ:
    {
        const Object seqBody = v->ref(1);
        return findSetsRecMap(theVM, lvars, seqBody, labelsSeen);
    }
    case LAMBDA:
    {
        const Object lambdaBody = v->ref(6);
        return findSetsRec(theVM, lambdaBody, lvars, labelsSeen);
    }
    case LOCAL_ASSIGN:
    {
        const Object localAssignLvar = v->ref(1);
        const Object localAssignVal = v->ref(2);
        if (memq(localAssignLvar, lvars).isFalse()) {
            return findSetsRec(theVM, localAssignVal, lvars, labelsSeen);
        } else {
            return Object::cons(localAssignLvar, findSetsRec(theVM, localAssignVal, lvars, labelsSeen));
        }
    }
    case LOCAL_REF:
    {
        return Object::Nil;
    }
    case GLOBAL_REF:
    {
        return Object::Nil;
    }
    case UNDEF:
        return Object::Nil;
    case IF:
    {
        const Object testF = findSetsRec(theVM, v->ref(1), lvars, labelsSeen);
        const Object thenF = findSetsRec(theVM, v->ref(2), lvars, labelsSeen);
        const Object elseF = findSetsRec(theVM, v->ref(3), lvars, labelsSeen);
        return Pair::append2(testF, Pair::append2(thenF, elseF));
    }
    case ASM:
    {
        const Object asmArgs = v->ref(2);
        return findSetsRecMap(theVM, lvars, asmArgs, labelsSeen);
    }
    case DEFINE:
    {
        const Object defineVal = v->ref(3);
        return findSetsRec(theVM, defineVal, lvars, labelsSeen);
    }
    case CALL:
    {
        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findSetsRecMap(theVM, lvars, callArgs, labelsSeen),
                             findSetsRec(theVM, callProc, lvars, labelsSeen));
    }
    case CALL_CC:
    {
        const Object callccProc = v->ref(1);
        return findSetsRec(theVM, callccProc, lvars, labelsSeen);
    }
    case GLOBAL_ASSIGN:
    {
        const Object globalAssignVal = v->ref(2);
        return findSetsRec(theVM, globalAssignVal, lvars, labelsSeen);
    }
    case LIST:
    {
        const Object listArgs = v->ref(1);
        return findSetsRecMap(theVM, lvars, listArgs, labelsSeen);
    }
    case LABEL:
    {
        return findSetsLabel(theVM, i, lvars, labelsSeen);
    }
    case IT:
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
                   insn.toVector()->ref(0).toFixnum() == LABEL) {
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
            LOG1("~a ", Instruction::toString(c.val));
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

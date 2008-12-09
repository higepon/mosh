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

using namespace scheme;

static Object pass4FixupLabelCollect(Object vec);
static Object pass4FixupLabel(Object vec);
static Object findFree(Object iform, Object locals, Object canFrees);
static Object findFreeRec(Object i, Object l, Object canFrees, Object labelsSeen);
static Object findFreeRecMap(Object l, Object canFrees, Object labelsSeen, Object list);
static Object findSetsRecMap(Object lvars, Object list);
static Object findSets(Object iform, Object lvars);
static Object findSetsRec(Object i, Object lvars);

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
    LIBRARY       = 17,
    IMPORT        = 18,
    IT            = 20,
    RECEIVE       = 21
};

Object scheme::labelEx(int argc, const Object* argv)
{
    DeclareProcedureName("$label");
    checkArgumentLength(1);
    const Object label = Object::makeVector(3);
    label.toVector()->set(0, Object::makeFixnum(LABEL));
    label.toVector()->set(1, argv[0]);
    label.toVector()->set(2, Object::False);
    return label;
}

Object scheme::localRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("$localRefEx");
    checkArgumentLength(1);
    const Object label = Object::makeVector(2);
    label.toVector()->set(0, Object::makeFixnum(LOCAL_REF));
    label.toVector()->set(1, argv[0]);
    return label;
}

Object scheme::pass1FindSymbolInLvarsEx(int argc, const Object* argv)
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

Object scheme::pass3CompileReferEx(int argc, const Object* argv)
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
    callAssertionViolationAfter(procedureName, "bug? Unknown lvar", L1(variable));
    return Object::Undef;
}


Object scheme::pass3FindFreeEx(int argc, const Object* argv)
{
    return findFree(argv[0], argv[1], argv[2]);
}

Object scheme::pass3FindSetsEx(int argc, const Object* argv)
{
    return findSets(argv[0], argv[1]);
}

static ObjectMap cache_;

Object findFree(Object iform, Object locals, Object canFrees)
{
    const Object ret = findFreeRec(iform, locals, canFrees, Object::Nil);
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

Object findFreeLet(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object letLvars = v->ref(2);
        const Object letInits = v->ref(3);
        const Object letBody = v->ref(4);
        return Pair::append2(findFreeRecMap(l, canFrees, labelsSeen, letInits),
                             findFreeRec(letBody, letLvars, canFrees, labelsSeen));
}

Object findFreeReceive(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        const Object receiveLVars = v->ref(1);
        return Pair::append2(findFreeRec(receiveVals, l, canFrees, labelsSeen),
                             findFreeRec(receiveBody, receiveLVars, canFrees, labelsSeen));

}

Object findFreeSeq(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object seqBody = v->ref(1);
        return findFreeRecMap(l, canFrees, labelsSeen, seqBody);
}

Object findFreeLambda(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object lambdaBody = v->ref(6);
        const Object lambdaLvars = v->ref(5);
        return findFreeRec(lambdaBody, lambdaLvars, canFrees, labelsSeen);
}

Object findFreeLocalAssign(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
    const Object sym = v->ref(1).toVector()->ref(1);
        const Object val = v->ref(2);
        if (existsInCanFrees(sym, canFrees)) {
            return Object::cons(sym, findFreeRec(val, l, canFrees, labelsSeen));
        } else {
            return findFreeRec(val, l, canFrees, labelsSeen);
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
        const Object sym = v->ref(2);
        if (existsInCanFrees(sym, canFrees)) {
            return Pair::list1(sym);
        } else {
            return Object::Nil;
        }
}

Object findFreeIf(Vector* v, Object l, Object canFrees, Object labelsSeen)
{
    const Object testF = findFreeRec(v->ref(1), l, canFrees, labelsSeen);
    const Object thenF = findFreeRec(v->ref(2), l, canFrees, labelsSeen);
    const Object elseF = findFreeRec(v->ref(3), l, canFrees, labelsSeen);
    return Pair::append2(testF, Pair::append2(thenF, elseF));
}

Object findFreeLabel(Object i, Vector* v, Object l, Object canFrees, Object labelsSeen)
{
        const Object labelBody = v->ref(1);
        if (!memq(i, labelsSeen).isFalse()) {
            return Object::Nil;
        } else {
            return findFreeRec(labelBody, l, canFrees, Object::cons(i, labelsSeen));
        }
}

Object findFreeCall(Vector* v, Object l, Object canFrees, Object labelsSeen)
{

        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findFreeRecMap(l, canFrees, labelsSeen, callArgs),
                             findFreeRec(callProc, l, canFrees, labelsSeen));
}


Object findFreeRec(Object i, Object l, Object canFrees, Object labelsSeen)
{
    Vector* v = i.toVector();
    MOSH_ASSERT(v->ref(0).isFixnum());
    switch(v->ref(0).toFixnum()) {
    case CONST:
        return Object::Nil;
    case LET:
    {
        return findFreeLet(v, l, canFrees, labelsSeen);
    }
    case RECEIVE:
    {
        return findFreeReceive(v, l, canFrees, labelsSeen);
    }
    case SEQ:
    {
        return findFreeSeq(v, l, canFrees, labelsSeen);
    }
    case LAMBDA:
    {
        return findFreeLambda(v, l, canFrees, labelsSeen);
    }
    case LOCAL_ASSIGN:
    {
        return findFreeLocalAssign(v, l, canFrees, labelsSeen);
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
        return findFreeIf(v, l, canFrees, labelsSeen);
    }
    case ASM:
    {
        const Object asmArgs = v->ref(2);
        return findFreeRecMap(l, canFrees, labelsSeen, asmArgs);
    }
    case DEFINE:
    {
        const Object defineVal = v->ref(3);
        return findFreeRec(defineVal, l, canFrees, labelsSeen);
    }
    case CALL:
    {
        return findFreeCall(v, l, canFrees, labelsSeen);
    }
    case CALL_CC:
    {
        const Object callccProc = v->ref(1);
        return findFreeRec(callccProc, l, canFrees, labelsSeen);
    }
    case GLOBAL_ASSIGN:
    {
        const Object globalAssignVal = v->ref(3);
        return findFreeRec(globalAssignVal, l, canFrees, labelsSeen);
    }
    case LIST:
    {
        const Object listArgs = v->ref(1);
        return findFreeRecMap(l, canFrees, labelsSeen, listArgs);
    }
    case LABEL:
    {
        return findFreeLabel(i, v, l, canFrees, labelsSeen);
    }
    case IMPORT:
        return Object::Nil;
    case LIBRARY:
        return Object::Nil;
    case IT:
        return Object::Nil;
    default:
        callAssertionViolationAfter("find-free", "unknown iform", L1(v->ref(0)));
        return Object::Undef;
    }
    return Object::Undef;
}


Object findFreeRecMap(Object l, Object canFrees, Object labelsSeen, Object list)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        ret = Pair::append2(ret, findFreeRec(p.car(), l, canFrees, labelsSeen));
    }
    return ret;
}

Object findSetsRecMap(Object lvars, Object list)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        ret = Pair::append2(ret, findSetsRec(p.car(), lvars));
    }
    return ret;
}

Object findSets(Object iform, Object lvars)
{
    return uniq(findSetsRec(iform, lvars));
}

Object findSetsRec(Object i, Object lvars)
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
        return Pair::append2(findSetsRecMap(lvars, letInits),
                             findSetsRec(letBody, lvars));
    }
    case RECEIVE:
    {
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        return Pair::append2(findSetsRec(receiveVals, lvars),
                             findSetsRec(receiveBody, lvars));
    }
    case SEQ:
    {
        const Object seqBody = v->ref(1);
        return findSetsRecMap(lvars, seqBody);
    }
    case LAMBDA:
    {
        const Object lambdaBody = v->ref(6);
        return findSetsRec(lambdaBody, lvars);
    }
    case LOCAL_ASSIGN:
    {
        const Object localAssignLvar = v->ref(1);
        const Object localAssignVal = v->ref(2);
        if (memq(localAssignLvar, lvars).isFalse()) {
            return findSetsRec(localAssignVal, lvars);
        } else {
            return Object::cons(localAssignLvar, findSetsRec(localAssignVal, lvars));
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
        const Object testF = findSetsRec(v->ref(1), lvars);
        const Object thenF = findSetsRec(v->ref(2), lvars);
        const Object elseF = findSetsRec(v->ref(3), lvars);
        return Pair::append2(testF, Pair::append2(thenF, elseF));
    }
    case ASM:
    {
        const Object asmArgs = v->ref(2);
        return findSetsRecMap(lvars, asmArgs);
    }
    case DEFINE:
    {
        const Object defineVal = v->ref(3);
        return findSetsRec(defineVal, lvars);
    }
    case CALL:
    {
        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findSetsRecMap(lvars, callArgs),
                             findSetsRec(callProc, lvars));
    }
    case CALL_CC:
    {
        const Object callccProc = v->ref(1);
        return findSetsRec(callccProc, lvars);
    }
    case GLOBAL_ASSIGN:
    {
        const Object globalAssignVal = v->ref(3);
        return findSetsRec(globalAssignVal, lvars);
    }
    case LIST:
    {
        const Object listArgs = v->ref(1);
        return findSetsRecMap(lvars, listArgs);
    }
    case LABEL:
    {
        return Object::Nil;
    }
    case IMPORT:
        return Object::Nil;
    case LIBRARY:
        return Object::Nil;
    case IT:
        return Object::Nil;
    default:
        callAssertionViolationAfter("find-free", "unknown iform", L1(v->ref(0)));
        return Object::Undef;
    }
    return Object::Undef;
}


Object scheme::pass4FixupLabelsEx(int argc, const Object* argv)
{
    return pass4FixupLabel(argv[0]);
}

Object scheme::makeCodeBuilderEx(int argc, const Object* argv)
{
    return Object::makeCodeBuilder();
}

Object scheme::codeBuilderPutExtra1DEx(int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put1!");
    checkArgumentLength(2);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    return Object::Undef;
}


Object scheme::codeBuilderPutExtra2DEx(int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put2!");
    checkArgumentLength(3);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    codeBuilder->putExtra(argv[2]);
    return Object::Undef;
}

Object scheme::codeBuilderPutExtra3DEx(int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put3!");
    checkArgumentLength(4);

    argumentAsCodeBuilder(0, codeBuilder);
    codeBuilder->putExtra(argv[1]);
    codeBuilder->putExtra(argv[2]);
    codeBuilder->putExtra(argv[3]);
    return Object::Undef;
}

Object scheme::codeBuilderPutExtra4DEx(int argc, const Object* argv)
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


Object scheme::codeBuilderPutExtra5DEx(int argc, const Object* argv)
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


Object scheme::codeBuilderAppendDEx(int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-append!");
    checkArgumentLength(2);

    argumentAsCodeBuilder(0, destCodeBuilder);
    argumentAsCodeBuilder(1, sourceCodeBuilder);
    destCodeBuilder->append(sourceCodeBuilder);
    return Object::Undef;
}

Object scheme::codeBuilderEmitEx(int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-emit2");
    checkArgumentLength(1);

    argumentAsCodeBuilder(0, codeBuilder);
    return codeBuilder->emit();
}

Object scheme::codeBuilderPutInsnArg1DEx(int argc, const Object* argv)
{
    DeclareProcedureName("code-builder-put-insn-arg1!");
    checkArgumentLength(3);

    argumentAsCodeBuilder(0, codeBuilder);
    const Object instruction = argv[1];
    const Object argument = argv[2];

    codeBuilder->putInstructionArgument1(instruction, argument);
    return Object::Undef;
}

Object scheme::codeBuilderPutInsnArg0DEx(int argc, const Object* argv)
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
    static const Object NOP                   = Object::makeRaw(Instruction::NOP);
    static const Object UNFIXED_JUMP          = Object::makeRaw(Instruction::UNFIXED_JUMP);
    static const Object TEST                  = Object::makeRaw(Instruction::TEST);
    static const Object NUMBER_LE_TEST        = Object::makeRaw(Instruction::NUMBER_LE_TEST);
    static const Object NOT_TEST              = Object::makeRaw(Instruction::NOT_TEST);
    static const Object REFER_LOCAL0_EQV_TEST = Object::makeRaw(Instruction::REFER_LOCAL0_EQV_TEST);
    static const Object FRAME                 = Object::makeRaw(Instruction::FRAME);
    static const Object PUSH_FRAME            = Object::makeRaw(Instruction::PUSH_FRAME);
    static const Object CLOSURE               = Object::makeRaw(Instruction::CLOSURE);

    const Vector* const v = vec.toVector();
    const int length = v->length();
    const Object ret = Object::makeVector(length, NOP);
    Vector* const rv= ret.toVector();
    Object labels = Object::makeEqHashTable();
    EqHashTable* const table = labels.toEqHashTable();
    for (int i = 0, j = 0; i < length;) {
        const Object insn = v->ref(i);
        if (insn == UNFIXED_JUMP          ||
            insn == TEST                  ||
            insn == NUMBER_LE_TEST        ||
            insn == NOT_TEST              ||
            insn == REFER_LOCAL0_EQV_TEST ||
            insn == FRAME                 ||
            insn == PUSH_FRAME            ||
            insn == CLOSURE) {
            rv->set(j, insn);
            rv->set(j + 1, v->ref(i + 1));
            i += 2;
            j += 2;
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
    static const Object UNFIXED_JUMP          = Object::makeRaw(Instruction::UNFIXED_JUMP);
    static const Object TEST                  = Object::makeRaw(Instruction::TEST);
    static const Object NUMBER_LE_TEST        = Object::makeRaw(Instruction::NUMBER_LE_TEST);
    static const Object NOT_TEST              = Object::makeRaw(Instruction::NOT_TEST);
    static const Object REFER_LOCAL0_EQV_TEST = Object::makeRaw(Instruction::REFER_LOCAL0_EQV_TEST);
    static const Object FRAME                 = Object::makeRaw(Instruction::FRAME);
    static const Object PUSH_FRAME            = Object::makeRaw(Instruction::PUSH_FRAME);
    static const Object CLOSURE               = Object::makeRaw(Instruction::CLOSURE);
    static const Object LOCAL_JMP             = Object::makeRaw(Instruction::LOCAL_JMP);
    static const Object RETURN                = Object::makeRaw(Instruction::RETURN);

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
        } else if (insn == TEST                  ||
                   insn == NUMBER_LE_TEST        ||
                   insn == NOT_TEST              ||
                   insn == REFER_LOCAL0_EQV_TEST ||
                   insn == FRAME                 ||
                   insn == PUSH_FRAME            ||
                   insn == CLOSURE) {
            const Object label = table->ref(code->ref(i + 1), Object::False);
            if (!labels.isFalse()) {
                code->set(i, insn);
                code->set(i + 1, Object::makeFixnum(label.toFixnum() - i - 1));
                i += 2;
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

        } else if (insn == TEST) {
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

Object scheme::disasmEx(int argc, const Object* argv)
{
    DeclareProcedureName("disasm");
    checkArgumentLength(1);
    argumentAsClosure(0, closure);
    Object* code = theVM->disasm(closure);
    for (int i = 0; i < closure->size + Closure::HEADER_SIZE; i++) {
        const Object c = code[i];
        if (c.isInstruction()) {
            VM_LOG1("~a ", Instruction::toString(c.val));
        } else {
            VM_LOG1("~a ", c);
        }
    }
    return Object::Undef;
}

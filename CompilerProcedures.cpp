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

#include "CompilerProcedures.h"

using namespace scheme;

extern scheme::VM* theVM;

static Object pass4FixupLabelCollect(Object vec);
static Object pass4FixupLabel(Object vec);
static Object findFree(Object iform, Object locals, Object canFrees);
static Object findFreeRec(Object i, Object l, Object canFrees, Object labelsSeen);
static Object findFreeRecMap(Object l, Object canFrees, Object labelsSeen, Object list);
static Object findSetsRecMap(Object lvars, Object list);
static Object findSets(Object iform, Object lvars);
static Object findSetsRec(Object i, Object lvars);

Object scheme::pass1FindSymbolInLvarsEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "pass1/find-symbol-in-lvars");
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
    checkArgLength(4, argc, "pass3/compile-refer");
    const Object codeBuilder        = argv[0];
    const Object variable           = argv[1];
    const Object localVariablesList = argv[2];
    int localsIndex = 0;
    for (Object p = localVariablesList; p.isPair(); p = p.cdr(), localsIndex++) {
        const Object localVariable = p.car();
        if (localVariable == variable) {
            codeBuilder.toCodeBuilder()->put(Object::makeRaw(Instruction::REFER_LOCAL), Object::makeInt(localsIndex));
            return Object::makeInt(0);
        }
    }

    const Object freeVariablesList = argv[3];
    int freesIndex = 0;
    for (Object p = freeVariablesList; p.isPair(); p = p.cdr(), freesIndex++) {
        const Object freeVariable = p.car();
        if (freeVariable == variable) {
            codeBuilder.toCodeBuilder()->put(Object::makeRaw(Instruction::REFER_FREE), Object::makeInt(freesIndex));
            return Object::makeInt(0);
        }
    }
    VM_RAISE1("pass3/symbol-lookup bug? Unknown lvar:", variable);
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

Object findFree(Object iform, Object locals, Object canFrees)
{
    const Object ret = findFreeRec(iform, locals, canFrees, Object::Nil);
    return uniq(ret);
}

bool existsInCanFrees(Object lvar, Object canFrees)
{
    for (Object frees = canFrees; frees.isPair(); frees = frees.cdr()) {
        if (!memq(lvar, frees.car()).isFalse()) {
            return true;
        }
    }
    return false;
}

Object findSymbolInCanFrees(Object sym, Object canFrees)
{
    for (Object frees = canFrees; frees.isPair(); frees = frees.cdr()) {
        for (Object p = frees.car(); p.isPair(); p = p.cdr()) {
            const Object lvarSym = p.car().toVector()->ref(1);
            if (lvarSym == sym) {
                return p.car();
            }
        }
    }
    return Object::False;
}

Object findFreeRec(Object i, Object l, Object canFrees, Object labelsSeen)
{
    const int CONST = 0;
    const int LET = 2;
    const int SEQ = 3;
    const int LAMBDA = 4;
    const int LOCAL_REF = 5;
    const int LOCAL_ASSIGN = 6;
    const int GLOBAL_REF = 7;
    const int GLOBAL_ASSIGN = 8;
    const int UNDEF = 9;
    const int IF = 10;
    const int ASM = 11;
    const int DEFINE = 12;
    const int CALL_CC = 13;
    const int CALL = 14;
    const int LABEL = 15;
    const int LIST = 16;
    const int LIBRARY = 17;
    const int IMPORT = 18;
    const int IT = 20;
    const int RECEIVE = 21;
    Vector* v = i.toVector();
    switch(v->ref(0).toInt()) {
    case CONST:
        return Object::Nil;
    case LET:
    {
        const Object letLvars = v->ref(2);
        const Object letInits = v->ref(3);
        const Object letBody = v->ref(4);
        return Pair::append2(findFreeRecMap(l, canFrees, labelsSeen, letInits),
                             findFreeRec(letBody, letLvars, canFrees, labelsSeen));
    }
    case RECEIVE:
    {
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        const Object receiveLVars = v->ref(1);
        return Pair::append2(findFreeRec(receiveVals, l, canFrees, labelsSeen),
                             findFreeRec(receiveBody, receiveLVars, canFrees, labelsSeen));

    }
    case SEQ:
    {
        const Object seqBody = v->ref(1);
        return findFreeRecMap(l, canFrees, labelsSeen, seqBody);
    }
    case LAMBDA:
    {
        const Object lambdaBody = v->ref(6);
        const Object lambdaLvars = v->ref(5);
        return findFreeRec(lambdaBody, lambdaLvars, canFrees, labelsSeen);
    }
    case LOCAL_ASSIGN:
    {
        const Object lvar = v->ref(1);
        const Object val = v->ref(2);
        if (existsInCanFrees(lvar, canFrees)) {
            return Object::cons(lvar, findFreeRec(val, l, canFrees, labelsSeen));
        } else {
            return findFreeRec(val, l, canFrees, labelsSeen);
        }
    }
    case LOCAL_REF:
    {
        const Object lvar = v->ref(1);
        if (!memq(lvar, l).isFalse()) {
            return Object::Nil;
        } else if (existsInCanFrees(lvar, canFrees)) {
            return Pair::list1(lvar);
        } else {
            return Object::Nil;
        }
    }
    case GLOBAL_REF:
    {
        const Object sym = v->ref(2);
        Object foundSym = findSymbolInCanFrees(sym, canFrees);
        return !foundSym.isFalse() ? Pair::list1(foundSym) : Object::Nil;
    }
    case UNDEF:
        return Object::Nil;
    case IF:
    {
        const Object testF = findFreeRec(v->ref(1), l, canFrees, labelsSeen);
        const Object thenF = findFreeRec(v->ref(2), l, canFrees, labelsSeen);
        const Object elseF = findFreeRec(v->ref(3), l, canFrees, labelsSeen);
        return Pair::append2(testF, Pair::append2(thenF, elseF));
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
        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findFreeRecMap(l, canFrees, labelsSeen, callArgs),
                             findFreeRec(callProc, l, canFrees, labelsSeen));
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
        const Object labelBody = v->ref(2);
        if (!memq(i, labelsSeen).isFalse()) {
            return Object::Nil;
        } else {
            return findFreeRec(labelBody, l, canFrees, Object::cons(i, labelsSeen));
        }
    }
    case IMPORT:
        return Object::Nil;
    case LIBRARY:
        return Object::Nil;
    case IT:
        return Object::Nil;
    default:
        VM_RAISE1("pass3/find-free unknown iform: ~a", v->ref(0));
        break;
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
    const int CONST = 0;
    const int LET = 2;
    const int SEQ = 3;
    const int LAMBDA = 4;
    const int LOCAL_REF = 5;
    const int LOCAL_ASSIGN = 6;
    const int GLOBAL_REF = 7;
    const int GLOBAL_ASSIGN = 8;
    const int UNDEF = 9;
    const int IF = 10;
    const int ASM = 11;
    const int DEFINE = 12;
    const int CALL_CC = 13;
    const int CALL = 14;
    const int LABEL = 15;
    const int LIST = 16;
    const int LIBRARY = 17;
    const int IMPORT = 18;
    const int IT = 20;
    const int RECEIVE = 21;

    Vector* v = i.toVector();
    switch(v->ref(0).toInt()) {
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
        VM_RAISE1("pass3/find-sets unknown iform: ~a", v->ref(0));
        break;
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

Object scheme::codeBuilderPut1DEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "code-builder-put1!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1]);
    return Object::Undef;
}


Object scheme::codeBuilderPut2DEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "code-builder-put2!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1],
                            argv[2]);
    return Object::Undef;
}

Object scheme::codeBuilderPut3DEx(int argc, const Object* argv)
{
    checkArgLength(4, argc, "code-builder-put3!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }

    cb.toCodeBuilder()->put(argv[1], argv[2], argv[3]);
    return Object::Undef;
}

Object scheme::codeBuilderPut4DEx(int argc, const Object* argv)
{
    checkArgLength(5, argc, "code-builder-put4!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1],
                            argv[2],
                            argv[3],
                            argv[4]);
    return Object::Undef;
}


Object scheme::codeBuilderPut5DEx(int argc, const Object* argv)
{
    checkArgLength(6, argc, "code-builder-put5!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1],
                            argv[2],
                            argv[3],
                            argv[4],
                            argv[5]);
    return Object::Undef;
}



Object scheme::codeBuilderAppendDEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "code-builder-append!");
    const Object cbDst = argv[0];
    const Object cbSrc = argv[1];
    if (!cbDst.isCodeBuilder() || !cbSrc.isCodeBuilder()) {
        VM_RAISE2("code-builder required, but got ~a, ~a\n", cbDst, cbSrc);
    }
    cbDst.toCodeBuilder()->append(cbSrc.toCodeBuilder());
    return Object::Undef;
}

Object scheme::codeBuilderEmitEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "code-builder-emit2");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~an", cb);
    }
   return cb.toCodeBuilder()->emit();
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
    const int LABEL = 15;

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
        } else if (insn.isVector() && insn.toVector()->length() > 0 &&
                   insn.toVector()->ref(0).toInt() == LABEL) {
            i++;
            table->set(insn, Object::makeInt(j));
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
                code->set(i + 1, Object::makeInt(label.toInt() - i - 1));
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
                code->set(i + 1, Object::makeInt(label.toInt() - i - 1));
                i += 2;
            } else {
                i++;
            }
        } else {
            i++;
        }
    }
    return collected.car();
}

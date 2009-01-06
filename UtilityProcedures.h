/*
 * UtilityProcedures.h:
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

#ifndef __SCHEME_FREE_PROC_H__
#define __SCHEME_FREE_PROC_H__

namespace scheme {

    Object microsecondsEx(VM* theVM, int argc, const Object* argv);
    Object localTzOffsetEx(VM* theVM, int argc, const Object* argv);
    Object unGenSym(Object symbol);
    Object unGenSyms(Object symbols);
    Object dummy(Object obj);
    Object vectorFillDEx(VM* theVM, int argc, const Object* argv);
    Object symbolEqPEx(VM* theVM, int argc, const Object* argv);
    Object booleanEqPEx(VM* theVM, int argc, const Object* argv);
    Object makeCompilerInstructionEx(VM* theVM, int argc, const Object* argv);
    Object makeInstructionEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorPEx(VM* theVM, int argc, const Object* argv);
    Object regexpPEx(VM* theVM, int argc, const Object* argv);
    Object numberTostringEx(VM* theVM, int argc, const Object* argv);
    Object charEqPEx(VM* theVM, int argc, const Object* argv);
    Object stringPEx(VM* theVM, int argc, const Object* argv);
    Object getEnvironmentVariableEx(VM* theVM, int argc, const Object* argv);
    Object getEnvironmentVariablesEx(VM* theVM, int argc, const Object* argv);
    Object equalPEx(VM* theVM, int argc, const Object* argv);
    Object fastEqualPEx(VM* theVM, int argc, const Object* argv);
    Object digitTointegerEx(VM* theVM, int argc, const Object* argv);
    Object getRemainingInputStringEx(VM* theVM, int argc, const Object* argv);
    Object charTointegerEx(VM* theVM, int argc, const Object* argv);
    Object integerTocharEx(VM* theVM, int argc, const Object* argv);
    Object charPEx(VM* theVM, int argc, const Object* argv);
    Object gensymEx(VM* theVM, int argc, const Object* argv);
    Object ungensymEx(VM* theVM, int argc, const Object* argv);
    Object vectorPEx(VM* theVM, int argc, const Object* argv);
    Object eqPEx(VM* theVM, int argc, const Object* argv);
    Object eqvPEx(VM* theVM, int argc, const Object* argv);
    Object booleanPEx(VM* theVM, int argc, const Object* argv);
    Object symbolTostringEx(VM* theVM, int argc, const Object* argv);

    Object getTimeofdayEx(VM* theVM, int argc, const Object* argv);
    Object valuesEx(VM* theVM, int argc, const Object* argv);
    Object vmApplyEx(VM* theVM, int argc, const Object* argv);
    Object pairPEx(VM* theVM, int argc, const Object* argv);
    Object initLibraryTableEx(VM* theVM, int argc, const Object* argv);
    Object map10Ex(VM* theVM, int argc, const Object* argv);
    Object standardInputPortEx(VM* theVM, int argc, const Object* argv);
    Object vectorEx(VM* theVM, int argc, const Object* argv);
    //    Object errorfEx(VM* theVM, int argc, const Object* argv);
    Object evalEx(VM* theVM, int argc, const Object* argv);
    Object applyEx(VM* theVM, int argc, const Object* argv);
    Object modEx(VM* theVM, int argc, const Object* argv);
    Object divEx(VM* theVM, int argc, const Object* argv);
    Object exitEx(VM* theVM, int argc, const Object* argv);
    Object macroexpand1Ex(VM* theVM, int argc, const Object* argv);
    Object procedurePEx(VM* theVM, int argc, const Object* argv);
    Object loadEx(VM* theVM, int argc, const Object* argv);
    Object symbolPEx(VM* theVM, int argc, const Object* argv);
    Object charGePEx(VM* theVM, int argc, const Object* argv);
    Object charGtPEx(VM* theVM, int argc, const Object* argv);
    Object charLePEx(VM* theVM, int argc, const Object* argv);
    Object charLtPEx(VM* theVM, int argc, const Object* argv);
    Object readEx(VM* theVM, int argc, const Object* argv);
    Object vectorTolistEx(VM* theVM, int argc, const Object* argv);
    Object callProcessEx(VM* theVM, int argc, const Object* argv);
    Object internalGetClosureNameEx(VM* theVM, int argc, const Object* argv);
    Object internalsetUnionEx(VM* theVM, int argc, const Object* argv);
    Object internalsetIntersectEx(VM* theVM, int argc, const Object* argv);




    int mod(int x, int y);
    int div(int x, int y);

    Object stringTosymbol(Object str);


    Object pass3CompileReferEx(VM* theVM, int argc, const Object* argv);

    Object setSymbolValueDEx(VM* theVM, int argc, const Object* argv);
    Object symbolValueEx(VM* theVM, int argc, const Object* argv);
//     Object testEx(VM* theVM, int argc, const Object* argv);
//     Object test2Ex(VM* theVM, int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_FREE_PROC_H__

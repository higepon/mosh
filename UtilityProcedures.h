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

    Object unGenSym(Object symbol);
    Object dummy(Object obj);

    Object makeCompilerInstructionEx(int argc, const Object* argv);
    Object makeInstructionEx(int argc, const Object* argv);
    Object numberPEx(int argc, const Object* argv);
    Object bytevectorPEx(int argc, const Object* argv);
    Object regexpPEx(int argc, const Object* argv);
    Object numberTostringEx(int argc, const Object* argv);
    Object charEqPEx(int argc, const Object* argv);
    Object stringPEx(int argc, const Object* argv);
    Object getEnvironmentVariableEx(int argc, const Object* argv);
    Object getEnvironmentVariablesEx(int argc, const Object* argv);
    Object equalPEx(int argc, const Object* argv);
    Object digitTointegerEx(int argc, const Object* argv);
    Object getRemainingInputStringEx(int argc, const Object* argv);
    Object charTointegerEx(int argc, const Object* argv);
    Object integerTocharEx(int argc, const Object* argv);
    Object charPEx(int argc, const Object* argv);
    Object gensymEx(int argc, const Object* argv);
    Object vectorPEx(int argc, const Object* argv);
    Object eqPEx(int argc, const Object* argv);
    Object eqvPEx(int argc, const Object* argv);
    Object booleanPEx(int argc, const Object* argv);
    Object symbolTostringEx(int argc, const Object* argv);

    Object getTimeofdayEx(int argc, const Object* argv);
    Object valuesEx(int argc, const Object* argv);
    Object vmApplyEx(int argc, const Object* argv);
    Object pairPEx(int argc, const Object* argv);
    Object initLibraryTableEx(int argc, const Object* argv);
    Object map10Ex(int argc, const Object* argv);
    Object standardInputPortEx(int argc, const Object* argv);
    Object vectorEx(int argc, const Object* argv);
    //    Object errorfEx(int argc, const Object* argv);
    Object evalEx(int argc, const Object* argv);
    Object applyEx(int argc, const Object* argv);
    Object modEx(int argc, const Object* argv);
    Object divEx(int argc, const Object* argv);
    Object exitEx(int argc, const Object* argv);
    Object macroexpand1Ex(int argc, const Object* argv);
    Object procedurePEx(int argc, const Object* argv);
    Object loadEx(int argc, const Object* argv);
    Object symbolPEx(int argc, const Object* argv);
    Object charGePEx(int argc, const Object* argv);
    Object charGtPEx(int argc, const Object* argv);
    Object charLePEx(int argc, const Object* argv);
    Object charLtPEx(int argc, const Object* argv);
    Object readEx(int argc, const Object* argv);
    Object vectorTolistEx(int argc, const Object* argv);
    Object callProcessEx(int argc, const Object* argv);
    Object internalgetClosureNameEx(int argc, const Object* argv);
    Object internalsetUnionEx(int argc, const Object* argv);
    Object internalsetIntersectEx(int argc, const Object* argv);




    int mod(int x, int y);
    int div(int x, int y);

    Object stringTosymbol(Object str);


    Object pass3CompileReferEx(int argc, const Object* argv);

    Object setSymbolValueDEx(int argc, const Object* argv);
    Object symbolValueEx(int argc, const Object* argv);
//     Object testEx(int argc, const Object* argv);
//     Object test2Ex(int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_FREE_PROC_H__

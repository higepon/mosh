/*
 * freeproc.h: procedures refereced as free variable.
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
 *  $Id: freeproc.h 5306 2008-05-06 11:06:23Z higepon $
 */

#ifndef __SCHEME_FREE_PROC_H__
#define __SCHEME_FREE_PROC_H__

#include "scheme.h"

namespace scheme {

Object dummy(Object obj);
Object currentErrorPortEx(Object args, int argc, Object* argv);
Object hashtableKeysEx(Object args, int argc, Object* argv);
Object hashtableSetDEx(Object args, int argc, Object* argv);
Object hashtableRefEx(Object args, int argc, Object* argv);
Object eqHashtableCopyEx(Object args, int argc, Object* argv);
Object makeEqHashtableEx(Object args, int argc, Object* argv);
Object numberPEx(Object args, int argc, Object* argv);
Object consEx(Object args, int argc, Object* argv);
Object carEx(Object args, int argc, Object* argv);
Object cdrEx(Object args, int argc, Object* argv);
Object sourceInfoEx(Object args, int argc, Object* argv);
Object nullPEx(Object args, int argc, Object* argv);
Object setCarDEx(Object args, int argc, Object* argv);
Object setCdrDEx(Object args, int argc, Object* argv);
Object sysDisplayEx(Object args, int argc, Object* argv);
Object rxmatchEx(Object args, int argc, Object* argv);
Object regexpPEx(Object args, int argc, Object* argv);
Object regexpTostringEx(Object args, int argc, Object* argv);
Object rxmatchStartEx(Object args, int argc, Object* argv);
Object rxmatchEndEx(Object args, int argc, Object* argv);
Object rxmatchAfterEx(Object args, int argc, Object* argv);
Object rxmatchBeforeEx(Object args, int argc, Object* argv);
Object rxmatchSubstringEx(Object args, int argc, Object* argv);
Object regMatchProxy(Object args, int argc, Object* argv);
Object makeStringEx(Object args, int argc, Object* argv);
Object stringSetDEx(Object args, int argc, Object* argv);
Object stringLengthEx(Object args, int argc, Object* argv);
Object stringTosymbolEx(Object args, int argc, Object* argv);
Object stringTonumberEx(Object args, int argc, Object* argv);
Object stringAppendEx(Object args, int argc, Object* argv);
Object makeList(gc_vector<ucs4string>& v, gc_vector<ucs4string>::size_type i);
Object stringSplitEx(Object args, int argc, Object* argv);
Object numberTostringEx(Object args, int argc, Object* argv);
Object reverseIter(Object rest, Object ret);
Object reverseEx(Object args, int argc, Object* argv);
Object eofObjectPEx(Object args, int argc, Object* argv);
Object readCharEx(Object args, int argc, Object* argv);
Object charEqPEx(Object args, int argc, Object* argv);
Object stringPEx(Object args, int argc, Object* argv);
Object sysGetenvEx(Object args, int argc, Object* argv);
Object equalPEx(Object args, int argc, Object* argv);
Object openStringInputPortEx(Object args, int argc, Object* argv);
Object sysOpenOutputStringEx(Object args, int argc, Object* argv);
Object openOutputFileEx(Object args, int argc, Object* argv);
Object closeOutputPortEx(Object args, int argc, Object* argv);
Object closeInputPortEx(Object args, int argc, Object* argv);
Object digitTointegerEx(Object args, int argc, Object* argv);
Object getRemainingInputStringEx(Object args, int argc, Object* argv);
Object sysReaddirEx(Object args, int argc, Object* argv);
Object fileExistsPEx(Object args, int argc, Object* argv);
Object sysGetOutputStringEx(Object args, int argc, Object* argv);
Object stringToregexpEx(Object args, int argc, Object* argv);
Object charTointegerEx(Object args, int argc, Object* argv);
Object integerTocharEx(Object args, int argc, Object* argv);
Object formatEx(Object args, int argc, Object* argv);
Object currentInputPortEx(Object args, int argc, Object* argv);
Object currentOutputPortEx(Object args, int argc, Object* argv);
Object setCurrentInputPortDEx(Object args, int argc, Object* argv);
Object setCurrentOutputPortDEx(Object args, int argc, Object* argv);
Object charPEx(Object args, int argc, Object* argv);
Object writeEx(Object args, int argc, Object* argv);
Object gensymEx(Object args, int argc, Object* argv);
Object stringEqPEx(Object args, int argc, Object* argv);
Object vectorPEx(Object args, int argc, Object* argv);
Object listPEx(Object args, int argc, Object* argv);
Object memqEx(Object args, int argc, Object* argv);
Object memvEx(Object args, int argc, Object* argv);
Object eqPEx(Object args, int argc, Object* argv);
Object memberEx(Object args, int argc, Object* argv);
Object booleanPEx(Object args, int argc, Object* argv);
Object symbolTostringEx(Object args, int argc, Object* argv);
Object stringRefEx(Object args, int argc, Object* argv);
Object errorEx(Object args, int argc, Object* argv);
Object getTimeofdayEx(Object args, int argc, Object* argv);
Object valuesEx(Object args, int argc, Object* argv);
Object vmApplyEx(Object args, int argc, Object* argv);
Object pairPEx(Object args, int argc, Object* argv);
Object initLibraryTableEx(Object args, int argc, Object* argv);
Object map10Ex(Object args, int argc, Object* argv);
Object find10Ex(Object args, int argc, Object* argv);
Object sysPortSeekEx(Object args, int argc, Object* argv);
Object makeCustomBinaryInputPortEx(Object args, int argc, Object* argv);
Object getU8Ex(Object args, int argc, Object* argv);
Object bytevectorU8SetDEx(Object args, int argc, Object* argv);
Object transcodedPortEx(Object args, int argc, Object* argv);
Object utf8CodecEx(Object args, int argc, Object* argv);
Object makeTranscoderEx(Object args, int argc, Object* argv);
Object eofObjectEx(Object args, int argc, Object* argv);
Object sysOpenBytevectorOutputPortEx(Object args, int argc, Object* argv);
Object sysGetBytevectorEx(Object args, int argc, Object* argv);
Object bytevectorU8RefEx(Object args, int argc, Object* argv);
Object bytevectorLengthEx(Object args, int argc, Object* argv);
Object standardInputPortEx(Object args, int argc, Object* argv);
Object getBytevectorNEx(Object args, int argc, Object* argv);
Object utf8TostringEx(Object args, int argc, Object* argv);
Object openFileOutputPortEx(Object args, int argc, Object* argv);
Object openFileInputPortEx(Object args, int argc, Object* argv);
Object vectorEx(Object args, int argc, Object* argv);
Object regexpReplaceEx(Object args, int argc, Object* argv);
Object regexpReplaceAllEx(Object args, int argc, Object* argv);
Object errorfEx(Object args, int argc, Object* argv);
Object evalEx(Object args, int argc, Object* argv);
Object raiseEx(Object args, int argc, Object* argv);
Object raiseContinuableEx(Object args, int argc, Object* argv);
Object withExceptionHandlerEx(Object args, int argc, Object* argv);
Object makeVectorTypeEx(Object args, int argc, Object* argv);
Object vectorTypePEx(Object args, int argc, Object* argv);
Object vectorTypeDataEx(Object args, int argc, Object* argv);
Object vectorTypeInstanceOfPEx(Object args, int argc, Object* argv);
Object makeTypedVectorEx(Object args, int argc, Object* argv);
Object typedVectorGetNthEx(Object args, int argc, Object* argv);
Object typedVectorSetNthEx(Object args, int argc, Object* argv);
Object typedVectorTypeEx(Object args, int argc, Object* argv);
Object typedVectorPEx(Object args, int argc, Object* argv);
Object applyEx(Object args, int argc, Object* argv);
Object modEx(Object args, int argc, Object* argv);
Object divEx(Object args, int argc, Object* argv);
Object assqEx(Object args, int argc, Object* argv);
Object exitEx(Object args, int argc, Object* argv);
Object macroexpand1Ex(Object args, int argc, Object* argv);
Object procedurePEx(Object args, int argc, Object* argv);
Object loadEx(Object args, int argc, Object* argv);
Object symbolPEx(Object args, int argc, Object* argv);
Object dynamicWindEx(Object args, int argc, Object* argv);
Object charGePEx(Object args, int argc, Object* argv);
Object charGtPEx(Object args, int argc, Object* argv);
Object charLePEx(Object args, int argc, Object* argv);
Object charLtPEx(Object args, int argc, Object* argv);
Object readEx(Object args, int argc, Object* argv);
Object vectorTolistEx(Object args, int argc, Object* argv);
Object setSourceInfoDEx(Object args, int argc, Object* argv);
Object callProcessEx(Object args, int argc, Object* argv);
Object internalgetClosureNameEx(Object args, int argc, Object* argv);
Object appendEx(Object args, int argc, Object* argv);
Object append2Ex(Object args, int argc, Object* argv);
Object appendAEx(Object args, int argc, Object* argv);
Object appendDEx(Object args, int argc, Object* argv);
Object internalsetUnionEx(Object args, int argc, Object* argv);
Object internalsetIntersectEx(Object args, int argc, Object* argv);
Object makeCodeBuilderEx(Object args, int argc, Object* argv);
Object codeBuilderPut1DEx(Object args, int argc, Object* argv);
Object codeBuilderPut2DEx(Object args, int argc, Object* argv);
Object codeBuilderPut3DEx(Object args, int argc, Object* argv);
Object codeBuilderPut4DEx(Object args, int argc, Object* argv);
Object codeBuilderPut5DEx(Object args, int argc, Object* argv);
Object doNothingEx(Object args, int argc, Object* argv);

Object codeBuilderAppendDEx(Object args, int argc, Object* argv);
Object codeBuilderEmitEx(Object args, int argc, Object* argv);

Object pass3FindFreeEx(Object args, int argc, Object* argv);
Object pass3FindSetsEx(Object args, int argc, Object* argv);
inline bool existsInList(Object o, Object list)
{
    for (Object q = list; q.isPair(); q = q.cdr()) {
        if (q.car() == o) return true;
    }
    return false;
}

Object pass4FixupLabelsEx(Object args, int argc, Object* argv);

Object memq(Object o, Object list);
Object uniq(Object list);
Object assq(Object o, Object alist);
Object findFree(Object iform, Object locals, Object canFrees);
Object findFreeRec(Object i, Object l, Object canFrees, Object labelsSeen);
Object findFreeRecMap(Object l, Object canFrees, Object labelsSeen, Object list);
Object findSetsRecMap(Object lvars, Object list);
Object findSets(Object iform, Object lvars);
Object findSetsRec(Object i, Object lvars);

Object pass4FixupLabelCollect(Object vec);
Object pass4FixupLabel(Object vec);

int mod(int x, int y);
int div(int x, int y);






}; // namespace scheme

#endif // __SCHEME_FREE_PROC_H__

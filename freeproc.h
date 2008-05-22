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
Object currentErrorPortEx(Object args);
Object hashtableKeysEx(Object args);
Object hashtableSetEx(Object args);
Object hashtableRefEx(Object args);
Object makeEqHashtableEx(Object args);
Object numberPEx(Object args);
Object consEx(Object args);
Object carEx(Object args);
Object cdrEx(Object args);
Object sourceInfoEx(Object args);
Object nullPEx(Object args);
Object setCarEx(Object args);
Object setCdrEx(Object args);
Object sysDisplayEx(Object args);
Object rxmatchEx(Object args);
Object regexpPEx(Object args);
Object regexpTostringEx(Object args);
Object rxmatchStartEx(Object args);
Object rxmatchEndEx(Object args);
Object rxmatchAfterEx(Object args);
Object rxmatchBeforeEx(Object args);
Object rxmatchSubstringEx(Object args);
Object regMatchProxy(Object args);
Object makeStringEx(Object args);
Object stringSetEx(Object args);
Object stringLengthEx(Object args);
Object stringTosymbolEx(Object args);
Object stringTonumberEx(Object args);
Object stringAppendEx(Object args);
Object makeList(gc_vector<ucs4string>& v, gc_vector<ucs4string>::size_type i);
Object stringSplitEx(Object args);
Object numberTostringEx(Object args);
Object reverseIter(Object rest, Object ret);
Object reverseEx(Object args);
Object eofObjectPEx(Object args);
Object readCharEx(Object args);
Object charEqPEx(Object args);
Object stringPEx(Object args);
Object sysGetenvEx(Object args);
Object equalPEx(Object args);
Object openStringInputPortEx(Object args);
Object sysOpenOutputStringEx(Object args);
Object openOutputFileEx(Object args);
Object closeOutputPortEx(Object args);
Object closeInputPortEx(Object args);
Object digitTointegerEx(Object args);
Object getRemainingInputStringEx(Object args);
Object sysReaddirEx(Object args);
Object fileExistsPEx(Object args);
Object sysGetOutputStringEx(Object args);
Object stringToregexpEx(Object args);
Object charTointegerEx(Object args);
Object integerTocharEx(Object args);
Object formatEx(Object args);
Object currentInputPortEx(Object args);
Object currentOutputPortEx(Object args);
Object setCurrentInputPortEx(Object args);
Object setCurrentOutputPortEx(Object args);
Object charPEx(Object args);
Object writeEx(Object args);
Object gensymEx(Object args);
Object stringEqPEx(Object args);
Object vectorPEx(Object args);
Object listPEx(Object args);
Object memqEx(Object args);
Object eqPEx(Object args);
Object memberEx(Object args);
Object booleanPEx(Object args);
Object symbolTostringEx(Object args);
Object stringRefEx(Object args);
Object errorEx(Object args);
Object getTimeofdayEx(Object args);
Object valuesEx(Object args);
Object vmApplyEx(Object a);
Object pairPEx(Object args);
Object initLibraryTableEx(Object args);
Object map10Ex(Object args);
Object find10Ex(Object args);
Object sysPortSeekEx(Object args);
Object makeCustomBinaryInputPortEx(Object args);
Object getU8Ex(Object args);
Object bytevectorU8SetEx(Object args);
Object transcodedPortEx(Object args);
Object utf8CodecEx(Object args);
Object makeTranscoderEx(Object args);
Object eofObjectEx(Object args);
Object sysOpenBytevectorOutputPortEx(Object args);
Object sysGetBytevectorEx(Object args);
Object bytevectorU8RefEx(Object args);
Object bytevectorLengthEx(Object args);
Object standardInputPortEx(Object args);
Object getBytevectorNEx(Object args);
Object utf8TostringEx(Object args);
Object openFileOutputPortEx(Object args);
Object openFileInputPortEx(Object args);
Object vectorEx(Object args);
Object regexpReplaceEx(Object args);
Object regexpReplaceAllEx(Object args);
Object errorfEx(Object args);
Object evalEx(Object args);
Object raiseEx(Object args);
Object raiseContinuableEx(Object args);
Object withExceptionHandlerEx(Object args);
Object makeVectorTypeEx(Object args);
Object vectorTypePEx(Object args);
Object vectorTypeDataEx(Object args);
Object vectorTypeInstanceOfPEx(Object args);
Object makeTypedVectorEx(Object args);
Object typedVectorGetNthEx(Object args);
Object typedVectorSetNthEx(Object args);
Object typedVectorTypeEx(Object args);
Object typedVectorPEx(Object args);
Object applyEx(Object args);
Object modEx(Object args);
Object divEx(Object args);
Object assqEx(Object args);
Object exitEx(Object args);
int mod(int x, int y);
int div(int x, int y);
}; // namespace scheme

#endif // __SCHEME_FREE_PROC_H__

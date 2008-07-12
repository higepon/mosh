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
 *  $Id$
 */

#ifndef __SCHEME_FREE_PROC_H__
#define __SCHEME_FREE_PROC_H__

#include "scheme.h"

namespace scheme {

    Object dummy(Object obj);
    Object currentErrorPortEx(int argc, const Object* argv);
    Object hashtableKeysEx(int argc, const Object* argv);
    Object hashtableSetDEx(int argc, const Object* argv);
    Object hashtableRefEx(int argc, const Object* argv);
    Object eqHashtableCopyEx(int argc, const Object* argv);
    Object makeEqHashtableEx(int argc, const Object* argv);
    Object numberPEx(int argc, const Object* argv);
    Object consEx(int argc, const Object* argv);
    Object carEx(int argc, const Object* argv);
    Object cdrEx(int argc, const Object* argv);
    Object sourceInfoEx(int argc, const Object* argv);
    Object nullPEx(int argc, const Object* argv);
    Object setCarDEx(int argc, const Object* argv);
    Object setCdrDEx(int argc, const Object* argv);
    Object sysDisplayEx(int argc, const Object* argv);
    Object rxmatchEx(int argc, const Object* argv);
    Object regexpPEx(int argc, const Object* argv);
    Object regexpTostringEx(int argc, const Object* argv);
    Object rxmatchStartEx(int argc, const Object* argv);
    Object rxmatchEndEx(int argc, const Object* argv);
    Object rxmatchAfterEx(int argc, const Object* argv);
    Object rxmatchBeforeEx(int argc, const Object* argv);
    Object rxmatchSubstringEx(int argc, const Object* argv);
    Object regMatchProxy(int argc, const Object* argv);
    Object makeStringEx(int argc, const Object* argv);
    Object stringSetDEx(int argc, const Object* argv);
    Object stringLengthEx(int argc, const Object* argv);
    Object stringTosymbolEx(int argc, const Object* argv);
    Object stringTonumberEx(int argc, const Object* argv);
    Object stringAppendEx(int argc, const Object* argv);
    Object makeList(gc_vector<ucs4string>& v, gc_vector<ucs4string>::size_type i);
    Object stringSplitEx(int argc, const Object* argv);
    Object numberTostringEx(int argc, const Object* argv);
    Object reverseIter(Object rest, Object ret);
    Object reverseEx(int argc, const Object* argv);
    Object eofObjectPEx(int argc, const Object* argv);
    Object readCharEx(int argc, const Object* argv);
    Object charEqPEx(int argc, const Object* argv);
    Object stringPEx(int argc, const Object* argv);
    Object sysGetenvEx(int argc, const Object* argv);
    Object equalPEx(int argc, const Object* argv);
    Object openStringInputPortEx(int argc, const Object* argv);
    Object sysOpenOutputStringEx(int argc, const Object* argv);
    Object openOutputFileEx(int argc, const Object* argv);
    Object closeOutputPortEx(int argc, const Object* argv);
    Object closeInputPortEx(int argc, const Object* argv);
    Object digitTointegerEx(int argc, const Object* argv);
    Object getRemainingInputStringEx(int argc, const Object* argv);
    Object sysReaddirEx(int argc, const Object* argv);
    Object fileExistsPEx(int argc, const Object* argv);
    Object sysGetOutputStringEx(int argc, const Object* argv);
    Object stringToregexpEx(int argc, const Object* argv);
    Object charTointegerEx(int argc, const Object* argv);
    Object integerTocharEx(int argc, const Object* argv);
    Object formatEx(int argc, const Object* argv);
    Object currentInputPortEx(int argc, const Object* argv);
    Object currentOutputPortEx(int argc, const Object* argv);
    Object setCurrentInputPortDEx(int argc, const Object* argv);
    Object setCurrentOutputPortDEx(int argc, const Object* argv);
    Object charPEx(int argc, const Object* argv);
    Object writeEx(int argc, const Object* argv);
    Object gensymEx(int argc, const Object* argv);
    Object stringEqPEx(int argc, const Object* argv);
    Object vectorPEx(int argc, const Object* argv);
    Object listPEx(int argc, const Object* argv);
    Object memqEx(int argc, const Object* argv);
    Object memvEx(int argc, const Object* argv);
    Object eqPEx(int argc, const Object* argv);
    Object memberEx(int argc, const Object* argv);
    Object booleanPEx(int argc, const Object* argv);
    Object symbolTostringEx(int argc, const Object* argv);
    Object stringRefEx(int argc, const Object* argv);
    Object errorEx(int argc, const Object* argv);
    Object getTimeofdayEx(int argc, const Object* argv);
    Object valuesEx(int argc, const Object* argv);
    Object vmApplyEx(int argc, const Object* argv);
    Object pairPEx(int argc, const Object* argv);
    Object initLibraryTableEx(int argc, const Object* argv);
    Object map10Ex(int argc, const Object* argv);
    Object find10Ex(int argc, const Object* argv);
    Object sysPortSeekEx(int argc, const Object* argv);
    Object makeCustomBinaryInputPortEx(int argc, const Object* argv);
    Object getU8Ex(int argc, const Object* argv);
    Object bytevectorU8SetDEx(int argc, const Object* argv);
    Object transcodedPortEx(int argc, const Object* argv);
    Object utf8CodecEx(int argc, const Object* argv);
    Object makeTranscoderEx(int argc, const Object* argv);
    Object eofObjectEx(int argc, const Object* argv);
    Object sysOpenBytevectorOutputPortEx(int argc, const Object* argv);
    Object sysGetBytevectorEx(int argc, const Object* argv);
    Object bytevectorU8RefEx(int argc, const Object* argv);
    Object bytevectorLengthEx(int argc, const Object* argv);
    Object standardInputPortEx(int argc, const Object* argv);
    Object getBytevectorNEx(int argc, const Object* argv);
    Object utf8TostringEx(int argc, const Object* argv);
    Object openFileOutputPortEx(int argc, const Object* argv);
    Object openFileInputPortEx(int argc, const Object* argv);
    Object vectorEx(int argc, const Object* argv);
    Object regexpReplaceEx(int argc, const Object* argv);
    Object regexpReplaceAllEx(int argc, const Object* argv);
    Object errorfEx(int argc, const Object* argv);
    Object evalEx(int argc, const Object* argv);
    Object raiseEx(int argc, const Object* argv);
    Object raiseContinuableEx(int argc, const Object* argv);
    Object withExceptionHandlerEx(int argc, const Object* argv);
    Object makeVectorTypeEx(int argc, const Object* argv);
    Object vectorTypePEx(int argc, const Object* argv);
    Object vectorTypeDataEx(int argc, const Object* argv);
    Object vectorTypeInstanceOfPEx(int argc, const Object* argv);
    Object makeTypedVectorEx(int argc, const Object* argv);
    Object typedVectorGetNthEx(int argc, const Object* argv);
    Object typedVectorSetNthEx(int argc, const Object* argv);
    Object typedVectorTypeEx(int argc, const Object* argv);
    Object typedVectorPEx(int argc, const Object* argv);
    Object applyEx(int argc, const Object* argv);
    Object modEx(int argc, const Object* argv);
    Object divEx(int argc, const Object* argv);
    Object assqEx(int argc, const Object* argv);
    Object exitEx(int argc, const Object* argv);
    Object macroexpand1Ex(int argc, const Object* argv);
    Object procedurePEx(int argc, const Object* argv);
    Object loadEx(int argc, const Object* argv);
    Object symbolPEx(int argc, const Object* argv);
    Object dynamicWindEx(int argc, const Object* argv);
    Object charGePEx(int argc, const Object* argv);
    Object charGtPEx(int argc, const Object* argv);
    Object charLePEx(int argc, const Object* argv);
    Object charLtPEx(int argc, const Object* argv);
    Object readEx(int argc, const Object* argv);
    Object vectorTolistEx(int argc, const Object* argv);
    Object setSourceInfoDEx(int argc, const Object* argv);
    Object callProcessEx(int argc, const Object* argv);
    Object internalgetClosureNameEx(int argc, const Object* argv);
    Object appendEx(int argc, const Object* argv);
    Object append2Ex(int argc, const Object* argv);
    Object appendAEx(int argc, const Object* argv);
    Object appendDEx(int argc, const Object* argv);
    Object internalsetUnionEx(int argc, const Object* argv);
    Object internalsetIntersectEx(int argc, const Object* argv);
    Object stringEx(int argc, const Object* argv);

    Object lengthEx(int argc, const Object* argv);
    inline bool existsInList(Object o, Object list)
    {
        for (Object q = list; q.isPair(); q = q.cdr()) {
            if (q.car() == o) return true;
        }
        return false;
    }


    // callee should check <list>.
    inline Object memq(Object o, Object list)
    {
        for (Object p = list; p.isPair(); p = p.cdr()) {
            if (p.car() == o) {
                return p;
            }
        }
        return Object::False;
    }

    Object uniq(Object list);
    Object assq(Object o, Object alist);


    int mod(int x, int y);
    int div(int x, int y);

    Object stringTosymbol(Object str);

    inline Object assq(Object o, Object alist)
    {
        for (Object p = alist; p.isPair(); p = p.cdr()) {
            if (p.car().car() == o) {
                return p.car();
            }
        }
        return Object::False;
    }

    Object listTovectorEx(int argc, const Object* argv);

    Object pass3CompileReferEx(int argc, const Object* argv);
}; // namespace scheme

#endif // __SCHEME_FREE_PROC_H__

/*
 * FlonumProcedures.h -
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
 *  $Id: FlonumProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_FLONUM_PROCEDURES_
#define SCHEME_FLONUM_PROCEDURES_

#include "scheme.h"

namespace scheme {

    Object fixnumToflonumEx(VM* theVM, int argc, const Object* argv);
    Object flexptEx(VM* theVM, int argc, const Object* argv);
    Object flsqrtEx(VM* theVM, int argc, const Object* argv);
    Object flatanEx(VM* theVM, int argc, const Object* argv);
    Object flacosEx(VM* theVM, int argc, const Object* argv);
    Object flasinEx(VM* theVM, int argc, const Object* argv);
    Object fltanEx(VM* theVM, int argc, const Object* argv);
    Object flcosEx(VM* theVM, int argc, const Object* argv);
    Object flsinEx(VM* theVM, int argc, const Object* argv);
    Object fllogEx(VM* theVM, int argc, const Object* argv);
    Object flexpEx(VM* theVM, int argc, const Object* argv);
    Object flroundEx(VM* theVM, int argc, const Object* argv);
    Object fltruncateEx(VM* theVM, int argc, const Object* argv);
    Object flceilingEx(VM* theVM, int argc, const Object* argv);
    Object flfloorEx(VM* theVM, int argc, const Object* argv);
    Object fldenominatorEx(VM* theVM, int argc, const Object* argv);
    Object flnumeratorEx(VM* theVM, int argc, const Object* argv);
    Object flIntegerDiv0Ex(VM* theVM, int argc, const Object* argv);
    Object flIntegerMod0Ex(VM* theVM, int argc, const Object* argv);
    Object flIntegerDivEx(VM* theVM, int argc, const Object* argv);
    Object flIntegerModEx(VM* theVM, int argc, const Object* argv);
    Object flabsEx(VM* theVM, int argc, const Object* argv);
    Object flAddEx(VM* theVM, int argc, const Object* argv);
    Object flSubEx(VM* theVM, int argc, const Object* argv);
    Object flMulEx(VM* theVM, int argc, const Object* argv);
    Object flDivEx(VM* theVM, int argc, const Object* argv);
    Object flmaxEx(VM* theVM, int argc, const Object* argv);
    Object flminEx(VM* theVM, int argc, const Object* argv);
    Object flnanPEx(VM* theVM, int argc, const Object* argv);
    Object flinfinitePEx(VM* theVM, int argc, const Object* argv);
    Object flfinitePEx(VM* theVM, int argc, const Object* argv);
    Object flevenPEx(VM* theVM, int argc, const Object* argv);
    Object floddPEx(VM* theVM, int argc, const Object* argv);
    Object flnegativePEx(VM* theVM, int argc, const Object* argv);
    Object flpositivePEx(VM* theVM, int argc, const Object* argv);
    Object flzeroPEx(VM* theVM, int argc, const Object* argv);
    Object flintegerPEx(VM* theVM, int argc, const Object* argv);
    Object flLePEx(VM* theVM, int argc, const Object* argv);
    Object flGePEx(VM* theVM, int argc, const Object* argv);
    Object flGtPEx(VM* theVM, int argc, const Object* argv);
    Object flLtPEx(VM* theVM, int argc, const Object* argv);
    Object flEqPEx(VM* theVM, int argc, const Object* argv);
    Object realToflonumEx(VM* theVM, int argc, const Object* argv);

} // namespace scheme

#endif // SCHEME_FLONUM_PROCEDURES_

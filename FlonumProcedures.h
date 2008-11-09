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

#ifndef __SCHEME_FLONUM_PROCEDURES__
#define __SCHEME_FLONUM_PROCEDURES__

#include "scheme.h"

namespace scheme {

    Object fixnumToflonumEx(int argc, const Object* argv);
    Object flexptEx(int argc, const Object* argv);
    Object flsqrtEx(int argc, const Object* argv);
    Object flatanEx(int argc, const Object* argv);
    Object flacosEx(int argc, const Object* argv);
    Object flasinEx(int argc, const Object* argv);
    Object fltanEx(int argc, const Object* argv);
    Object flcosEx(int argc, const Object* argv);
    Object flsinEx(int argc, const Object* argv);
    Object fllogEx(int argc, const Object* argv);
    Object flexpEx(int argc, const Object* argv);
    Object flroundEx(int argc, const Object* argv);
    Object fltruncateEx(int argc, const Object* argv);
    Object flceilingEx(int argc, const Object* argv);
    Object flfloorEx(int argc, const Object* argv);
    Object fldenominatorEx(int argc, const Object* argv);
    Object flnumeratorEx(int argc, const Object* argv);
    Object flIntegerDiv0Ex(int argc, const Object* argv);
    Object flIntegerMod0Ex(int argc, const Object* argv);
    Object flIntegerDivEx(int argc, const Object* argv);
    Object flIntegerModEx(int argc, const Object* argv);
    Object flabsEx(int argc, const Object* argv);
    Object flAddEx(int argc, const Object* argv);
    Object flSubEx(int argc, const Object* argv);
    Object flMulEx(int argc, const Object* argv);
    Object flDivEx(int argc, const Object* argv);
    Object flmaxEx(int argc, const Object* argv);
    Object flminEx(int argc, const Object* argv);
    Object flnanPEx(int argc, const Object* argv);
    Object flinfinitePEx(int argc, const Object* argv);
    Object flfinitePEx(int argc, const Object* argv);
    Object flevenPEx(int argc, const Object* argv);
    Object floddPEx(int argc, const Object* argv);
    Object flnegativePEx(int argc, const Object* argv);
    Object flpositivePEx(int argc, const Object* argv);
    Object flzeroPEx(int argc, const Object* argv);
    Object flintegerPEx(int argc, const Object* argv);
    Object flLePEx(int argc, const Object* argv);
    Object flGePEx(int argc, const Object* argv);
    Object flGtPEx(int argc, const Object* argv);
    Object flLtPEx(int argc, const Object* argv);
    Object flEqPEx(int argc, const Object* argv);
    Object realToflonumEx(int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_FLONUM_PROCEDURES__

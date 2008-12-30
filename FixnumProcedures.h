/*
 * FixnumProcedures.h -
 *
 *   Copyright (c) 2008  Kokosabu(MIURA Yasuyuki)  <kokosabu@gmail.com>
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

#ifndef __SCHEME_FIXNUM_PROCEDURES__
#define __SCHEME_FIXNUM_PROCEDURES__

#include "scheme.h"

namespace scheme {

    Object fxEqPEx(VM* theVM, int argc, const Object* argv);
    Object fxGtPEx(VM* theVM, int argc, const Object* argv);
    Object fxLtPEx(VM* theVM, int argc, const Object* argv);
    Object fxGePEx(VM* theVM, int argc, const Object* argv);
    Object fxLePEx(VM* theVM, int argc, const Object* argv);
    Object fxzeroPEx(VM* theVM, int argc, const Object* argv);
    Object fxpositivePEx(VM* theVM, int argc, const Object* argv);
    Object fxnegativePEx(VM* theVM, int argc, const Object* argv);
    Object fxoddPEx(VM* theVM, int argc, const Object* argv);
    Object fxevenPEx(VM* theVM, int argc, const Object* argv);
    Object fxmaxEx(VM* theVM, int argc, const Object* argv);
    Object fxminEx(VM* theVM, int argc, const Object* argv);
    Object fxAddEx(VM* theVM, int argc, const Object* argv);
    Object fxMulEx(VM* theVM, int argc, const Object* argv);
    Object fxSubEx(VM* theVM, int argc, const Object* argv);
    Object fxdivEx(VM* theVM, int argc, const Object* argv);
    Object fxmodEx(VM* theVM, int argc, const Object* argv);
    Object fxdiv0Ex(VM* theVM, int argc, const Object* argv);
    Object fxmod0Ex(VM* theVM, int argc, const Object* argv);
    Object fxnotEx(VM* theVM, int argc, const Object* argv);
    Object fxandEx(VM* theVM, int argc, const Object* argv);
    Object fxiorEx(VM* theVM, int argc, const Object* argv);
    Object fxxorEx(VM* theVM, int argc, const Object* argv);
    Object fxifEx(VM* theVM, int argc, const Object* argv);
    Object fxbitCountEx(VM* theVM, int argc, const Object* argv);
    Object fxlengthEx(VM* theVM, int argc, const Object* argv);
    Object fxfirstBitSetEx(VM* theVM, int argc, const Object* argv);
    Object fxbitSetPEx(VM* theVM, int argc, const Object* argv);
    Object fxcopyBitEx(VM* theVM, int argc, const Object* argv);
    Object fxbitFieldEx(VM* theVM, int argc, const Object* argv);
    Object fxcopyBitFieldEx(VM* theVM, int argc, const Object* argv);
    Object fxarithmeticShiftEx(VM* theVM, int argc, const Object* argv);
    Object fxarithmeticShiftLeftEx(VM* theVM, int argc, const Object* argv);
    Object fxarithmeticShiftRightEx(VM* theVM, int argc, const Object* argv);
    Object fxrotateBitFieldEx(VM* theVM, int argc, const Object* argv);
    Object fxreverseBitFieldEx(VM* theVM, int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_FIXNUM_PROCEDURES__

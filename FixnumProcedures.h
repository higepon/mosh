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

    Object fxEqPEx(int argc, const Object* argv);
    Object fxGtPEx(int argc, const Object* argv);
    Object fxLtPEx(int argc, const Object* argv);
    Object fxGePEx(int argc, const Object* argv);
    Object fxLePEx(int argc, const Object* argv);
    Object fxzeroPEx(int argc, const Object* argv);
    Object fxpositivePEx(int argc, const Object* argv);
    Object fxnegativePEx(int argc, const Object* argv);
    Object fxoddPEx(int argc, const Object* argv);
    Object fxevenPEx(int argc, const Object* argv);
    Object fxmaxEx(int argc, const Object* argv);
    Object fxminEx(int argc, const Object* argv);
    Object fxAddEx(int argc, const Object* argv);
    Object fxMulEx(int argc, const Object* argv);
    Object fxSubEx(int argc, const Object* argv);
    Object fxdivEx(int argc, const Object* argv);
    Object fxmodEx(int argc, const Object* argv);
    Object fxdiv0Ex(int argc, const Object* argv);
    Object fxmod0Ex(int argc, const Object* argv);
    ////fx+/carry
    ////fx-/carry
    //Object fxnotEx(int argc, const Object* argv);
    //Object fxandEx(int argc, const Object* argv);
    //Object fxiorEx(int argc, const Object* argv);
    //Object fxxorEx(int argc, const Object* argv);
    //Object fxifEx(int argc, const Object* argv);
    //Object fxbitCountEx(int argc, const Object* argv);
    //Object fxlengthEx(int argc, const Object* argv);
    //Object fxfirstBitSetEx(int argc, const Object* argv);
    //Object fxcopyBitEx(int argc, const Object* argv);
    //Object fxbitFieldEx(int argc, const Object* argv);
    //Object fxcopyBitFieldEx(int argc, const Object* argv);
    //Object fxarithmeticShiftEx(int argc, const Object* argv);
    //Object fxarithmeticShiftLeftEx(int argc, const Object* argv);
    //Object fxarithmeticShiftRightEx(int argc, const Object* argv);
    //Object fxrotateBitFieldEx(int argc, const Object* argv);
    //Object fxreverseBitFieldEx(int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_FIXNUM_PROCEDURES__

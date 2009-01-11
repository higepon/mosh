/*
 * ByteVectorProcedures.h -
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
 *  $Id: ByteVectorProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_BYTEVECTOR_PROCEDURES__
#define __SCHEME_BYTEVECTOR_PROCEDURES__

#include "scheme.h"

namespace scheme {

    Object u8ListToByteVector(Object list);
    Object bytevectorIeeeDoubleSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorIeeeDoubleNativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorIeeeSingleSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorIeeeSingleNativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorIeeeDoubleRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorIeeeDoubleNativeRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorIeeeSingleRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorIeeeSingleNativeRefEx(VM* theVM, int argc, const Object* argv);
    Object utf8TostringEx(VM* theVM, int argc, const Object* argv);
    Object stringToutf8Ex(VM* theVM, int argc, const Object* argv);
    Object utf32TostringEx(VM* theVM, int argc, const Object* argv);
    Object utf16TostringEx(VM* theVM, int argc, const Object* argv);
    Object stringToutf32Ex(VM* theVM, int argc, const Object* argv);
    Object stringToutf16Ex(VM* theVM, int argc, const Object* argv);
    Object stringTobytevectorEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorTostringEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS64NativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU64NativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS64SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU64SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS64NativeRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU64NativeRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS64RefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU64RefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS32NativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU32NativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS32SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU32SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS32NativeRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU32NativeRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS32RefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU32RefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS16NativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU16NativeSetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS16SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU16SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS16NativeRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU16NativeRefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS16RefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU16RefEx(VM* theVM, int argc, const Object* argv);
    Object u8ListTobytevectorEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorTou8ListEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS8SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorS8RefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU8SetDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU8RefEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorCopyEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorCopyDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorFillDEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorEqPEx(VM* theVM, int argc, const Object* argv);
    Object makeBytevectorEx(VM* theVM, int argc, const Object* argv);
    Object nativeEndiannessEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorLengthEx(VM* theVM, int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_BYTEVECTOR_PROCEDURES__

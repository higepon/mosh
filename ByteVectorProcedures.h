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
    Object utf8TostringEx(int argc, const Object* argv);
    Object stringToutf8Ex(int argc, const Object* argv);
    Object utf32TostringEx(int argc, const Object* argv);
    Object utf16TostringEx(int argc, const Object* argv);
    Object stringToutf32Ex(int argc, const Object* argv);
    Object stringToutf16Ex(int argc, const Object* argv);
    Object stringTobytevectorEx(int argc, const Object* argv);
    Object bytevectorTostringEx(int argc, const Object* argv);
    Object bytevectorS64NativeSetDEx(int argc, const Object* argv);
    Object bytevectorU64NativeSetDEx(int argc, const Object* argv);
    Object bytevectorS64SetDEx(int argc, const Object* argv);
    Object bytevectorU64SetDEx(int argc, const Object* argv);
    Object bytevectorS64NativeRefEx(int argc, const Object* argv);
    Object bytevectorU64NativeRefEx(int argc, const Object* argv);
    Object bytevectorS64RefEx(int argc, const Object* argv);
    Object bytevectorU64RefEx(int argc, const Object* argv);
    Object bytevectorS32NativeSetDEx(int argc, const Object* argv);
    Object bytevectorU32NativeSetDEx(int argc, const Object* argv);
    Object bytevectorS32SetDEx(int argc, const Object* argv);
    Object bytevectorU32SetDEx(int argc, const Object* argv);
    Object bytevectorS32NativeRefEx(int argc, const Object* argv);
    Object bytevectorU32NativeRefEx(int argc, const Object* argv);
    Object bytevectorS32RefEx(int argc, const Object* argv);
    Object bytevectorU32RefEx(int argc, const Object* argv);
    Object bytevectorS16NativeSetDEx(int argc, const Object* argv);
    Object bytevectorU16NativeSetDEx(int argc, const Object* argv);
    Object bytevectorS16SetDEx(int argc, const Object* argv);
    Object bytevectorU16SetDEx(int argc, const Object* argv);
    Object bytevectorS16NativeRefEx(int argc, const Object* argv);
    Object bytevectorU16NativeRefEx(int argc, const Object* argv);
    Object bytevectorS16RefEx(int argc, const Object* argv);
    Object bytevectorU16RefEx(int argc, const Object* argv);
    Object u8ListTobytevectorEx(int argc, const Object* argv);
    Object bytevectorTou8ListEx(int argc, const Object* argv);
    Object bytevectorS8SetDEx(int argc, const Object* argv);
    Object bytevectorS8RefEx(int argc, const Object* argv);
    Object bytevectorU8SetDEx(int argc, const Object* argv);
    Object bytevectorU8RefEx(int argc, const Object* argv);
    Object bytevectorCopyEx(int argc, const Object* argv);
    Object bytevectorCopyDEx(int argc, const Object* argv);
    Object bytevectorFillDEx(int argc, const Object* argv);
    Object bytevectorEqPEx(int argc, const Object* argv);
    Object makeBytevectorEx(int argc, const Object* argv);
    Object nativeEndiannessEx(int argc, const Object* argv);
    Object bytevectorLengthEx(int argc, const Object* argv);
    Object getBytevectorNEx(int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_BYTEVECTOR_PROCEDURES__

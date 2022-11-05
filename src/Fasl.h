/*
 * Fasl.h - Fast loading.
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
 *  $Id: Fasl.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_FASL_
#define SCHEME_FASL_

#include "scheme.h"
#include "Vector.h"
#include "ByteVector.h"

namespace scheme {

class EqHashTable;
class BinaryInputPort;
class BinaryOutputPort;

class Fasl EXTEND_GC
{
public:
    enum {
        TAG_LOOKUP = 1,
        TAG_FIXNUM,
        TAG_PAIR,
        TAG_VECTOR,
        TAG_BVECTOR,
        TAG_REGEXP,
        TAG_CHAR,
        TAG_NIL,
        TAG_T,
        TAG_F,
        TAG_SYMBOL,
        TAG_STRING,
        TAG_INSTRUCTION,
        TAG_COMPILER_INSTRUCTION,
        TAG_FLONUM,
        TAG_SMALL_FIXNUM,
        TAG_ASCII_STRING, /* not used */
        TAG_ASCII_SYMBOL, /* not used */
        TAG_MEDIUM_FIXNUM,
        TAG_RTD,
        TAG_RECORD /* not used */,
        TAG_EQ_HASH_TABLE,
        TAG_BIGNUM,
        TAG_FIXNUM_0,
        TAG_FIXNUM_1,
        TAG_ASCII_UNINTERNED_SYMBOL,
        TAG_UNINTERNED_SYMBOL,
        TAG_SIMPLE_STRUCT,
        TAG_RATNUM,
        TAG_COMPNUM,
        TAG_PLIST,
        TAG_DLIST,
        TAG_SHORT_ASCII_SYMBOL,
        TAG_SHORT_ASCII_UNINTERNED_SYMBOL,
        TAG_SHORT_ASCII_STRING,
        TAG_DEFINING_SHARED,
        TAG_LARGE_FIXNUM, // For sizeof(fixedint) == sizeof(uint64_t)
        forbidden_comma
    };
};

} // namespace scheme

#endif // SCHEME_FASL_

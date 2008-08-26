/*
 * StringProcedures.h - <string> procedures.
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
 *  $Id: StringProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_STRING_PROCEDURES__
#define __SCHEME_STRING_PROCEDURES__

#include "scheme.h"

namespace scheme {

    Object format(const ucs4char* message, Object values);
    Object stringEx(int argc, const Object* argv);
    Object makeStringEx(int argc, const Object* argv);
    Object stringSetDEx(int argc, const Object* argv);
    Object stringLengthEx(int argc, const Object* argv);
    Object stringTosymbolEx(int argc, const Object* argv);
    Object stringTonumberEx(int argc, const Object* argv);
    Object stringAppendEx(int argc, const Object* argv);
    Object stringSplitEx(int argc, const Object* argv);
    Object stringToregexpEx(int argc, const Object* argv);
    Object stringEqPEx(int argc, const Object* argv);
    Object stringRefEx(int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_STRING_PROCEDURES__

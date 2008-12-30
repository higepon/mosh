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

    Object stringCopyEx(VM* theVM, int argc, const Object* argv);
    Object stringTosymbol(Object str);
    Object format(const ucs4char* message, Object values);
    Object stringEx(VM* theVM, int argc, const Object* argv);
    Object makeStringEx(VM* theVM, int argc, const Object* argv);
    Object stringSetDEx(VM* theVM, int argc, const Object* argv);
    Object stringLengthEx(VM* theVM, int argc, const Object* argv);
    Object stringTosymbolEx(VM* theVM, int argc, const Object* argv);
    Object stringTonumberEx(VM* theVM, int argc, const Object* argv);
    Object stringAppendEx(VM* theVM, int argc, const Object* argv);
    Object stringSplitEx(VM* theVM, int argc, const Object* argv);
    Object stringToregexpEx(VM* theVM, int argc, const Object* argv);
    Object stringEqPEx(VM* theVM, int argc, const Object* argv);
    Object stringRefEx(VM* theVM, int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_STRING_PROCEDURES__

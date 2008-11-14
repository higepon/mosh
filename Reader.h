/*
 * reader.h - 
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
 *  $Id: reader.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_READER__
#define __SCHEME_READER__

#include "scheme.h"
#include "ucs4string.h"

namespace scheme {

    Object readOld(TextualInputPort* port, bool& errorOccured);
    Object readNumber(const ucs4string& text, bool& errorOccured);
    Object read(TextualInputPort* port, bool& errorOccured);

    class Reader EXTEND_GC
    {
    public:
        static Object read(TextualInputPort* port, bool& isErrorOccured);
        static ucs4string readString(const ucs4string& s);
        static TextualInputPort* port() { return in_; }
        static Object parsed;
    private:
        static TextualInputPort* in_;
    };
};

typedef struct EXTEND_GC {
    union {
        bool  boolValue;
        int   exactValue;
        int   intValue;
        ucs4char charValue;
    };
    scheme::ucs4string stringValue;
    scheme::Object object;
} YYSTYPE;

#define YYSTYPE_IS_DECLARED 1

#endif // __SCHEME_READER__

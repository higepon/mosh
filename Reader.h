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

#ifndef SCHEME_READER_
#define SCHEME_READER_

#include "scheme.h"
#include "ucs4string.h"

namespace scheme {

    class ReaderContext EXTEND_GC
    {
    public:
        Object read(TextualInputPort* port, bool& isErrorOccured);
        TextualInputPort* port()
        {
            MOSH_ASSERT(port_ != NULL);
            return port_;
        }
        void setPort(TextualInputPort* port)
        {
            port_ = port;
        }
        void setParsed(Object parsed)
        {
            parsed_ = parsed;
        }
        Object parsed()
        {
            return parsed_;
        }
    private:
        Object parsed_;
        TextualInputPort* port_;
    };

    class ReaderHelper EXTEND_GC
    {
    public:
        static ucs4string readString(const ucs4string& s);
        static ucs4string readSymbol(const ucs4string& s);
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

#endif // SCHEME_READER_

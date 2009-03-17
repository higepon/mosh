/*
 * Latin1Codec.h -
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

#ifndef SCHEME_LATIN1_CODEC_
#define SCHEME_LATIN1_CODEC_

#include "Codec.h"

namespace scheme {

class Latin1Codec : public Codec
{
public:
    int putChar(BinaryOutputPort* port, ucs4char u, enum ErrorHandlingMode mode);
    int putChar(uint8_t* buf, ucs4char c, enum ErrorHandlingMode mode);
    ucs4char getChar(BinaryInputPort* port, enum ErrorHandlingMode mode, bool checkBOM = false);
    ucs4string getCodecName() const
    {
        return UC("latin-1-codec");
    }

    enum Codec::Type type() const
    {
        return Codec::Type(LATIN1);
    }


    static Codec* getCodec();
};

}; // namespace scheme

#endif // SCHEME_LATIN1_CODEC_

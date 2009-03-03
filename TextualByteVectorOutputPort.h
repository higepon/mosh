/*
 * TextualByteVectorOutputPort.h - 
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
 *  $Id: TextualByteVectorOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_TEXTUAL_BYTEVECTOR_OUTPUT_PORT__
#define __SCHEME_TEXTUAL_BYTEVECTOR_OUTPUT_PORT__

#include "scheme.h"
#include "TextualOutputPort.h"

namespace scheme {

class Codec;
class Transcoder;

class TextualByteVectorOutputPort : public TextualOutputPort
{
public:
    TextualByteVectorOutputPort(Transcoder* transcoder);
    virtual ~TextualByteVectorOutputPort();

    void putChar(ucs4char c);
    const gc_vector<uint8_t>& getByteVector() const;
    virtual int close();
    virtual void flush();
    ucs4string toString();
    Transcoder* transcoder() const;

private:
    Transcoder* transcoder_;
    Codec* codec_;
    gc_vector<uint8_t> v_;
};

}; // namespace scheme

#endif // __SCHEME_TEXTUAL_BYTEVECTOR_OUTPUT_PORT__

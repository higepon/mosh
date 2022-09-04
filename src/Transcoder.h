/*
 * Transcoder.h -
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
 *  $Id$
 */

#ifndef SCHEME_TRANSCODER_
#define SCHEME_TRANSCODER_

#include "scheme.h"
#include "Codec.h"

namespace scheme {

class Transcoder EXTEND_GC
{
public:

    explicit Transcoder(Codec* codec);
    Transcoder(Codec* codec, EolStyle eolStyle);
    Transcoder(Codec* codec, EolStyle eolStyle, enum ErrorHandlingMode handlingMode);

    Object codec() const { return Object::makeCodec(codec_); }
    enum EolStyle eolStyle();
    enum ErrorHandlingMode errorHandlingMode();
    Object eolStyleSymbol();
    Object errorHandlingModeSymbol();


    void putChar(BinaryOutputPort* port, ucs4char c);
    void putString(BinaryOutputPort* port, const ucs4string& s);
//    int putChar(uint8_t* buf, ucs4char c);
    ucs4char getChar(BinaryInputPort* port);
    ucs4string getString(BinaryInputPort* port);
    void unGetChar(ucs4char c);
    int getLineNo() const;

    static enum EolStyle nativeEolStyle();
    static bool validateEolStyle(Object eolStyle, enum EolStyle& result);
    static bool validateErrorHandlingMode(Object handlingMode, enum ErrorHandlingMode& result);

private:
    ucs4char getCharInternal(BinaryInputPort* port);

    static Object eolStyleToSymbol(const enum EolStyle eolstyle);
    static Object errorHandlingModeToSymbol(enum ErrorHandlingMode errorHandlingMode);

    bool beginningOfInput_;
    Codec* codec_;
    enum EolStyle eolStyle_;
    enum ErrorHandlingMode errorHandlingMode_;
    ucs4string buffer_;
    int lineNo_;
};

} // namespace scheme

#endif // SCHEME_TRANSCODER_

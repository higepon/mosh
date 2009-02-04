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

#ifndef __SCHEME_TRANSCODER__
#define __SCHEME_TRANSCODER__

#include "scheme.h"

namespace scheme {

class Codec;

class Transcoder EXTEND_GC
{
public:
    enum EolStyle
    {
        LF,
        CR,
        CRLF,
        NEL,
        CRNEL,
        LS,
        NONE,
    };
    enum ErrorHandlingMode
    {
        IGNORE_ERROR,
        RAISE,
        REPLACE,
    };

    Transcoder(Codec* codec, enum EolStyle e, enum ErrorHandlingMode m) : codec_(codec)
    {
    }
    Transcoder(Codec* codec);
    Transcoder(Codec* codec, const Object eolStyle);
    Transcoder(Codec* codec, const Object eolStyle, const Object handlingMode);


    Object codec() const { return Object::makeCodec(codec_); }
    Object eolStyle();
    Object errorHandlingMode();

    static Transcoder* nativeTranscoder();

private:
    Codec* codec_;
    enum EolStyle eolStyle_;
    enum ErrorHandlingMode errorHandlingMode_;

    static enum EolStyle nativeEolStyle();
    enum EolStyle symbolToEolStyle(const Object symbol);
    Object eolStyleToSymbol(const enum EolStyle eolstyle);
    enum ErrorHandlingMode symbolToErrorHandlingMode(const Object symbol);
    Object errorHandlingModeToSymbol(const enum ErrorHandlingMode errorHandlingMode);
};

}; // namespace scheme

#endif // __SCHEME_TRANSCODER__

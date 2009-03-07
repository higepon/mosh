/*
 * Transcoder.cpp -
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

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Symbol.h"
#include "Transcoder.h"
#include "UTF8Codec.h"

using namespace scheme;

Transcoder::Transcoder(Codec* codec) : codec_(codec)
{
    eolStyle_ = nativeEolStyle();
    errorHandlingMode_ = Codec::REPLACE;
}

Transcoder::Transcoder(Codec* codec, const Object eolStyle) : codec_(codec)
{
    eolStyle_ = symbolToEolStyle(eolStyle);
    errorHandlingMode_ = Codec::REPLACE;
}

Transcoder::Transcoder(Codec* codec, const Object eolStyle, const Object errorHandlingMode) : codec_(codec)
{
    eolStyle_          = symbolToEolStyle(eolStyle);
    errorHandlingMode_ = symbolToErrorHandlingMode(errorHandlingMode);
}

Object Transcoder::eolStyle()
{
    return eolStyleToSymbol(eolStyle_);
}

Object Transcoder::errorHandlingMode()
{
    return errorHandlingModeToSymbol(errorHandlingMode_);
}

Transcoder* Transcoder::nativeTranscoder()
{
    return new Transcoder(UTF8Codec::getCodec(), nativeEolStyle(), Codec::IGNORE_ERROR);
}

enum Transcoder::EolStyle Transcoder::nativeEolStyle()
{
#if LINE_FEED_CODE_LF
    return Transcoder::LF;
#elif LINE_FEED_CODE_CRLF
    return Transcoder::CRLF;
#elif LINE_FEED_CODE_CR
    return Transcoder::CR;
#else
    MOSH_FATAL("not found platform native eol style\n");
#endif
}

enum Transcoder::EolStyle Transcoder::symbolToEolStyle(const Object symbol)
{
    if (symbol == Symbol::LF) {
        return Transcoder::LF;
    } else if (symbol == Symbol::CR) {
        return Transcoder::CR;
    } else if (symbol == Symbol::CRLF) {
        return Transcoder::CRLF;
    } else if (symbol == Symbol::NEL) {
        return Transcoder::NEL;
    } else if (symbol == Symbol::CRNEL) {
        return Transcoder::CRNEL;
    } else if (symbol == Symbol::LS) {
        return Transcoder::LS;
    } else {
        return Transcoder::NONE;
    }
}

Object Transcoder::eolStyleToSymbol(const enum Transcoder::EolStyle eolstyle)
{
    switch (eolstyle) {
    case Transcoder::LF:
        return Symbol::LF;
    case Transcoder::CR:
        return Symbol::CR;
    case Transcoder::CRLF:
        return Symbol::CRLF;
    case Transcoder::NEL:
        return Symbol::NEL;
    case Transcoder::CRNEL:
        return Symbol::CRNEL;
    case Transcoder::LS:
        return Symbol::LS;
    default:
        return Symbol::NONE;
    }
}

enum Codec::ErrorHandlingMode Transcoder::symbolToErrorHandlingMode(const Object symbol)
{
    if (symbol == Symbol::IGNORE) {
        return Codec::IGNORE_ERROR;
    } else if (symbol == Symbol::RAISE) {
        return Codec::RAISE;
    } else if (symbol == Symbol::REPLACE) {
        return Codec::REPLACE;
    }
    MOSH_FATAL("dont't match error-handling-mode\n");
    return Codec::IGNORE_ERROR;
}

Object Transcoder::errorHandlingModeToSymbol(const enum Codec::ErrorHandlingMode errorHandlingMode)
{
    switch (errorHandlingMode) {
    case Codec::IGNORE_ERROR:
        return Symbol::IGNORE;
    case Codec::RAISE:
        return Symbol::RAISE;
    case Codec::REPLACE:
        return Symbol::REPLACE;
    default:
        MOSH_FATAL("not found errorHandlingMode\n");
    }
    return Object::Undef;
}

int Transcoder::out(BinaryOutputPort* port, ucs4char c)
{
    return codec_->out(port, c, errorHandlingMode_);
}

int Transcoder::out(uint8_t* buf, ucs4char c)
{
    return codec_->out(buf, c, errorHandlingMode_);
}

ucs4char Transcoder::in(BinaryInputPort* port)
{
    return codec_->in(port, errorHandlingMode_);
}

ucs4string Transcoder::readWholeString(BinaryInputPort* port)
{
    return codec_->in(port, errorHandlingMode_);
}

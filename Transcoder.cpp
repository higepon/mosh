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
#include "BinaryOutputPort.h"
#include "Transcoder.h"
#include "UTF8Codec.h"

using namespace scheme;

Transcoder::Transcoder(Codec* codec) :
    codec_(codec),
    eolStyle_(nativeEolStyle()),
    errorHandlingMode_(ErrorHandlingMode(REPLACE))
{
}

Transcoder::Transcoder(Codec* codec, EolStyle eolStyle) :
    codec_(codec),
    eolStyle_(eolStyle),
    errorHandlingMode_(ErrorHandlingMode(REPLACE))
{
}

Transcoder::Transcoder(Codec* codec, EolStyle eolStyle, enum ErrorHandlingMode errorHandlingMode) :
    codec_(codec),
    eolStyle_(eolStyle),
    errorHandlingMode_(errorHandlingMode)
{
}

enum EolStyle Transcoder::eolStyle()
{
    return eolStyle_;
}

enum ErrorHandlingMode Transcoder::errorHandlingMode()
{
    return errorHandlingMode_;
}

Object Transcoder::eolStyleSymbol()
{
    return eolStyleToSymbol(eolStyle_);
}

Object Transcoder::errorHandlingModeSymbol()
{
    return errorHandlingModeToSymbol(errorHandlingMode_);
}

Transcoder* Transcoder::nativeTranscoder()
{
    return new Transcoder(UTF8Codec::getCodec(), nativeEolStyle(), ErrorHandlingMode(IGNORE_ERROR));
}

enum EolStyle Transcoder::nativeEolStyle()
{
#if LINE_FEED_CODE_LF
    return EolStyle(LF);
#elif LINE_FEED_CODE_CRLF
    return EolStyle(CRLF);
#elif LINE_FEED_CODE_CR
    return EolStyle::CR;
#else
    MOSH_FATAL("not found platform native eol style\n");
#endif
}

Object Transcoder::eolStyleToSymbol(const enum EolStyle eolstyle)
{
    switch (eolstyle) {
    case EolStyle(LF):
        return Symbol::LF;
    case EolStyle(CR):
        return Symbol::CR;
    case EolStyle(CRLF):
        return Symbol::CRLF;
    case EolStyle(NEL):
        return Symbol::NEL;
    case EolStyle(CRNEL):
        return Symbol::CRNEL;
    case EolStyle(LS):
        return Symbol::LS;
    default:
        return Symbol::NONE;
    }
}

Object Transcoder::errorHandlingModeToSymbol(const enum ErrorHandlingMode errorHandlingMode)
{
    switch (errorHandlingMode) {
    case ErrorHandlingMode(IGNORE_ERROR):
        return Symbol::IGNORE;
    case ErrorHandlingMode(RAISE):
        return Symbol::RAISE;
    case ErrorHandlingMode(REPLACE):
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

bool Transcoder::validateEolStyle(Object eolStyle, EolStyle& result)
{
    MOSH_ASSERT(eolStyle.isSymbol());
    if (eolStyle == Symbol::LF) {
        result = EolStyle(LF);
    } else if (eolStyle == Symbol::CR) {
        result = EolStyle(CR);
    } else if (eolStyle == Symbol::CRLF) {
        result = EolStyle(CRLF);
    } else if (eolStyle == Symbol::NEL) {
        result = EolStyle(NEL);
    } else if (eolStyle == Symbol::CRNEL) {
        result = EolStyle(CRNEL);
    } else if (eolStyle == Symbol::LS) {
        result = EolStyle(LS);
    } else if (eolStyle == Symbol::NONE) {
        result = EolStyle(NONE);
    } else {
        return false;
    }
    return true;
}

bool Transcoder::validateErrorHandlingMode(Object symbol, enum ErrorHandlingMode& result)
{
    MOSH_ASSERT(symbol.isSymbol());
    if (symbol == Symbol::IGNORE) {
        result = ErrorHandlingMode(IGNORE_ERROR);
    } else if (symbol == Symbol::RAISE) {
        result = ErrorHandlingMode(RAISE);
    } else if (symbol == Symbol::REPLACE) {
        result = ErrorHandlingMode(REPLACE);
    } else {
        return false;
    }
    return true;
}

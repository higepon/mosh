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

using namespace scheme;

Transcoder::Transcoder(Codec* codec, const Object eolStyle) : codec_(codec)
{
    eolStyle_ = symbolToEolStyle(eolStyle);
}

Transcoder::Transcoder(Codec* codec, const Object eolStyle, const Object errorHandlingMode) : codec_(codec)
{
    eolStyle_          = symbolToEolStyle(eolStyle);
    errorhandlingMode_ = symbolToErrorHandlingMode(errorHandlingMode);
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

enum Transcoder::ErrorHandlingMode Transcoder::symbolToErrorHandlingMode(const Object symbol)
{
    if (symbol == Symbol::IGNORE) {
        return Transcoder::IGNORE_ERROR;
    } else if (symbol == Symbol::RAISE) {
        return Transcoder::RAISE;
    } else if (symbol == Symbol::REPLACE) {
        return Transcoder::REPLACE;
    }
}

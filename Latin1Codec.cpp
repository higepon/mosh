/*
 * Latin1Codec.cpp -
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
#include "Latin1Codec.h"
#include "VM.h"
#include "BinaryOutputPort.h"
#include "BinaryInputPort.h"
#include "ErrorProcedures.h"

using namespace scheme;

Codec* Latin1Codec::getCodec()
{
    static Codec* codec = NULL;
    if (codec == NULL) {
        codec = new Latin1Codec();
    }
    return codec;
}

int Latin1Codec::putChar(BinaryOutputPort* port, ucs4char u, enum ErrorHandlingMode mode)
{
    static uint8_t buf[1];
    const int size = putChar(buf, u, mode);
    return port->putU8(buf, size);
}

int Latin1Codec::putChar(uint8_t* buf, ucs4char u, enum ErrorHandlingMode mode)
{
    buf[0] = (uint8_t)u;
    return 1;
}

ucs4char Latin1Codec::getChar(BinaryInputPort* port, enum ErrorHandlingMode mode, bool checkBOM /* = false */)
{
    const int f = port->getU8();
    if (f == EOF) return EOF;
    return (uint8_t)(f & 0xff);
}


/*
 * UTF32Codec.cpp -
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
 *  $Id: UTF32Codec.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "UTF32Codec.h"
#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "UTF8Codec.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "ByteVector.h"
#include "BinaryOutputPort.h"
#include "BinaryInputPort.h"
#include "ErrorProcedures.h"

using namespace scheme;

Codec* UTF32Codec::getCodec()
{
#if WORDS_BIGENDIAN
    return getCodec(UTF_32BE);
#else
    return getCodec(UTF_32LE);
#endif
}

Codec* UTF32Codec::getCodec(int endianness)
{
    static Codec* codec[2] = { NULL, NULL };
    MOSH_ASSERT(endianness == 0 || endianness == 1);
    if (codec[endianness] == NULL) {
        codec[endianness] = new UTF32Codec(endianness);
    }
    return codec[endianness];
}

UTF32Codec::UTF32Codec()
{
#if WORDS_BIGENDIAN
    isLittleEndian_ = false;
#else
    isLittleEndian_ = true;
#endif
}

UTF32Codec::UTF32Codec(int endianness) : isLittleEndian_(endianness == UTF_32LE)
{
    MOSH_ASSERT(endianness == UTF_32BE || endianness == UTF_32LE);
}

int UTF32Codec::out(BinaryOutputPort* port, ucs4char u)
{
    MOSH_ASSERT(false);
    return 0;
}

int UTF32Codec::out(uint8_t* buf, ucs4char u)
{
    if (isLittleEndian_) {
        buf[0] = u;
        buf[1] = u >> 8;
        buf[2] = u >> 16;
        buf[3] = u >> 24;
    } else {
        buf[0] = u >> 24;
        buf[1] = u >> 16;
        buf[2] = u >> 8;
        buf[3] = u;
    }
    return 4;
}

ucs4char UTF32Codec::in(BinaryInputPort* port)
{
    int a = port->getU8();
    if (EOF == a) return EOF;
    int b = port->getU8();
    if (EOF == b) throwIOError("malformed utf32 byte sequence");
    int c = port->getU8();
    if (EOF == c)  throwIOError("malformed utf32 byte sequence");
    int d = port->getU8();
    if (EOF == d)  throwIOError("malformed utf32 byte sequence");

    if (isLittleEndian_) {
        return
            ((uint8_t)a) |
            ((uint8_t)b) << 8|
            ((uint8_t)c) << 16|
            ((uint8_t)d) << 24;
    } else {
        return
            ((uint8_t)d) |
            ((uint8_t)c) << 8|
            ((uint8_t)b) << 16|
            ((uint8_t)a) << 24;
    }
}

ucs4string UTF32Codec::readWholeString(BinaryInputPort* port)
{
    MOSH_ASSERT(false);
    return UC("");
}

int UTF32Codec::checkBOM(ByteVector* bytevector)
{
    if (bytevector->length() >= 4) {
        if (bytevector->u8Ref(0) == 0x00 &&
            bytevector->u8Ref(1) == 0x00 &&
            bytevector->u8Ref(2) == 0xFE &&
            bytevector->u8Ref(3) == 0xFF) {
            return UTF_32BE;
        } else if (bytevector->u8Ref(0) == 0xFF &&
                   bytevector->u8Ref(1) == 0xFE &&
                   bytevector->u8Ref(2) == 0x00 &&
                   bytevector->u8Ref(3) == 0x00) {
            return UTF_32LE;
        } else {
            return NO_BOM;
        }
    } else {
        return NO_BOM;
    }
}

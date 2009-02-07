/*
 * UTF16Codec.cpp -
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
 *  $Id: UTF16Codec.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "VM.h"
#include "ByteVector.h"
#include "BinaryOutputPort.h"
#include "BinaryInputPort.h"
#include "ErrorProcedures.h"
#include "UTF16Codec.h"

using namespace scheme;

Codec* UTF16Codec::getCodec()
{
#if WORDS_BIGENDIAN
    return getCodec(UTF_16BE);
#else
    return getCodec(UTF_16LE);
#endif
}

Codec* UTF16Codec::getCodec(int endianness)
{
    static Codec* codec[2] = { NULL, NULL };
    MOSH_ASSERT(endianness == 0 || endianness == 1);
    if (codec[endianness] == NULL) {
        codec[endianness] = new UTF16Codec(endianness);
    }
    return codec[endianness];
}

UTF16Codec::UTF16Codec()
{
#if WORDS_BIGENDIAN
    isLittleEndian_ = false;
#else
    isLittleEndian_ = true;
#endif
}

UTF16Codec::UTF16Codec(int endianness) : isLittleEndian_(endianness == UTF_16LE)
{
    MOSH_ASSERT(endianness == UTF_16BE || endianness == UTF_16LE);
}

int UTF16Codec::out(BinaryOutputPort* port, ucs4char u)
{
    MOSH_ASSERT(false);
    return 0;
}

int UTF16Codec::out(uint8_t* buf, ucs4char ch)
{
    if (ch > 0x10FFFF) {
        throwIOError("character out of utf16 range");
    }
    if (ch < 0x10000) {
        if (isLittleEndian_) {
            buf[0] = ch;
            buf[1] = ch >> 8;
        } else {
            buf[0] = ch >> 8;
            buf[1] = ch;
        }
        return 2;
    } else {
        const int u = (ch >> 16);
        const int w = u - 1;
        const int part1 = (ch >> 10) & 0x63;
        const int part2 = u & 1023;
        const uint16_t val1 = (54 << 10) | (w << 6) | part1;
        const uint16_t val2 = (55 << 10) | part2;
        if (isLittleEndian_) {
            buf[0] = val1;
            buf[1] = val1 >> 8;
            buf[2] = val2;
            buf[3] = val2 >> 8;
        } else {
            buf[0] = val1 >> 8;
            buf[1] = val1;
            buf[2] = val2 >> 8;
            buf[3] = val2;
        }
        return 4;
    }
}

ucs4char UTF16Codec::in(BinaryInputPort* port)
{
    const int a = port->getU8();
    if (EOF == a) return EOF;
    const int b = port->getU8();
    if (EOF == b) throwIOError("malformed utf16 byte sequence");

    const uint16_t val1 = isLittleEndian_ ? ((b << 8) | a) : ((a << 8) | b);
    if (val1 < 0xD800 || val1 > 0xDFFF) {
        return val1;
    }

    const int c = port->getU8();
    if (EOF == c) throwIOError("malformed utf16 byte sequence");
    const int d = port->getU8();
    if (EOF == d) throwIOError("malformed utf16 byte sequence");

    const uint16_t val2 = isLittleEndian_ ? ((d << 8) | c) : ((c << 8) | d);
    const int u = ((val1 >> 6) & 0x0D) + 1;
    const int part1 = val1 & 63;
    const int part2 = val2 & 1023;
    return (u << 16) | (part1 << 10) | part2;
}

ucs4string UTF16Codec::readWholeString(BinaryInputPort* port)
{
    MOSH_ASSERT(false);
    return UC("");
}

int UTF16Codec::checkBOM(ByteVector* bytevector)
{
    if (bytevector->length() >= 2) {
        if (bytevector->u8Ref(0) == 0xFE &&
            bytevector->u8Ref(1) == 0xFF) {
            return UTF_16BE;
        } else if (bytevector->u8Ref(0) == 0xFF &&
                   bytevector->u8Ref(1) == 0xFE) {
            return UTF_16LE;
        } else {
            return NO_BOM;
        }
    } else {
        return NO_BOM;
    }
}

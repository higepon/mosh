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

UTF16Codec::UTF16Codec()  
= default;

// constructor for UTF16_LE and UTF16_BE. (dontCheckBOM_ is true)
UTF16Codec::UTF16Codec(int endianness) : isLittleEndian_(endianness == UTF_16LE),  dontCheckBOM_(true)
{
    MOSH_ASSERT(endianness == UTF_16BE || endianness == UTF_16LE);
}

ucs4string UTF16Codec::getCodecName() const
{
    return ucs4string((UC("<utf-16-codec>")));
}

namespace {
void put2byte(uint8_t *out, uint16_t in, bool isLittleEndian)
{
    if (isLittleEndian) {
        out[0] = static_cast<uint8_t>(in);
        out[1] = static_cast<uint8_t>(in >> 8);
    } else {
        out[0] = static_cast<uint8_t>(in >> 8);
        out[1] = static_cast<uint8_t>(in);
    }
}

}

int UTF16Codec::putChar(uint8_t* buf, ucs4char ch, enum ErrorHandlingMode mode)
{
    if (ch > 0x10FFFF) {
        if (mode == ErrorHandlingMode(RAISE_ERROR)) {
            throwIOError2(IOError::ENCODE, UC("character out of utf16 range"), L1(Object::makeChar(ch)));
        } else if (mode == ErrorHandlingMode(REPLACE_ERROR)) {
            buf[0] = 0xff;
            buf[1] = 0xfd;
            return 2;
        } else {
            MOSH_ASSERT(mode == ErrorHandlingMode(IGNORE_ERROR));
            return 0;
        }
    }
    if (ch < 0x10000) {
        put2byte(buf, ch, isLittleEndian_);
        return 2;
    } else {
        // http://unicode.org/faq/utf_bom.html#utf16-3
        const uint16_t HI_SURROGATE_START = 0xD800;
        uint16_t X = (uint16_t)ch;
        ucs4char U = (ch >> 16) & ((1 << 5) - 1);
        uint16_t W = (uint16_t) U - 1;
        uint16_t HiSurrogate = HI_SURROGATE_START | (W << 6) | X >> 10;
        const uint16_t LO_SURROGATE_START = 0xDC00;
        X = (uint16_t)ch;
        uint16_t LoSurrogate = (uint16_t) (LO_SURROGATE_START | (X & ((1 << 10) - 1)));
        put2byte(buf + 0, HiSurrogate, isLittleEndian_);
        put2byte(buf + 2, LoSurrogate, isLittleEndian_);
        return 4;
    }
}

#define decodeError() \
    if (mode == ErrorHandlingMode(RAISE_ERROR)) {                             \
        throwIOError2(IOError::DECODE, UC("invalid utf-16 byte sequence")); \
    } else if (mode == ErrorHandlingMode(REPLACE_ERROR)) {                    \
        return 0xFFFD;                                                  \
    } else {                                                            \
        MOSH_ASSERT(mode == ErrorHandlingMode(IGNORE_ERROR));           \
        goto retry;                                                     \
    }

ucs4char UTF16Codec::getChar(BinaryInputPort* port, enum ErrorHandlingMode mode, bool checkBOMNow /* = false */)
{
retry:
    const int a = port->getU8();
    if (EOF == a) {
        return EOF;
    }
    const int b = port->getU8();
    if (EOF == b) {
        decodeError();
    }

    if (checkBOMNow && !dontCheckBOM_) {
        if (a == 0xFE && b == 0xFF) {
            isLittleEndian_ = false;
            // checkBOM = false
            return getChar(port, mode, false);
        } else if (a == 0xFF && b == 0xFE) {
            isLittleEndian_ = true;
            // checkBOM = false
            return getChar(port, mode, false);
        } else {
            isLittleEndian_ = nativeIsLittleEndinan_;
            // fall through
        }
    }

    const uint16_t val1 = isLittleEndian_ ? ((b << 8) | a) : ((a << 8) | b);
    if (val1 < 0xD800 || val1 > 0xDFFF) {
        return val1;
    }

    const int c = port->getU8();
    if (EOF == c) {
        decodeError();
    }
    const int d = port->getU8();
    if (EOF == d) {
        decodeError();
    }
    const uint16_t val2 = isLittleEndian_ ? ((d << 8) | c) : ((c << 8) | d);
    // http://unicode.org/faq/utf_bom.html#utf16-3
    uint16_t hi = val1;
    uint16_t lo = val2;
    ucs4char X = (hi & ((1 << 6) -1)) << 10 | (lo & ((1 << 10) -1));
    ucs4char W = (hi >> 6) & ((1 << 5) - 1);
    ucs4char U = W + 1;
    ucs4char C = U << 16 | X;
    return C;
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

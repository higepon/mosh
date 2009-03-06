/*
 * UTF8Codec.cpp - 
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
 *  $Id: UTF8Codec.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "UTF8Codec.h"
#include "BinaryOutputPort.h"
#include "BinaryInputPort.h"
#include "ErrorProcedures.h"

using namespace scheme;

Codec* UTF8Codec::getCodec()
{
    static Codec* codec = NULL;
    if (codec == NULL) {
        codec = new UTF8Codec();
    }
    return codec;
}

// Be careful, buf is shared.
int UTF8Codec::out(BinaryOutputPort* port, ucs4char u)
{
    static uint8_t buf[4];
    const int size = out(buf, u);
    return port->putU8(buf, size);
}

int UTF8Codec::out(uint8_t* buf, ucs4char u)
{
    // UTF8-1
    if (u < 0x80) {
        buf[0] = (uint8_t)u;
        return 1;
        // UTF8-2
    } else if (u < 0x7ff) {
        buf[0] = 0xc0 | ((u >> 6) & 0x1f);
        buf[1] = 0x80 | (u & 0x3f);
        return 2;
        // UTF8-3
    } else if (u < 0xffff) {
        buf[0] = 0xe0 | ((u >> 12) & 0xf);
        buf[1] = 0x80 | ((u >> 6) & 0x3f);
        buf[2] = 0x80 | (u & 0x3f);
        return 3;
        // UTF8-4
    } else if (u < 0x10ffff) {
        buf[0] = 0xf0 | ((u >> 18) & 0x7);
        buf[1] = 0x80 | ((u >> 12) & 0x3f);
        buf[2] = 0x80 | ((u >> 6) & 0x3f);
        buf[3] = 0x80 | (u & 0x3f);
        return 4;
    } else {
//            fprintf(stderr, "malformed utf8 error\n");
        return 0;
//            exit(-1);
    }
}

bool UTF8Codec::isUtf8Tail(uint8_t b)
{
    return (0x80 <= b && b <= 0xbf);
}

ucs4char UTF8Codec::in(BinaryInputPort* port)
{
    const int f = port->getU8();
    if (f == EOF) return EOF;
    uint8_t first = (uint8_t)(f & 0xff);

    // UTF8-1(ascii) = %x00-7F
    if (first < 0x80) {
        return first;
        // UTF8-2 = %xC2-DF UTF8-tail
    } else if (0xc2 <= first && first <= 0xdf) {
        uint8_t second = port->getU8();
        if (isUtf8Tail(second)) {
            return ((first & 0x1f) << 6) | (second & 0x3f);
        } else {
            throwIOError("invalid byte sequence");
        }
        // UTF8-3 = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
        //          %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
    } else if (0xe0 <= first && first <= 0xef) {
        uint8_t second = port->getU8();
        uint8_t third =  port->getU8();
        if (!isUtf8Tail(third)) {
            throwIOError("invalid byte sequence");
        } else if ((0xe0 == first && 0xa0 <= second && second <= 0xbf)    |
                   (0xed == first && 0x80 <= second && second <= 0x9f)    |
                   (0xe1 <= first && first <= 0xec && isUtf8Tail(second)) |
                   (0xee == first || 0xef == first) && isUtf8Tail(second)) {
            return ((first & 0xf) << 12) | ((second & 0x3f) << 6) | (third & 0x3f);
        } else {
            throwIOError("invalid byte sequence");
        }
        // UTF8-4 = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
        //          %xF4 %x80-8F 2( UTF8-tail )
    } else if (0xf0 <= first && first <= 0xf4) {
        uint8_t second =  port->getU8();
        uint8_t third =  port->getU8();
        uint8_t fourth = port->getU8();
        if (!isUtf8Tail(third) || !isUtf8Tail(fourth)) {
            throwIOError("invalid byte sequence");
        } else if ((0xf0 == first && 0x90 <= second && second <= 0xbf)     |
                   (0xf4 == first && 0x80 <= second && second <= 0x8f)     |
                   (0xf1 <= first && first <= 0xf3 && isUtf8Tail(second))) {
            return ((first & 0x7) << 18) | ((second & 0x3f) << 12) | ((third & 0x3f) << 6) | fourth;
        } else {
            throwIOError("invalid byte sequence");
        }
    } else {
        throwIOError2(IOError::DECODE, "invalid utf-8 byte sequence", Pair::list1(Object::makeByteVector(1, first)));
    }
    return ' ';
}

ucs4string UTF8Codec::readWholeString(BinaryInputPort* port)
{
    ucs4string ret;
    for (;;) {
        const ucs4char ch = in(port);
        if (EOF == ch) {
            break;
        }
        ret += ch;
    }
    return ret;
}

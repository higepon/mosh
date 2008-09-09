/*
 * reader.cpp -
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
 *  $Id: reader.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "TextualInputPort.h"
#include "reader.h"


using namespace scheme;

static TextualInputPort* in;
static Codec* codec;

Codec* parser_codec()
{
    MOSH_ASSERT(codec);
    return codec;
}


bool parser_input(char* buf, int max_size)
{
    int c = in->getU8();
    buf[0] = c;
    return c == EOF;
}

TextualInputPort* parser_port()
{
    return in;
}



Object scheme::read2(TextualInputPort* port, bool& errorOccured)
{
    extern int yyparse ();
    extern Object parsed;
    MOSH_ASSERT(port);
    in = port;
    codec = in->codec();
    const bool isParseError = yyparse() == 1;
    if (isParseError) {
        errorOccured = true;
        return Object::Undef;
    } else {
        return parsed;
    }
}

ucs4string readString(const ucs4string& s)
{
    ucs4string ret;
    for (ucs4string::const_iterator it = s.begin(); it != s.end(); ++it) {
        const ucs4char ch = *it;
        if (ch == '\\') {
            const ucs4char ch2 = *(++it);
            if (it == s.end()) break;
            switch (ch2)
            {
            case '"':
                ret += '"';
                break;
            case '\\':
                ret += '\\';
                break;
            case 'a':
                ret += 0x07;
                break;
            case 'b':
                ret += 0x08;
                break;
            case 't':
                ret += 0x09;
                break;
            case 'n':
                ret += 0x0a;
                break;
            case 'v':
                ret += 0x0b;
                break;
            case 'f':
                ret += 0x0c;
                break;
            case 'r':
                ret += 0x0d;
                break;
            default:
                ret += ch;
                ret += ch2;
                break;
            }

        } else {
            ret += ch;
        }
    }
    return ret;
}

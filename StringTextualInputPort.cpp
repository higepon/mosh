/*
 * StringTextualInputPort.cpp - 
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
 *  $Id: StringTextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "scheme.h"
#include "Object.h"
#include "StringTextualInputPort.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "ByteArrayBinaryInputPort.h"

using namespace scheme;

BinaryInputPort* stringToUTF8(const ucs4string& str)
{
    UTF8Codec codec;
    ::gc_vector<uint8_t> utf8Data;
    for (ucs4string::const_iterator it = str.begin(); it != str.end(); ++it) {
        uint8_t buffer[sizeof(ucs4char)];
        const int size = codec.out(buffer, *it);
        for (int i = 0; i < size; i++) {
            utf8Data.push_back(buffer[i]);
        }
    }
    uint8_t* ret = new(GC) uint8_t[utf8Data.size()];
    for (::gc_vector<uint8_t>::size_type i = 0; i < utf8Data.size(); i++) {
        ret[i] = utf8Data[i];
    }
    return new ByteArrayBinaryInputPort(ret, utf8Data.size());
}

// we can't parse UTF32, so convert into UTF8.
// ugly, ugly.
// We need flex for multi byte character.
StringTextualInputPort::StringTextualInputPort(const ucs4string& str)
  : TextualInputPort(stringToUTF8(str),
                     new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR))
{
}

StringTextualInputPort::~StringTextualInputPort()
{
}

ucs4string StringTextualInputPort::toString()
{
    return UC("<string port>");
}

int StringTextualInputPort::close()
{
    return 0;
}

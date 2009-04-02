/*
 * BasicTextualInputPort.cpp -
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: BasicTextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "SString.h"
#include "Codec.h"
#include "Transcoder.h"
#include "BinaryInputPort.h"
#include "TextualInputPort.h"
#include "Reader.h"
#include "Scanner.h"
#include "NumberScanner.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "TextualOutputPort.h"
#include "ProcedureMacro.h"

using namespace scheme;

TextualInputPort::TextualInputPort() :
    error_(Object::Nil),
    scanner_(new Scanner),
    numberScanner_(new NumberScanner)
{
}

TextualInputPort::~TextualInputPort()
{
}

Object TextualInputPort::position() const
{
    // caller should check hasPosition().
    MOSH_ASSERT(false);
    return Object::Undef;;
}

bool TextualInputPort::setPosition(int position)
{
    // caller should check hasPosition().
    MOSH_ASSERT(false);
    return false;
}

// On Mosh Textual port doesn't support position();
bool TextualInputPort::hasPosition() const
{
    return false;
}

bool TextualInputPort::hasSetPosition() const
{
    return false;
}


ucs4string TextualInputPort::getStringAll()
{
    ucs4string accum;
    for (;;) {
        const ucs4char ch = getChar();
        if (EOF == ch) {
            break;
        }
        accum += ch;
    }
    return accum;
}

ucs4string TextualInputPort::getString(int n)
{
    ucs4string accum;
    for (int size = 0; size < n; size++) {
        const ucs4char ch = getChar();
        if (EOF == ch) {
            break;
        }
        accum += ch;
    }
    return accum;
}

Scanner* TextualInputPort::scanner() const
{
    return scanner_;
}

NumberScanner* TextualInputPort::numberScanner() const
{
    return numberScanner_;
}


ucs4char TextualInputPort::lookaheadChar()
{
    const ucs4char c = getChar();
    if (c != EOF) {
        unGetChar(c);
    }
    return c;
}

Object TextualInputPort::getLine()
{
    ucs4string ret;
    for (;;) {
        ucs4char ch = getChar();
        if (ret.empty() && ch == EOF) {
            return Object::Eof;
        }
        if (ch == '\n' || ch == EOF) {
            break;
        }
        ret += ch;
    }
    return Object::makeString(ret);
}

void TextualInputPort::setError(Object error)
{
    error_ = error;
}

Object TextualInputPort::error() const
{
    return error_;
}

Object TextualInputPort::getDatum(bool& errorOccured)
{
    return Reader::read(this, errorOccured);
}

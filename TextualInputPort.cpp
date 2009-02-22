/*
 * TextualInputPort.cpp -
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
 *  $Id: TextualInputPort.cpp 183 2008-07-04 06:19:28Z higepon $
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

TextualInputPort::TextualInputPort(BinaryInputPort* port, Transcoder* coder) : codec_(coder->codec().toCodec()),
                                                                               port_(port),
                                                                               transcoder_(coder),
                                                                               buffer_(UC("")),
                                                                               line_(1),
                                                                               error_(Object::Nil),
                                                                               scanner_(new Scanner),
                                                                               numberScanner_(new NumberScanner)
{
}

TextualInputPort::TextualInputPort() : port_(NULL),
                                       error_(Object::Nil),
                                       scanner_(new Scanner),
                                       numberScanner_(new NumberScanner())
{
}

TextualInputPort::TextualInputPort(const TextualInputPort& o)
{
    MOSH_ASSERT(false);
}

TextualInputPort::~TextualInputPort()
{
    close();
}

int TextualInputPort::getU8()
{
    MOSH_ASSERT(port_);
    return port_->getU8();
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

ucs4char TextualInputPort::getChar()
{
    ucs4char c;
    if (buffer_.empty()) {
        c= codec_->in(port_);
    } else {
        c = buffer_[buffer_.size() - 1];
        buffer_.erase(buffer_.size() - 1, 1);
    }
    if (c == '\n') ++line_;
    return c;
}

ucs4char TextualInputPort::lookaheadChar(int offset /* = 1 */)
{
    gc_vector<ucs4char> characters;
    ucs4char ret;
    for (int i = 0; i < offset; i++) {
        ret = getChar();
        characters.push_back(ret);
    }
    for (gc_vector<ucs4char>::const_iterator it = characters.begin(); it != characters.end(); ++it) {
        unGetChar(*it);
    }
    return ret;
}

ucs4char TextualInputPort::lookaheadChar()
{
    const ucs4char c = getChar();
    unGetChar(c);
    return c;
}

bool TextualInputPort::hasPortPosition() const
{
    return true;
}

bool TextualInputPort::hasSetPortPosition() const
{
    return true;
}

int TextualInputPort::portPosition() const
{
    return 0;
}

bool TextualInputPort::setPortPostion()
{
    return false;
}

int TextualInputPort::getLineNo() const
{
    return line_;
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
            ret += ch;
            break;
        }
        ret += ch;
    }
    return Object::makeString(ret);
}

void TextualInputPort::unGetChar(ucs4char c)
{
    if (EOF == c) return;
    buffer_ += c;
}

ucs4string TextualInputPort::toString() {
    return port_->toString();
}

void TextualInputPort::setError(Object error)
{
    error_ = error;
}

Object TextualInputPort::error() const
{
    return error_;
}

Object TextualInputPort::getDatumOld(bool& errorOccured)
{
    return readOld(this, errorOccured);
}

Object TextualInputPort::getDatum(bool& errorOccured)
{
    return Reader::read(this, errorOccured);
}

int TextualInputPort::close()
{
    if (NULL != port_) {
        return port_->close();
    } else {
        return 0;
    }
}

Transcoder* TextualInputPort::transcoder() const
{
    return transcoder_;
}

Codec* TextualInputPort::codec() const
{
    return codec_;
}

Scanner* TextualInputPort::scanner() const
{
    return scanner_;
}

NumberScanner* TextualInputPort::numberScanner() const
{
    return numberScanner_;
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

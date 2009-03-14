/*
 * TranscodedTextualInputOutputPort.h - 
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
 *  $Id: TranscodedTextualInputOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_TRANSCODED_TEXTUAL_INPUT_OUTPUT_PORT__
#define __SCHEME_TRANSCODED_TEXTUAL_INPUT_OUTPUT_PORT__

#include "TextualOutputPort.h"
#include "TextualInputPort.h"
#include "BinaryInputOutputPort.h"

namespace scheme {

class TextualInputOutputPort : public TextualOutputPort, public TextualInputPort
{
public:
    virtual ~TextualInputOutputPort() {}
    Object position() const = 0;
    bool setPosition(int position) = 0;
    bool hasPosition() const = 0;
    bool hasSetPosition() const = 0;

};

class TranscodedTextualInputOutputPort : public TextualInputOutputPort
{
public:
    TranscodedTextualInputOutputPort(BinaryInputOutputPort* port, Transcoder* coder);
    virtual ~TranscodedTextualInputOutputPort();

    Object position() const;
    bool setPosition(int position);
    bool hasPosition() const;
    bool hasSetPosition() const;
    int close();

    // TextualInputPort interfaces
    ucs4char getChar();
    int getLineNo() const;
    void unGetChar(ucs4char c);
    Transcoder* transcoder() const;

    // TextualOutputPort interfaces
    void putChar(ucs4char c);
    void flush();

    // Port interfaces
    ucs4string toString();

private:
    ucs4char getCharInternal();

    BinaryInputOutputPort* port_;
    Transcoder* transcoder_;
    ucs4string buffer_;
    int line_;
    const enum EolStyle eolStyle_;
};

}; // namespace scheme

#endif // __SCHEME_TRANSCODED_TEXTUAL_INPUT_OUTPUT_PORT__

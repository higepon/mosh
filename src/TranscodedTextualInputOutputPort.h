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

#ifndef SCHEME_TRANSCODED_TEXTUAL_INPUT_OUTPUT_PORT_
#define SCHEME_TRANSCODED_TEXTUAL_INPUT_OUTPUT_PORT_

#include "TextualOutputPort.h"
#include "TextualInputPort.h"
#include "BinaryInputOutputPort.h"

namespace scheme {

class TextualInputOutputPort : public TextualOutputPort, public TextualInputPort
{
public:
    ~TextualInputOutputPort() override = default;
    Object position() const override = 0;
    bool setPosition(int64_t position) override = 0;
    bool hasPosition() const override = 0;
    bool hasSetPosition() const override = 0;

};

class TranscodedTextualInputOutputPort : public TextualInputOutputPort
{
public:
    TranscodedTextualInputOutputPort(BinaryInputOutputPort* port, Transcoder* coder);
    ~TranscodedTextualInputOutputPort() override;

    Object position() const override;
    bool setPosition(int64_t position) override;
    bool hasPosition() const override;
    bool hasSetPosition() const override;
    int close() override;
    bool isClosed() const override;

    // TextualInputPort interfaces
    ucs4char getChar() override;
    int getLineNo() const override;
    void unGetChar(ucs4char c) override;
    Transcoder* transcoder() const override;

    // TextualOutputPort interfaces
    void putChar(ucs4char c) override;
    void flush() override;

    // Port interfaces
    ucs4string toString() override;

private:
    BinaryInputOutputPort* port_;
    Transcoder* transcoder_;
    int line_;
    bool isClosed_;
};

} // namespace scheme

#endif // SCHEME_TRANSCODED_TEXTUAL_INPUT_OUTPUT_PORT_

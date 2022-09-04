/*
 * TranscodedTextualOutputPort.h - 
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
 *  $Id: TextualOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_TRANSCODED_TEXTUAL_OUTPUT_PORT_
#define SCHEME_TRANSCODED_TEXTUAL_OUTPUT_PORT_

#include "TextualOutputPort.h"

namespace scheme {

class TranscodedTextualOutputPort : public TextualOutputPort
{
public:
    TranscodedTextualOutputPort(BinaryOutputPort* port, Transcoder* coder);
    ~TranscodedTextualOutputPort() override;

    int close() override;
    bool isClosed() const override;
    void flush() override;
    enum OutputPort::bufferMode bufferMode() const override;
    void putChar(ucs4char c) override;
    BinaryOutputPort* binaryPort() const;
    Transcoder* transcoder() const override;
    ucs4string toString() override;

private:
    BinaryOutputPort* port_;
    Transcoder* transcoder_;
    const enum EolStyle eolStyle_;
    bool isClosed_{false};
};

} // namespace scheme

#endif // SCHEME_TRANSCODED_TEXTUAL_OUTPUT_PORT_

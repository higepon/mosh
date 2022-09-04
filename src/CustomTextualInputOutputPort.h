/*
 * CustomTextualInputOutputPort.h -
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
 *  $Id: CustomTextualInputOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_CUSTOM_TEXTUAL_INPUTOUTPUT_PORT_
#define SCHEME_CUSTOM_TEXTUAL_INPUTOUTPUT_PORT_

#include "TranscodedTextualInputOutputPort.h"

namespace scheme {

class CustomTextualInputOutputPort : public TextualInputOutputPort
{
public:
    CustomTextualInputOutputPort(VM* theVM,
                                 const ucs4string& id,
                                 Object readProc,
                                 Object writeProc,
                                 Object getPositionProc,
                                 Object setPositionProc,
                                 Object closeProc);
    ~CustomTextualInputOutputPort() override;

    // textual-input
    ucs4char getChar() override;
    int getLineNo() const override;
    void unGetChar(ucs4char c) override;

    // textual-output
    void flush() override;
    enum OutputPort::bufferMode bufferMode() const override;
    void putChar(ucs4char c) override;

    Transcoder* transcoder() const override;

    // Port interface
    ucs4string toString() override;
    bool hasPosition() const override;
    bool hasSetPosition() const override;
    Object position() const override;
    bool setPosition(int64_t position) override;
    int close() override;
    bool isClosed() const override;
private:
    VM* theVM_;
    const ucs4string id_;
    const Object readProc_;
    const Object writeProc_;
    const Object getPositionProc_;
    const Object setPositionProc_;
    const Object closeProc_;
    ucs4string buffer_;
    int line_{1};
    bool isClosed_{false};
};

} // namespace scheme

#endif // SCHEME_CUSTOM_TEXTUAL_INPUTOUTPUT_PORT_

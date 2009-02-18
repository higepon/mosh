/*
 * TextualOutputPort.h - 
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
 *  $Id: TextualOutputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_TEXTUAL_OUTPUT_PORT__
#define __SCHEME_TEXTUAL_OUTPUT_PORT__

#include "Port.h"

namespace scheme {

class String;
class Codec;
class Transcoder;
class BinaryOutputPort;

// N.B. We need to close the port automatically when it is not referenced.
// So use gc_cleanup.
// TextualOutputPort never close binary output port, it will be closed on BinaryOutputPort's destructors.
class TextualOutputPort : public Port
{
public:
    TextualOutputPort();
    TextualOutputPort(BinaryOutputPort* port, Transcoder* coder);
    virtual ~TextualOutputPort();

    virtual int close();
    virtual void putChar(ucs4char c);
    virtual void flush();

    void putString(String* str);
    void putString(const ucs4string& s);
    void putString(const char* s);
    void putDatum(Object o, bool inList = false);
    void display(Object o, bool inList = false);
    void putPair(Object obj, bool inList = false);
    void format(const ucs4string& fmt, Object args);
    BinaryOutputPort* binaryPort() const;
    bool isErrorOccured() const;
    Object errorMessage() const;
    Object irritants() const;
    Transcoder* transcoder() const;
    bool hasPosition() const { return true; }
    bool hasSetPosition() const { return true; }
    int position() const {
        MOSH_ASSERT(false);
        return 0;
    }
    bool setPosition(int position)
    {
        MOSH_ASSERT(false);
    }


private:
    BinaryOutputPort* port_;
    Codec* codec_;
    Transcoder* transcoder_;
    bool isErrorOccured_;
    Object errorMessage_;
    Object irritants_;
};

}; // namespace scheme

#endif // __SCHEME_TEXTUAL_OUTPUT_PORT__

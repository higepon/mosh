/*
 * BasicTextualInputPort.h -
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
 *  $Id: BasicTextualInputPort.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_TEXTUAL_INPUT_PORT__
#define __SCHEME_TEXTUAL_INPUT_PORT__

#include "Port.h"

namespace scheme {

class Codec;
class Transcoder;
class BinaryInputPort;
class Scanner;
class NumberScanner;

class TextualInputPort : virtual public Port
{
public:
    TextualInputPort();
    virtual ~TextualInputPort();

    virtual ucs4char getChar() = 0;
    virtual int getLineNo() const = 0;
    virtual void unGetChar(ucs4char c) = 0;
    virtual Transcoder* transcoder() const = 0;

    // template method pattern
    virtual ucs4string getString(int n);
    virtual ucs4string getStringAll();
    virtual ucs4char lookaheadChar();
    virtual Object getLine();
    virtual void setError(Object error);
    virtual Object error() const;
    virtual Object getDatumOld(bool& errorOccured);
    virtual Object getDatum(bool& errorOccured);
    virtual Scanner* scanner() const;
    virtual NumberScanner* numberScanner() const;
    virtual bool hasPosition() const;
    virtual bool hasSetPosition() const;
    virtual Object position() const;
    virtual bool setPosition(int position);

private:
    Object error_;
    Scanner* scanner_;
    NumberScanner* numberScanner_;
};

}; // namespace scheme

#endif // __SCHEME_TEXTUAL_INPUT_PORT__

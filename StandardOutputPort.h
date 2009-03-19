/*
 * StandardOutputPort.h - <standard output port>
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
 *  $Id: StandardOutputPort.h.h 1207 2009-02-18 14:54:11Z higepon $
 */

#ifndef SCHEME_STANDARD_OUTPUT_PORT_
#define SCHEME_STANDARD_OUTPUT_PORT_

#include "LineBufferedFileBinaryOutputPort.h"
#ifdef _WIN32
    #define fileno _fileno
#endif

namespace scheme {

// stdout does'n support port-position
class StandardOutputPort : public LineBufferedFileBinaryOutputPort
{
public:
    StandardOutputPort() : LineBufferedFileBinaryOutputPort(fileno(stdout)) {}
    virtual ~StandardOutputPort() {}

    ucs4string toString()
    {
        return UC("standard-out-port");
    }

    bool hasPosition() const
    {
        return false;
    }

    int close()
    {
        flush();
        return MOSH_SUCCESS;
    }

    bool hasSetPosition() const
    {
        return false;
    }

    Object position() const {
        return Object::Undef;
    }

    bool setPosition(int position)
    {
        return false;
    }
};

}; // namespace scheme

#endif // SCHEME_STANDARD_OUTPUT_PORT_

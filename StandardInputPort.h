/*
 * StandardInputPort.h - <standard input port>
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
 *  $Id: StandardInputPort.h 1207 2009-02-18 14:54:11Z higepon $
 */

#ifndef SCHEME_STANDARD_INPUT_PORT_
#define SCHEME_STANDARD_INPUT_PORT_

#include "FileBinaryInputPort.h"

namespace scheme {

// stdin does'n support port-position
class StandardInputPort : public FileBinaryInputPort
{
public:
    StandardInputPort() : FileBinaryInputPort(new File(File::STANDARD_IN)) {}
    virtual ~StandardInputPort() {}
    ucs4string toString()
    {
        return UC("<standard input port>");
    }
    bool hasPosition() const
    {
        return false;
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

} // namespace scheme

#endif // SCHEME_STANDARD_INPUT_PORT_

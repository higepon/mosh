/*
 * StandardErrorPort.h - <standard error port>
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
 *  $Id:$
 */

#ifndef SCHEME_STANDARD_ERROR_PORT_
#define SCHEME_STANDARD_ERROR_PORT_

#include "FileBinaryOutputPort.h"

namespace scheme {

// stderr does'n support port-position
class StandardErrorPort : public FileBinaryOutputPort
{
public:
    StandardErrorPort() : FileBinaryOutputPort(&File::STANDARD_ERR) {}
    ~StandardErrorPort() override = default;

    ucs4string toString() override
    {
        return ucs4string(UC("standard-error-port"));
    }

    bool hasPosition() const override
    {
        return false;
    }

    bool hasSetPosition() const override
    {
        return false;
    }

    Object position() const override {
        return Object::Undef;
    }

    bool setPosition(int64_t position) override
    {
        return false;
    }
};

} // namespace scheme

#endif // SCHEME_STANDARD_ERROR_PORT_

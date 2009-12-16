/*
 * TimeService.h -
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
 *  $Id: TimeService.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_TIMESERVICE_
#define SCHEME_TIMESERVICE_

#include "scheme.h"
#include <sys/time.h>

namespace scheme {

class Time EXTEND_GC
{
public:
    Time(time_t sec, suseconds_t usec) : sec_(sec), usec_(usec)
    {
    }

    static Time now()
    {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        return Time(tv.tv_sec, tv.tv_usec);
    }

    static intptr_t diffMsec(const Time& t1, const Time& t2)
    {
        return (t1.sec_ - t2.sec_) * 1000 + (t1.usec_ - t2.usec_) / 1000;
    }

    static intptr_t diffUsec(const Time& t1, const Time& t2)
    {
        return (t1.sec_ - t2.sec_) * 1000 * 1000 + (t1.usec_ - t2.usec_);
    }


private:
    time_t sec_;
    suseconds_t usec_;
};


}; // namespace scheme

#endif // SCHEME_TIMESERVICE_

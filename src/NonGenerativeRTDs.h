/*
 * NonGenerativeRTDs.h - Storage for nongenerative records.
 *
 *   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: RecordTypeDescriptor.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_NONGENERATIVE_RTDS_
#define SCHEME_NONGENERATIVE_RTDS_

#include "scheme.h"

namespace scheme {

class NonGenerativeRTDs EXTEND_GC
{
public:
    static Object lookup(Object uid)
    {
        mutex_.lock();
        ObjectMap::const_iterator found = nonGenerativeRTDs_.find(uid);
        if (found == nonGenerativeRTDs_.end()) {
            mutex_.unlock();
            return Object::False;
        } else {
            mutex_.unlock();
            return found->second;
        }
    }

    static void add(Object uid, Object rtd)
    {
        MOSH_ASSERT(rtd.isRecordTypeDescriptor());
        mutex_.lock();
        nonGenerativeRTDs_[uid] = rtd;
        mutex_.unlock();
    }

private:
   static ObjectMap nonGenerativeRTDs_;
   static Mutex mutex_;
};

} // namespace scheme

#endif // SCHEME_NONGENERATIVE_RTDS_

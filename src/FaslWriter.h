/*
 * FaslWriter.h - 
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
 *  $Id: FaslWriter.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_FASL_WRITER_
#define SCHEME_FASL_WRITER_

#include "scheme.h"
#include "Fasl.h"

namespace scheme {

class FaslWriter EXTEND_GC
{
public:
    FaslWriter(BinaryOutputPort* outputPort);

    void put(Object obj);
private:
    bool isInteresting(Object obj);
    void scanSharedObjects(Object obj);
    void putList(Object list);
    void emitU8(uint8_t value);
    void emitU16(uint16_t value);
    void emitU32(uint32_t value);
    void emitU64(uint64_t value);
    void emitString(const ucs4string& string);
    void emitShortAsciiString(const ucs4string& string);
    void putDatum(Object obj);

    EqHashTable* sharedObjects_;
    ObjectVector sharedObjectVector_;
    BinaryOutputPort* outputPort_;

    int uid_;
};

}; // namespace scheme

#endif // SCHEME_FASL_WRITER_

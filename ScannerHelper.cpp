/*
 * ScannerHelper.cpp - 
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
 *  $Id: ScannerHelper.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <errno.h>
#include "Object.h"
#include "Object-inl.h"
#include "Arithmetic.h"
#include "ScannerHelper.h"

#ifdef _WIN32
    #define strtoll _strtoi64
#endif

using namespace scheme;

ucs4char ScannerHelper::hexStringToUCS4Char(ucs4char* start, ucs4char* end)
{
    ucs4char ret = 0;
    for (ucs4char* ch = start; ch != end; ch++) {
        const ucs4char hexChar = *ch;
        if (isdigit(hexChar)) {
            ret = (ret << 4) | (hexChar - '0');
        } else if ('a' <= hexChar && hexChar <= 'f') {
            ret = (ret << 4) | (hexChar - 'a' + 10);
        } else if ('A' <= hexChar && hexChar <= 'F') {
            ret = (ret << 4) | (hexChar - 'A' + 10);
        } else {
            MOSH_ASSERT(false); // not reached
        }
    }
    return ret;
}

int ScannerHelper::num16StringToInt(ucs4char* start, ucs4char* end)
{
    MOSH_ASSERT(end - start > 2);
    ucs4char* p = start + 2; // skip "#x" prefix
    char* buf = new(GC) char[end - p];
    for (int i = 0; i < end - p; i++) {
        buf[i] = p[i];
    }
    errno = 0;
    int64_t ret = strtoll(buf, NULL, 16);
    if ((errno == ERANGE && (ret == LONG_MAX || ret == LONG_MIN))
        || (errno != 0 && ret == 0)) {
        fprintf(stderr, "error num-16 buf=<%s>", buf);
        exit(-1);
    } else {
        return (int)ret;
    }

}

int ScannerHelper::num10StringToInt(ucs4char* start, ucs4char* end)
{
    char* buf = new(GC) char[end - start];
    for (int i = 0; i < end - start; i++) {
        buf[i] = start[i];
    }
    errno = 0;
    int64_t ret = strtoll(buf, NULL, 10);
    if ((errno == ERANGE && (ret == LONG_MAX || ret == LONG_MIN))
        || (errno != 0 && ret == 0)) {
        fprintf(stderr, "error num-10 buf=<%s>", buf);
        exit(-1);
    } else {
        return (int)ret;
    }
}

Object ScannerHelper::applyExactness(int exactness, Object num)
{
    switch(exactness)
    {
    case 0:
        return num;
    case 1:
        return Arithmetic::exact(num);
    case -1:
        return Arithmetic::inexact(num);
    default:
        MOSH_ASSERT(false);
    }
    return Object::Undef;
}

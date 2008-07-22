/*
 * GenericMap.h
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
 *  $Id: EqHashTable.h 251 2008-07-22 05:12:58Z higepon $
 */

#ifndef __SCHEME_GENERIC_MAP__
#define __SCHEME_GENERIC_MAP__

#include "config.h"

#if HAVE_EXT_HASHES && HAVE_TR1_HASHES
#include <tr1/unordered_map>
#include <ext/hash_map>
struct generic_hash_func
{
    size_t operator()(scheme::Object const & s) const
    {
            return std::tr1::hash<word>()(s.val);
    }
};

struct generic_equal_to
{
    bool operator()(scheme::Object const& o1, scheme::Object const& o2) const
    {
        return o1.val == o2.val;
    }
};


typedef __gnu_cxx::hash_map<scheme::Object,
                            scheme::Object,
                            generic_hash_func,
                            generic_equal_to,
                            gc_allocator<std::pair<const scheme::Object, scheme::Object> > > GenericMap;
#else
#error todo_ext_hash_map_not_found
#endif

#endif // __SCHEME_GENERIC_MAP__

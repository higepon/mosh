/*
 * CProcedure.h - <c-proc>
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
 *  $Id$
 */

#ifndef SCHEME_EQ_HASH_TABLE_
#define SCHEME_EQ_HASH_TABLE_

#include "HashTable.h"

#if HAVE_EXT_HASHES
#include <ext/hash_map>
struct hash_func
{
    size_t operator()(scheme::Object const & s) const
    {
        return static_cast<intptr_t>(s.val);
    }
};
typedef __gnu_cxx::hash_map<scheme::Object,
                            scheme::Object,
                            hash_func,
                            std::equal_to<scheme::Object>,
                            gc_allocator<std::pair<const scheme::Object, scheme::Object> > > ObjectMap;

#elif HAVE_TR1_HASHES
#ifdef _WIN32
#include <unordered_map>
#include "gc_allocator.h"
#else
#include <tr1/unordered_map>
#endif
struct hash_func
{
    size_t operator()(scheme::Object const & s) const
    {
        return std::tr1::hash<intptr_t>()(s.val);
    }
};

typedef std::tr1::unordered_map<scheme::Object,
                                scheme::Object,
                                hash_func, std::equal_to<scheme::Object>,
                                gc_allocator<std::pair<const scheme::Object, scheme::Object> > > ObjectMap;
#else // std::map
typedef std::map<scheme::Object,
                 scheme::Object,
                 std::less<scheme::Object>,
                 gc_allocator<std::pair<const scheme::Object, scheme::Object> > > ObjectMap;
#endif


namespace scheme {

class EqHashTable : public HashTable
{

public:
    EqHashTable();
    virtual ~EqHashTable();

    size_t size() const;
    bool containsP(Object key);
    Object ref(Object key, Object defaultVal);
    void set(Object key, Object val);
    void deleteD(Object key);
    Object hashFunction() const;
    Object equivalenceFunction() const;
    void insert(Object key, Object val);
    void eraseAllExcept(Object key);
    Object swap();
    void clearD();
    Object keys();
    Object copy(bool mutableP);
    bool mutableP() const;
    void setMutableP(bool mutableP);

private:
    void setTable(ObjectMap i);
    ObjectMap getTable() const;

private:
    ObjectMap table_;
    bool mutable_;
};

} // namespace scheme

#endif // SCHEME_EQ_HASH_TABLE_

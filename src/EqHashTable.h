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
#include <unordered_map>

struct hash_func
{
    size_t operator()(scheme::Object const & s) const
    {
        return std::hash<intptr_t>()(s.val);
    }
};

typedef std::unordered_map<scheme::Object,
                                scheme::Object,
                                hash_func, std::equal_to<scheme::Object>,
                                gc_allocator<std::pair<const scheme::Object, scheme::Object> > > ObjectMap;
//#endif

namespace scheme {

class EqHashTable : public HashTable
{

public:
    EqHashTable();
    ~EqHashTable() override;

    size_t size() const override;
    bool containsP(Object key) override;
    Object ref(Object key, Object defaultVal) override;
    void set(Object key, Object val) override;
    void deleteD(Object key) override;
    Object hashFunction() const override;
    Object equivalenceFunction() const override;
    void insert(Object key, Object val);
    void eraseAllExcept(Object key);
    Object swap();
    void clearD() override;
    Object keys() override;
    Object copy(bool mutableP) override;
    bool mutableP() const override;
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

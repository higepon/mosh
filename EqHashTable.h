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

#ifndef __SCHEME_EQ_HASH_TABLE__
#define __SCHEME_EQ_HASH_TABLE__

#define USE_GNU_CXX_HASH_MAP

#if defined(HAVE_EXT_HASHES) && defined(HAVE_TR1_HASHES)
#include <tr1/unordered_map>
#include <ext/hash_map>
struct hash_func
{
    size_t operator()(scheme::Object const & s) const
    {
            return std::tr1::hash<word>()(s.val);
    }
};
typedef __gnu_cxx::hash_map<scheme::Object,
                            scheme::Object,
                            hash_func,
                            std::equal_to<scheme::Object>,
                            gc_allocator<std::pair<const scheme::Object, scheme::Object> > > ObjectMap;

// #elif defined(HAVE_TR1) && defined(USE_TR1_UNORDERED_MAP)

// #include <tr1/unordered_map>
// struct hash_func
// {
//     size_t operator()(scheme::Object const & s) const
//     {
//         return std::tr1::hash<word>()(s.val);
//     }
// };

// typedef std::tr1::unordered_map<scheme::Object,
//                                 scheme::Object,
//                                 hash_func, std::equal_to<scheme::Object>,
//                                 gc_allocator<std::pair<const scheme::Object, scheme::Object> > > ObjectMap;
#else // std::map
typedef std::map<scheme::Object,
                 scheme::Object,
                 std::less<scheme::Object>,
                 gc_allocator<std::pair<const scheme::Object, scheme::Object> > > ObjectMap;
#endif

namespace scheme {

class EqHashTable EXTEND_GC
{

public:
    EqHashTable() :
#ifdef HAVE_TR1_HASHES
        table_(ObjectMap(200)){}
#else
        table_(ObjectMap()){}
#endif

    // todo
    //    EqHashObjectMap(int capacity) {}

    Object ref(Object key, Object defaultVal)
    {
        ObjectMap::iterator p = table_.find(key);
        if (p == table_.end()) {
            return defaultVal;
        } else {
            return p->second;
        }
    }

    void set(Object key, Object val)
    {
        table_[key] = val;
    }

    void insert(Object key, Object val)
    {
        table_.insert(std::pair<const Object, Object>(key, val));
    }

    void eraseAllExcept(Object key)
    {
        const Object o = ref(key, Object::False);
        table_.clear();
        set(key, o);
    }

    Object swap()
    {
        // swap (key, value)
        Object ht = Object::makeEqHashTable();
        for (ObjectMap::const_iterator it = table_.begin(); it != table_.end(); ++it) {
            ht.toEqHashTable()->set(it->second, it->first);
        }
        return ht;
    }

    Object keys()
    {
        Object ht = Object::makeEqHashTable();
        Object v = Object::makeVector(table_.size());
        int i = 0;
        for (ObjectMap::const_iterator it = table_.begin(); it != table_.end(); ++it, i++) {
            v.toVector()->set(i, it->first);
        }
        return v;
    }

    Object copy()
    {
        Object ret = Object::makeEqHashTable();
        ret.toEqHashTable()->setTable(getTable());
        return ret;
    }

    ~EqHashTable() {} // not virtual

private:
    inline void setTable(ObjectMap i) { table_ = i; }
    inline ObjectMap getTable() const { return table_; }

protected:
    ObjectMap table_;
};

}; // namespace scheme

#endif // __SCHEME_EQ_HASH_TABLE__

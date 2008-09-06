/*
 * EqHashTable.cpp - 
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
 *  $Id: EqHashTable.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "EqHashTable.h"
#include "Vector.h"
#include "freeproc.h"

using namespace scheme;

extern Object eqHashEx(int argc, const Object* argv);

EqHashTable::EqHashTable() :
#if HAVE_EXT_HASHES && HAVE_TR1_HASHES
    table_(ObjectMap(200))
#else
    table_(ObjectMap())
#endif
    , mutable_(true)
{
}

EqHashTable::~EqHashTable()
{
}

size_t EqHashTable::size() const
{
    return table_.size();
}

bool EqHashTable::containsP(Object key)
{
    ObjectMap::iterator p = table_.find(key);
    return p != table_.end();
}

Object EqHashTable::ref(Object key, Object defaultVal)
{
    ObjectMap::iterator p = table_.find(key);
    if (p == table_.end()) {
        return defaultVal;
    } else {
        return p->second;
    }
}

void EqHashTable::set(Object key, Object val)
{
    table_[key] = val;
}

void EqHashTable::deleteD(Object key)
{
    table_.erase(key);
}


Object EqHashTable::hashFunction() const
{
    return Object::False;
}

Object EqHashTable::equivalenceFunction() const
{
    return Object::makeCProcedure(eqPEx);
}

void EqHashTable::insert(Object key, Object val)
{
    table_.insert(std::pair<const Object, Object>(key, val));
}

void EqHashTable::eraseAllExcept(Object key)
{
    const Object o = ref(key, Object::False);
    table_.clear();
    set(key, o);
}

Object EqHashTable::swap()
{
    // swap (key, value)
    Object ht = Object::makeEqHashTable();
    for (ObjectMap::const_iterator it = table_.begin(); it != table_.end(); ++it) {
        ht.toEqHashTable()->set(it->second, it->first);
    }
    return ht;
}

void EqHashTable::clearD()
{
    table_.clear();
}

Object EqHashTable::keys()
{
    Object v = Object::makeVector(table_.size());
    int i = 0;
    for (ObjectMap::const_iterator it = table_.begin(); it != table_.end(); ++it, i++) {
        v.toVector()->set(i, it->first);
    }
    return v;
}

Object EqHashTable::copy(bool mutableP)
{
    Object ret = Object::makeEqHashTable();
    ret.toEqHashTable()->setTable(getTable());
    ret.toEqHashTable()->setMutableP(mutableP);
    return ret;
}

bool EqHashTable::mutableP() const
{
    return mutable_;
}

void EqHashTable::setMutableP(bool mutableP)
{
    mutable_ = mutableP;
}

void EqHashTable::setTable(ObjectMap i)
{
    table_ = i;
}

ObjectMap EqHashTable::getTable() const
{
    return table_;
}

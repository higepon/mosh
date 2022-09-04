/*
 * GenericHashTable.cpp - GenericHashTable
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
 *  $Id: CompilerProcedures.cpp 213 2008-07-10 15:03:40Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "GenericHashTable.h"


#include <utility>

#include "Vector.h"
#include "VM.h"
#include "Bignum.h"

using namespace scheme;

Object genericHashFunction;
Object genericEquivalenceFunction;
static VM* theVM = nullptr; // todo multi thread

int callHashFunction(Object hashFunction, Object key)
{
    const Object hashValue =  theVM->callClosure1(hashFunction, key);
    MOSH_ASSERT(hashValue.isFixnum() || hashValue.isBignum());
    if (hashValue.isFixnum()) {
        return hashValue.toFixnum();
    } else if (hashValue.isBignum()) {
        return (int)hashValue.toBignum()->toS64();
    }
    return -1;
}

bool callEquivalenceFunction(Object equivalenceFunction, Object o1, Object o2)
{
    return !theVM->callClosure2(equivalenceFunction, o1, o2).isFalse();
}

GenericHashTable::GenericHashTable(VM* vm, Object hashFunction, Object equivalenceFunction) :
    vm_(vm),
    hashFunction_(hashFunction),
    equivalenceFunction_(equivalenceFunction),
    mutable_(true)
{
}

GenericHashTable::~GenericHashTable()
= default;

void GenericHashTable::prepareFunctions()
{
    // TODO: should be thread safe.
    genericHashFunction = hashFunction_;
    genericEquivalenceFunction = equivalenceFunction();
    theVM = vm_;
}

void GenericHashTable::setMap(GenericMap map)
{
    map_ = std::move(map);
}

void GenericHashTable::setMutableP(bool mutableP)
{
    mutable_ = mutableP;
}

size_t GenericHashTable::size() const
{
    return map_.size();
}

Object GenericHashTable::ref(Object key, Object defaultValue)
{
    prepareFunctions();
    GenericMap::iterator p = map_.find(key);
    if (p == map_.end()) {
        return defaultValue;
    } else {
        return p->second;
    }
}

void GenericHashTable::set(Object key, Object value)
{
    prepareFunctions();
    map_[key] = value;
}

void GenericHashTable::clearD()
{
    prepareFunctions();
    map_.clear();
}

void GenericHashTable::deleteD(Object key)
{
    prepareFunctions();
    map_.erase(key);
}

bool GenericHashTable::containsP(Object key)
{
    prepareFunctions();
    return map_.find(key) != map_.end();
}

Object GenericHashTable::copy(bool mutableP)
{
    Object ret = Object::makeGenericHashTable(theVM,
                                              hashFunction_,
                                              equivalenceFunction());
    GenericHashTable* genericHashTable = ret.toGenericHashTable();
    genericHashTable->setMap(map_);
    genericHashTable->setMutableP(mutableP);
    return ret;
}

Object GenericHashTable::keys()
{
    prepareFunctions();
    Object v = Object::makeVector(map_.size());
    int i = 0;
    for (GenericMap::const_iterator it = map_.begin(); it != map_.end(); ++it, i++) {
        v.toVector()->set(i, it->first);
    }
    return v;
}


Object GenericHashTable::hashFunction() const
{
    return hashFunction_;
}

Object GenericHashTable::equivalenceFunction() const
{
    return equivalenceFunction_;
}

bool GenericHashTable::mutableP() const
{
    return mutable_;
}

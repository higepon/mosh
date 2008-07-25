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

#include "GenericHashTable.h"
#include "VM.h"

using namespace scheme;

Object genericHashFunction;
Object genericEquivalenceFunction;
extern scheme::VM* theVM;

int callHashFunction(Object hashFunction, Object key)
{
    int r = theVM->callClosure(hashFunction, key).toInt();
    return r;
}

bool callEquivalenceFunction(Object equivalenceFunction, Object o1, Object o2)
{
    return !theVM->callClosure2(equivalenceFunction, o1, o2).isFalse();
}

GenericHashTable::GenericHashTable(Object hashFunction, Object equivalenceFunction) :
    hashFunction_(hashFunction), equivalenceFunction_(equivalenceFunction)
{
}

GenericHashTable::~GenericHashTable()
{
}

void GenericHashTable::prepareFunctions()
{
    // TODO: should be thread safe.
    genericHashFunction = hashFunction();
    genericEquivalenceFunction = equivalenceFunction();
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

void GenericHashTable::deleteD(Object key)
{
    prepareFunctions();
    map_.erase(key);
}

bool GenericHashTable::contains(Object key)
{
    prepareFunctions();
    return map_.find(key) != map_.end();
}

Object GenericHashTable::copy()
{

}

Object GenericHashTable::keys()
{
    return Object::makeVector(1, Object::False);
}


Object GenericHashTable::hashFunction() const
{
    return hashFunction_;
}

Object GenericHashTable::equivalenceFunction() const
{
    return equivalenceFunction_;
}

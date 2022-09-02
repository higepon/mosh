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
 *  $Id: EqHashTable.h 251 2008-07-22 05:12:58Z higepon $
 */

#ifndef SCHEME_GENERIC_HASH_TABLE_
#define SCHEME_GENERIC_HASH_TABLE_

#include "scheme.h"
#include "HashTable.h"
#include "GenericMap.h"

namespace scheme {

class GenericHashTable : public HashTable
{
public:
    GenericHashTable(VM* theVM, Object hashFunction, Object equivalenceFunction);
    ~GenericHashTable() override;

    size_t size() const override;
    Object ref(Object key, Object defaultValue) override;
    void set(Object key, Object values) override;
    void clearD() override;
    void deleteD(Object key) override;
    bool containsP(Object key) override;
    Object copy(bool mutableP) override;
    Object keys() override;
    Object hashFunction() const override;
    Object equivalenceFunction() const override;
    bool mutableP() const override;

    void prepareFunctions();
    void setMap(GenericMap map);
    void setMutableP(bool mutableP);
private:
    VM* vm_;
    GenericMap map_;
    Object hashFunction_;
    Object equivalenceFunction_;
    bool mutable_;
};

} // namespace scheme

#endif // SCHEME_GENERIC_HASH_TABLE_

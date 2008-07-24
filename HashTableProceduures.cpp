/*
 * HashTableProceduures.cpp - Procedures written in C++ for compiler.
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
 *  $Id: HashTableProceduures.cpp 213 2008-07-10 15:03:40Z higepon $
 */

#include "HashTableProceduures.h"
#include "VM.h"
using namespace scheme;

extern scheme::VM* theVM;

Object scheme::hashtableSizeEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "hashtable-size");
    const Object ht = argv[0];
    if (ht.isHashTable()) {
        return Object::makeInt(ht.toHashTable()->size());
    } else {
        VM_RAISE1("hashtable-size hashtable required, but got ~a\n", ht);
        return Object::Undef;
    }
}

Object scheme::hashtablePEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "hashtable?");
    return Object::makeBool(argv[0].isHashTable());
}


Object scheme::stringHashEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "string-hash");
    const Object str = argv[0];
    if (!str.isString()) {
        VM_RAISE1("string-hash string required, but got ~a\n", str);
    }

    const ucs4string& s = str.toString()->data();
    int hashValue = 0;
    for (ucs4string::const_iterator it = s.begin(); it != s.end(); ++it) {
        hashValue = (hashValue << 5) - hashValue + (unsigned char)(*it);
    }
    return Object::makeInt(hashValue);
}

Object scheme::makeHashTableEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "make-hash-table");
    Object hashFunction        = argv[0];
    Object equivalenceFunction = argv[1];
    if (hashFunction.isCallable() && equivalenceFunction.isCallable()) {
        return Object::makeGenericHashTable(hashFunction, equivalenceFunction);
    } else {
        VM_RAISE2("make-hash-table procedure required, but got ~a, ~a\n", hashFunction, equivalenceFunction);
        return Object::Undef;
    }
}

Object scheme::eqHashEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "eq-hash");
    return Object::makeInt(argv[0].val);
}

Object scheme::hashtableKeysEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "hash-table-keys");
    const Object ht = argv[0];
    if (!ht.isHashTable()) {
        VM_RAISE1("hashtable-keys hash-table required, but got ~a\n", ht);
    }
    return ht.toHashTable()->keys();
}
Object scheme::hashtableSetDEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "hash-table-set!");
    const Object ht = argv[0];
    if (!ht.isHashTable()) {
        VM_RAISE1("hashtable-set! hash-table required, but got ~a\n", ht);
    }

    const Object key = argv[1];
    const Object val = argv[2];
    ht.toHashTable()->set(key, val);
    return Object::Undef;
}

Object scheme::hashtableRefEx(int argc, const Object* argv)
{
    if (argc == 2 || argc == 3) {
        const Object ht = argv[0];
        if (!ht.isHashTable()) {
            VM_RAISE1("hashtable-ref hash-table required, but got ~a\n", ht);
        }
        const Object key = argv[1];
        const Object defaultVal = (argc == 3 ? argv[2] : Object::False);
        return ht.toHashTable()->ref(key, defaultVal);
    } else {
        VM_RAISE1("wrong number of arguments for hash-table-get (required 2 or 3, got ~d)\n", Object::makeInt(argc));
    }
    return Object::UnBound;
}

Object scheme::makeEqHashtableEx(int argc, const Object* argv)
{
    checkArgLength(0, argc, "make-eq-hashtable");
    return Object::makeEqHashTable();
}

Object scheme::eqHashtableCopyEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "eq-hashtable-copy");
    const Object ht = argv[0];
    if (!ht.isHashTable()) {
        VM_RAISE1("eq-hashtable required, but got ~an", ht);
    }
    return ht.toHashTable()->copy();
}

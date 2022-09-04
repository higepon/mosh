/*
 * HashTableProcedures.cpp - Procedures written in C++ for compiler.
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

#include <ctype.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Symbol.h"
#include "Vector.h"
#include "SString.h"
#include "HashTable.h"
#include "VM.h"
#include "HashTableProcedures.h"
#include "ProcedureMacro.h"
#include "Arithmetic.h"
#include "StringProcedures.h"

using namespace scheme;

extern scheme::VM* theVM;

int scheme::equalHash(Object obj)
{
    // borrowed from ypsilon scheme by Yoshikatsu Fujita
    if (obj.isPair()) {
        int hash1 = equalHash(obj.car());
        int hash2 = equalHash(obj.cdr());
        return (hash1 + hash2 * 64 - hash2);
    } else if (obj.isVector()) {
        int hash = 1;
        const Vector* const vec = obj.toVector();
        const int length = vec->length();
        for (int i = 0; i < length; i++) {
            hash = hash * 32 - hash + equalHash(vec->ref(i));
        }
        return hash;
    } else if (obj.isString()) {
        return stringHash(obj.toString()->data());
    } else if (obj.isSymbol()) {
        return symbolHash(obj.toSymbol());
    } else {
        return obj.val;
    }
}

int scheme::stringHash(const ucs4string& str)
{
    int hashValue = 0;
    for (int it : str) {
        hashValue = (hashValue << 5) - hashValue + (unsigned char)it;
    }
    return hashValue;
}

int scheme::stringCiHash(const ucs4string& str)
{
    int hashValue = 0;
    for (int it : str) {
        hashValue = (hashValue << 5) - hashValue + (unsigned char)toupper(it);
    }
    return hashValue;
}

int scheme::symbolHash(Symbol* symbol)
{
    // we can use pointer as hash, because symbol is interned.
    return reinterpret_cast<intptr_t>(symbol);
}


Object scheme::hashtableDeleteDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-delete!");
    checkArgumentLength(2);

    argumentAsHashTable(0, hashtable);
    const Object key = argv[1];

    if (hashtable->mutableP()) {
        hashtable->deleteD(key);
    } else {
        callAssertionViolationAfter(theVM, Symbol::intern(procedureName), UC("can't delete an immutable hashtable."), L1(argv[0]));
        return Object::Undef;
    }
    return Object::Undef;
}

Object scheme::hashtableContainsPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-contains?");
    checkArgumentLength(2);

    argumentAsHashTable(0, hashtable);
    const Object key = argv[1];

    return Object::makeBool(hashtable->containsP(key));
}


Object scheme::hashtableSizeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-size");
    checkArgumentLength(1);

    argumentAsHashTable(0, hashtable);
    return Object::makeFixnum(hashtable->size());
}

Object scheme::hashtablePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isHashTable());
}

Object scheme::eqvHashEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eqv-hash");
    checkArgumentLength(1);
    const Object obj = argv[0];
    if (obj.isNumber()) {
        return Object::makeFixnum(stringTosymbol(Arithmetic::numberToString(obj, 10)).val);
    } else {
        return Object::makeFixnum(obj.val);
    }
}

Object scheme::stringHashEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string-hash");
    checkArgumentLength(1);

    argumentAsString(0, text);
    return Object::makeFixnum(stringHash(text->data()));
}

Object scheme::symbolHashEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("symbol-hash");
    checkArgumentLength(1);

    argumentAsSymbol(0, symbol);

    // we can use pointer as hash, because symbol is interned.
    return Object::makeFixnum(symbolHash(symbol));
}

Object scheme::stringCiHashEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string-ci-hash");
    checkArgumentLength(1);

    argumentAsString(0, text);
    return Object::makeFixnum(stringCiHash(text->data()));
}


Object scheme::equalHashEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("equal-hash");
    checkArgumentLength(1);
    return Object::makeFixnum(equalHash(argv[0]));
}


Object scheme::makeHashtableEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-hashtable");
    checkArgumentLengthBetween(2, 3);

    argumentCheckProcedure(0, hashFunction);
    argumentCheckProcedure(1, equivalenceFunction);
    return Object::makeGenericHashTable(theVM, hashFunction, equivalenceFunction);
}

Object scheme::hashtableKeysEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hash-table-keys");
    checkArgumentLength(1);

    argumentAsHashTable(0, hashtable);
    return hashtable->keys();
}
Object scheme::hashtableSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hash-table-set!");
    checkArgumentLength(3);

    argumentAsHashTable(0, hashtable);

    const Object key = argv[1];
    const Object val = argv[2];

    if (hashtable->mutableP()) {
        hashtable->set(key, val);
    } else {
        callAssertionViolationAfter(theVM, Symbol::intern(procedureName), UC("can't hashtable-set! to immutable hashtable."), Pair::list1(argv[0]));
    }
    return Object::Undef;
}

Object scheme::hashtableRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-ref");
    checkArgumentLengthBetween(2, 3);

    argumentAsHashTable(0, hashtable);
    const Object key = argv[1];
    const Object defaultVal = (argc == 3 ? argv[2] : Object::False);

    return hashtable->ref(key, defaultVal);
}

Object scheme::makeEqHashtableEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-eq-hashtable");
    checkArgumentLengthBetween(0, 1);
    return Object::makeEqHashTable();
}

Object scheme::makeEqvHashtableEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-eqv-hashtable");
    checkArgumentLengthBetween(0, 1);
    return Object::makeEqvHashTable(theVM);
}

Object scheme::eqHashtableCopyEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eq-hashtable-copy");
    checkArgumentLength(1);

    argumentAsHashTable(0, hashtable);
    return hashtable->copy(true);
}

Object scheme::hashtableCopyEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-copy");
    checkArgumentLengthBetween(1, 2);
    bool mutableP = false;
    if (argc == 2 && !argv[1].isFalse()) {
        mutableP = true;
    }

    argumentAsHashTable(0, hashtable);
    return hashtable->copy(mutableP);
}

Object scheme::hashtableMutablePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-mutable?");
    checkArgumentLength(1);

    argumentAsHashTable(0, hashtable);
    return Object::makeBool(hashtable->mutableP());
}

Object scheme::hashtableClearDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-clear!");
    checkArgumentLengthBetween(1, 2);

    // we now ignore "k" argument.
    argumentAsHashTable(0, hashtable);

    if (hashtable->mutableP()) {
        hashtable->clearD();
        return Object::Undef;
    } else {
        callAssertionViolationAfter(theVM, procedureName, UC("can't clear an immutable hashtable."), Pair::list1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::hashtableEquivalenceFunctionEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-equivalence-function");
    checkArgumentLength(1);

    argumentAsHashTable(0, hashtable);
    return hashtable->equivalenceFunction();
}

Object scheme::hashtableHashFunctionEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-hash-function");
    checkArgumentLength(1);

    argumentAsHashTable(0, hashtable);
    return hashtable->hashFunction();
}

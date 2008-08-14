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
#include "ViolationProcedures.h"
#include "VM.h"
using namespace scheme;

extern scheme::VM* theVM;

//     if (depth > EQUAL_HASH_DEPTH_LIMIT) return 1;
//     if (PAIRP(obj)) {
//         uint32_t hash1 = equal_hash2(CAR(obj), bound, depth + 1);
//         uint32_t hash2 = equal_hash2(CDR(obj), bound, depth + 1);
//         return (hash1 + hash2 * 64 - hash2) % bound;
//     }
//     if (VECTORP(obj)) {
//         scm_vector_t vector = (scm_vector_t)obj;
//         int n = HDR_VECTOR_COUNT(vector->hdr);
//         scm_obj_t* elts = vector->elts;
//         uint32_t hash = 1;
//         for (int i = 0; i < n; i++) {
//             hash = hash * 32 - hash + equal_hash2(elts[i], bound, depth + 1);
//         }
//         return hash % bound;
//     }
//     if (SYMBOLP(obj)) {
//         return string_hash(((scm_symbol_t)obj)->name, bound);
//     }
//     if (STRINGP(obj)) {
//         return (string_hash(obj, bound) * 3) % bound;
//     }
//     if (number_pred(obj)) return n_hash(obj, bound);
//     if (CELLP(obj)) return HDR_TC(HDR(obj)) % bound;
//     return 1;
//     const int taga = tag();
//     const int tagb = o.tag();
//     if (taga && tagb) {
//         return eq(o);
//     } else if (taga && 0 == tagb) {
//         return Object::False;
//     } else if (0 == taga && tagb) {
//         return Object::False;
//     } else if (isPair() && o.isPair()) {
//         RETURN_BOOL(car().equal(o.car()) != Object::False &&
//                     cdr().equal(o.cdr()) != Object::False);
//     } else if (isHeapObject() && o.isHeapObject()) {
//         if (isString() && o.isString()) {
//             RETURN_BOOL(toString()->data() == o.toString()->data());
//         } else if (isSymbol() && o.isSymbol()) {
//             return eq(o);
//         } else if (isVector() && o.isVector()) {
//             Vector* av = toVector();
//             Vector* bv = o.toVector();
//             const int aLength = av->length();
//             const int bLength = bv->length();
//             if (aLength == bLength) {
//                 for (int i = 0; i < aLength; i++) {
//                     if (av->ref(i).equal(bv->ref(i)).isFalse()) return Object::False;
//                 }
//                 return Object::True;
//             } else {
//                 return Object::False;
//             }
//         } else if (isRegexp() && o.isRegexp()) {
//             RETURN_BOOL(toRegexp()->pattern() == o.toRegexp()->pattern());
//         } else if (isCProcedure() && o.isCProcedure()) {
//             RETURN_BOOL(toCProcedure()->proc == o.toCProcedure()->proc);
//         }
// // todo
// //         } else if (isPointer() && o.isPointer()) {
// //             // address of instruction label is pointer
// //             RETURN_BOOL(o.val == val);
// //         }

//     }

//     // todo
//     return Object::False;


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
    for (ucs4string::const_iterator it = str.begin(); it != str.end(); ++it) {
        hashValue = (hashValue << 5) - hashValue + (unsigned char)(*it);
    }
    return hashValue;
}

int scheme::stringCiHash(const ucs4string& str)
{
    int hashValue = 0;
    for (ucs4string::const_iterator it = str.begin(); it != str.end(); ++it) {
        hashValue = (hashValue << 5) - hashValue + (unsigned char)toupper(*it);
    }
    return hashValue;
}

int scheme::symbolHash(Symbol* symbol)
{
    // we can use pointer as hash, because symbol is interned.
    return reinterpret_cast<int>(symbol);
}


Object scheme::hashtableDeleteDEx(int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-delete!");
    checkArgLength(2, argc, "hashtable-delete!");
    const Object hashtable = argv[0];
    const Object key = argv[1];
    if (hashtable.isHashTable()) {
        HashTable* const table = hashtable.toHashTable();
        if (table->mutableP()) {
            table->deleteD(key);
        } else {
            callAssertionViolationAfter(Symbol::intern(procedureName), "can't delete an immutable hashtable.", Object::Nil);
        }
    } else {
        VM_RAISE1("hashtable-delete! hashtable required, but got ~a\n", hashtable);
    }
    return Object::Undef;
}

Object scheme::hashtableContainsPEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "hashtable-contains?");
    const Object ht = argv[0];
    const Object key = argv[1];
    if (ht.isHashTable()) {
        return Object::makeBool(ht.toHashTable()->containsP(key));
    } else {
        VM_RAISE1("hashtable-contains? hashtable required, but got ~a\n", ht);
    }
    return Object::Undef;
}


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
    return Object::makeInt(stringHash(str.toString()->data()));
}

Object scheme::symbolHashEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "symbol-hash");
    const Object symbol = argv[0];
    if (!symbol.isSymbol()) {
        VM_RAISE1("symbol-hash string required, but got ~a\n", symbol);
    }
    // we can use pointer as hash, because symbol is interned.
    return Object::makeInt(symbolHash(symbol.toSymbol()));
}

Object scheme::stringCiHashEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "string-ci-hash");
    const Object str = argv[0];
    if (!str.isString()) {
        VM_RAISE1("string-ci-hash string required, but got ~a\n", str);
    }
    return Object::makeInt(stringCiHash(str.toString()->data()));
}


Object scheme::equalHashEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "equal-hash");
    return Object::makeInt(equalHash(argv[0]));
}


Object scheme::makeHashtableEx(int argc, const Object* argv)
{
    checkArgLengthBetween(2, 3, argc, "make-hash-table");
    Object hashFunction        = argv[0];
    Object equivalenceFunction = argv[1];
    if (hashFunction.isProcedure() && equivalenceFunction.isProcedure()) {
        return Object::makeGenericHashTable(hashFunction, equivalenceFunction);
    } else {
        VM_RAISE2("make-hash-table procedure required, but got ~a, ~a\n", hashFunction, equivalenceFunction);
        return Object::Undef;
    }
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
    DeclareProcedureName("hash-table-set!");
    checkArgLength(3, argc, "hash-table-set!");
    const Object hashtable = argv[0];
    if (!hashtable.isHashTable()) {
        VM_RAISE1("hashtable-set! hash-table required, but got ~a\n", hashtable);
    }

    const Object key = argv[1];
    const Object val = argv[2];

    HashTable* const table = hashtable.toHashTable();
    if (table->mutableP()) {
        table->set(key, val);
    } else {
        callAssertionViolationAfter(Symbol::intern(procedureName), "can't hashtable-set! to immutable hashtable.", Object::Nil);
    }
    return Object::Undef;
}

Object scheme::hashtableRefEx(int argc, const Object* argv)
{
    checkArgLengthBetween(2, 3, argc, "hashtable-ref");
    const Object ht = argv[0];
    if (!ht.isHashTable()) {
        VM_RAISE1("hashtable-ref hashtable required, but got ~a\n", ht);
    }
    const Object key = argv[1];
    const Object defaultVal = (argc == 3 ? argv[2] : Object::False);
    return ht.toHashTable()->ref(key, defaultVal);
}

Object scheme::makeEqHashtableEx(int argc, const Object* argv)
{
    checkArgLengthBetween(0, 1, argc, "make-eq-hashtable");
    return Object::makeEqHashTable();
}

Object scheme::makeEqvHashtableEx(int argc, const Object* argv)
{
    checkArgLengthBetween(0, 1, argc, "make-eqv-hashtable");
    return Object::makeEqvHashTable();
}

Object scheme::eqHashtableCopyEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "eq-hashtable-copy");
    const Object ht = argv[0];
    if (!ht.isHashTable()) {
        VM_RAISE1("eq-hashtable required, but got ~an", ht);
    }
    return ht.toHashTable()->copy(true);
}

Object scheme::hashtableCopyEx(int argc, const Object* argv)
{
    checkArgLengthBetween(1, 2, argc, "hashtable-copy");
    bool mutableP = false;
    if (argc == 2 && !argv[1].isFalse()) {
        mutableP = true;
    }
    const Object hashtable = argv[0];
    if (hashtable.isHashTable()) {
        return hashtable.toHashTable()->copy(mutableP);
    } else {
        VM_RAISE1("hashtable-copy hashtable required, but got ~an", hashtable);
        return Object::Undef;
    }
}

Object scheme::hashtableMutablePEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "hashtable-mutable?");
    const Object hashtable = argv[0];
    if (hashtable.isHashTable()) {
        return Object::makeBool(hashtable.toHashTable()->mutableP());
    } else {
        VM_RAISE1("hashtable-mutable? hashtable required, but got ~an", hashtable);
        return Object::Undef;
    }
}

Object scheme::hashtableClearDEx(int argc, const Object* argv)
{
    DeclareProcedureName("hashtable-clear!");
    checkArgLengthBetween(1, 2, argc, "hashtable-clear!");
    // we now ignore "k" argument.
    const Object hashtable = argv[0];
    if (hashtable.isHashTable()) {
        HashTable* const table = hashtable.toHashTable();
        if (table->mutableP()) {
            table->clearD();
        } else {
            callAssertionViolationAfter(Symbol::intern(procedureName), "can't clear an immutable hashtable.", Object::Nil);
        }
    } else {
        VM_RAISE1("hashtable-mutable? hashtable required, but got ~an", hashtable);
    }
    return Object::Undef;
}

Object scheme::hashtableEquivalenceFunctionEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "hashtable-equivalence-function");
    const Object hashtable = argv[0];
    if (hashtable.isHashTable()) {
        return hashtable.toHashTable()->equivalenceFunction();
    } else {
        VM_RAISE1("hashtable-equivalence-function hashtable required, but got ~an", hashtable);
        return Object::Undef;
    }
}

Object scheme::hashtableHashFunctionEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "hashtable-hash-function");
    const Object hashtable = argv[0];
    if (hashtable.isHashTable()) {
        return hashtable.toHashTable()->hashFunction();
    } else {
        VM_RAISE1("hashtable-hash-function hashtable required, but got ~an", hashtable);
        return Object::Undef;
    }
}

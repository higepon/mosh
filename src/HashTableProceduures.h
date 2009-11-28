/*
 * HashTableProceduures.h - Procedures written in C++ for compiler.
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
 *  $Id: HashTableProceduures.h 210 2008-07-10 03:02:14Z higepon $
 */

#ifndef SCHEME_HASH_TABLE_PROCEDURES_
#define SCHEME_HASH_TABLE_PROCEDURES_

#include "scheme.h"

namespace scheme {

    Object hashtableEquivalenceFunctionEx(VM* theVM, int argc, const Object* argv);
    Object hashtableHashFunctionEx(VM* theVM, int argc, const Object* argv);
    Object hashtableKeysEx(VM* theVM, int argc, const Object* argv);
    Object hashtableClearDEx(VM* theVM, int argc, const Object* argv);
    Object hashtableCopyEx(VM* theVM, int argc, const Object* argv);
    Object hashtableMutablePEx(VM* theVM, int argc, const Object* argv);
    Object hashtableDeleteDEx(VM* theVM, int argc, const Object* argv);
    Object hashtableContainsPEx(VM* theVM, int argc, const Object* argv);
    Object hashtableSizeEx(VM* theVM, int argc, const Object* argv);
    Object hashtablePEx(VM* theVM, int argc, const Object* argv);
    Object makeHashtableEx(VM* theVM, int argc, const Object* argv);
    Object hashtableKeysEx(VM* theVM, int argc, const Object* argv);
    Object hashtableSetDEx(VM* theVM, int argc, const Object* argv);
    Object hashtableRefEx(VM* theVM, int argc, const Object* argv);
    Object eqHashtableCopyEx(VM* theVM, int argc, const Object* argv);
    Object makeEqHashtableEx(VM* theVM, int argc, const Object* argv);
    Object makeEqvHashtableEx(VM* theVM, int argc, const Object* argv);
    Object eqvHashEx(VM* theVM, int argc, const Object* argv);
    Object stringHashEx(VM* theVM, int argc, const Object* argv);
    Object stringCiHashEx(VM* theVM, int argc, const Object* argv);
    Object symbolHashEx(VM* theVM, int argc, const Object* argv);
    Object equalHashEx(VM* theVM, int argc, const Object* argv);

    int stringHash(const ucs4string& str);
    int stringCiHash(const ucs4string& str);
    int symbolHash(Symbol* symbol);
    int equalHash(Object o);
} // namespace scheme

#endif // SCHEME_HASH_TABLE_PROCEDURES_

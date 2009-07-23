/*
 * Equivalent.h - equivalent procedures.
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
 *  $Id: Equivalent.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_EQUIVALENT_
#define SCHEME_EQUIVALENT_

#include "Arithmetic.h"
#include "Codec.h"
#include "scheme.h"
#include "Box.h"

namespace scheme {
    bool equal(Object object1, Object object2, EqHashTable* visited);
    bool equal(Object object1, Object object2);
    bool fastEqual(Object object1, Object object2);

    inline bool eqv(Object o1, Object o2)
    {
        if (o1.isRecord()) {
            if (o2.isRecord()) {
                Record* const record1 = o1.toRecord();
                Record* const record2 = o2.toRecord();
                return record1->rtd() == record2->rtd();
            } else {
                return false;
            }
        }

        if (o1.isNumber()) {
            if (o2.isNumber()) {
                // See Flonum.h.
                if (o1.isFlonum() && o2.isFlonum()) {
                    return Flonum::eqv(o1.toFlonum(), o2.toFlonum());
                } else {
                    return Arithmetic::eq(o1, o2);
                }
            } else {
                return false;
            }
        }

        if (o1.isCodec()) {
            if (o2.isCodec()) {
                return o1.toCodec()->type() == o2.toCodec()->type();
            } else {
                return false;
            }
        }
        return o1.eq(o2);
    }

//  Copied from
//  Efficient Nondestructive Equality Checking for Trees and Graphs
//  Michael D. Adams and R. Kent Dybvig
//  ICFP 2008

class Equal EXTEND_GC
{
public:
    Equal();
    Object equalP(Object x, Object y);

private:
    const Object k0_;
    const Object kb_;

    Object find(Object b);
    Object unionFind(EqHashTable* ht, Object x, Object y);
    Object preP(Object x, Object y, Object k);
    Object eP(EqHashTable** pht, Object x, Object y, Object k);
    Object callUnionFind(EqHashTable** pht, Object x, Object y);
    Object slowP(EqHashTable** pht, Object x, Object y, Object k);
    Object fastP(EqHashTable** pht, Object x, Object y, Object k);
    Object interleaveP(Object x, Object y, Object k);
    Object interleaveEqualP(Object x, Object y);
    Object precheckInterleaveEqualP(Object x, Object y);
};


} // namespace scheme

#endif // SCHEME_EQUIVALENT_

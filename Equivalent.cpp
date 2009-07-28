/*
 * Equivalent.cpp - equivalent procedures.
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
 *  $Id: Equivalent.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Vector.h"
#include "SString.h"
#include "Regexp.h"
#include "Record.h"
#include "CProcedure.h"
#include "EqHashTable.h"
#include "ByteVector.h"
#include "Arithmetic.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Equivalent.h"
#include "Box.h"
#include "StringProcedures.h"
#include "Symbol.h"

using namespace scheme;

bool scheme::fastEqual(Object obj1, Object obj2)
{
    Object object1 = obj1;
    Object object2 = obj2;
entry:
    if (object1 == object2) {
        return true;
    }

    if (object1.isPair()) {
        if (object2.isPair()) {
            if (fastEqual(object1.car(), object2.car())) {
                object1 = object1.cdr();
                object2 = object2.cdr();
                goto entry;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    if (object1.isNumber()) {
        if (object2.isNumber()) {
            return Arithmetic::eq(object1, object2);
        } else {
            return false;
        }
    }

    if (object1.isVector()) {
        if (object2.isVector()) {
            Vector* const vector1 = object1.toVector();
            Vector* const vector2 = object2.toVector();
            if (vector1->length() == vector2->length()) {
                const int length = vector1->length();
                for (int i = 0; i < length; i++) {
                    if (!fastEqual(vector1->ref(i), vector2->ref(i))) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    if (object1.isString()) {
        if (object2.isString()) {
            String* const string1 = object1.toString();
            String* const string2 = object2.toString();
            return string1->data() == string2->data();
        } else {
            return false;
        }
    }

    if (object1.isRegexp()) {
        if (object2.isRegexp()) {
            Regexp* const regexp1 = object1.toRegexp();
            Regexp* const regexp2 = object2.toRegexp();
            return regexp1->pattern() == regexp2->pattern();
        } else {
            return false;
        }
    }

    if (object1.isCProcedure()) {
        if (object2.isCProcedure()) {
            CProcedure* const cprocedure1 = object1.toCProcedure();
            CProcedure* const cprocedure2 = object2.toCProcedure();
            return cprocedure1->proc == cprocedure2->proc;
        } else {
            return false;
        }
    }

    if (object1.isByteVector()) {
        if (object2.isByteVector()) {
            ByteVector* const bv1 = object1.toByteVector();
            ByteVector* const bv2 = object2.toByteVector();
            return bv1->equal(bv2);
        } else {
            return false;
        }
    }
    return eqv(object1, object2);
}

//  Efficient Nondestructive Equality Checking for Trees and Graphs
//  Michael D. Adams and R. Kent Dybvig
//  ICFP 2008

Equal::Equal() : k0_(Object::makeFixnum(400)),
                    kb_(Object::makeFixnum(-40))
{
}


//   (define (equal? x y)
//     (precheck/interleave-equal? x y))
bool Equal::equal(Object x, Object y)
{
    return precheckInterleaveEqualP(x, y);
}

// (define (find b)
//       (let ([n (box-content b)])
//         (if (box? n)
//             (let loop ([b b] [n n])
//               (let ([nn (box-content n)])
//                 (if (box? nn) (begin (set-box-content! b nn) (loop n nn)) n)))
//             b)))
Object Equal::find(Object b)
{
    MOSH_ASSERT(b.isBox());
    Object n = b.toBox()->value();
    if (n.isBox()) {
        for (;;) {
            Object nn = n.toBox()->value();
            if (nn.isBox()) {
                b.toBox()->set(nn);
                b = n;
                n = nn;
            } else {
                return n;
            }
        }
    } else {
        return b;
    }
}

//   (define (union-find ht x y)
//     (let ([bx (eq-hashtable-ref ht x #f)]
//           [by (eq-hashtable-ref ht y #f)])
//       (if (not bx)
//           (if (not by)
//               (let ([b (make-box 1)])
//                 (eq-hashtable-set! ht x b)
//                 (eq-hashtable-set! ht y b)
//                 #f)
//               (let ([ry (find by)]) (eq-hashtable-set! ht x ry) #f))
//           (if (not by)
//               (let ([rx (find bx)]) (eq-hashtable-set! ht y rx) #f)
//               (let ([rx (find bx)] [ry (find by)])
//                 (or (eq? rx ry)
//                     (let ([nx (box-content rx)] [ny (box-content ry)])
//                       (if (> nx ny)
//                           (begin
//                             (set-box-content! ry rx)
//                             (set-box-content! rx (+ nx ny))
//                             #f)
//                           (begin
//                             (set-box-content! rx ry)
//                             (set-box-content! ry (+ ny nx))
//                             #f)))))))))
Object Equal::unionFind(EqHashTable* ht, Object x, Object y)
{
    Object bx = ht->ref(x, Object::False);
    Object by = ht->ref(y, Object::False);
    if (bx.isFalse()) {
        if (by.isFalse()) {
            Object b = Object::makeBox(Object::makeFixnum(1));
            ht->set(x, b);
            ht->set(y, b);
            return Object::False;
        } else {
            Object ry = find(by);
            ht->set(x, ry);
            return Object::False;
        }
    } else if (by.isFalse()) {
        Object rx = find(bx);
        ht->set(y, rx);
        return Object::False;
    } else {
        Object rx = find(bx);
        Object ry = find(by);
        if (rx == ry) {
            return Object::True;
        }
        Object nx = rx.toBox()->value();
        Object ny = ry.toBox()->value();
        MOSH_ASSERT(nx.isFixnum());
        MOSH_ASSERT(ny.isFixnum());
        if (nx.toFixnum() > ny.toFixnum()) {
            ry.toBox()->set(rx);
            rx.toBox()->set(Object::makeFixnum(nx.toFixnum() + ny.toFixnum()));
            return Object::False;
        } else {
            rx.toBox()->set(ry);
            ry.toBox()->set(Object::makeFixnum(ny.toFixnum() + nx.toFixnum()));
            return Object::False;
        }
    }
}

// (define (pre? x y k)
//     (import UNSAFE)
//     (cond
//       [(eq? x y) k]
//       [(pair? x)
//        (and (pair? y)
//             (if (<= k 0)
//                 k
//                 (let ([k (pre? (car x) (car y) (- k 1))])
//                   (and k (pre? (cdr x) (cdr y) k)))))]
//       [(vector? x)
//        (and (vector? y)
//             (let ([n (vector-length x)])
//               (and (= (vector-length y) n)
//                    (let f ([i 0] [k k])
//                      (if (or (= i n) (<= k 0))
//                          k
//                          (let ([k (pre?
//                                     (vector-ref x i)
//                                     (vector-ref y i)
//                                     (- k 1))])
//                            (and k (f (+ i 1) k))))))))]
//       [(string? x) (and (string? y) (string=? x y) k)]
//       [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
//       [else (and (eqv? x y) k)]))
Object Equal::preP(Object x, Object y, Object k)
{
    if (x == y) {
        return k;
    }
    if (x.isPair()) {
        if (!y.isPair()) {
            return Object::False;
        }
        MOSH_ASSERT(k.isFixnum());
        if (k.toFixnum() <= 0) {
            return k;
        } else {
            Object k2 = preP(x.car(), y.car(), Object::makeFixnum(k.toFixnum() - 1));
            if (k2.isFalse()) {
                return Object::False;
            }
            return preP(x.cdr(), y.cdr(), k2);
        }
    }
    if (x.isVector()) {
        if (!y.isVector()) {
            return Object::False;
        }
        int n = x.toVector()->length();
        if (y.toVector()->length() != n) {
            return Object::False;
        }
        int i = 0;
        MOSH_ASSERT(k.isFixnum());
        for (;;) {
            if (i == n || k.toFixnum() <= 0) {
                return k;
            } else {
                Object k2 = preP(x.toVector()->ref(i),
                                 y.toVector()->ref(i),
                                 Object::makeFixnum(k.toFixnum() - 1));
                if (k2.isFalse()) {
                    return Object::False;
                }
                i++;
                k = k2;
            }
        }
    }
    if (x.isString()) {
        if (!y.isString()) {
            return Object::False;
        }
        if (x.toString()->data() == y.toString()->data()) {
            return k;
        } else {
            return Object::False;
        }
    }
    if (x.isByteVector()) {
        if (!y.isByteVector()) {
            return Object::False;
        }
        if (x.toByteVector()->equal(y.toByteVector())) {
            return k;
        } else {
            return Object::False;
        }
    }
    if (x.isRegexp()) {
        if (y.isRegexp()) {
            Regexp* const regexp1 = x.toRegexp();
            Regexp* const regexp2 = y.toRegexp();
            if (regexp1->pattern() == regexp2->pattern()) {
                return k;
            } else {
                return Object::False;
            }
        } else {
            return Object::False;
        }
    }

    if (x.isCProcedure()) {
        if (y.isCProcedure()) {
            CProcedure* const cprocedure1 = x.toCProcedure();
            CProcedure* const cprocedure2 = y.toCProcedure();
            if (cprocedure1->proc == cprocedure2->proc) {
                return k;
            } else {
                return Object::False;
            }
        } else {
            return Object::False;
        }
    }

    if (eqv(x, y)) {
        return k;
    } else {
        return Object::False;
    }
}


#ifdef _WIN32
#define random rand
#endif
//       (define (e? x y k)
//         (if (<= k 0)
//             (if (= k kb) (fast? x y (random (* 2 k0))) (slow? x y k))
//             (fast? x y k)))
Object Equal::eP(EqHashTable** pht, Object x, Object y, Object k)
{
    MOSH_ASSERT(k.isFixnum());
    if (k.toFixnum() <= 0) {
        if (k == kb_) {
            return fastP(pht, x, y, Object::makeFixnum(random() % (2 * k0_.toFixnum())));
        } else {
            return slowP(pht, x, y, k);
        }
    } else {
        return fastP(pht, x, y, k);
    }
}

//       (define (call-union-find x y)
//         (unless ht (set! ht (make-eq-hashtable)))
//         (union-find ht x y))
Object Equal::callUnionFind(EqHashTable** pht, Object x, Object y)
{
    if (*pht == NULL) {
        *pht = new EqHashTable;
    }
    return unionFind(*pht, x, y);
}

// (define (slow? x y k)
//         (cond
//           [(eq? x y) k]
//           [(pair? x)
//            (and (pair? y)
//                 (if (call-union-find x y)
//                     0
//                     (let ([k (e? (car x) (car y) (- k 1))])
//                       (and k (e? (cdr x) (cdr y) k)))))]
//           [(vector? x)
//            (and (vector? y)
//                 (let ([n (vector-length x)])
//                   (and (= (vector-length y) n)
//                        (if (call-union-find x y)
//                            0
//                            (let f ([i 0] [k (- k 1)])
//                              (if (= i n)
//                                  k
//                                  (let ([k (e? (vector-ref x i)
//                                               (vector-ref y i)
//                                               k)])
//                                    (and k (f (+ i 1) k)))))))))]
//           [(string? x) (and (string? y) (string=? x y) k)]
//           [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
//           [else (and (eqv? x y) k)]))
Object Equal::slowP(EqHashTable** pht, Object x, Object y, Object k)
{
    if (x == y) {
        return k;
    }

    if (x.isPair()) {
        if (!y.isPair()) {
            return Object::False;
        }
        if (!callUnionFind(pht, x, y).isFalse()) {
            return Object::makeFixnum(0);
        } else {
            MOSH_ASSERT(k.isFixnum());
            k = eP(pht, x.car(), y.car(), Object::makeFixnum(k.toFixnum() - 1));
            if (k.isFalse()) {
                return Object::False;
            }
            return eP(pht, x.cdr(), y.cdr(), k);
        }
    }
    if (x.isVector()) {
        if (!y.isVector()) {
            return Object::False;
        }
        int n = x.toVector()->length();
        if (y.toVector()->length() != n) {
            return Object::False;
        }
        if (!callUnionFind(pht, x, y).isFalse()) {
            return Object::makeFixnum(0);
        }
        int i = 0;
        MOSH_ASSERT(k.isFixnum());
        k = Object::makeFixnum(k.toFixnum() - 1);
        for (;;) {
            if (i == n) {
                return k;
            }
            k = eP(pht,
                   x.toVector()->ref(i),
                   y.toVector()->ref(i),
                   k);
            if (k.isFalse()) {
                return Object::False;
            }
            i++;
        }
    }
    if (x.isString()) {
        if (!y.isString()) {
            return Object::False;
        }
        if (x.toString()->data() == y.toString()->data()) {
            return k;
        } else {
            return Object::False;
        }
    }
    if (x.isByteVector()) {
        if (!y.isByteVector()) {
            return Object::False;
        }
        if (x.toByteVector()->equal(y.toByteVector())) {
            return k;
        } else {
            return Object::False;
        }
    }
    if (x.isRegexp()) {
        if (y.isRegexp()) {
            Regexp* const regexp1 = x.toRegexp();
            Regexp* const regexp2 = y.toRegexp();
            return Object::makeBool(regexp1->pattern() == regexp2->pattern());
        } else {
            return Object::False;
        }
    }

    if (x.isCProcedure()) {
        if (y.isCProcedure()) {
            CProcedure* const cprocedure1 = x.toCProcedure();
            CProcedure* const cprocedure2 = y.toCProcedure();
            return Object::makeBool(cprocedure1->proc == cprocedure2->proc);
        } else {
            return Object::False;
        }
    }
    if (eqv(x, y)) {
        return k;
    } else {
        return Object::False;
    }
}

// (define (fast? x y k)
//         (let ([k (- k 1)])
//           (cond
//             [(eq? x y) k]
//             [(pair? x)
//              (and (pair? y)
//                   (let ([k (e? (car x) (car y) k)])
//                     (and k (e? (cdr x) (cdr y) k))))]
//             [(vector? x)
//              (and (vector? y)
//                   (let ([n (vector-length x)])
//                     (and (= (vector-length y) n)
//                          (let f ([i 0] [k k])
//                            (if (= i n)
//                                k
//                                (let ([k (e? (vector-ref x i)
//                                             (vector-ref y i)
//                                             k)])
//                                  (and k (f (+ i 1) k))))))))]
//             [(string? x) (and (string? y) (string=? x y) k)]
//             [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
//             [else (and (eqv? x y) k)])))
Object Equal::fastP(EqHashTable** pht, Object x, Object y, Object k)
{
    if (x == y) {
        return k;
    }

    if (x.isPair()) {
        if (!y.isPair()) {
            return Object::False;
        }
        k = eP(pht, x.car(), y.car(), k);
        if (k.isFalse()) {
            return Object::False;
        }
        return eP(pht, x.cdr(), y.cdr(), k);
    }
    if (x.isVector()) {
        if (!y.isVector()) {
            return Object::False;
        }

        int n = x.toVector()->length();
        if (y.toVector()->length() != n) {
            return Object::False;
        }
        int i = 0;
        for (;;) {
            if (i == n) {
                return k;
            }
            k = eP(pht,
                   x.toVector()->ref(i),
                   y.toVector()->ref(i),
                   k);
            if (k.isFalse()) {
                return Object::False;
            }
            i++;
        }
    }
    if (x.isString()) {
        if (!y.isString()) {
            return Object::False;
        }
        if (x.toString()->data() == y.toString()->data()) {
            return k;
        } else {
            return Object::False;
        }
    }
    if (x.isByteVector()) {
        if (!y.isByteVector()) {
            return Object::False;
        }
        if (x.toByteVector()->equal(y.toByteVector())) {
            return k;
        } else {
            return Object::False;
        }
    }
    if (x.isRegexp()) {
        if (y.isRegexp()) {
            Regexp* const regexp1 = x.toRegexp();
            Regexp* const regexp2 = y.toRegexp();
            return Object::makeBool(regexp1->pattern() == regexp2->pattern());
        } else {
            return Object::False;
        }
    }

    if (x.isCProcedure()) {
        if (y.isCProcedure()) {
            CProcedure* const cprocedure1 = x.toCProcedure();
            CProcedure* const cprocedure2 = y.toCProcedure();
            return Object::makeBool(cprocedure1->proc == cprocedure2->proc);
        } else {
            return Object::False;
        }
    }

    if (eqv(x, y)) {
        return k;
    } else {
        return Object::False;
    }
}

//     (define (interleave? x y k)
//       (and (e? x y k) #t))
bool Equal::interleaveP(Object x, Object y, Object k)
{
    EqHashTable* ht = NULL;
    if (eP(&ht, x, y, k).isFalse()) {
        return false;
    }
    return true;
}

//   (define (interleave-equal? x y)
//     (interleave? x y k0))
bool Equal::interleaveEqualP(Object x, Object y)
{
    return interleaveP(x, y, k0_);
}

//   (define (precheck/interleave-equal? x y)
//     (let ([k (pre? x y k0)])
//       (and k (or (> k 0) (interleave? x y 0)))))
bool Equal::precheckInterleaveEqualP(Object x, Object y)
{
    Object k = preP(x, y, k0_);
    if (k.isFalse()) {
        return false;
    }

    MOSH_ASSERT(k.isFixnum());

    if (k.toFixnum() > 0) {
        return true;
    }
    return interleaveP(x, y, Object::makeFixnum(0));
}

/*
 * Pair.h - cons cell
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
 *  $Id$
 */

#ifndef __SCHEME_PAIR_H__
#define __SCHEME_PAIR_H__

namespace scheme {

struct Pair EXTEND_GC
{
    Pair(Object car, Object cdr, Object sourceInfo = Object::False) : car(car), cdr(cdr), sourceInfo(sourceInfo) {}
    static int length(Object obj)
    {
        if (!obj.isPair()) return 0;
        int len;
        for (len = 1; obj.cdr() != Object::Nil; len++, obj = obj.cdr())
        {
            if (!obj.isNil() && !obj.isPair()) {
                return -1;
            }
        }
        return len;
    }

    static bool isList(Object p)
    {
        Object obj = p;
        Object seen = obj;
        for (;;) {
            if (obj.isNil()) return true;
            if (!obj.isPair()) return false; // dot pair
            obj = obj.cdr();
            if (obj.isNil()) return true;
            if (!obj.isPair()) return false; // dot pair
            obj = obj.cdr();
            seen = seen.cdr();
            if (obj == seen) return false; // circular
        }
        return false;
    }

    // append!
    static Object appendD(Object list1, Object list2)
    {
        MOSH_ASSERT(list1.isNil() || list1.isPair());
        MOSH_ASSERT(list2.isNil() || list2.isPair());
        if (list1.isNil()) {
            return list2;
        } else {
            const Object last = getLastPair(list1);
            last.cdr() = list2;
            return list1;
        }
    }


    static Object cloneList(Object list1)
    {
        MOSH_ASSERT(list1.isNil() || list1.isPair());
        return reverse(reverse(list1));
    }

    static Object reverse(Object list1)
    {
        MOSH_ASSERT(list1.isNil() || list1.isPair());
        Object ret = Object::Nil;
        for (Object p = list1; p.isPair(); p = p.cdr()) {
            ret = Object::cons(p.car(), ret);
        }
        return ret;
    }

    // append o (list or obj) to l.
    // if l is not list return o.
    // allocate new cons sell.
    static Object append2(Object l, Object o)
    {
        if (!l.isPair()) return o;
        Object start = Object::Nil;
        Object last  = Object::Nil;
        for (Object p = l; p.isPair(); p = p.cdr()) {
            if (start.isNil()) {
                start = last = Object::cons(p.car(), Object::Nil);
            } else {
                last.cdr() = Object::cons(p.car(), Object::Nil);
                last = last.cdr();
            }
        }
        last.cdr() = o;
        return start;
    }

    static Object appendD2(Object l, Object o)
    {
        if (!l.isPair()) return o;
        const Object last = getLastPair(l);
        last.cdr() = o;
        return l;
    }

    static Object getLastPair(Object obj)
    {
        MOSH_ASSERT(obj.isPair());
        for (Object o = obj; ;) {
            const Object kdr = o.cdr();
            if (kdr.isNil()) {
                return o;
            } else {
                o = kdr;
            }
        }
    }

    static Object list1(Object obj)
    {
        return Object::cons(obj, Object::Nil);
    }

    static Object list2(Object a, Object b)
    {
        return Object::cons(a, list1(b));
    }

    static Object list3(Object a, Object b, Object c)
    {
        return Object::cons(a, list2(b, c));
    }

    static Object list4(Object a, Object b, Object c, Object d)
    {
        return Object::cons(a, list3(b, c, d));
    }

    static Object objectVectorToList(const ObjectVector& objectVector)
    {
        Object ret = Object::Nil;
        for (int i = objectVector.size() - 1; i >= 0; i--) {
            ret = Object::cons(objectVector[i], ret);
        }
        return ret;
    }

    static Object arrayToList(Object* array, int size)
    {
        Object p = Object::Nil;
        for (int i = size - 1; i >= 0; i--) {
            p = Object::cons(array[i], p);
        }
        return p;
    }

    static Object wordArrayToList(word* array, int size)
    {
        Object p = Object::Nil;
        for (int i = size - 1; i >= 0; i--) {
            p = Object::cons(Object::makeRaw(array[i]), p);
        }
        return p;
    }


    Object car;
    Object cdr;
    Object sourceInfo;
};

}; // namespace scheme

#endif // __SCHEME_PAIR_H__

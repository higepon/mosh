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
 *  $Id: Pair.h 5307 2008-05-06 14:43:00Z higepon $
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
        for (len = 1; obj.cdr() != Object::Nil; len++, obj = obj.cdr());
        return len;
    }

    // append!
    static Object appendD(Object list1, Object list2)
    {
        if (list1.isNil()) {
            return list2;
        } else {
            const Object last = getLastPair(list1);
            last.cdr() = list2;
            return list1;
        }
    }

//     static Object cloneList(Object list1)
//     {
//         if (list1.isNil()) {
//             return Object::Nil;
//         } else {
//             return Object::cons(list1.car(), cloneList(list1.cdr()));
//         }
//     }

    static Object cloneList(Object list1)
    {
//         Object ret = Object::Nil;
//         for (Object p = list1; p.isPair(); p = p.cdr()) {
//             ret = Object::cons(p.car(), ret);
//         }
        return reverse(reverse(list1));
    }

    static Object reverse(Object list1)
    {
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

    static Object append(Object l) {
        gc_vector<Object> lists;
        for (Object p = l; p.isPair(); p = p.cdr()) {
            lists.push_back(p.car());
        }
        printf("lists.size() = %d\n", lists.size());
        Object ret = lists[lists.size() - 1];
        for (int i = lists.size() - 2; i >= 0; i--) {
            if (!lists[i].isPair()) {
                // error
            }
            ret = append2(lists[i], ret);
        }
        return ret;
    }


//     // append
//     static Object append(Object list1, Object list2)
//     {
//         return appendD(cloneList(list1), cloneList(list2));
//     }


    static Object getLastPair(Object obj)
    {
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


    Object car;
    Object cdr;
    Object sourceInfo;
};

}; // namespace scheme

#endif // __SCHEME_PAIR_H__

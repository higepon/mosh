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
#include "Equivalent.h"

using namespace scheme;

static bool checkAndSetVisited(EqHashTable* visited, Object object1, Object object2)
{
    const Object visitedList = visited->ref(object1, Object::Undef);
    if (visitedList.isUndef()) {
        visited->set(object1, Object::Nil);
        return false;
    } else {
        for (Object p = visitedList; p.isPair(); p = p.cdr()) {
            if (p.car() == object2) {
                return true;
            }
        }
        visited->set(object1, Object::cons(object2, visitedList));
        return false;
    }
}

bool scheme::equal(Object obj1, Object obj2)
{
    return equal(obj1, obj2, new EqHashTable);
}

bool scheme::equal(Object obj1, Object obj2, EqHashTable* visited)
{
    Object object1 = obj1;
    Object object2 = obj2;
entry:
    if (object1 == object2) {
        return true;
    }

    if (object1.isPair()) {
        if (object2.isPair()) {
            if (checkAndSetVisited(visited, object1, object2)) {
                return true;
            } else if (equal(object1.car(), object2.car(), visited)) {
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

    if (object1.isVector()) {
        if (object2.isVector()) {
            if (checkAndSetVisited(visited, object1, object2)) {
                return true;
            }
            Vector* const vector1 = object1.toVector();
            Vector* const vector2 = object2.toVector();
            if (vector1->length() == vector2->length()) {
                const int length = vector1->length();
                for (int i = 0; i < length; i++) {
                    if (!equal(vector1->ref(i), vector2->ref(i), visited)) {
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

    if (object1.isRecord()) {
        if (object2.isRecord()) {
            Record* const record1 = object1.toRecord();
            Record* const record2 = object2.toRecord();
            return record1->rtd() == record2->rtd();
        } else {
            return false;
        }
    }

    return eqv(object1, object2);
}

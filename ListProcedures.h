/*
 * ListProcedures.h - list utilities
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
 *  $Id: ListProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_LIST_PROCEDURES__
#define __SCHEME_LIST_PROCEDURES__

#include "scheme.h"

namespace scheme {

    Object consEx(int argc, const Object* argv);
    Object carEx(int argc, const Object* argv);
    Object cdrEx(int argc, const Object* argv);
    Object setSourceInfoDEx(int argc, const Object* argv);
    Object sourceInfoEx(int argc, const Object* argv);
    Object nullPEx(int argc, const Object* argv);
    Object setCarDEx(int argc, const Object* argv);
    Object setCdrDEx(int argc, const Object* argv);
    Object reverseEx(int argc, const Object* argv);
    Object listPEx(int argc, const Object* argv);
    Object memqEx(int argc, const Object* argv);
    Object memvEx(int argc, const Object* argv);
    Object memberEx(int argc, const Object* argv);
    Object assqEx(int argc, const Object* argv);
    Object appendEx(int argc, const Object* argv);
    Object append2Ex(int argc, const Object* argv);
    Object appendDEx(int argc, const Object* argv);
    Object lengthEx(int argc, const Object* argv);

    inline bool existsInList(Object o, Object list)
    {
        for (Object q = list; q.isPair(); q = q.cdr()) {
            if (q.car() == o) return true;
        }
        return false;
    }


    // callee should check <list>.
    inline Object memq(Object o, Object list)
    {
        for (Object p = list; p.isPair(); p = p.cdr()) {
            if (p.car() == o) {
                return p;
            }
        }
        return Object::False;
    }

    Object uniq(Object list);
    Object assq(Object o, Object alist);
    inline Object assq(Object o, Object alist)
    {
        for (Object p = alist; p.isPair(); p = p.cdr()) {
            if (p.car().car() == o) {
                return p.car();
            }
        }
        return Object::False;
    }

    Object listTovectorEx(int argc, const Object* argv);


}; // namespace scheme

#endif // __SCHEME_LIST_PROCEDURES__

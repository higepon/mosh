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

#ifndef SCHEME_LIST_PROCEDURES_
#define SCHEME_LIST_PROCEDURES_

#include "scheme.h"

namespace scheme {
    Object caaaarEx(VM* theVM, int argc, const Object* argv);
    Object caaadrEx(VM* theVM, int argc, const Object* argv);
    Object caaarEx(VM* theVM, int argc, const Object* argv);
    Object caadarEx(VM* theVM, int argc, const Object* argv);
    Object caaddrEx(VM* theVM, int argc, const Object* argv);
    Object caadrEx(VM* theVM, int argc, const Object* argv);
    Object caarEx(VM* theVM, int argc, const Object* argv);
    Object cadaarEx(VM* theVM, int argc, const Object* argv);
    Object cadadrEx(VM* theVM, int argc, const Object* argv);
    Object cadarEx(VM* theVM, int argc, const Object* argv);
    Object caddarEx(VM* theVM, int argc, const Object* argv);
    Object cadddrEx(VM* theVM, int argc, const Object* argv);
    Object caddrEx(VM* theVM, int argc, const Object* argv);
    Object cadrEx(VM* theVM, int argc, const Object* argv);
    Object cdaaarEx(VM* theVM, int argc, const Object* argv);
    Object cdaadrEx(VM* theVM, int argc, const Object* argv);
    Object cdaarEx(VM* theVM, int argc, const Object* argv);
    Object cdadarEx(VM* theVM, int argc, const Object* argv);
    Object cdaddrEx(VM* theVM, int argc, const Object* argv);
    Object cdadrEx(VM* theVM, int argc, const Object* argv);
    Object cdarEx(VM* theVM, int argc, const Object* argv);
    Object cddaarEx(VM* theVM, int argc, const Object* argv);
    Object cddadrEx(VM* theVM, int argc, const Object* argv);
    Object cddarEx(VM* theVM, int argc, const Object* argv);
    Object cdddarEx(VM* theVM, int argc, const Object* argv);
    Object cddddrEx(VM* theVM, int argc, const Object* argv);
    Object cdddrEx(VM* theVM, int argc, const Object* argv);
    Object cddrEx(VM* theVM, int argc, const Object* argv);
    Object listRefEx(VM* theVM, int argc, const Object* argv);
    Object listTailEx(VM* theVM, int argc, const Object* argv);
    Object consEx(VM* theVM, int argc, const Object* argv);
    Object carEx(VM* theVM, int argc, const Object* argv);
    Object cdrEx(VM* theVM, int argc, const Object* argv);
    Object consMulEx(VM* theVM, int argc, const Object* argv);
    Object setSourceInfoDEx(VM* theVM, int argc, const Object* argv);
    Object sourceInfoEx(VM* theVM, int argc, const Object* argv);
    Object nullPEx(VM* theVM, int argc, const Object* argv);
    Object setCarDEx(VM* theVM, int argc, const Object* argv);
    Object setCdrDEx(VM* theVM, int argc, const Object* argv);
    Object reverseEx(VM* theVM, int argc, const Object* argv);
    Object listPEx(VM* theVM, int argc, const Object* argv);
    Object memqEx(VM* theVM, int argc, const Object* argv);
    Object memvEx(VM* theVM, int argc, const Object* argv);
    Object memberEx(VM* theVM, int argc, const Object* argv);
    Object assqEx(VM* theVM, int argc, const Object* argv);
    Object appendEx(VM* theVM, int argc, const Object* argv);
    Object append2Ex(VM* theVM, int argc, const Object* argv);
    Object appendDEx(VM* theVM, int argc, const Object* argv);
    Object lengthEx(VM* theVM, int argc, const Object* argv);
    Object makeVectorEx(VM* theVM, int argc, const Object* argv);
    Object vectorRefEx(VM* theVM, int argc, const Object* argv);
    Object vectorSetDEx(VM* theVM, int argc, const Object* argv);
    Object vectorLengthEx(VM* theVM, int argc, const Object* argv);

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

    Object listTovectorEx(VM* theVM, int argc, const Object* argv);


} // namespace scheme

#endif // SCHEME_LIST_PROCEDURES_

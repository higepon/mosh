/*
 * CompilerProcedures.h - Procedures written in C++ for compiler.
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

#ifndef SCHEME_COMPILER_PROCEDURES_
#define SCHEME_COMPILER_PROCEDURES_

#include "scheme.h"
#include "VM.h"
#include "Vector.h"

namespace scheme {

    inline Object lvarSym(Object lvar)
    {
        return lvar.toVector()->ref(1);
    }

    Object pass1FindSymbolInLvarsEx(VM* theVM, int argc, const Object* argv);
    Object printStackEx(VM* theVM, int argc, const Object* argv);
    Object disasmEx(VM* theVM, int argc, const Object* argv);
    Object labelEx(VM* theVM, int argc, const Object* argv);
    Object localRefEx(VM* theVM, int argc, const Object* argv);
    Object pass3FindFreeEx(VM* theVM, int argc, const Object* argv);
    Object pass3FindSetsEx(VM* theVM, int argc, const Object* argv);
    Object pass3CompileReferEx(VM* theVM, int argc, const Object* argv);

    Object pass4FixupLabelsEx(VM* theVM, int argc, const Object* argv);

    Object makeCodeBuilderEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutExtra1DEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutExtra2DEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutExtra3DEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutExtra4DEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutExtra5DEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderAppendDEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderEmitEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutInsnArg0DEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutInsnArg1DEx(VM* theVM, int argc, const Object* argv);
    Object codeBuilderPutInsnArg2DEx(VM* theVM, int argc, const Object* argv);

}; // namespace scheme

#endif // SCHEME_COMPILER_PROCEDURES_

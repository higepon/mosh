/*
 * ArrayProcedures.cpp - <f64array> procedures.
 *
 *   Copyright (c) 2022  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Vector.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "SString.h"
#include "Symbol.h"
#include "Array.h"
#include "VM.h"
#include "ArrayProcedures.h"
#include "ProcedureMacro.h"

using namespace scheme;

// (make-f64array ncols nrows)
// (make-f64array ncols nrows value)
Object scheme::makeF64arrayEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-f64array");
    checkArgumentLengthBetween(2, 3);
    argumentAsFixnum(0, nrows);
    argumentAsFixnum(1, ncols);
    double fillValue = 0.0;
    if (argc == 3) {
        if (argv[2].isFlonum()) {
            fillValue = argv[2].toFlonum()->value();
        } else if (argv[2].isFixnum()) {
            fillValue = argv[2].toFixnum();
        } else {
            callAssertionViolationAfter(theVM, procedureName, UC("flonum or fixnum required"), Pair::list1(argv[2]));
            return Object::Undef;
        }
    }
    return Object::makeF64Array(nrows, ncols, fillValue);
}

// (f64array? obj)
Object scheme::f64arrayPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("f64array?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isF64Array());
}

// (f64array-shape array)
Object scheme::f64arrayShapeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("f64array-shape");
    checkArgumentLength(1);
    argumentAsF64Array(0, array);
    ArrayShape shape = array->shape();
    Object v = Object::makeVector(2);
    v.toVector()->set(0, Object::makeFixnum(shape.first));
    v.toVector()->set(1, Object::makeFixnum(shape.second));
    return v;
}

// (f64array-set! array value row col)
Object scheme::f64arraySetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("f64array-set!");
    checkArgumentLength(4);
    argumentAsF64Array(0, array);
    argumentAsFixnum(2, row);
    argumentAsFixnum(3, col);
    if (argv[1].isFlonum()) {
        array->set(row, col, argv[1].toFlonum()->value());
    } else if (argv[1].isFixnum()) {
        array->set(row, col, argv[1].toFixnum());
    } else {
        callAssertionViolationAfter(theVM, procedureName, UC("flonum or fixnum required"), Pair::list1(argv[0]));
        return Object::Undef;
    }
    return Object::Undef;
}

// (f64array-ref array row col)
Object scheme::f64arrayRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("f64array-ref");
    checkArgumentLength(3);
    argumentAsF64Array(0, array);
    argumentAsFixnum(1, row);
    argumentAsFixnum(2, col);
    if ((size_t)row < array->nrows() && (size_t)col < array->ncols()) {
        return Object::makeFlonum(array->ref(row, col));
    } else {
        callAssertionViolationAfter(theVM, procedureName, UC("row or column index out of range"), Pair::list3(argv[0], argv[1], argv[2]));
        return Object::Undef;
    }
}

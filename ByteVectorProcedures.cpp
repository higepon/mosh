/*
 * Bytevectorprocedures.cpp -
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
 *  $Id: ByteVectorProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ByteVector.h"
#include "VM.h"
#include "ByteVectorProcedures.h"
#include "ErrorProcedures.h"
#include "PortProcedures.h"
#include "ByteArrayBinaryInputPort.h"
#include "BinaryInputPort.h"
#include "Symbol.h"
#include "ProcedureMacro.h"
#include "Transcoder.h"
#include "UTF8Codec.h"
#include "UTF16Codec.h"
#include "UTF32Codec.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Arithmetic.h"
#include "TextualOutputPort.h"

using namespace scheme;

Object scheme::u8ListToByteVector(Object list)
{
    MOSH_ASSERT(list.isList());
    for (Object p = list; p.isPair(); p = p.cdr()) {
        const Object num = p.car();
        if (num.isFixnum() && ByteVector::isOctet(num.toFixnum())) {
            continue;
        } else {
            return Object::Nil;
        }
    }
    return Object::makeByteVector(list);
}

Object scheme::utf8TostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("utf8->string");
    checkArgumentLength(1);
    Object transcoder = Object::makeTranscoder(UTF8Codec::getCodec());
    Object args[2];
    args[0] = argv[0];
    args[1] = transcoder;
    return bytevectorTostringEx(theVM, 2, args);
}

Object scheme::stringToutf8Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string->utf8");
    checkArgumentLength(1);
    Object transcoder = Object::makeTranscoder(UTF8Codec::getCodec());
    Object args[2];
    args[0] = argv[0];
    args[1] = transcoder;
    return stringTobytevectorEx(theVM, 2, args);
}

Object scheme::utf32TostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("utf32->string");
    checkArgumentLengthBetween(2, 3);
    argumentAsByteVector(0, bytevector);
    int endianness = UTF32Codec::NO_BOM;
    bool skipBOM = false;
    if (argc == 2) {
        endianness = UTF32Codec::checkBOM(bytevector);
        if (endianness != UTF32Codec::NO_BOM) {
            skipBOM = true;
        }
    } else {
    }

    bool endiannessMandatory = (argc == 3 && !argv[2].isFalse());
    if (endiannessMandatory || endianness == UTF32Codec::NO_BOM) {
        argumentCheckSymbol(1, endiannessSymbol);
        if (endiannessSymbol == Symbol::LITTLE) {
            endianness = UTF32Codec::UTF_32LE;
        } else if (endiannessSymbol == Symbol::BIG) {
            endianness = UTF32Codec::UTF_32BE;
        } else {
            callAssertionViolationAfter(theVM, procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
    }
    const int skipSize = (skipBOM ? 4 : 0);

    ByteArrayBinaryInputPort in(bytevector->data() + skipSize, bytevector->length() - skipSize);
    Transcoder transcoder(UTF32Codec::getCodec(endianness));
    ucs4string ret;
    TRY {
        for (ucs4char c = transcoder.getChar(&in); c != EOF; c = transcoder.getChar(&in)) {
            ret += c;
        }
        return Object::makeString(ret);
    } CATCH(ioError) {
        ioError.arg1 = Object::Nil;
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::utf16TostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("utf16->string");
    checkArgumentLengthBetween(2, 3);
    argumentAsByteVector(0, bytevector);
    int endianness = UTF16Codec::NO_BOM;
    bool skipBOM = false;
    if (argc == 2) {
        endianness = UTF16Codec::checkBOM(bytevector);
        if (endianness != UTF16Codec::NO_BOM) {
            skipBOM = true;
        }
    } else {
    }

    bool endiannessMandatory = (argc == 3 && !argv[2].isFalse());
    if (endiannessMandatory || endianness == UTF16Codec::NO_BOM) {
        argumentCheckSymbol(1, endiannessSymbol);
        if (endiannessSymbol == Symbol::LITTLE) {
            endianness = UTF16Codec::UTF_16LE;
        } else if (endiannessSymbol == Symbol::BIG) {
            endianness = UTF16Codec::UTF_16BE;
        } else {
            callAssertionViolationAfter(theVM, procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
    }
    const int skipSize = (skipBOM ? 2 : 0);
    ByteArrayBinaryInputPort in(bytevector->data() + skipSize, bytevector->length() - skipSize);
    Transcoder transcoder(UTF16Codec::getCodec(endianness));
    ucs4string ret;
    TRY {
        for (ucs4char c = transcoder.getChar(&in); c != EOF; c = transcoder.getChar(&in)) {
            ret += c;
        }
        return Object::makeString(ret);
    } CATCH(ioError) {
        ioError.arg1 = Object::Nil;
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::stringToutf16Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string->utf16");
    checkArgumentLengthBetween(1, 2);
    Object args[2];
    args[0] = argv[0];
    if (argc == 2) {
        argumentCheckSymbol(1, endianness);
        if (endianness == Symbol::LITTLE) {
            args[1] = Object::makeTranscoder(UTF16Codec::getCodec(UTF16Codec::UTF_16LE));
        } else if (endianness == Symbol::BIG) {
            args[1] = Object::makeTranscoder(UTF16Codec::getCodec(UTF16Codec::UTF_16BE));
        } else {
            callAssertionViolationAfter(theVM, procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
        return stringTobytevectorEx(theVM, 2, args);
    } else {
        args[1] = Object::makeTranscoder(UTF16Codec::getCodec(UTF16Codec::UTF_16BE));
        return stringTobytevectorEx(theVM, 2, args);
    }
}

Object scheme::stringToutf32Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string->utf32");
    checkArgumentLengthBetween(1, 2);
    Object args[2];
    args[0] = argv[0];
    if (argc == 2) {
        argumentCheckSymbol(1, endianness);
        if (endianness == Symbol::LITTLE) {
            args[1] = Object::makeTranscoder(UTF32Codec::getCodec(UTF32Codec::UTF_32LE));
        } else if (endianness == Symbol::BIG) {
            args[1] = Object::makeTranscoder(UTF32Codec::getCodec(UTF32Codec::UTF_32BE));
        } else {
            callAssertionViolationAfter(theVM, procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
        return stringTobytevectorEx(theVM, 2, args);
    } else {
        args[1] = Object::makeTranscoder(UTF32Codec::getCodec(UTF32Codec::UTF_32BE));
        return stringTobytevectorEx(theVM, 2, args);
    }
}

Object scheme::bytevectorS64NativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckExactInteger(2, value);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!Arithmetic::fitsS64(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    const int64_t s64Value = Arithmetic::toS64(value);
    bytevector->s64SetNative(index, s64Value);
    return Object::Undef;
}

Object scheme::bytevectorU64NativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckExactInteger(2, value);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!Arithmetic::fitsU64(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    const uint64_t u64Value = Arithmetic::toU64(value);
    bytevector->u64SetNative(index, u64Value);
    return Object::Undef;
}

Object scheme::bytevectorS64SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckExactInteger(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!Arithmetic::fitsS64(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    const int64_t s64Value = Arithmetic::toS64(value);

    if (endianness == Symbol::LITTLE) {
        bytevector->s64SetLittle(index, s64Value);
    } else if (endianness == Symbol::BIG) {
        bytevector->s64SetBig(index, s64Value);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorU64SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u64-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckExactInteger(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!Arithmetic::fitsU64(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    const uint64_t u64Value = Arithmetic::toU64(value);

    if (endianness == Symbol::LITTLE) {
        bytevector->u64SetLittle(index, u64Value);
    } else if (endianness == Symbol::BIG) {
        bytevector->u64SetBig(index, u64Value);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorS64NativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 8 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Bignum::makeIntegerFromS64(bytevector->s64RefNative(index));
}

Object scheme::bytevectorU64NativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u64-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 8 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Bignum::makeIntegerFromU64(bytevector->u64RefNative(index));
}

Object scheme::bytevectorS64RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Bignum::makeIntegerFromS64(bytevector->s64RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Bignum::makeIntegerFromS64(bytevector->s64RefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorU64RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u64-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Bignum::makeIntegerFromU64(bytevector->u64RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Bignum::makeIntegerFromU64(bytevector->u64RefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorS32NativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS32Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->s32SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorU32NativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU32Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->u32SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorS32SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS32Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->s32SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->s32SetBig(index, value);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}
Object scheme::bytevectorU32SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u32-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU32Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->u32SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->u32SetBig(index, value);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorS32NativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 4 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Bignum::makeIntegerFromS64(bytevector->s32RefNative(index));
}

Object scheme::bytevectorU32NativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u32-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 4 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Bignum::makeIntegerFromU32(bytevector->u32RefNative(index));
}

Object scheme::bytevectorS32RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Bignum::makeIntegerFromS64(bytevector->s32RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Bignum::makeIntegerFromS64(bytevector->s32RefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorU32RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u32-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Bignum::makeIntegerFromU32(bytevector->u32RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Bignum::makeIntegerFromU32((uint32_t)bytevector->u32RefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorS16NativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS16Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->s16SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorU16NativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU16Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->u16SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorS16SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS16Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->s16SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->s16SetBig(index, value);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}
Object scheme::bytevectorU16SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u16-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU16Range(value)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->u16SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->u16SetBig(index, value);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorS16NativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 2 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeFixnum(bytevector->s16RefNative(index));
}

Object scheme::bytevectorU16NativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u16-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 2 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeFixnum(bytevector->u16RefNative(index));
}

Object scheme::bytevectorS16RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeFixnum(bytevector->s16RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeFixnum(bytevector->s16RefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorU16RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u16-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeFixnum(bytevector->u16RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeFixnum(bytevector->u16RefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::u8ListTobytevectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("u8-list->bytevector");
    checkArgumentLength(1);
    argumentCheckList(0, list);
    const Object bytevector = u8ListToByteVector(list);
    if (bytevector.isNil()) {
        callAssertionViolationAfter(theVM, procedureName, "proper u8 list required", L1(list));
        return Object::Undef;
    } else {
        return bytevector;
    }
}

Object scheme::bytevectorTou8ListEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector->u8-list");
    argumentAsByteVector(0, bytevector);
    Object ret = Object::Nil;
    for (int i = bytevector->length() - 1; i >= 0; i--) {
        ret = Object::cons(Object::makeFixnum(bytevector->u8Ref(i)), ret);
    }
    return ret;
}

Object scheme::bytevectorCopyEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-copy");
    checkArgumentLength(1);
    argumentAsByteVector(0, bytevector);
    return Object::makeByteVector(bytevector->copy());
}

Object scheme::bytevectorCopyDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-copy!");
    checkArgumentLength(5);
    argumentAsByteVector(0, source);
    argumentAsFixnum(1, sourceStart);
    argumentAsByteVector(2, target)
    argumentAsFixnum(3, targetStart);
    argumentAsFixnum(4, k);

    const int sourceLength = source->length();
    const int targetLength = target->length();

    if ((sourceStart <= sourceStart + k)  &&
        (sourceStart + k <= sourceLength) &&
        (0 <= targetStart)                &&
        (targetStart <= targetStart + k)  &&
        (targetStart + k <= targetLength)) {
        memmove(target->data() + targetStart, source->data() + sourceStart, k);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "invalid range");
    }
    return Object::Undef;
}

Object scheme::bytevectorFillDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-fill!");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, value);
    if (ByteVector::isByte(value) || ByteVector::isOctet(value)) {
        bytevector->fill(static_cast<uint8_t>(value));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "invalid bytevector value. should be 0 <= v <= 255", L1(argv[1]));
    }
    return Object::Undef;
}

Object scheme::bytevectorEqPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector=?");
    checkArgumentLength(2);
    argumentAsByteVector(0, bv0);
    argumentAsByteVector(1, bv1);
    return Object::makeBool(bv0->equal(bv1));
}

Object scheme::makeBytevectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-bytevector");
    checkArgumentLengthBetween(1, 2);
    argumentAsFixnum(0, length);
    if (1 == argc) {
        return Object::makeByteVector(length);
    } else {
        argumentAsFixnum(1, value);
        if (ByteVector::isByte(value) || ByteVector::isOctet(value)) {
            return Object::makeByteVector(length, static_cast<uint8_t>(value));
        } else {
            callAssertionViolationAfter(theVM, procedureName, "invalid bytevector value. should be 0 <= v <= 255", L1(argv[1]));
            return Object::Undef;
        }
    }
}

Object scheme::nativeEndiannessEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("native-endianness");
    checkArgumentLength(0);

#if WORDS_BIGENDIAN
    return Symbol::BIG;
#else
    return Symbol::LITTLE;
#endif
}

Object scheme::bytevectorU8SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u8-set!");
    checkArgumentLength(3);

    if (!argv[1].isFixnum()) {
        printf("bignum? %d %d %d\n", argv[1].isBignum(), argv[1].isFlonum(), argv[2].isRatnum());
    }

    if (!argv[2].isFixnum()) {
        printf("bignum? %d %d %d\n", argv[2].isBignum(), argv[2].isFlonum(), argv[2].isRatnum());
    }

    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);

    if (bytevector->isValidIndex(index)) {
        if (ByteVector::isOctet(value)) {
            bytevector->u8Set(index, static_cast<uint8_t>(value));
        } else {
            callAssertionViolationAfter(theVM, procedureName, "value out of range. should be 0 <= value <= 255", L1(argv[2]));
        }
    } else {
        callAssertionViolationAfter(theVM, procedureName, "index out of range.", L1(argv[1]));
    }
    return Object::Undef;
}

Object scheme::bytevectorU8RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u8-ref");
    checkArgumentLength(2);

    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    if (bytevector->isValidIndex(index)) {
        return Object::makeFixnum(bytevector->u8Ref(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "index out of range.", L1(argv[2]));
        return Object::Undef;
    }
}

Object scheme::bytevectorS8SetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s8-set!");
    checkArgumentLength(3);

    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentAsFixnum(2, value);

    if (bytevector->isValidIndex(index)) {
        if (ByteVector::isByte(value)) {
            bytevector->s8Set(index, static_cast<int8_t>(value));
        } else {
            callAssertionViolationAfter(theVM, procedureName, "value out of range. should be -127 <= value <= 128", L1(argv[2]));
        }
    } else {
        callAssertionViolationAfter(theVM, procedureName, "index out of range.", L1(argv[1]));
    }

    return Object::Undef;
}

Object scheme::bytevectorS8RefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s8-ref");
    checkArgumentLength(2);

    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);

    if (bytevector->isValidIndex(index)) {
        return Object::makeFixnum(bytevector->s8Ref(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "index out of range.", L1(argv[1]));
        return Object::Undef;
    }
}


Object scheme::bytevectorLengthEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-length");
    checkArgumentLength(1);

    argumentAsByteVector(0, bytevector);
    return Object::makeFixnum(bytevector->length());
}

Object scheme::bytevectorIeeeSingleRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-single-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);

    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeFlonum(bytevector->ieeeSingleRefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeFlonum(bytevector->ieeeSingleRefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorIeeeSingleNativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-single-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);

    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 4 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }

    return Object::makeFlonum(bytevector->ieeeSingleNativeRef(index));
}

Object scheme::bytevectorIeeeSingleSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-single-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckReal(2, value);
    argumentCheckSymbol(3, endianness);

    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }
    const double v = Arithmetic::realToDouble(value);

    if (endianness == Symbol::LITTLE) {
        bytevector->ieeeSingleSetLittle(index, v);
    } else if (endianness == Symbol::BIG) {
        bytevector->ieeeSingleSetBig(index, v);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorIeeeSingleNativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-single-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckReal(2, value);

    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 4 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    const double v = Arithmetic::realToDouble(value);
    bytevector->ieeeSingleNativeSet(index, v);
    return Object::Undef;
}

Object scheme::bytevectorIeeeDoubleRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-double-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckSymbol(2, endianness);

    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeFlonum(bytevector->ieeeDoubleRefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeFlonum(bytevector->ieeeDoubleRefBig(index));
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorIeeeDoubleNativeRefEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-double-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);

    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 8 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeFlonum(bytevector->ieeeDoubleNativeRef(index));
}

Object scheme::bytevectorIeeeDoubleSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-double-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckReal(2, value);
    argumentCheckSymbol(3, endianness);

    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    const double v = Arithmetic::realToDouble(value);

    if (endianness == Symbol::LITTLE) {
        bytevector->ieeeDoubleSetLittle(index, v);
    } else if (endianness == Symbol::BIG) {
        bytevector->ieeeDoubleSetBig(index, v);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorIeeeDoubleNativeSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-ieee-double-native- set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsFixnum(1, index);
    argumentCheckReal(2, value);

    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(theVM, procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 8 != 0) {
        callAssertionViolationAfter(theVM, procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }

    const double v = Arithmetic::realToDouble(value);
    bytevector->ieeeDoubleNativeSet(index, v);
    return Object::Undef;
}

/*
 * ByteVectorProcedures.cpp -
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
#include "ByteVectorProcedures.h"
#include "ErrorProcedures.h"
#include "ByteArrayBinaryInputPort.h"
#include "BinaryInputPort.h"
#include "Symbol.h"
#include "ProcedureMacro.h"
#include "Transcoder.h"
#include "UTF8Codec.h"
#include "UTF16Codec.h"
#include "UTF32Codec.h"

using namespace scheme;

Object scheme::u8ListToByteVector(Object list)
{
    MOSH_ASSERT(list.isList());
    for (Object p = list; p.isPair(); p = p.cdr()) {
        const Object num = p.car();
        if (num.isInt() && ByteVector::isOctet(num.toInt())) {
            continue;
        } else {
            return Object::Nil;
        }
    }
    return Object::makeByteVector(list);
}

Object scheme::utf8TostringEx(int argc, const Object* argv)
{
    DeclareProcedureName("utf8->string");
    checkArgumentLength(1);
    Object transcoder = Object::makeTranscoder(new UTF8Codec());
    Object args[2];
    args[0] = argv[0];
    args[1] = transcoder;
    return bytevectorTostringEx(2, args);
}

Object scheme::stringToutf8Ex(int argc, const Object* argv)
{
    DeclareProcedureName("string->utf8");
    checkArgumentLength(1);
    Object transcoder = Object::makeTranscoder(new UTF8Codec());
    Object args[2];
    args[0] = argv[0];
    args[1] = transcoder;
    return stringTobytevectorEx(2, args);
}

Object scheme::utf32TostringEx(int argc, const Object* argv)
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
            callAssertionViolationAfter(procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
    }
    const int skipSize = (skipBOM ? 4 : 0);
    BinaryInputPort* in = new ByteArrayBinaryInputPort(bytevector->data() + skipSize, bytevector->length() - skipSize);
    ucs4string ret;
    UTF32Codec codec(endianness);
    TRY_IO {
        for (ucs4char c = codec.in(in); c != EOF; c = codec.in(in)) {
            ret += c;
        }
        return Object::makeString(ret);
    } CATCH_IO {
        callAssertionViolationAfter(procedureName, IO_ERROR_MESSAGE, L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::utf16TostringEx(int argc, const Object* argv)
{
    DeclareProcedureName("utf16->string");
    checkArgumentLengthBetween(2, 3);
    argumentAsByteVector(0, bytevector);
    int endianness;
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
            callAssertionViolationAfter(procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
    }
    const int skipSize = (skipBOM ? 2 : 0);
    BinaryInputPort* in = new ByteArrayBinaryInputPort(bytevector->data() + skipSize, bytevector->length() - skipSize);
    ucs4string ret;
    UTF16Codec codec(endianness);
    TRY_IO {
        for (ucs4char c = codec.in(in); c != EOF; c = codec.in(in)) {
            ret += c;
        }
        return Object::makeString(ret);
    } CATCH_IO {
        callAssertionViolationAfter(procedureName, IO_ERROR_MESSAGE, L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::stringToutf16Ex(int argc, const Object* argv)
{
    DeclareProcedureName("string->utf16");
    checkArgumentLengthBetween(1, 2);
    Object args[2];
    args[0] = argv[0];
    if (argc == 2) {
        argumentCheckSymbol(1, endianness);
        if (endianness == Symbol::LITTLE) {
            args[1] = Object::makeTranscoder(new UTF16Codec(UTF16Codec::UTF_16LE));
        } else if (endianness == Symbol::BIG) {
            args[1] = Object::makeTranscoder(new UTF16Codec(UTF16Codec::UTF_16BE));
        } else {
            callAssertionViolationAfter(procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
        return stringTobytevectorEx(2, args);
    } else {
        args[1] = Object::makeTranscoder(new UTF16Codec(UTF16Codec::UTF_16BE));
        return stringTobytevectorEx(2, args);
    }
}

Object scheme::stringToutf32Ex(int argc, const Object* argv)
{
    DeclareProcedureName("string->utf32");
    checkArgumentLengthBetween(1, 2);
    Object args[2];
    args[0] = argv[0];
    if (argc == 2) {
        argumentCheckSymbol(1, endianness);
        if (endianness == Symbol::LITTLE) {
            args[1] = Object::makeTranscoder(new UTF32Codec(UTF32Codec::UTF_32LE));
        } else if (endianness == Symbol::BIG) {
            args[1] = Object::makeTranscoder(new UTF32Codec(UTF32Codec::UTF_32BE));
        } else {
            callAssertionViolationAfter(procedureName, "endianness should be little or big", L1(argv[1]));
            return Object::Undef;
        }
        return stringTobytevectorEx(2, args);
    } else {
        args[1] = Object::makeTranscoder(new UTF32Codec(UTF32Codec::UTF_32BE));
        return stringTobytevectorEx(2, args);
    }
}

Object scheme::stringTobytevectorEx(int argc, const Object* argv)
{
    DeclareProcedureName("string->bytevector");
    checkArgumentLength(2);
    argumentAsString(0, text);
    argumentAsTranscoder(1, transcoder);
    gc_vector<uint8_t> accum;
    uint8_t buf[4];
    for (ucs4string::const_iterator it = text->data().begin();
         it != text->data().end(); ++it) {
        TRY_IO {
            const int length = transcoder->codec()->out(buf, *it);
            for (int i = 0; i < length; i++) {
                accum.push_back(buf[i]);
            }
        } CATCH_IO {
            callAssertionViolationAfter(procedureName, IO_ERROR_MESSAGE, L1(argv[0]));
            return Object::Undef;
        }
    }
    return Object::makeByteVector(new ByteVector(accum));
}

Object scheme::bytevectorTostringEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector->string");
    checkArgumentLength(2);

    argumentAsByteVector(0, bytevector);
    argumentAsTranscoder(1, transcoder);

    BinaryInputPort* in = new ByteArrayBinaryInputPort(bytevector->data(), bytevector->length());
    ucs4string ret;
    Codec* const codec = transcoder->codec();

    TRY_IO {
        for (ucs4char c = codec->in(in); c != EOF; c = codec->in(in)) {
            ret += c;
        }
    } CATCH_IO {
        callAssertionViolationAfter(procedureName, IO_ERROR_MESSAGE, L1(argv[0]));
        return Object::Undef;
    }
    return Object::makeString(ret);
}

Object scheme::bytevectorS64NativeSetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS64Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->s64SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorU64NativeSetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU64Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->u64SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorS64SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS64Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->s64SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->s64SetBig(index, value);
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorU64SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u64-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU64Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->u64SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->u64SetBig(index, value);
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorS64NativeRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 8 != 0) {
        callAssertionViolationAfter(procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeInt(bytevector->s64RefNative(index));
}

Object scheme::bytevectorU64NativeRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u64-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 8 != 0) {
        callAssertionViolationAfter(procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeInt(bytevector->u64RefNative(index));
}

Object scheme::bytevectorS64RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s64-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeInt(bytevector->s64RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeInt(bytevector->s64RefBig(index));
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorU64RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u64-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid64RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeInt(bytevector->u64RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeInt(bytevector->u64RefBig(index));
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorS32NativeSetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS32Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->s32SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorU32NativeSetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU32Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->u32SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorS32SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS32Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->s32SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->s32SetBig(index, value);
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}
Object scheme::bytevectorU32SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u32-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU32Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->u32SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->u32SetBig(index, value);
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorS32NativeRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 4 != 0) {
        callAssertionViolationAfter(procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeInt(bytevector->s32RefNative(index));
}

Object scheme::bytevectorU32NativeRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u32-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 4 != 0) {
        callAssertionViolationAfter(procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeInt(bytevector->u32RefNative(index));
}

Object scheme::bytevectorS32RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s32-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeInt(bytevector->s32RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeInt(bytevector->s32RefBig(index));
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorU32RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u32-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid32RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeInt(bytevector->u32RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeInt(bytevector->u32RefBig(index));
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorS16NativeSetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS16Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->s16SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorU16NativeSetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-native-set!");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU16Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    bytevector->u16SetNative(index, value);
    return Object::Undef;
}

Object scheme::bytevectorS16SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inS16Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->s16SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->s16SetBig(index, value);
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}
Object scheme::bytevectorU16SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u16-set!");
    checkArgumentLength(4);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);
    argumentCheckSymbol(3, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (!ByteVector::inU16Range(value)) {
        callAssertionViolationAfter(procedureName, "value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        bytevector->u16SetLittle(index, value);
    } else if (endianness == Symbol::BIG) {
        bytevector->u16SetBig(index, value);
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
    }
    return Object::Undef;
}

Object scheme::bytevectorS16NativeRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 2 != 0) {
        callAssertionViolationAfter(procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeInt(bytevector->s16RefNative(index));
}

Object scheme::bytevectorU16NativeRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u16-native-ref");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    } else if (index % 2 != 0) {
        callAssertionViolationAfter(procedureName, "index not aligned", L1(argv[1]));
        return Object::Undef;
    }
    return Object::makeInt(bytevector->u16RefNative(index));
}

Object scheme::bytevectorS16RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s16-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeInt(bytevector->s16RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeInt(bytevector->s16RefBig(index));
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::bytevectorU16RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u16-ref");
    checkArgumentLength(3);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentCheckSymbol(2, endianness);
    if (!bytevector->isValid16RefIndex(index)) {
        callAssertionViolationAfter(procedureName, "index out of range", L1(argv[1]));
        return Object::Undef;
    }

    if (endianness == Symbol::LITTLE) {
        return Object::makeInt(bytevector->u16RefLittle(index));
    } else if (endianness == Symbol::BIG) {
        return Object::makeInt(bytevector->u16RefBig(index));
    } else {
        callAssertionViolationAfter(procedureName, "unsupporeted endianness", L1(endianness));
        return Object::Undef;
    }
}

Object scheme::u8ListTobytevectorEx(int argc, const Object* argv)
{
    DeclareProcedureName("u8-list->bytevector");
    checkArgumentLength(1);
    argumentCheckList(0, list);
    const Object bytevector = u8ListToByteVector(list);
    if (bytevector.isNil()) {
        callAssertionViolationAfter(procedureName, "proper u8 list required", L1(list));
        return Object::Undef;
    } else {
        return bytevector;
    }
}

Object scheme::bytevectorTou8ListEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector->u8-list");
    argumentAsByteVector(0, bytevector);
    Object ret = Object::Nil;
    for (int i = bytevector->length() - 1; i >= 0; i--) {
        ret = Object::cons(Object::makeInt(bytevector->u8Ref(i)), ret);
    }
    return ret;
}

Object scheme::bytevectorCopyEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-copy");
    checkArgumentLength(1);
    argumentAsByteVector(0, bytevector);
    return Object::makeByteVector(bytevector->copy());
}

Object scheme::bytevectorCopyDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-copy!");
    checkArgumentLength(5);
    argumentAsByteVector(0, source);
    argumentAsInt(1, sourceStart);
    argumentAsByteVector(2, target)
    argumentAsInt(3, targetStart);
    argumentAsInt(4, k);

    const int sourceLength = source->length();
    const int targetLength = target->length();

    if ((sourceStart <= sourceStart + k)  &&
        (sourceStart + k <= sourceLength) &&
        (0 <= targetStart)                &&
        (targetStart <= targetStart + k)  &&
        (targetStart + k <= targetLength)) {
        memmove(target->data() + targetStart, source->data() + sourceStart, k);
    } else {
        callAssertionViolationAfter(procedureName, "invalid range");
    }
    return Object::Undef;
}

Object scheme::bytevectorFillDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-fill!");
    checkArgumentLength(2);
    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, value);
    if (ByteVector::isByte(value) || ByteVector::isOctet(value)) {
        bytevector->fill(static_cast<uint8_t>(value));
    } else {
        callAssertionViolationAfter(procedureName, "invalid bytevector value. should be 0 <= v <= 255", L1(argv[1]));
    }
    return Object::Undef;
}

Object scheme::bytevectorEqPEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector=?");
    checkArgumentLength(2);
    argumentAsByteVector(0, bv0);
    argumentAsByteVector(1, bv1);
    return Object::makeBool(bv0->equal(bv1));
}

Object scheme::makeBytevectorEx(int argc, const Object* argv)
{
    DeclareProcedureName("make-bytevector");
    checkArgumentLengthBetween(1, 2);
    argumentAsInt(0, length);
    if (1 == argc) {
        return Object::makeByteVector(length);
    } else {
        argumentAsInt(1, value);
        if (ByteVector::isByte(value) || ByteVector::isOctet(value)) {
            return Object::makeByteVector(length, static_cast<uint8_t>(value));
        } else {
            callAssertionViolationAfter(procedureName, "invalid bytevector value. should be 0 <= v <= 255", L1(argv[1]));
            return Object::Undef;
        }
    }
}

Object scheme::nativeEndiannessEx(int argc, const Object* argv)
{
    DeclareProcedureName("native-endianness");
    checkArgumentLength(0);

#if WORDS_BIGENDIAN
    return Symbol::BIG;
#else
    return Symbol::LITTLE;
#endif
}

Object scheme::bytevectorU8SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u8-set!");
    checkArgumentLength(3);

    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);

    if (bytevector->isValidIndex(index)) {
        if (ByteVector::isOctet(value)) {
            bytevector->u8Set(index, static_cast<uint8_t>(value));
        } else {
            callAssertionViolationAfter(procedureName, "value out of range. should be 0 <= value <= 255", L1(argv[2]));
        }
    } else {
        callAssertionViolationAfter(procedureName, "index out of range.", L1(argv[1]));
    }
    return Object::Undef;
}

Object scheme::bytevectorU8RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u8-ref");
    checkArgumentLength(2);

    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    if (bytevector->isValidIndex(index)) {
        return Object::makeInt(bytevector->u8Ref(index));
    } else {
        callAssertionViolationAfter(procedureName, "index out of range.", L1(argv[2]));
        return Object::Undef;
    }
}

Object scheme::bytevectorS8SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s8-set!");
    checkArgumentLength(3);

    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);

    if (bytevector->isValidIndex(index)) {
        if (ByteVector::isByte(value)) {
            bytevector->s8Set(index, static_cast<int8_t>(value));
        } else {
            callAssertionViolationAfter(procedureName, "value out of range. should be -127 <= value <= 128", L1(argv[2]));
        }
    } else {
        callAssertionViolationAfter(procedureName, "index out of range.", L1(argv[1]));
    }

    return Object::Undef;
}

Object scheme::bytevectorS8RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-s8-ref");
    checkArgumentLength(2);

    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);

    if (bytevector->isValidIndex(index)) {
        return Object::makeInt(bytevector->s8Ref(index));
    } else {
        callAssertionViolationAfter(procedureName, "index out of range.", L1(argv[1]));
        return Object::Undef;
    }
}


Object scheme::bytevectorLengthEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-length");
    checkArgumentLength(1);

    argumentAsByteVector(0, bytevector);
    return Object::makeInt(bytevector->length());
}

Object scheme::getBytevectorNEx(int argc, const Object* argv)
{
    DeclareProcedureName("get-byte-vector-n");
    checkArgumentLength(2);

    argumentAsBinaryInputPort(0, binaryInputPort);
    argumentAsInt(1, count);

    ByteVector* ret = binaryInputPort->getByteVector(count);
    if (ret->length() == 0) {
        return Object::Eof;
    } else {
        return Object::makeByteVector(ret);
    }
}

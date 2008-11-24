/*
 * StringProcedures.cpp - <string> prcedures.
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
 *  $Id: StringProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <errno.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "Symbol.h"
#include "StringProcedures.h"
#include "ProcedureMacro.h"
#include "PortProcedures.h"
#include "TextualOutputPort.h"
#include "NumberReader.h"

using namespace scheme;

static Object makeList(scheme::gc_vector<ucs4string>& v, scheme::gc_vector<ucs4string>::size_type i);

Object scheme::format(const ucs4char* message, Object values)
{
    MOSH_ASSERT(values.isNil() || values.isPair());
    const Object sport = Object::makeStringOutputPort();
    TextualOutputPort* const port = sport.toTextualOutputPort();
    port->format(message, values);
    return sysGetOutputStringEx(1, &sport);
}

Object scheme::stringEx(int argc, const Object* argv)
{
    DeclareProcedureName("string");
    ucs4string ret;
    for (int i = 0; i < argc; i++) {
        argumentAsChar(i, ch);
        ret += ch;
    }
    return Object::makeString(ret);
}

Object scheme::stringCopyEx(int argc, const Object* argv)
{
    DeclareProcedureName("string-copy");
    checkArgumentLength(1);

    argumentAsString(0, text);
    return Object::makeString(text->data());
}


Object scheme::stringRefEx(int argc, const Object* argv)
{
    DeclareProcedureName("string-ref");
    checkArgumentLength(2);

    argumentAsString(0, text);
    argumentAsFixnum(1, index);

    if (index < text->length()) {
        return Object::makeChar(text->charAt(index));
    } else {
        callAssertionViolationAfter(procedureName,
                                    "index out of range",
                                    L2(Object::makeFixnum(text->length()),
                                       argv[1]
                                        ));
        return Object::Undef;
    }
}

Object scheme::stringEqPEx(int argc, const Object* argv)
{
    DeclareProcedureName("string=?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentAsString(i, string1);
        argumentAsString(i + 1, string2);
        if (*string1 == *string2) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::stringToregexpEx(int argc, const Object* argv)
{
    DeclareProcedureName("string->regexp");
    checkArgumentLength(1);
    argumentAsString(0, text);
    return Object::makeRegexp(text->data());
}

Object scheme::makeStringEx(int argc, const Object* argv)
{
    DeclareProcedureName("make-string");
    checkArgumentLengthBetween(1, 2);
    argumentAsFixnum(0, stringSize);

    if (2 == argc) {
        argumentAsChar(1, ch);
        return Object::makeString(stringSize, ch);
    } else {
        return Object::makeString(stringSize);
    }
}

Object scheme::stringSetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("string-set!");
    checkArgumentLength(3);

    argumentAsString(0, text);
    argumentAsFixnum(1, index);
    argumentAsChar(2, ch);

    text->data()[index] = ch;
    return Object::Undef;
}

Object scheme::stringLengthEx(int argc, const Object* argv)
{
    DeclareProcedureName("string-length");
    checkArgumentLength(1);
    argumentAsString(0, text);
    return Object::makeFixnum(text->data().length());
}

Object scheme::stringTosymbol(Object str)
{
    MOSH_ASSERT(str.isString());
    return Symbol::intern(str.toString()->data().c_str());
}

Object scheme::stringTosymbolEx(int argc, const Object* argv)
{
    DeclareProcedureName("string->symbol");
    checkArgumentLength(1);
    argumentCheckString(0, text);
    return stringTosymbol(text);
}

Object stringToNumber(const ucs4string& text)
{
    bool isErrorOccured = false;
    const Object number = NumberReader::read(text, isErrorOccured);

    if (isErrorOccured) {
        return Object::False;
    } else {
        return number;
    }
}

Object scheme::stringTonumberEx(int argc, const Object* argv)
{
    DeclareProcedureName("string->number");
    checkArgumentLengthBetween(1, 2);
    argumentAsString(0, text);
    const ucs4string& numberString = text->data();
    if (argc == 1) {
        return stringToNumber(numberString);
    } else {
        argumentAsFixnum(1, radix);
        switch (radix) {
            case 2:
            {
                ucs4string text = UC("#b");
                text += numberString;
                return stringToNumber(text);
            }
            case 8:
            {
                ucs4string text = UC("#o");
                text += numberString;
                return stringToNumber(text);
            }
            case 10:
                return stringToNumber(numberString);
            case 16:
            {
                ucs4string text = UC("#x");
                text += numberString;
                return stringToNumber(text);
            }
            default:
                callAssertionViolationAfter(procedureName, "radix should be 2, 8, 10 ro 16", L1(argv[1]));
                return Object::Undef;
        }
    }
}

Object scheme::stringAppendEx(int argc, const Object* argv)
{
    DeclareProcedureName("string-append");
    ucs4string ret;
    for (int i = 0; i < argc; i++) {
        argumentAsString(i, text);
        ret += text->data();
    }
    return Object::makeString(ret);
}

Object scheme::stringSplitEx(int argc, const Object* argv)
{
    DeclareProcedureName("string-split");
    argumentAsString(0, text);
    argumentAsChar(1, ch);

    gc_vector<ucs4string> v;
    text->data().split(ch, v);
    return makeList(v, 0);
}

Object makeList(scheme::gc_vector<ucs4string>& v, scheme::gc_vector<ucs4string>::size_type i)
{
    if (i == v.size()) {
        return Object::Nil;
    } else {
        return Object::cons(Object::makeString(v[i]), makeList(v, i + 1));
    }
}

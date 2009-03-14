/*
 * TranscodedTextualOutputPort.cpp - 
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
 *  $Id: TranscodedTextualOutputPort.cpp 183 2008-07-04 06:19:28Z higepon $
 */


#include "Object.h"
#include "Object-inl.h"
#include "Transcoder.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "Vector.h"
#include "Symbol.h"
#include "Regexp.h"
#include "ByteVector.h"
#include "Record.h"
#include "Codec.h"
#include "Transcoder.h"
#include "BinaryInputPort.h"
#include "BinaryOutputPort.h"
#include "TextualOutputPort.h"
#include "TextualInputPort.h"
#include "ProcedureMacro.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Compnum.h"
#include "Arithmetic.h"
#include "CompoundCondition.h"
#include "BinaryOutputPort.h"
#include "BinaryInputOutputPort.h"

using namespace scheme;

TextualOutputPort::TextualOutputPort() :
    isErrorOccured_(false),
    errorMessage_(Object::Nil),
    irritants_(Object::Nil)
{
}

TextualOutputPort::~TextualOutputPort()
{
}

bool TextualOutputPort::isErrorOccured() const
{
    return isErrorOccured_;
}

Object TextualOutputPort::errorMessage() const
{
    return errorMessage_;
}

Object TextualOutputPort::position() const
{
    return Object::Undef;
}

bool TextualOutputPort::setPosition(int position)
{
    return false;
}

bool TextualOutputPort::hasPosition() const
{
    return false;
}

bool TextualOutputPort::hasSetPosition() const
{
    return false;
}

void TextualOutputPort::putString(String* str)
{
    putString(str->data());
}

void TextualOutputPort::putString(const ucs4string& s)
{
    for (ucs4string::size_type i = 0; i < s.size(); i++) {
        putChar(s[i]);
    }
}

void TextualOutputPort::putString(const char* s)
{
    const int len = strlen(s);
    for (int i = 0; i < len; i++) {
        putChar(s[i]);
    }
}

Object TextualOutputPort::irritants() const
{
    return irritants_;
}

void TextualOutputPort::format(const ucs4string& fmt, Object args)
{
    ucs4string buffer = UC("");
    for (uint32_t i = 0; i < fmt.size(); i++) {
        if (fmt[i] == '~') {
            i++;
            if (!buffer.empty()) {
                putString(buffer);
                buffer.clear();
            }
            switch (fmt[i]) {
            case '~':
                display(Object::makeChar('~'));
                break;
            case '%':
                display(Object::makeChar('\n'));
                break;
            case 'a':
            case 'A':
            case 'd':
            case 'D':
            {
                if (args.isPair()) {
                    display(args.car());
                    args = args.cdr();
                } else {
                    isErrorOccured_ = true;
                    errorMessage_ = "too few arguments for format string";
                    irritants_ = Pair::list1(Object::makeString(fmt));
                    return;
                }
                break;
            }
            case 's':
            case 'S':
            {
                if (args.isPair()) {
                    putDatum(args.car());
                    args = args.cdr();
                } else {
                    isErrorOccured_ = true;
                    errorMessage_ = "too few arguments for format string";
                    irritants_ = Pair::list1(Object::makeString(fmt));
                    return;
                }
                break;
            }
            case '\0':
                i--;
                break;
            }
        } else {
            buffer += fmt[i];
        }
    }

    if (!buffer.empty()) {
        putString(buffer);
    }
    flush();
    //fflush(stdout); // temp
    return;
}

void TextualOutputPort::putDatum(Object o, bool inList /* = false */)
{
    // todo more faster code
    if (o.isTrue()) {
        putString(UC("#t"));
    } else if (o.isFalse()) {
        putString(UC("#f"));
    } else if (o.isNil()) {
        putString(UC("()"));
    } else if (o.isUndef()) {
        putString(UC("#<unspecified>"));
    } else if (o.isUnbound()) {
        putString(UC("#<unbound variable>"));
    } else if (o.isEof()) {
        putString(UC("#<eof>"));
    } else if (o.isCallable()) {
        putString(UC("callable"));
    } else if (o.isFixnum()) {
        static char buf[32];
        snprintf(buf, 32, "%ld", o.toFixnum());
        putString(buf);
    } else if (o.isFlonum()) {
        Flonum* const flonum = o.toFlonum();
        static char buf[512];
        if (flonum->isNan()) {
            putString(UC("+nan.0"));
        } else if (flonum->isInfinite()) {
            if (flonum->value() > 0.0) {
                putString(UC("+inf.0"));
            } else {
                putString(UC("-inf.0"));
            }
        } else {
//            double val = flonum->value();
// (log 0.0)
//            snprintf(buf, sizeof(buf), "%.20g", flonum->value());
            snprintf(buf, sizeof(buf), "%f", flonum->value());
            putString(buf);
        }
    } else if (o.isInstruction()) {
        static char buf[32];
        snprintf(buf, 32, "[insn %d]", o.toInstruction());
        putString(buf);
    } else if (o.isCompilerInstruction()) {
        static char buf[32];
        snprintf(buf, 32, "[comp:%d]", o.toCompilerInstruction());
        putString(buf);
    } else if (o.isChar()) {
        putString(UC("#\\"));
        ucs4char c = o.toChar();
        switch (c) {
        case 0:
            putString(UC("nul"));
            break;
        case ' ':
            putString(UC("space"));
            break;
        case '\n':
            // R6RS (4.2.6 Characters) says: "The #\newline notation is retained for backward compatibility. Its
            // use is deprecated; #\linefeed should be used instead."
            putString(UC("linefeed"));
            break;
        case '\a':
            putString(UC("alarm"));
            break;
        case '\b':
            putString(UC("backspace"));
            break;
        case '\t':
            putString(UC("tab"));
            break;
        case '\v':
            putString(UC("vtab"));
            break;
        case 0x0C:
            putString(UC("page"));
            break;
        case 0x0D:
            putString(UC("return"));
            break;
        case 0x1B:
            putString(UC("esc"));
            break;
        case 0x7F:
            putString(UC("delete"));
            break;

        default:
            putChar(c);
        }
    } else if (o.isString()) {
        const ucs4char DOUBLE_QUOTE = '\"';
        const ucs4char ESCAPSE      = '\\';

        // Escape patterns.
        //   [a][\n][b]                           => [\"][a][\\][n][b][\"]
        //   [a][\\][\n][b]                       => [\"][a][\\][\\][\\][n][b][\"]
        //   [a][\"][b][\"][c]                    => [\"][a][\\][\"][b][\\][\"][c]
        //   [a][\"][b][\\][\"][c][\\][\"][\"][d] => [\"][a][\\][\"][[b][\\][\\][\\][\"][c][\\][\\][\\][\"][\\][\"][d]
        ucs4string& s = o.toString()->data();
        putChar(DOUBLE_QUOTE);
        for (size_t i = 0; i < s.size(); i++) {
            const ucs4char ch = s[i];
            switch(ch) {
            case(ESCAPSE):
                putChar(ESCAPSE);
                putChar(ESCAPSE);
                break;
            case '\n':
                putChar(ESCAPSE);
                putChar('n');
                break;
            case '\a':
                putChar(ESCAPSE);
                putChar('a');
                break;
            case '\b':
                putChar(ESCAPSE);
                putChar('b');
                break;
            case '\t':
                putChar(ESCAPSE);
                putChar('t');
                break;
            case '\v':
                putChar(ESCAPSE);
                putChar('v');
                break;
            case '\r':
                putChar(ESCAPSE);
                putChar('r');
                break;

            case DOUBLE_QUOTE:
                putChar(ESCAPSE);
                putChar(DOUBLE_QUOTE);
                break;
            default:
                putChar(ch);
            }
        }
        putChar(DOUBLE_QUOTE);
    } else if (o.isPair()) {
        putPair(o, inList);
    } else if (o.isVector()) {
        Vector* v = o.toVector();
        putString(UC("#("));
        for (int i = 0; i < v->length(); i++) {
            putDatum(v->ref(i));
            if (i != v->length() - 1) putChar(' ');
        }
        putString(UC(")"));
    } else if (o.isSymbol()) {
        Symbol* symbol = o.toSymbol();
//        Object s = symbol->toString();
        const ucs4string& content = symbol->c_str();//s.toString()->data();
        bool hasSpecial = content.find(' ') != ucs4string::npos;
        if (hasSpecial) {
            putChar('|');
        }
        putString(symbol->c_str());
        if (hasSpecial) {
            putChar('|');
        }
    } else if (o.isRegexp()) {
        putChar('#');
        putChar('/');
        putString(o.toRegexp()->pattern());
        putChar('/');
    } else if (o.isRegMatch()) {
        putString(UC("#<reg-match>"));
    } else if (o.isEqHashTable()) {
        putString(UC("#<eq-hash-table>"));
    } else if (o.isClosure()) {
        putString(UC("#<closure "));
        putDatum(Object::makeFixnum(o.val));
        putString(UC(">"));
    } else if (o.isCProcedure()) {
        putDatum(getCProcedureName(o));
    } else if (o.isByteVector()) {
        ByteVector* const byteVector = o.toByteVector();
        const int length = byteVector->length();
        putString(UC("#vu8("));
        for (int i = 0; i < length; i++) {
            if (i != 0) {
                putString(" ");
            }
            putDatum(Object::makeFixnum(byteVector->u8Ref(i)));
        }
        putString(UC(")"));
    } else if (o.isBox()) {
        putString(UC("#<box>"));
    } else if (o.isTextualOutputPort()) {
        putString(o.toTextualOutputPort()->toString());
    } else if (o.isStack()) {
        putString(UC("#<stack>"));
    } else if (o.isCodec()) {
        Codec* codec = o.toCodec();
        putString(UC("#<codec "));
        putString(codec->getCodecName());
        putString(UC(">"));
    } else if (o.isBinaryInputPort()) {
        putString(o.toBinaryInputPort()->toString().data());
    } else if (o.isBinaryOutputPort()) {
        putString(o.toBinaryOutputPort()->toString().data());
    } else if (o.isBinaryInputOutputPort()) {
        putString(o.toBinaryInputOutputPort()->toString().data());
    } else if (o.isRecordConstructorDescriptor()) {
        putString(UC("#<record-constructor-descriptor>"));
    } else if (o.isRecordTypeDescriptor()) {
        putString(UC("#<record-type-descriptor>"));
    } else if (o.isCompoundCondition()) {
        putString(UC("#<compound-condition "));
        CompoundCondition* const c = o.toCompoundCondition();
        const ObjectVector& conditions = c->conditions();
        for (ObjectVector::const_iterator it = conditions.begin(); it != conditions.end(); ++it) {
            putDatum(*it);
            putString(UC(" "));
        }
        putString(UC(">"));
    } else if (o.isRecord()) {
        Record* const record = o.toRecord();
        putString(UC("#<record "));
        putDatum(record->recordTypeDescriptor()->name(), inList);
        putString(UC(" "));
//         for (int i = 0; i < record->fieldsLength(); i++) {
//             putDatum(record->fieldAt(i));
//             putString(UC(" "));
//         }
        putString(UC(">"));
    } else if (o.isObjectPointer()) {
        putString(UC("#<object pointer>"));
    } else if (o.isTextualInputPort()) {
        putString(o.toTextualInputPort()->toString());
    } else if (o.isTextualOutputPort()) {
        putString(UC("#<textual-output-port>"));
    } else if (o.isRatnum()) {
        putString(o.toRatnum()->toString());
    } else if (o.isBignum()) {
        putString(o.toBignum()->toString());
    } else if (o.isCompnum()) {
        Compnum* const c = o.toCompnum();
        const Object real = c->real();
        const Object imag = c->imag();
        if (!Arithmetic::isExactZero(real)) {
            putDatum(real);
        }
        if (Arithmetic::ge(imag, Object::makeFixnum(0)) &&
            !(imag.isFlonum() && (imag.toFlonum()->isNegativeZero() || (imag.toFlonum()->isInfinite())))) {
            putString(UC("+"));
        } else {
        }
        putDatum(imag);
        putString(UC("i"));
    } else if (o.isCodeBuilder()) {
        putString(UC("<code-builder "));
        putDatum(Object::makeFixnum(o.val));
        putString(UC(">"));
    } else if (o.isTranscoder()) {
        Transcoder* transcoder = o.toTranscoder();
        putString(UC("<transcoder codec="));
        putDatum(transcoder->codec());
        putString(UC(", eol-style="));
        putDatum(transcoder->eolStyleSymbol());
        putString(UC(", error-handling-mode="));
        putDatum(transcoder->errorHandlingModeSymbol());
        putString(UC(">"));

    } else {
        putString(UC("#<unknown datum>"));
    }

    // temp
//     fflush(stdout);
//     fflush(stderr);
}

void TextualOutputPort::display(Object o, bool inList /* = false */)
{
    // todo more faster code
    if (o.isTrue()) {
        putString(UC("#t"));
    } else if (o.isFalse()) {
        putString(UC("#f"));
    } else if (o.isNil()) {
        putString(UC("()"));
    } else if (o.isUndef()) {
        putString(UC("#<unspecified>"));
    } else if (o.isUnbound()) {
        putString(UC("#<unbound variable>"));
    } else if (o.isEof()) {
        putString(UC("#<eof>"));
    } else if (o.isFixnum()) {
        static char buf[32];
        snprintf(buf, 32, "%ld", o.toFixnum());
        putString(buf);
    } else if (o.isCallable()) {
        putString(UC("callable"));
    } else if (o.isFlonum()) {
        Flonum* const flonum = o.toFlonum();
        static char buf[256];
        if (flonum->isNan()) {
            putString(UC("+nan.0"));
        } else if (flonum->isInfinite()) {
            if (flonum->value() > 0.0) {
                putString(UC("+inf.0"));
            } else {
                putString(UC("-inf.0"));
            }
        } else {
//            snprintf(buf, sizeof(buf), "%.20g", flonum->value());
            snprintf(buf, sizeof(buf), "%f", flonum->value());
            putString(buf);
        }
    } else if (o.isInstruction()) {
        static char buf[32];
        snprintf(buf, 32, "[insn %d]", o.toInstruction());
        putString(buf);
    } else if (o.isCompilerInstruction()) {
        static char buf[32];
        snprintf(buf, 32, "[comp:%d]", o.toInstruction());
        putString(buf);
    } else if (o.isChar()) {
        putChar(o.toChar());
    } else if (o.isString()) {
        putString(o.toString());
    } else if (o.isPair()) {
        putPair(o, inList);
    } else if (o.isVector()) {
        Vector* v = o.toVector();
        putString(UC("#("));
        for (int i = 0; i < v->length(); i++) {
            putDatum(v->ref(i));
            if (i != v->length() - 1) putChar(' ');
        }
        putString(UC(")"));
    } else if (o.isSymbol()) {
        Symbol* symbol = o.toSymbol();
//        Object s = symbol->toString();
        const ucs4string& content = symbol->c_str();
        bool special = content.find(' ') != ucs4string::npos;
        if (special) {
            putChar('|');
        }
        putString(symbol->c_str());
        if (special) {
            putChar('|');
        }
    } else if (o.isRegexp()) {
        putChar('#');
        putChar('/');
        putString(o.toRegexp()->pattern());
        putChar('/');
    } else if (o.isRegMatch()) {
        putString(UC("#<reg-match>"));
    } else if (o.isEqHashTable()) {
        putString(UC("#<eq-hash-table>"));
    } else if (o.isClosure()) {
        putString(UC("#<closure "));
        putDatum(Object::makeFixnum(o.val));
        putString(UC(">"));
    } else if (o.isCProcedure()) {
        putDatum(getCProcedureName(o));
    } else if (o.isByteVector()) {
        ByteVector* const byteVector = o.toByteVector();
        const int length = byteVector->length();
        putString(UC("#vu8("));
        for (int i = 0; i < length; i++) {
            if (i != 0) {
                putString(" ");
            }
            putDatum(Object::makeFixnum(byteVector->u8Ref(i)));
        }
        putString(UC(")"));
    } else if (o.isBox()) {
        putString(UC("#<box>"));
    } else if (o.isTextualInputPort()) {
        putString(o.toTextualInputPort()->toString());
    } else if (o.isStack()) {
        putString(UC("#<stack>"));
    } else if (o.isCodec()) {
        putString(UC("#<codec>"));
    } else if (o.isBinaryInputPort()) {
        putString(o.toBinaryInputPort()->toString().data());
    } else if (o.isBinaryOutputPort()) {
        putString(o.toBinaryOutputPort()->toString().data());
    } else if (o.isBinaryInputOutputPort()) {
        putString(o.toBinaryInputOutputPort()->toString().data());
    } else if (o.isRecordConstructorDescriptor()) {
        putString(UC("#<record-constructor-descriptor>"));
    } else if (o.isRecordTypeDescriptor()) {
        putString(UC("#<record-type-descriptor>"));
    } else if (o.isCompoundCondition()) {
        putString(UC("#<compound-condition "));
        CompoundCondition* const c = o.toCompoundCondition();
        const ObjectVector& conditions = c->conditions();
        for (ObjectVector::const_iterator it = conditions.begin(); it != conditions.end(); ++it) {
            putDatum(*it);
            putString(UC(" "));
        }
        putString(UC(">"));
    } else if (o.isRecord()) {
        Record* const record = o.toRecord();
        putString(UC("#<record "));
        putDatum(record->recordTypeDescriptor()->name(), inList);
        putString(UC(" "));
//         for (int i = 0; i < record->fieldsLength(); i++) {
//             putDatum(record->fieldAt(i));
//             putString(UC(" "));
//         }
        putString(UC(">"));
    } else if (o.isObjectPointer()) {
        putString(UC("#<object pointer>"));
    } else if (o.isTextualOutputPort()) {
        putString(o.toTextualOutputPort()->toString());
    } else if (o.isRatnum()) {
        display(o.toRatnum()->toString());
    } else if (o.isBignum()) {
        putString(o.toBignum()->toString());
    } else if (o.isCompnum()) {
        Compnum* const c = o.toCompnum();
        const Object real = c->real();
        const Object imag = c->imag();
        if (!Arithmetic::isExactZero(real)) {
            display(real);
        }
        if (Arithmetic::ge(imag, Object::makeFixnum(0)) &&
            !(imag.isFlonum() && (imag.toFlonum()->isNegativeZero() || (imag.toFlonum()->isInfinite())))) {
            putString(UC("+"));
        } else {
        }
        display(imag);
        putString(UC("i"));
    } else if (o.isCodeBuilder()) {
        putString(UC("<code-builder "));
        putDatum(Bignum::makeInteger(o.val));
        putString(UC(">"));
    } else if (o.isTranscoder()) {
        Transcoder* transcoder = o.toTranscoder();
        putString(UC("<transcoder codec="));
        putDatum(transcoder->codec());
        putString(UC(", eol-style="));
        putDatum(transcoder->eolStyleSymbol());
        putString(UC(", error-handling-mode="));
        putDatum(transcoder->errorHandlingModeSymbol());
        putString(UC(">"));

    } else {
        putString(UC("#<unknown datum>"));
    }
}

void TextualOutputPort::putPair(Object obj, bool inList /* = false */)
{
    if (!inList) {
        const Object kar = obj.car();
        if (obj.cdr().isPair()) {
            if (Symbol::UNQUOTE == kar) {
                putString(UC(","));
                putDatum(obj.second(), false);
                return;
            } else if (Symbol::UNQUOTE_SPLICING == kar) {
                putString(UC(",@"));
                putDatum(obj.second(), false);
                return;
            } else if (Symbol::QUOTE == kar) {
                putString(UC("'"));
                putDatum(obj.second(), false);
                return;
            } else if (Symbol::QUASIQUOTE == kar) {
                putString(UC("`"));
                putDatum(obj.second(), false);
                return;
            }
        }
    }

    if (obj.cdr().isPair()) {
        if (inList) {
            putDatum(obj.car(), false);
            putString(UC(" "));
            putDatum(obj.cdr(), true);
        } else {
            putString(UC("("));
            putDatum(obj.car(), false);
            putString(UC(" "));
            putDatum(obj.cdr(), true);
            putString(UC(")"));
        }
    } else if (obj.cdr().isNil()) {
        if (inList) {
            putDatum(obj.car(), false);
        } else {
            putString(UC("("));
            putDatum(obj.car(), false);
            putString(UC(")"));
        }
    } else {
        if (inList) {
            putDatum(obj.car(), false);
            putString(UC(" . "));
            putDatum(obj.cdr(), false);
        } else {
            putString(UC("("));
            putDatum(obj.car(), false);
            putString(UC(" . "));
            putDatum(obj.cdr(), false);
            putString(UC(")"));
        }
    }
}


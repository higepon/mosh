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
#include "Array.h"
#include "Symbol.h"
#include "Regexp.h"
#include "ByteVector.h"
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
#include "BinaryOutputPort.h"
#include "BinaryInputOutputPort.h"
#include "OSCompatSocket.h"
#include "OSCompatThread.h"
#include "FFI.h"
#include "SimpleStruct.h"

#ifdef _WIN32
    #define snprintf _snprintf
#endif
using namespace scheme;

TextualOutputPort::TextualOutputPort() :
    
    errorMessage_(Object::Nil),
    irritants_(Object::Nil)
{
}

TextualOutputPort::~TextualOutputPort()
= default;

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

bool TextualOutputPort::setPosition(int64_t position)
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

void TextualOutputPort::putString(const ucs4char* str)
{
    putString(ucs4string(str));
}

void TextualOutputPort::putCharHandleSpecial(ucs4char c, bool inString)
{

    const int ASCII_SPC = 32;
    const int ASCII_DEL = 127;
    if ((c != 0xa && c != 0xd && c != '\t' && c != '\a' && c != '\b' && c != '\v' && c != '\f' && c < ASCII_SPC) ||
        c == ASCII_DEL ||
        c == 0x80 ||
        c == 0xff ||
        c == 0xD7FF ||
        c == 0xE000 ||
        c == 0x10FFFF) { // todo
        char buf[32];
        if (inString) {
            snprintf(buf, sizeof(buf), "\\x%X;", c);
        } else {
            snprintf(buf, sizeof(buf), "x%X;", c);
        }
        putString(ucs4string::from_c_str(buf));
    } else {
        putChar(c);
    }
}

void TextualOutputPort::putString(const ucs4string& s)
{
    for (ucs4string::size_type i = 0; i < s.size(); i++) {
        putChar(s[i]);
    }
}

void TextualOutputPort::putString(const char* s)
{
    const size_t len = strlen(s);
    for (size_t i = 0; i < len; i++) {
        putCharHandleSpecial(s[i], true);
    }
}

Object TextualOutputPort::irritants() const
{
    return irritants_;
}

void TextualOutputPort::format(const VM* theVM, const ucs4char* fmt, Object args)
{
    format(theVM, ucs4string(fmt), args);
}

void TextualOutputPort::format(const VM* theVM, const ucs4string& fmt, Object args)
{
    ucs4string buffer(UC(""));
    for (uint32_t i = 0; i < fmt.size(); i++) {
        if (fmt[i] == '~') {
            i++;
            if (!buffer.empty()) {
                putString(buffer);
                buffer.clear();
            }
            switch (fmt[i]) {
            case '~':
                display(theVM, Object::makeChar('~'));
                break;
            case '%':
                display(theVM, Object::makeChar('\n'));
                break;
            case 'a':
            case 'A':
            case 'd':
            case 'D':
            {
                if (args.isPair()) {
                    display(theVM, args.car(), /* isSharedAware = */ true);
                    args = args.cdr();
                } else {
                    isErrorOccured_ = true;
                    errorMessage_ = Object("too few arguments for format string");
                    irritants_ = Pair::list1(Object::makeString(fmt));
                    return;
                }
                break;
            }
            case 's':
            case 'S':
            {
                if (args.isPair()) {
                    putDatum(theVM, args.car(), /* isSharedAware = */true);
                    args = args.cdr();
                } else {
                    isErrorOccured_ = true;
                    errorMessage_ = Object("too few arguments for format string");
                    irritants_ = Pair::list1(Object::makeString(fmt));
                    return;
                }
                break;
            }
            case 'w':
            case 'W':
            {
                if (args.isPair()) {
                    bool isSharedAware = true;
                    putDatum(theVM, args.car(), isSharedAware);
                    args = args.cdr();
                } else {
                    isErrorOccured_ = true;
                    errorMessage_ = Object("too few arguments for format string");
                    irritants_ = Pair::list1(Object::makeString(fmt));
                    return;
                }
                break;
            }
            // Mosh only
            case 'e':
            case 'E':
            {
                if (args.isPair()) {
                    bool isSharedAware = true;
                    display(theVM, args.car(), isSharedAware);
                    args = args.cdr();
                } else {
                    isErrorOccured_ = true;
                    errorMessage_ = Object("too few arguments for format string");
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

// borrwed from Ypsilon
bool TextualOutputPort::writeAbbreviated(Object obj)
{
    if (obj.isSymbol()) {
        if (obj == Symbol::QUOTE || obj == Symbol::QUOTE_B) {
            putChar('\'');
            return true;
        } else if (obj == Symbol::UNQUOTE || obj == Symbol::UNQUOTE_B) {
            putChar(',');
            return true;
        } else if (obj == Symbol::UNQUOTE_SPLICING || obj == Symbol::UNQUOTE_SPLICING_B) {
            putString(",@");
            return true;
        } else if (obj == Symbol::QUASIQUOTE || obj == Symbol::QUASIQUOTE_B) {
            putChar('`');
            return true;
        }

        // Gauche Can't read this! (psyntax)
//         } else if (obj == Symbol::SYNTAX || obj == Symbol::SYNTAX_B) {
//             putString(UC("#\'"));
//             return true;
//         } else if (obj == Symbol::UNSYNTAX || obj == Symbol::UNSYNTAX_B) {
//             putString(UC("#,"));
//             return true;
//         } else if (obj == Symbol::UNSYNTAX_SPLICING || obj == Symbol::UNSYNTAX_SPLICING_B) {
//             putString("#,@");
//             return true;
//         } else if (obj == Symbol::QUASISYNTAX || obj == Symbol::QUASISYNTAX_B) {
//             putString(UC("#`"));
//             return true;
//         }

    }
    return false;
}

template<bool isHumanReadable> void TextualOutputPort::print(const VM* theVM, Object o, EqHashTable* seen)
{
    if (seen != nullptr) {
        Object seenState = seen->ref(o, Object::False);
        if (seenState.isTrue()) {
            seen->set(o, Object::makeFixnum(sharedId_));
            char buf[32];
            snprintf(buf, 32, "#%d=", sharedId_);
            putString(buf);
            sharedId_++;
        } else if (seenState.isFixnum()) {
            char buf[32];
            snprintf(buf, 32, "#%d#", (int)seenState.toFixnum());
            putString(buf);
            return;
        }
    }
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
        putString(UC("#<eof-object>"));
    } else if (o.isCallable()) {
        putString(UC("callable"));
    } else if (o.isFixnum()) {
        char buf[32];
        snprintf(buf, 32, "%ld", (long)o.toFixnum());
        putString(buf);
    } else if (o.isFlonum()) {
        Flonum* const flonum = o.toFlonum();
        putString(FlonumUtil::flonumToUcs4String(flonum->value(), false));
//         char buf[512];
//         if (flonum->isNan()) {
//             putString(UC("+nan.0"));
//         } else if (flonum->isInfinite()) {
//             if (flonum->value() > 0.0) {
//                 putString(UC("+inf.0"));
//             } else {
//                 putString(UC("-inf.0"));
//             }
//         } else {
//             snprintf(buf, sizeof(buf), "%f", flonum->value());
//             putString(buf);
//             #if 0
//             snprintf(buf, sizeof(buf), "%.20g", flonum->value());
//             size_t n = strcspn(buf, ".eE");
//             if (buf[n]) {
//                 putString(buf);
//             } else {
//                 strcat(buf,".0");
//                 putString(buf);
//             }
//             #endif
//         }
    } else if (o.isInstruction()) {
        char buf[32];
        snprintf(buf, 32, "[insn %d]", o.toInstruction());
        putString(buf);
    } else if (o.isCompilerInstruction()) {
        char buf[32];
        snprintf(buf, 32, "[comp:%d]", o.toCompilerInstruction());
        putString(buf);
    } else if (o.isChar()) {
        if (isHumanReadable) {
            putCharHandleSpecial(o.toChar(), false);
        } else { // isHumanReadable = false
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
                putCharHandleSpecial(c, false);
            }
        }
    } else if (o.isString()) {
        if (isHumanReadable) {
            putString(o.toString());
        } else {
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
                case '\f':
                    putChar(ESCAPSE);
                    putChar('f');
                    break;
                case DOUBLE_QUOTE:
                    putChar(ESCAPSE);
                    putChar(DOUBLE_QUOTE);
                    break;
                default:
                    putCharHandleSpecial(ch, true);
                }
            }
            putChar(DOUBLE_QUOTE);
        }
    } else if (o.isPair()) {
        bool abbreviated = o.cdr().isPair() && o.cdr().cdr().isNil() && writeAbbreviated(o.car());
        if (abbreviated) {
            o = o.cdr();
        } else {
            putChar('(');
        }
        print<isHumanReadable>(theVM, o.car(), seen);
        if (seen == nullptr) {
            for (Object e = o.cdr(); ; e = e.cdr()) {
                if (e.isPair() && e.car() == Symbol::UNQUOTE) {
                    if (e.cdr().isPair() && e.cdr().cdr().isNil()) {
                        putString(" . ,");
                        print<isHumanReadable>(theVM, e.cdr().car(), seen);
                        putChar(')');
                        return;
                    }
                }
                if (e.isPair()) {
                    putChar(' ');
                    print<isHumanReadable>(theVM, e.car(), seen);
                } else if (e.isNil()) {
                    break;
                } else {
                    putString(" . ");
                    print<isHumanReadable>(theVM, e, seen);
                    break;
                }
            }
        } else {
            for (Object e = o.cdr(); ; e = e.cdr()) {
                Object seenState2 = seen->ref(e, Object::False);

                if (e.isPair() && e.car() == Symbol::UNQUOTE) {
                    if (e.cdr().isPair() && e.cdr().cdr().isNil()) {
                        putString(" . ,");
                        print<isHumanReadable>(theVM, e.cdr().car(), seen);
                        putChar(')');
                        return;
                    }
                }
                if (e.isPair() && seenState2.isFalse()) {
                    putChar(' ');
                    print<isHumanReadable>(theVM, e.car(), seen);
                } else if (e.isNil()) {
                    break;
                } else {
                    putString(" . ");
                    print<isHumanReadable>(theVM, e, seen);
                    break;
                }
            }
        }
        if (!abbreviated) {
            putChar(')');
        }

    } else if (o.isVector()) {
        Vector* v = o.toVector();
        putString(UC("#("));
        for (size_t i = 0; i < v->length(); i++) {
            print<isHumanReadable>(theVM, v->ref(i), seen);
            if (i != v->length() - 1) putChar(' ');
        }
        putString(UC(")"));
    } else if (o.isSymbol()) {
        Symbol* symbol = o.toSymbol();
        const ucs4string& content = ucs4string(symbol->c_str());
        const ucs4char start = content[0];
        const bool isBarSymbol = (start ==  '|') && (content[content.length() - 1] == '|');
        if ((start >= '0' && start <= '9') || (start == ' ')) {
            char buf[16];
            snprintf(buf, 16, "\\x%x;", start);
            putString(buf);
        } else {
            putChar(start);
        }

        for (uintptr_t i = 1; i < content.size(); i++) {
            const ucs4char ch = content[i];
            // not enough
            if (!isBarSymbol && ch == ' ') {
                char buf[16];
                snprintf(buf, 16, "\\x%x;", ch);
                putString(buf);
            } else {
                putChar(ch);
            }
        }
    } else if (o.isRegexp()) {
        putChar('#');
        putChar('/');
        putString(o.toRegexp()->pattern());
        putChar('/');
    } else if (o.isSocket()) {
        putString(o.toSocket()->toString());
    } else if (o.isRegMatch()) {
        putString(UC("#<reg-match>"));
    } else if (o.isEqHashTable()) {
        putString(UC("#<eq-hashtable>"));
    } else if (o.isEqvHashTable()) {
        putString(UC("#<eqv-hashtable>"));
    } else if (o.isGenericHashTable()) {
        putString(UC("#<hashtable>"));
    } else if (o.isClosure()) {
        putString(UC("#<closure "));
        print<isHumanReadable>(theVM, Object::makeFixnum(static_cast<int>(o.val)), seen);
        putString(UC(">"));
    } else if (o.isCProcedure()) {

        // Reader.y doesn't have VM instance.
        if (theVM != nullptr) {
            putString(UC("#<subr "));
            print<isHumanReadable>(theVM, theVM->getCProcedureName(o), seen);
            putString(UC(">"));
        } else {
            putString(UC("#<subr>"));
        }
    } else if (o.isByteVector()) {
        ByteVector* const byteVector = o.toByteVector();
        const size_t length = byteVector->length();
        putString(UC("#vu8("));
        for (size_t i = 0; i < length; i++) {
            if (i != 0) {
                putString(" ");
            }
            print<isHumanReadable>(theVM, Object::makeFixnum(byteVector->u8Ref(i)), seen);
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
    } else if (o.isSimpleStruct()) {
        putString(UC("#<"));
        print<isHumanReadable>(theVM, o.toSimpleStruct()->name(), seen);
        for (int i = 0; i < o.toSimpleStruct()->fieldCount(); i++) {
            print<isHumanReadable>(theVM, o.toSimpleStruct()->ref(i), seen);
            putString(UC(" "));
        }

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
    } else if (o.isVM()) {
        putString(o.toVM()->toString());
    } else if (o.isF64Array()) {
        putString(o.toF64Array()->toString());        
#ifndef MONA
    } else if (o.isConditionVariable()) {
        putString(o.toConditionVariable()->toString());
#endif
    } else if (o.isMutex()) {
        putString(UC("#<mutex>"));
    } else if (o.isCompnum()) {
        Compnum* const c = o.toCompnum();
        const Object real = c->real();
        const Object imag = c->imag();
        if (!Arithmetic::isExactZero(real)) {
            print<isHumanReadable>(theVM, real, seen);
        }
        if (Arithmetic::ge(imag, Object::makeFixnum(0)) &&
            !(imag.isFlonum() && (imag.toFlonum()->isNegativeZero() || (imag.toFlonum()->isInfinite())))) {
            putString(UC("+"));
        } else {
        }
        print<isHumanReadable>(theVM, imag, seen);
        putString(UC("i"));
    } else if (o.isCodeBuilder()) {
        putString(UC("<code-builder "));
        print<isHumanReadable>(theVM, Object::makeFixnum(static_cast<int>(o.val)), seen);
        putString(UC(">"));
    } else if (o.isTranscoder()) {
        Transcoder* transcoder = o.toTranscoder();
        putString(UC("<transcoder codec="));
        print<isHumanReadable>(theVM, transcoder->codec(), seen);
        putString(UC(", eol-style="));
        print<isHumanReadable>(theVM, transcoder->eolStyleSymbol(), seen);
        putString(UC(", error-handling-mode="));
        print<isHumanReadable>(theVM, transcoder->errorHandlingModeSymbol(), seen);
        putString(UC(">"));
    } else if (o.isPointer()) {
        putString(UC("#<pointer "));
        char buf[16];
        snprintf(buf, 16, "%p", (void*)o.toPointer()->pointer());
        putString(buf);
        putString(UC(">"));
    } else if (o.isContinuation()) {
        putString(UC("#<continuation>"));
    } else if (o.isSharedReference()) {
        putString(UC("#<shared reference>"));
    } else {
        putString(UC("#<unknown datum>"));
    }
}

bool TextualOutputPort::isInteresting(Object obj)
{
    return // obj.isString() || obj.isSymbol() || 
        obj.isPair() || obj.isVector()
        || obj.isSimpleStruct() || obj.isEqHashTable();
}

void TextualOutputPort::scan(Object obj, EqHashTable* seen)
{
loop:
    if (!isInteresting(obj)) {
        return;
    }
    const Object val = seen->ref(obj, Object::Ignore);
    if (val.isFalse()) {
        seen->set(obj, Object::True);
        return;
    } else if (val.isTrue()) {
        return;
    } else {
        seen->set(obj, Object::False);
        if (obj.isPair()) {
            scan(obj.car(), seen);
            obj = obj.cdr();
            goto loop;
        } else if (obj.isVector()) {
            Vector* const v = obj.toVector();
            for (size_t i = 0; i < v->length(); i++) {
                scan(v->ref(i), seen);
            }
        } else if (obj.isEqHashTable()) {
            EqHashTable* const ht = obj.toEqHashTable();
            Vector* const keys = ht->keys().toVector();
            const size_t length = keys->length();
            for (size_t i = 0; i < length; i++) {
                const Object key = keys->ref(i);
                scan(key, seen);
                scan(ht->ref(key, Object::False), seen);
            }
        } else if (obj.isSimpleStruct()) {
            SimpleStruct* const record = obj.toSimpleStruct();
            scan(record->name(), seen);
            const int length = record->fieldCount();
            for (int i = 0; i < length; i++) {
                scan(record->ref(i), seen);
            }
        }
    }
}

void TextualOutputPort::display(const VM* theVM, Object o, bool isSharedAware)
{
    if (isSharedAware) {
        EqHashTable seen;
        scan(o, &seen);
        sharedId_ = 1;
        print<true>(theVM, o, &seen);
    } else {
        print<true>(theVM, o, nullptr);
    }
}

void TextualOutputPort::display(const VM* theVM, const ucs4char* o, bool isSharedAware)
{
    display(theVM, Object(o), isSharedAware);
}

void TextualOutputPort::putDatum(const VM* theVM, Object o, bool isSharedAware)
{
    if (isSharedAware) {
        EqHashTable seen;
        scan(o, &seen);
        sharedId_ = 1;
        print<false>(theVM, o, &seen);
    } else {
        print<false>(theVM, o, nullptr);
    }
}

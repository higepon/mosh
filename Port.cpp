/*
 * Port.cpp - port
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

#include "Port.h"
#include "VM.h"
using namespace scheme;

extern VM* theVM;

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
                    irritants_ = L1(Object::makeString(fmt));
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
                    irritants_ = L1(Object::makeString(fmt));
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
    fflush(stdout); // temp
    return;
}


bool scheme::fileExistsP(const ucs4string& file)
{
    FILE* stream = fopen(file.ascii_c_str(), "rb");
    if (NULL == stream) {
        return false;
    } else {
        fclose(stream);
        return true;
    }
}

FileBinaryOutputPort::FileBinaryOutputPort(ucs4string file)
{
    stream_ = fopen(file.ascii_c_str(), "w");
}

int FileBinaryOutputPort::open()
{
    if (NULL == stream_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
}


FileBinaryInputPort::FileBinaryInputPort(ucs4string file) : fileName_(file)
{
    stream_ = fopen(file.ascii_c_str(), "rb");
}

FileBinaryInputPort::FileBinaryInputPort(const char* file)
{
    fileName_ = Object::makeString(file).toString()->data();
    stream_ = fopen(file, "rb");
}

int FileBinaryInputPort::open()
{
    if (NULL == stream_) {
        return MOSH_FAILURE;
    } else {
        return MOSH_SUCCESS;
    }
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
    } else if (o.isInt()) {
        static char buf[32];
        snprintf(buf, 32, "%ld", o.toInt());
        putString(buf);
    } else if (o.isInstruction()) {
        static char buf[32];
        snprintf(buf, 32, "%d", o.toInstruction());
        putString(buf);
    } else if (o.isChar()) {
        putString(UC("#\\"));
        ucs4char c = o.toChar();
        switch (c) {
        case '0':
            putString(UC("nul"));
            break;
        case ' ':
            putString(UC("space"));
            break;
        case '\n':
            putString(UC("newline"));
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
        putDatum(Object::makeInt(o.val));
        putString(UC(">"));
    } else if (o.isCProcedure()) {
        putString(UC("#<subr>"));
    } else if (o.isByteVector()) {
        putString(UC("#<byte vector>"));
    } else if (o.isBox()) {
        putString(UC("#<box>"));
    } else if (o.isInputFilePort()) {
        putString(UC("#<input file port>"));
    } else if (o.isTextualInputPort()) {
        putString(UC("#<textual input port>"));
    } else if (o.isStack()) {
        putString(UC("#<stack>"));
    } else if (o.isCodec()) {
        putString(UC("#<codec>"));
    } else if (o.isBinaryInputPort()) {
        putString(UC("#<binary input port>"));
    } else if (o.isRecordConstructorDescriptor()) {
        putString(UC("#<record-constructor-descriptor>"));
    } else if (o.isRecordTypeDescriptor()) {
        putString(UC("#<record-type-descriptor>"));
    } else if (o.isCompoundCondition()) {
        putString(UC("#<compound-condition>"));
    } else if (o.isRecord()) {
        putString(UC("#<record "));
        putDatum(o.toRecord()->recordTypeDescriptor()->name(), inList);
        putString(UC(">"));
    } else if (o.isObjectPointer()) {
        putString(UC("#<object pointer>"));
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
    } else if (o.isInt()) {
        static char buf[32];
        snprintf(buf, 32, "%ld", o.toInt());
        putString(buf);
    } else if (o.isInstruction()) {
        static char buf[32];
        snprintf(buf, 32, "%d", o.toInstruction());
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
        putDatum(Object::makeInt(o.val));
        putString(UC(">"));
    } else if (o.isCProcedure()) {
        putString(UC("#<subr>"));
    } else if (o.isByteVector()) {
        putString(UC("#<byte vector>"));
    } else if (o.isBox()) {
        putString(UC("#<box>"));
    } else if (o.isInputFilePort()) {
        putString(UC("#<input file port>"));
    } else if (o.isTextualInputPort()) {
        putString(UC("#<textual input port>"));
    } else if (o.isStack()) {
        putString(UC("#<stack>"));
    } else if (o.isCodec()) {
        putString(UC("#<codec>"));
    } else if (o.isBinaryInputPort()) {
        putString(UC("#<binary input port>"));
    } else if (o.isRecordConstructorDescriptor()) {
        putString(UC("#<record-constructor-descriptor>"));
    } else if (o.isRecordTypeDescriptor()) {
        putString(UC("#<record-type-descriptor>"));
    } else if (o.isCompoundCondition()) {
        putString(UC("#<compound-condition>"));
    } else if (o.isRecord()) {
        putString(UC("#<record "));
        putDatum(o.toRecord()->recordTypeDescriptor()->name(), inList);
        putString(UC(">"));
    } else if (o.isObjectPointer()) {
        putString(UC("#<object pointer>"));


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

int CustomBinaryInputPort::getU8()
{
    const Object bv = Object::makeByteVector(1);
    const Object start = Object::makeInt(0);
    const Object count = Object::makeInt(1);
    const Object result = theVM->callClosure3(readProc_, bv, start, count);
    if (0 == result.toInt()) {
        return EOF;
    }
    return bv.toByteVector()->u8RefI(0);
}

ByteVector* CustomBinaryInputPort::getByteVector(int size)
{
    fprintf(stderr, "get-byte-vector-n not implemented");
    exit(-1);
}

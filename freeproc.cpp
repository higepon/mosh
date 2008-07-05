/*
 * freeproc.cpp - procedures refereced as free variable.
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

#include "freeproc.h"
#include "VM.h"

extern scheme::VM* theVM;

using namespace scheme;

Object scheme::currentErrorPortEx(int argc, const Object* argv)
{
    printf("current-error-port called\n");
    return Object::UnBound;
}

Object scheme::hashtableKeysEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "hash-table-keys");
    const Object ht = argv[0];
    if (!ht.isEqHashTable()) {
        VM_RAISE1("hashtable-keys hash-table required, but got ~a\n", ht);
    }
    return ht.toEqHashTable()->keys();
}
Object scheme::hashtableSetDEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "hash-table-set!");
    const Object ht = argv[0];
    if (!ht.isEqHashTable()) {
        VM_RAISE1("hashtable-set! hash-table required, but got ~a\n", ht);
    }

    const Object key = argv[1];
    const Object val = argv[2];
    ht.toEqHashTable()->set(key, val);
    return Object::Undef;
}

Object scheme::hashtableRefEx(int argc, const Object* argv)
{
    if (argc == 2 || argc == 3) {
        const Object ht = argv[0];
        if (!ht.isEqHashTable()) {
            VM_RAISE1("hashtable-ref hash-table required, but got ~a\n", ht);
        }
        const Object key = argv[1];
        const Object defaultVal = (argc == 3 ? argv[2] : Object::False);
        return ht.toEqHashTable()->ref(key, defaultVal);
    } else {
        VM_RAISE1("wrong number of arguments for hash-table-get (required 2 or 3, got ~d)\n", Object::makeInt(argc));
    }
    return Object::UnBound;
}

Object scheme::makeEqHashtableEx(int argc, const Object* argv)
{
    checkArgLength(0, argc, "make-eq-hashtable");
    return Object::makeEqHashTable();
}

Object scheme::numberPEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "number?");
    RETURN_BOOL(argv[0].isInt());
}

Object scheme::consEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "cons");
    return Object::cons(argv[0], argv[1]);
}

Object scheme::carEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "car");
    const Object p = argv[0];
    if (p.isPair()) {
        return p.car();
    } else {
        VM_RAISE1("car pair required, but got ~a\n", p);
    }
    return Object::Undef;
}

Object scheme::cdrEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "cdr");
    const Object p = argv[0];
    if (p.isPair()) {
        return p.cdr();
    } else {
        VM_RAISE1("cdr pair required, but got ~a\n", p);
    }
    return Object::Undef;
}

Object scheme::sourceInfoEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "source-info");
    const Object arg = argv[0];
    if (arg.isPair()) {
        return arg.sourceInfo();
    } else if (arg.isClosure()) {
        return arg.toClosure()->sourceInfo;
    } else {
        return Object::False;
    }
}

Object scheme::setSourceInfoDEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "set-source-info!");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    if (arg1.isPair()) {
        arg1.toPair()->sourceInfo = arg2;
    } else {
        VM_RAISE1("set-source-info! pair required, but got ~a\n", arg1);
    }
    return arg1;
}

Object scheme::nullPEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "null?!");
    if (argc > 1) {
        VM_RAISE1("wrong number of arguments for display required at most 1, got ~d)\n", Object::makeInt(argc));
    }
    if (1 == argc) {
        return Object::makeBool(argv[0].isNil());
    } else {
        return Object::False;
    }
}

Object scheme::setCarDEx(int argc, const Object* argv)
{
    printf("set-car! called\n");
    return Object::UnBound;
}

Object scheme::setCdrDEx(int argc, const Object* argv)
{
    printf("set-cdr! called\n");
    return Object::UnBound;
}

Object scheme::sysDisplayEx(int argc, const Object* argv)
{
    if (argc < 1) {
        VM_RAISE1("wrong number of arguments for display required at least 1, got ~d)\n", Object::makeInt(argc));
    }
    const Object arg1 = argv[0];
    if (1 == argc) {
        theVM->getOutputPort().display(arg1);
    } else {
        const Object arg2 = argv[1];
        if (!arg2.isTextualOutputPort()) {
            VM_RAISE1("display <textual-output-port> required, but got ~a\n", arg2);
        }
        arg2.toTextualOutputPort()->display(arg1);
    }
    return Object::Undef;
}

Object scheme::rxmatchEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "rxmatch");
    const Object arg1 = argv[0];
    if (!arg1.isRegexp()) {
        VM_RAISE1("rxmatch regexp required, but got ~a\n", arg1);
    }

    const Object arg2 = argv[1];
    if (!arg2.isString()) {
        VM_RAISE1("rxmatch string required, but got ~a\n", arg2);
    }
    return arg1.toRegexp()->match(arg2.toString()->data());
}

Object scheme::regexpPEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "regexp?");
    RETURN_BOOL(argv[0].isRegexp());
}

Object scheme::regexpTostringEx(int argc, const Object* argv)
{
    const Object arg1 = argv[0];
    if (!arg1.isRegexp()) {
        VM_RAISE1("regexp->string regexp required, but got ~a\n", arg1);
    }
    return Object::makeString(arg1.toRegexp()->pattern());
}

Object scheme::rxmatchStartEx(int argc, const Object* argv)
{
    if (argc < 1) {
        VM_RAISE1("wrong number of arguments for rxmatch-start required at least 1, got ~d)\n", Object::makeInt(argc));
    }
    const Object r = argv[0];
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-start regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (argc == 2) {
        const Object i = argv[1];
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-start number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return Object::makeInt(r.toRegMatch()->matchStart(index));
}

Object scheme::rxmatchEndEx(int argc, const Object* argv)
{
    if (argc < 1) {
        VM_RAISE1("wrong number of arguments for rxmatch-start required at least 1, got ~d)\n", Object::makeInt(argc));
    }
    const Object r = argv[0];
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-end regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (2 == argc) {
        const Object i = argv[1];
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-end number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return Object::makeInt(r.toRegMatch()->matchEnd(index));
}

Object scheme::rxmatchAfterEx(int argc, const Object* argv)
{
    if (argc < 1) {
        VM_RAISE1("wrong number of arguments for rxmatch-after required at least 1, got ~d)\n", Object::makeInt(argc));
    }

    const Object r = argv[0];
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-after regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (2 == argc) {
        const Object i = argv[1];
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-after number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return r.toRegMatch()->matchAfter(index);
}

Object scheme::rxmatchBeforeEx(int argc, const Object* argv)
{
    if (argc < 1) {
        VM_RAISE1("wrong number of arguments for rxmatch-after required at least 1, got ~d)\n", Object::makeInt(argc));
    }
    const Object r = argv[0];
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-before regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (2 == argc) {
        const Object i = argv[1];
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-before number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return r.toRegMatch()->matchBefore(index);
}

Object scheme::rxmatchSubstringEx(int argc, const Object* argv)
{
    if (argc < 1) {
        VM_RAISE1("wrong number of arguments for rxmatch-substring required at least 1, got ~d)\n", Object::makeInt(argc));
    }
    printf("rxmatch-substring %d \n", argc);
    const Object r = argv[0];
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-substring regexp required, but got ~a\n", r);
    }
    int index = 0;
    if (2 == argc) {
        const Object i = argv[1];
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-substring number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return r.toRegMatch()->matchSubString(index);
}

Object scheme::regMatchProxy(int argc, const Object* argv)
{
    const Object match = argv[0];
    if (argc == 2 && argv[1] == Symbol::AFTER) {
        return rxmatchAfterEx(1, argv);
    } else if (argc == 2 && argv[1] == Symbol::BEFORE) {
        return rxmatchBeforeEx(1, argv);
    } else {
        return rxmatchSubstringEx(argc, argv);
    }
}

Object scheme::makeStringEx(int argc, const Object* argv)
{
    if (argc < 1) {
        VM_RAISE1("wrong number of arguments for make-string required at least 1, got ~d)\n", Object::makeInt(argc));
    }
    const Object arg1 = argv[0];
    if (!arg1.isInt()) {
        VM_RAISE1("make-string number required, but got ~a\n", arg1);
    }

    if (argc == 2) {
        const Object arg2 = argv[1];
        if (!arg2.isChar()) {
            VM_RAISE1("make-string char required, but got ~a\n", arg2);
        }
        return Object::makeString(arg1.toInt(), arg2.toChar());
    } else {
        return Object::makeString(arg1.toInt());
    }
}

Object scheme::stringSetDEx(int argc, const Object* argv)
{
    if (argc != 3) {
        VM_RAISE1("string-set! 3 arguments required, but got ~d\n", Object::makeInt(argc));
    } else if (!argv[0].isString()) {
        VM_RAISE1("string-set! string required, but got ~a\n", argv[0]);
    } else if (!argv[1].isInt()) {
        VM_RAISE1("string-set! number required, but got ~a\n", argv[1]);
    } else if (!argv[2].isChar()) {
        VM_RAISE1("string-set! char required, but got ~a\n", argv[2]);
    }
    argv[0].toString()->data()[argv[1].toInt()] = argv[2].toChar();
    return Object::Undef;
}

Object scheme::stringLengthEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "string-length");
    if (argv[0].isString()) {
        return Object::makeInt(argv[0].toString()->data().length());
    } else {
        VM_RAISE1("string-length string required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::stringTosymbol(Object str)
{
    if (str.isString()) {
        return Symbol::intern(str.toString()->data().c_str());
    } else {
        VM_RAISE1("string->symbol string required, but got ~a\n", str);
    }
    return Object::Undef;
}

Object scheme::stringTosymbolEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "string->symbol");
    return stringTosymbol(argv[0]);
}

Object scheme::stringTonumberEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "string->number");
    if (argv[0].isString()) {
        return Object::makeInt(atoi(argv[0].toString()->data().ascii_c_str()));
    } else {
        VM_RAISE1("string->number string required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::stringAppendEx(int argc, const Object* argv)
{
    ucs4string ret;
    for (int i = 0; i < argc; i++) {
        if (!argv[i].isString()) {
            VM_RAISE1("string-append string required, but got ~a\n", argv[i]);
        }
        ret += argv[i].toString()->data();
    }
    return Object::makeString(ret);
}

Object scheme::makeList(gc_vector<ucs4string>& v, gc_vector<ucs4string>::size_type i)
{
    if (i == v.size()) {
        return Object::Nil;
    } else {
        return Object::cons(Object::makeString(v[i]), makeList(v, i + 1));
    }
}

Object scheme::stringSplitEx(int argc, const Object* argv)
{
    if (argc != 2) {
        VM_RAISE1("string-split 2 arguments required, but got ~d\n", Object::makeInt(argc));
    } else if (!argv[0].isString()) {
        VM_RAISE1("string-split string required, but got ~a\n", argv[0]);
    } else if (!argv[1].isChar()) {
        VM_RAISE1("string-split char required, but got ~a\n", argv[1]);
    }

    gc_vector<ucs4string> v;
    argv[0].toString()->data().split(argv[1].toChar(), v);
    return makeList(v, 0);
}

Object scheme::numberTostringEx(int argc, const Object* argv)
{
    if (argc == 2) {
        Object arg1 = argv[0];
        Object arg2 = argv[1];
        if (arg1.isInt() && arg2.isInt() && arg2.toInt() == 16) {
            static char buf[32];
            snprintf(buf, 32, "%lx", argv[0].toInt());
            return Object::makeString(buf);
        } else {
            VM_RAISE1("number->string number required, but got ~a\n", argv[0]);
        }
    }
    else if (argv[0].isInt()) {
        static char buf[32];
        snprintf(buf, 32, "%ld", argv[0].toInt());
        return Object::makeString(buf);
    } else {
        VM_RAISE1("number->string number required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::reverseIter(Object rest, Object ret)
{
    if (rest.isNil()) {
        return ret;
    } else {
        return reverseIter(rest.cdr(), Object::cons(rest.car(), ret));
    }
}

Object scheme::reverseEx(int argc, const Object* argv)
{
    return Pair::reverse(argv[0]);
}

Object scheme::eofObjectPEx(int argc, const Object* argv)
{
    RETURN_BOOL(argv[0].isEof());
}

Object scheme::readCharEx(int argc, const Object* argv)
{
    // todo
    if (argv[0].isTextualInputPort()) {
        const ucs4char c = argv[0].toTextualInputPort()->getChar();
        return c == EOF ? Object::Eof : Object::makeChar(c);
    } else {
        VM_RAISE1("read-char port required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::readEx(int argc, const Object* argv)
{
    if (0 == argc) {
        return theVM->currentInputPort().toTextualInputPort()->getDatum();
    } else if (argv[0].isTextualInputPort()) {
        return argv[0].toTextualInputPort()->getDatum();
    } else {
        VM_RAISE1("read required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::charEqPEx(int argc, const Object* argv)
{
    const Object start = argv[0];
    if (!start.isChar()) {
        return Object::False;
    }

    for (int i = 0; i < argc; i++) {
        if (!argv[i].isChar()) {
            return Object::False;
        }
        if (start != argv[i]) {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::stringPEx(int argc, const Object* argv)
{
    return Object::makeBool(argv[0].isString());
}

Object scheme::sysGetenvEx(int argc, const Object* argv)
{
    if (argv[0].isString()) {
        const char* str = getenv(argv[0].toString()->data().ascii_c_str());
        return str == NULL ? Object::False : Object::makeString(str);
    } else {
        VM_RAISE1("getenv string required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::equalPEx(int argc, const Object* argv)
{
    if (2 == argc) {
        return argv[0].equal(argv[1]);
    } else {
        VM_RAISE1("wrong number of arguments for equal? (required 2, got ~d)\n", Object::makeInt(argc));
    }
    return Object::UnBound;
}

Object scheme::openStringInputPortEx(int argc, const Object* argv)
{
    if (argv[0].isString()) {
        return Object::makeStringInputPort(argv[0].toString()->data());
    } else {
        VM_RAISE1("open-string-input-port string required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::sysOpenOutputStringEx(int argc, const Object* argv)
{
    return Object::makeStringOutputPort();
}

Object scheme::sysPortSeekEx(int argc, const Object* argv)
{

//todo
    return Object::UnBound;
}


Object scheme::openOutputFileEx(int argc, const Object* argv)
{
    printf("open-output-file called\n");
    return Object::UnBound;
}

Object scheme::closeOutputPortEx(int argc, const Object* argv)
{
    const Object port = argv[0];
    if (port.isTextualOutputPort()) {
        port.toTextualOutputPort()->close();
        return Object::Undef;
    } else if (port.isBinaryOutputPort()) {
        port.toBinaryOutputPort()->close();
        return Object::Undef;
    } else  {
        VM_RAISE1("close-output-port port required, but got ~a\n", port);
        return Object::Undef;
    }
}

Object scheme::closeInputPortEx(int argc, const Object* argv)
{
    const Object port = argv[0];
    if (port.isTextualInputPort()) {
        port.toTextualInputPort()->close();
        return Object::Undef;
    } else if (port.isBinaryInputPort()) {
        port.toBinaryInputPort()->close();
        return Object::Undef;
    } else  {
        VM_RAISE1("close-input-port port required, but got ~a\n", port);
        return Object::Undef;
    }
}

// todo from gauche
Object scheme::digitTointegerEx(int argc, const Object* argv)
{
    const ucs4char ch = argv[0].toChar();
    const int radix  =argv[1].toInt();
    if (ch < '0') { return Object::False; }
    if (radix <= 10) {
        if (ch < '0' + radix) return Object::makeInt(ch - '0');
    } else {
        if (ch <= '9') return Object::makeInt(ch - '0');
        if (ch < 'A') return Object::False;
        if (ch < 'A' + radix - 10) return Object::makeInt(ch - 'A' + 10);
        if (ch < 'a') return Object::False;
        if (ch < 'a' + radix - 10) return Object::makeInt(ch - 'a' + 10);
    }
    return Object::False;
}

Object scheme::getRemainingInputStringEx(int argc, const Object* argv)
{
    printf("get-remaining-input-string called\n");
    return Object::UnBound;
}

Object scheme::sysReaddirEx(int argc, const Object* argv)
{
    if (!argv[0].isString()) {
        VM_RAISE1("sys-readdir string required, but got ~a\n", argv[0]);
    }

   DIR* dir;
   if (NULL == (dir = opendir(argv[0].toString()->data().ascii_c_str()))) {
       VM_RAISE1("couldn't open dir: ~s", argv[0]);
   }
   Object ret = Object::Nil;
   for (struct dirent* entry = readdir(dir); entry != NULL; entry = readdir(dir))
   {
       ret = Object::cons(Object::makeString(entry->d_name), ret);
   }
   return ret;
}

Object scheme::fileExistsPEx(int argc, const Object* argv)
{
    if (!argv[0].isString()) {
        VM_RAISE1("file-exists? string required, but got ~a\n", argv[0]);
    }

    FILE* stream = fopen(argv[0].toString()->data().ascii_c_str(), "rb");
    if (NULL == stream) {
        return Object::False;
    } else {
        fclose(stream);
        return Object::True;;
    }
}


// string-output-port only
Object scheme::sysGetOutputStringEx(int argc, const Object* argv)
{
    StringTextualOutputPort* p = reinterpret_cast<StringTextualOutputPort*>(argv[0].toTextualOutputPort());
    return Object::makeString(p->getString());
}

Object scheme::stringToregexpEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "string->regexp");
    if (argv[0].isString()) {
        return Object::makeRegexp(argv[0].toString()->data());
    } else {
        VM_RAISE1("string->regexp string required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::charTointegerEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "char->integer");
    if (argv[0].isChar()) {
        return Object::makeInt(argv[0].toChar());
    } else {
        VM_RAISE1("char->integer char required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::integerTocharEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "integer->char");
    if (argv[0].isInt()) {
        return Object::makeChar(argv[0].toInt());
    } else {
        VM_RAISE1("integer->char number required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::errorfEx(int argc, const Object* argv)
{
    Object lst = Object::Nil;
    for (int i = argc - 1; i >= 1; i--) {
        lst = Object::cons(argv[i], lst);
    }
    theVM->raiseFormat(argv[0].toString()->data().c_str(), lst);
    return Object::Undef;
}

// todo cleanup
Object scheme::formatEx(int argc, const Object* argv)
{
    const Object arg1 = argv[0];
    if (arg1.isTextualOutputPort()) {
        if (argc == 1 || !argv[1].isString()) {
            VM_RAISE1("format string required, but got ~a\n", argv[1]);
        }
        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 2; i--) {
            lst = Object::cons(argv[i], lst);
        }
        return arg1.toTextualOutputPort()->format(argv[1].toString()->data(), lst);
    } else if (arg1.isTrue()) {
        if (argc == 1 || !argv[1].isString()) {
            VM_RAISE1("format string required, but got ~a\n", argv[1]);
        }
        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 2; i--) {
            lst = Object::cons(argv[i], lst);
        }
        return theVM->getOutputPort().format(argv[1].toString()->data(), lst);
    } else if (arg1.isFalse()) {
        if (argc == 1 || !argv[1].isString()) {
            VM_RAISE1("format string required, but got ~a\n", argv[1]);
        }
        const Object port = Object::makeStringOutputPort();
        StringTextualOutputPort* const p = static_cast<StringTextualOutputPort*>(port.toTextualOutputPort());
        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 2; i--) {
            lst = Object::cons(argv[i], lst);
        }

        p->format(argv[1].toString()->data(), lst);
        return Object::makeString(p->getString());
    } else if (arg1.isString()) {
        const Object port = Object::makeStringOutputPort();
        StringTextualOutputPort* const p = static_cast<StringTextualOutputPort*>(port.toTextualOutputPort());
        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 1; i--) {
            lst = Object::cons(argv[i], lst);
        }
        p->format(arg1.toString()->data(), lst);
        return Object::makeString(p->getString());
    } else {
        VM_RAISE1("format port and string required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::currentInputPortEx(int argc, const Object* argv)
{
    return theVM->currentInputPort();
}

Object scheme::currentOutputPortEx(int argc, const Object* argv)
{
    printf("current-output-port called\n");
    return Object::UnBound;
}

Object scheme::setCurrentInputPortDEx(int argc, const Object* argv)
{
    if (argv[0].isTextualInputPort()) {
        theVM->setInputPort(argv[0]);
    } else {
        VM_RAISE1("set-current-input-port! <textual-port> required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::setCurrentOutputPortDEx(int argc, const Object* argv)
{
    if (argv[0].isTextualOutputPort()) {
        theVM->setOutputPort(*(argv[0].toTextualOutputPort()));
    } else {
        VM_RAISE1("set-current-output-port! <textual-port> required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::charPEx(int argc, const Object* argv)
{
    RETURN_BOOL(argv[0].isChar());
}

Object scheme::writeEx(int argc, const Object* argv)
{
    const Object arg1 = argv[0];
    if (1 == argc) {
        theVM->getOutputPort().putDatum(arg1);
    } else {
        const Object arg2 = argv[1];
        if (!arg2.isTextualOutputPort()) {
            VM_RAISE1("write <textual-output-port> required, but got ~a\n", arg2);
        }
        arg2.toTextualOutputPort()->putDatum(arg1);
    }
    return Object::Undef;
}

Object scheme::gensymEx(int argc, const Object* argv)
{
    static int next = 0;
    char ubuf[32];
#ifdef USE_BOEHM_GC
    ucs4char* ibuf = new(PointerFreeGC) ucs4char[32];
#else
    ucs4char* ibuf = new ucs4char[32];
#endif

    sprintf(ubuf, "G%d", next++);
    const int len = strlen(ubuf) + 1;
    for (int i = 0; i < len; i++) {
        ibuf[i] = ubuf[i];
    }
    return Symbol::intern(ibuf);
}

Object scheme::stringEqPEx(int argc, const Object* argv)
{
    printf("string=? called\n");
    return Object::UnBound;
}

Object scheme::vectorPEx(int argc, const Object* argv)
{
    printf("vector? called\n");
    return Object::UnBound;
}

Object scheme::listPEx(int argc, const Object* argv)
{
    Object obj = argv[0];
    Object seen = obj;
    for (;;) {
        if (obj.isNil()) return Object::True;
        if (!obj.isPair()) return Object::False; // dot pair
        obj = obj.cdr();
        if (obj.isNil()) return Object::True;
        if (!obj.isPair()) return Object::False; // dot pair
        obj = obj.cdr();
        seen = seen.cdr();
        if (obj == seen) return Object::False; // circular
    }
    return Object::UnBound;
}

Object scheme::memqEx(int argc, const Object* argv)
{
    const Object arg1 = argv[0];
    if (2 == argc) {
        const Object arg2 = argv[1];
        if (!arg2.isPair() && !arg2.isNil()) {
            VM_RAISE1("memq pair required, but got ~a\n", arg2);
        }
        for (Object o = arg2; o != Object::Nil; o = o.cdr()) {
            if (o.car() == arg1) {
                return o;
            }
        }
        return Object::False;
    } else {
        VM_RAISE1("wrong number of arguments for memq (required 2, got ~d)\n", Object::makeInt(argc));
    }
    return Object::UnBound;
}

Object scheme::memvEx(int argc, const Object* argv)
{
    const Object arg1 = argv[0];
    if (2 == argc) {
        const Object arg2 = argv[1];
        if (!arg2.isPair() && !arg2.isNil()) {
            VM_RAISE1("memq pair required, but got ~a\n", arg2);
        }
        for (Object o = arg2; o != Object::Nil; o = o.cdr()) {
            if (o.car().eqv(arg1).isTrue()) {
                return o;
            }
        }
        return Object::False;
    } else {
        VM_RAISE1("wrong number of arguments for memq (required 2, got ~d)\n", Object::makeInt(argc));
    }
    return Object::UnBound;
}


Object scheme::eqPEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "eq?");
    RETURN_BOOL(argv[0] == argv[1]);
}

Object scheme::memberEx(int argc, const Object* argv)
{
    const Object arg1 = argv[0];
    if (2 == argc) {
        const Object arg2 = argv[1];
        if (!arg2.isPair()  && !arg2.isNil()) {
            VM_RAISE1("member pair required, but got ~a\n", arg2);
        }
        for (Object o = arg2; o != Object::Nil; o = o.cdr()) {
            if (o.car().equal(arg1).isTrue()) {
                return o;
            }
        }
        return Object::False;
    } else {
        VM_RAISE1("wrong number of arguments for member (required 2, got ~d)\n", Object::makeInt(argc));
    }
    return Object::UnBound;
}

Object scheme::booleanPEx(int argc, const Object* argv)
{
    const Object arg1 = argv[0];
    if (arg1.isFalse() || arg1.isTrue()) {
        return Object::True;
    } else {
        return Object::False;
    }
}

Object scheme::symbolTostringEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "symbol->string");
    if (argv[0].isSymbol()) {
        return Object::makeString(argv[0].toSymbol()->c_str());
    } else {
        VM_RAISE1("symbol->string symbol required, but got ~a\n", argv[0]);
    }
    return Object::UnBound;
}

Object scheme::stringRefEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "string-ref");
    Object arg1 = argv[0];
    Object arg2 = argv[1];
    if (arg1.isString() && arg2.isInt()) {
        return Object::makeChar(arg1.toString()->charAt(arg2.toInt()));
    } else {
        VM_RAISE2("wrong arguments for string-ref required (string number), got (~a ~a)\n", arg1, arg2);
    }
    return Object::UnBound;
}

Object scheme::errorEx(int argc, const Object* argv)
{
    Object msg = Object::Nil;
    for (int i = argc - 1; i >= 0; i--) {
        msg = Object::cons(argv[i], msg);
    }
    VM_RAISE1("error : ~a", msg);
    return Object::UnBound;
}

Object scheme::getTimeofdayEx(int argc, const Object* argv)
{
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return Object::cons(Object::makeInt(tv.tv_sec), Object::makeInt(tv.tv_usec));
}

Object scheme::vmApplyEx(int argc, const Object* argv)
{
    return theVM->apply(argv[0], argv[1]);
}

Object scheme::pairPEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "pair?");
    RETURN_BOOL(argv[0].isPair());
}

Object scheme::initLibraryTableEx(int argc, const Object* argv)
{
    theVM->initLibraryTable();
    return Object::Undef;
}

Object findIter(Object proc, Object lst)
{
    if (lst.isNil()) {
        return Object::False;
    } else if (!theVM->callClosure(proc, lst.car()).isFalse()) {
        return lst.car();
    } else {
        return findIter(proc, lst.cdr());
    }
}

Object scheme::find10Ex(int argc, const Object* argv)
{
    checkArgLength(2, argc, "find");
    const Object proc = argv[0];
    if (!proc.isClosure() && !proc.isCProcedure()) {
        VM_RAISE1("find closure required, but got ~a\n", proc);
    }
    const Object lst = argv[1];
    if (lst.isNil()) return Object::False;
    if (!lst.isPair()) {
        VM_RAISE1("map pair required, but got ~a\n", lst);
    }
    return findIter(proc, lst);
}

// todo incomplete
// (make-custom-binary-input-port id read! get-position set-position! close)
Object scheme::makeCustomBinaryInputPortEx(int argc, const Object* argv)
{
    checkArgLength(5, argc, "make-custom-binary-input-port");
    const Object id = argv[0];
    if (!id.isString()) {
        VM_RAISE1("make-custom-binary-input-port string required, but got ~a\n", id);
    }

    const Object readProc = argv[1];
    if (!readProc.isClosure() && !readProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc required, but got ~a\n", readProc);
    }

    const Object getPositionProc = argv[2];
    if (!getPositionProc.isFalse() && !getPositionProc.isClosure() && !getPositionProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc or #f required, but got ~a\n", getPositionProc);
    }

    const Object setPositionProc = argv[3];
    if (!setPositionProc.isFalse() && !setPositionProc.isClosure() && !setPositionProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc or #f required, but got ~a\n", setPositionProc);
    }

    const Object closeProc = argv[4];
    if (!closeProc.isFalse() && !closeProc.isClosure() && !closeProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc or #f required, but got ~a\n", closeProc);
    }

    // todo
    return Object::makeCustomBinaryInputPort(readProc);
}

Object scheme::getU8Ex(int argc, const Object* argv)
{
    checkArgLength(1, argc, "get-u8");
    if (argv[0].isBinaryInputPort()) {
        return Object::makeInt(argv[0].toBinaryInputPort()->getU8());
    } else {
        VM_RAISE1("get-u8 <binary-input-port> required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::bytevectorU8SetDEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "bytevector-u8-set!");
    const Object bv = argv[0];
    if (!bv.isByteVector()) {
        VM_RAISE1("bytevector-u8-set! byte-vector required, but got ~a\n", bv);
    }

    const Object i = argv[1];
    if (!i.isInt()) {
        VM_RAISE1("bytevector-u8-set! number required, but got ~a\n", i);
    }

    const Object v = argv[2];
    if (!v.isInt()) {
        VM_RAISE1("bytevector-u8-set! number required, but got ~a\n", v);
    }

    bv.toByteVector()->u8set(i.toInt(), v);
    return Object::Undef;
}

Object scheme::transcodedPortEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "transcoded-port");
    const Object port = argv[0];
    const Object transcoder = argv[1];
    if (port.isBinaryInputPort() && transcoder.isTranscoder()) {
        return Object::makeTextualInputPort(port.toBinaryInputPort(), transcoder.toTranscoder());
    } else {
        VM_RAISE2("transcoded-port (binary-input-port transcoder) required, but got (~a ~a)\n", port, transcoder);
    }
    return Object::Undef;
}

Object scheme::utf8CodecEx(int argc, const Object* argv)
{
    return Object::makeUTF8Codec();
}

Object scheme::makeTranscoderEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "make-trans-coder");
    if (argv[0].isCodec()) {
        return Object::makeTranscoder(argv[0].toCodec());
    } else {
        VM_RAISE1("make-transcoder code required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::eofObjectEx(int argc, const Object* argv)
{
    return Object::Eof;
}

Object scheme::sysOpenBytevectorOutputPortEx(int argc, const Object* argv)
{
    if (0 == argc || argv[0].isFalse()) {
        printf("not implemented binary port %s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    } else if (argv[0].isTranscoder()) {
        return Object::makeTextualByteVectorOuputPort(argv[0].toTranscoder());
    } else {
        VM_RAISE1("open-bytevector-output-port transcoder required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::sysGetBytevectorEx(int argc, const Object* argv)
{
    TextualByteVectorOutputPort* p = reinterpret_cast<TextualByteVectorOutputPort*>(argv[0].toTextualOutputPort());
    return Object::makeByteVector(p->getByteVector());
}

Object scheme::bytevectorU8RefEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "bytevector-u8-ref");
    const Object bv = argv[0];
    const Object i = argv[1];
    if (bv.isByteVector() && i.isInt()) {
        return bv.toByteVector()->u8Ref(i.toInt());
    } else {
        VM_RAISE2("bytevector-u8-ref (bytevector number) required, but got (~a ~a)\n", bv, i);
    }
    return Object::Undef;
}

Object scheme::bytevectorLengthEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "bytevector-length");
    if (argv[0].isByteVector()) {
        return Object::makeInt(argv[0].toByteVector()->length());
    } else {
        VM_RAISE1("bytevector-length bytevector required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::standardInputPortEx(int argc, const Object* argv)
{
    return theVM->standardInputPort();
}

Object scheme::getBytevectorNEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "get-byte-vector-n");
    const Object p = argv[0];
    if (!p.isBinaryInputPort()) {
        VM_RAISE1("get-byte-vector-n binary-input-port required, but got ~a\n", p);
    }

    const Object count = argv[1];
    if (!count.isInt()) {
        VM_RAISE1("get-byte-vector-n number required, but got ~a\n", count);
    }

    ByteVector* ret = p.toBinaryInputPort()->getByteVector(count.toInt());
    if (ret->length() == 0) {
        return Object::Eof;
    } else {
        return Object::makeByteVector(ret);
    }
}

Object scheme::utf8TostringEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "utf8->string");
    const Object bv = argv[0];
    if (!bv.isByteVector()) {
        VM_RAISE1("utf8->string bytevector required, but got ~a\n", bv);
    }

    ByteVector* b = bv.toByteVector();
    BinaryInputPort* in = new ByteArrayBinaryInputPort(reinterpret_cast<uint8_t*>(const_cast<int8_t*>(b->data())), b->length());
    ucs4string ret;
    UTF8Codec codec;
    for (ucs4char c = codec.in(in); c != EOF; c = codec.in(in)) {
        ret += c;
    }
    return Object::makeString(ret);
}

Object scheme::openFileOutputPortEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "open-file-output-port");
    const Object file = argv[0];
    if (!file.isString()) {
        VM_RAISE1("open-file-output-port string required, but got ~a\n", file);
    }

    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    Object ret = Object::makeTextualOutputPort(new FileBinaryOutputPort(file.toString()->data()), transcoder);
    return ret;
}

Object scheme::openFileInputPortEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "open-file-input-port");
    const Object file = argv[0];
    if (!file.isString()) {
        VM_RAISE1("open-file-input-port string required, but got ~a\n", file);
    }
    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    return Object::makeTextualInputPort(new FileBinaryInputPort(file.toString()->data()), transcoder);
}

Object scheme::vectorEx(int argc, const Object* argv)
{
    const Object vec = Object::makeVector(argc);
    Vector* const v = vec.toVector();
    for (int i = 0; i < argc; i++) {
        v->set(i, argv[i]);
    }
    return vec;
}

Object scheme::regexpReplaceEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "regexp-replace");
    const Object reg = argv[0];
    const Object str = argv[1];
    const Object sub = argv[2];

    if (reg.isRegexp() && str.isString() && sub.isString()) {
        return reg.toRegexp()->replace(str, sub);
    } else {
        VM_RAISE3("regexp-replace (regexp string string) required, but got (~a ~a ~a)\n", reg, str, sub);
    }
    return Object::Undef;
}

Object scheme::regexpReplaceAllEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "regexp-replace-all");
    const Object reg = argv[0];
    const Object str = argv[1];
    const Object sub = argv[2];

    if (reg.isRegexp() && str.isString() && sub.isString()) {
        return reg.toRegexp()->replaceAll(str, sub);
    } else {
        VM_RAISE3("regexp-replace (regexp string string) required, but got (~a ~a ~a)\n", reg, str, sub);
    }
    return Object::Undef;
}

Object scheme::evalEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "eval");
    return theVM->eval(argv[0], argv[1]);
}

Object scheme::raiseEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "raise");
    theVM->raise(argv[0]);
    return Object::Undef;
}

Object scheme::raiseContinuableEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "raise-continuable");
    return theVM->raiseContinuable(argv[0]);
}


Object scheme::withExceptionHandlerEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "with-exception-handler");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    if (arg1.isClosure() && arg2.isClosure()) {
        return theVM->withExceptionHandler(arg1, arg2);
    } else {
        VM_RAISE2("wrong arguments for with-exception-handler required (closure, closure), got (~a ~a)\n", arg1, arg2);
    }
    return Object::Undef;
}

Object scheme::makeVectorTypeEx(int argc, const Object* argv)
{
    checkArgLength(5, argc, "make-vector-type");
    const Object name = argv[0];
    const Object supertype = argv[1];
    const Object data = argv[2];
    const Object fieldMutability = argv[3];

    if (name.isSymbol() && (supertype.isFalse() || supertype.isTypedVectorDesc()) && fieldMutability.isPair()) {
        return Object::makeTypedVectorDesc(name, supertype, data, fieldMutability);
    } else {
        VM_RAISE0("wrong arguments for make-vector-type");
        return Object::Undef;
    }
}

Object scheme::vectorTypePEx(int argc, const Object* argv)
{
    return Object::makeBool(argv[0].isTypedVectorDesc());
}

Object scheme::vectorTypeDataEx(int argc, const Object* argv)
{
    const Object vt = argv[0];
    if (vt.isTypedVectorDesc()) {
        return vt.toTypedVectorDesc()->data;
    } else {
        VM_RAISE1("wrong argument for vector-type-data required vector-type, got ~a\n", vt);
        return Object::Undef;
    }
}

Object scheme::vectorTypeInstanceOfPEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "vector-type-instance-of");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    if (arg1.isTypedVector() && arg2.isTypedVectorDesc()) {
        return arg1.toTypedVector()->instanceOf(arg2);
    } else {
        VM_RAISE2("wrong arguments for vector-type-instance-of? required (vector-type-instance, vector-type), got (~a ~a)\n", arg1, arg2);
        return Object::Undef;
    }
}

Object scheme::makeTypedVectorEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "make-typed-vector");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    if (arg1.isTypedVectorDesc() && arg2.isPair()) {
        return Object::makeTypedVector(arg1, arg2);
    } else {
        VM_RAISE2("wrong arguments for make-typed-vector required (vector-type-instance, list of args), got (~a ~a)\n", arg1, arg2);
        return Object::Undef;
    }
}

Object scheme::typedVectorGetNthEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "typed-vector-get-nth");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    if (arg1.isTypedVector() && arg2.isInt()) {
        return arg1.toTypedVector()->getNth(arg2);
    } else {
        VM_RAISE2("wrong arguments for typed-vector-get-nth required (vector-type-instance, number), got (~a ~a)\n", arg1, arg2);
        return Object::Undef;
    }
}

Object scheme::typedVectorSetNthEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "typed-vector-set-nth");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    const Object arg3 = argv[2];
    if (arg1.isTypedVector() && arg2.isInt()) {
        arg1.toTypedVector()->setNth(arg2, arg3);
    } else {
        VM_RAISE3("wrong arguments for typed-vector-set-nth required (vector-type-instance, number, any object), got (~a ~a ~a)\n", arg1, arg2, arg3);
    }
    return Object::Undef;
}

Object scheme::typedVectorTypeEx(int argc, const Object* argv)
{
    const Object vt = argv[0];
    if (vt.isTypedVector()) {
        return vt.toTypedVector()->desc;
    } else {
        VM_RAISE1("wrong arguments for typed-vector-type required vector-type-instance, got ~a", vt);
        return Object::Undef;
    }
}

Object scheme::typedVectorPEx(int argc, const Object* argv)
{
    return Object::makeBool(argv[0].isTypedVector());
}

Object scheme::applyEx(int argc, const Object* argv)
{
    if (argc < 2) {
        VM_RAISE1("wrong number of arguments for apply (required at least 2, got ~d)\n", Object::makeInt(argc));
    }

    const Object proc = argv[0];
    if (!proc.isCallable()) {
        VM_RAISE1("wrong arguments for apply required closure, got ~a\n", proc);
    }

    Object argsAsList = Object::Nil;
    for (int i = 1; i < argc; i++) {
        if (i == argc - 1) {
            argsAsList = Pair::appendD(argsAsList, argv[i]);
        } else {
            argsAsList = Pair::appendD(argsAsList, Pair::list1(argv[i]));
        }
    }
    return theVM->applyClosure(proc, argsAsList);
}

Object scheme::valuesEx(int argc, const Object* argv)
{
    return theVM->values(argc, argv);
}

int scheme::div(int x, int y)
{
    const int sign = x * y > 0 ? 1 : -1;
    if (x < 0) {
        return sign * ((abs(x) + abs(y)) / abs(y));
    } else if (y < 0) {
        return sign * (x / abs(y));
    } else {
        return sign * (x / y);
    }
}

int scheme::mod(int x, int y)
{
    return x - div(x, y) * y;
}

Object scheme::modEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "mod");
    Object arg1 = argv[0];
    Object arg2 = argv[1];
    if (arg1.isInt() && arg2.isInt()) {
        const int a = arg1.toInt();
        const int b = arg2.toInt();
        if (0 == b) {
            VM_RAISE2("Dividing by zero (mod ~d ~d)", arg1, arg2);
            return Object::Undef;
        }
        return Object::makeInt(mod(a, b));
    } else {
        VM_RAISE2("wrong arguments for mod required (int int), got (~a ~a)\n", arg1, arg2);
    }
    return Object::Undef;
}

Object scheme::divEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "div");
    Object arg1 = argv[0];
    Object arg2 = argv[1];
    if (arg1.isInt() && arg2.isInt()) {
        const int a = arg1.toInt();
        const int b = arg2.toInt();
        if (0 == b) {
            VM_RAISE2("Dividing by zero (div ~d ~d)", arg1, arg2);
            return Object::Undef;
        }
        return Object::makeInt(div(a, b));
    } else {
        VM_RAISE2("wrong arguments for div required (number number), got (~a ~a)\n", arg1, arg2);
    }
    return Object::Undef;
}

Object scheme::assqEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "assq");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    if (arg2.isPair() || arg2.isNil()) {
        for (Object o = arg2; o != Object::Nil; o = o.cdr()) {
            if (o.car().car() == arg1) {
                return o.car();
            }
        }
        return Object::False;
    } else {
        VM_RAISE2("wrong arguments for mod required (obj list), got (~a ~a)\n", arg1, arg2);
    }
    return Object::Undef;
}

Object scheme::exitEx(int argc, const Object* argv)
{
    if (0 == argc) {
        exit(EXIT_SUCCESS);
    }

    const Object arg1 = argv[0];
    if (arg1.isInt()) {
        exit(arg1.toInt());
    } else {
        exit(-1);
    }
    return Object::Undef;
}

Object scheme::macroexpand1Ex(int argc, const Object* argv)
{
    static Object proc = Symbol::intern(UC("pass1/macroexpand"));
    return theVM->callClosureByName(proc, argv[0]);
}

Object scheme::procedurePEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "procedure");
    const Object arg1 = argv[0];
    return Object::makeBool(arg1.isClosure() || arg1.isCProcedure());
}

Object scheme::loadEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "load");
    if (argv[0].isString()) {
        theVM->loadFile(argv[0].toString()->data());
    } else {
        VM_RAISE1("load string required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::symbolPEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "symbol?");
    return Object::makeBool(argv[0].isSymbol());
}

Object scheme::dynamicWindEx(int argc, const Object* argv)
{
    checkArgLength(3, argc, "dynamic-wind");
    const Object arg1 = argv[0];
    const Object arg2 = argv[1];
    const Object arg3 = argv[2];
    if (!arg1.isCallable() || !arg2.isCallable() || !arg3.isCallable()) {
        VM_RAISE3("dynamic-wind closure required, but got (~a, ~a, ~a)\n", arg1, arg2, arg3);
    }
    // call before
    theVM->callClosure0(arg1);

    // call thunk
    const Object ret = theVM->callClosure0(arg2);

    // save the return values of thunk
    Values v = theVM->fetchValues();
    v.val = ret;

    // call after
    theVM->callClosure0(arg3);

    // returns return values of thunk.
    theVM->restoreValues(v);
    return v.val;
}

Object scheme::charGePEx(int argc, const Object* argv)
{
   if (argc < 2) {
       VM_RAISE1("wrong number of arguments for char>=? required at least 2, got ~d)\n", Object::makeInt(argc));
   }
   for (int i = 0; i < argc; i++) {
       const Object o = argv[i];
       if (!o.isChar()) {
           VM_RAISE1("char>=? char required, but got ~a\n", o);
       }
       if (i == argc - 1) break;
       const Object n = argv[i + 1];
       if (!n.isChar()) {
           VM_RAISE1("char>=? char required, but got ~a\n", n);
       }
       if (o.toChar() < n.toChar()) {
           return Object::False;
       }
   }
   return Object::True;

}

Object scheme::charGtPEx(int argc, const Object* argv)
{
   if (argc < 2) {
       VM_RAISE1("wrong number of arguments for char>? required at least 2, got ~d)\n", Object::makeInt(argc));
   }
   for (int i = 0; i < argc; i++) {
       const Object o = argv[i];
       if (!o.isChar()) {
           VM_RAISE1("char>? char required, but got ~a\n", o);
       }
       if (i == argc - 1) break;
       const Object n = argv[i + 1];
       if (!n.isChar()) {
           VM_RAISE1("char>? char required, but got ~a\n", n);
       }
       if (o.toChar() <= n.toChar()) {
           return Object::False;
       }
   }
   return Object::True;

}

Object scheme::charLePEx(int argc, const Object* argv)
{
   if (argc < 2) {
       VM_RAISE1("wrong number of arguments for char<=? required at least 2, got ~d)\n", Object::makeInt(argc));
   }
   for (int i = 0; i < argc; i++) {
       const Object o = argv[i];
       if (!o.isChar()) {
           VM_RAISE1("char<=? char required, but got ~a\n", o);
       }
       if (i == argc - 1) break;
       const Object n = argv[i + 1];
       if (!n.isChar()) {
           VM_RAISE1("char<=? char required, but got ~a\n", n);
       }
       if (o.toChar() > n.toChar()) {
           return Object::False;
       }
   }
   return Object::True;
}

Object scheme::charLtPEx(int argc, const Object* argv)
{
   if (argc < 2) {
       VM_RAISE1("wrong number of arguments for char<? required at least 2, got ~d)\n", Object::makeInt(argc));
   }
   for (int i = 0; i < argc; i++) {
       const Object o = argv[i];
       if (!o.isChar()) {
           VM_RAISE1("char<? char required, but got ~a\n", o);
       }
       if (i == argc - 1) break;
       const Object n = argv[i + 1];
       if (!n.isChar()) {
           VM_RAISE1("char<? char required, but got ~a\n", n);
       }
       if (o.toChar() >= n.toChar()) {
           return Object::False;
       }
   }
   return Object::True;
}

Object scheme::vectorTolistEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "vector->list");
    const Object vec = argv[0];
    if (!vec.isVector()) {
        VM_RAISE1("vector->list vector required, but got ~a\n", vec);
    }
    Vector* const v = vec.toVector();
    const int vLength = v->length();
    Object ret = Object::Nil;
    for (int i = vLength - 1; i >= 0; i--) {
        ret = Object::cons(v->ref(i), ret);
    }
    return ret;
}

Object scheme::callProcessEx(int argc, const Object* argv)
{
    const int BUFFER_SIZE = 1024;
    const Object cmd = argv[0];
    if (!cmd.isString()) {
       VM_RAISE1("call-process string required, but got ~a\n", cmd);
    }

    FILE* in = popen(cmd.toString()->data().ascii_c_str(), "r");
    char buffer[BUFFER_SIZE];
    if (in == NULL) {
        VM_RAISE1("call-process failed: ~a\n", cmd);
    }

    memset(buffer, '\0', BUFFER_SIZE);

    ucs4string ret;
    int size;
    while ((size = fread(buffer, sizeof(char), BUFFER_SIZE, in)) > 0) {
        ret += ucs4string::from_c_str(buffer, size);
    }
    if (pclose(in) != 0) {
        VM_RAISE0("call-process failed\n");
    }
    return Object::makeString(ret);
}

Object scheme::internalgetClosureNameEx(int argc, const Object* argv)
{
    return theVM->getClosureName(argv[0]);
}

Object scheme::appendEx(int argc, const Object* argv)
{
    if (0 == argc) return Object::Nil;
    Object ret = argv[argc - 1];
    for (int i = argc - 2; i >= 0; i--) {
        if (!argv[i].isPair()) {
            // error
        }
        ret = Pair::append2(argv[i], ret);
    }
    return ret;

}

Object scheme::append2Ex(int argc, const Object* argv)
{
    checkArgLength(2, argc, "append2");
    return Pair::append2(argv[0], argv[1]);
}

Object scheme::appendAEx(int argc, const Object* argv)
{
    checkArgLength(2, argc, "appendA");
    return Pair::append2(argv[0], argv[1]);
}


Object scheme::appendDEx(int argc, const Object* argv)
{
    if (0 == argc) return Object::Nil;
    Object ret = argv[argc - 1];
    for (int i = argc - 2; i >= 0; i--) {
        if (!argv[i].isPair()) {
            // error
        }
        ret = Pair::appendD2(argv[i], ret);
    }
    return ret;

}

Object scheme::uniq(Object list)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        if (!memq(p.car(), ret).isFalse()) {
            continue;
        } else {
            ret = Object::cons(p.car(), ret);
        }
    }
    return ret;
}





Object scheme::eqHashtableCopyEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "eq-hashtable-copy");
    const Object ht = argv[0];
    if (!ht.isEqHashTable()) {
        VM_RAISE1("eq-hashtable required, but got ~an", ht);
    }
    return ht.toEqHashTable()->copy();
}


Object scheme::lengthEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "length");
    const Object lst = argv[0];
    if (lst.isNil()) {
        return Object::makeInt(0);
    } else if (!lst.isPair()) {
        VM_RAISE1("length pair required, but got ~an", lst);
    }
    int ret = 0;
    for (Object p = lst; p.isPair(); p = p.cdr()) {
        ret++;
    }
    return Object::makeInt(ret);
}

Object scheme::listTovectorEx(int argc, const Object* argv)
{
    checkArgLength(1, argc, "list->vector");
    const Object list = argv[0];
    if (list.isPair()) {
        return Object::makeVector(list);
    } else if (list.isNil()) {
        return Object::makeVector(0);
    } else {
        VM_RAISE1("list->vector proper list required, but got ~an", list);
        return Object::Undef;
    }
}


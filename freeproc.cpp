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
 *  $Id: freeproc.cpp 5306 2008-05-06 11:06:23Z higepon $
 */

#include "freeproc.h"
#include "VM.h"

extern scheme::VM* theVM;

using namespace scheme;

Object scheme::dummy(Object obj)
{
    return Object::False;
}

Object scheme::currentErrorPortEx(Object args)
{
    printf("current-error-port called\n");
    return Object::UnBound;

}
Object scheme::hashtableKeysEx(Object args)
{
    const Object ht = args.first();
    if (!ht.isEqHashTable()) {
        VM_RAISE1("hashtable-keys hash-table required, but got ~a\n", ht);
    }
    return ht.toEqHashTable()->keys();
}
Object scheme::hashtableSetEx(Object args)
{
    const Object ht = args.first();
    if (!ht.isEqHashTable()) {
        VM_RAISE1("hashtable-set! hash-table required, but got ~a\n", ht);
    }

    if (Pair::length(args) == 3) {
        const Object key = args.second();
        const Object val = args.third();
        ht.toEqHashTable()->set(key, val);
        return Object::Undef;
    } else {
        VM_RAISE1("wrong number of arguments for hashtable-set! (required 3, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::hashtableRefEx(Object args)
{
    const Object ht = args.first();
    if (!ht.isEqHashTable()) {
        VM_RAISE1("hashtable-ref hash-table required, but got ~a\n", ht);
    }

    const int length = Pair::length(args);
    if (length == 3) {
        const Object key = args.second();
        const Object defaultVal = args.third();
        return ht.toEqHashTable()->ref(key, defaultVal);
    } else if (length == 2) {
        const Object key = args.second();
        return ht.toEqHashTable()->ref(key, Object::False);
    } else {
        VM_RAISE1("wrong number of arguments for hash-table-get (required 2 or 3, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::makeEqHashtableEx(Object args)
{
    return Object::makeEqHashTable();
}



Object scheme::numberPEx(Object args)
{
    RETURN_BOOL(args.first().isInt());
}

Object scheme::consEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for cons (required 2, got ~d)\n", Object::makeInt(length));
    }
    return Object::cons(args.first(), args.second());
}

Object scheme::carEx(Object args)
{
    if (args.first().isPair()) {
        return args.first().car();
    } else {
        VM_RAISE1("car pair required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::cdrEx(Object args)
{
    if (args.first().isPair()) {
        return args.first().cdr();
    } else {
        VM_RAISE1("cdr pair required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::sourceInfoEx(Object args)
{
    const Object arg = args.first();
    if (arg.isPair()) {
        return arg.sourceInfo();
    } else if (arg.isClosure()) {
        return arg.toClosure()->sourceInfo;
    } else {
        return Object::False;
    }
    return Object::Undef;
}

Object scheme::setSourceInfoEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("set-source-info! 2 arguments required, but got ~d\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
    if (arg1.isPair()) {
        arg1.toPair()->sourceInfo = arg2;
    } else {
        VM_RAISE1("set-source-info! pair required, but got ~a\n", arg1);
    }
    return arg1;
}

Object scheme::nullPEx(Object args)
{
    if (args.isPair()) {
        return Object::makeBool(args.first().isNil());
    } else {
        return Object::False;
    }
}

Object scheme::setCarEx(Object args)
{
    printf("set-car! called\n");
    return Object::UnBound;
}

Object scheme::setCdrEx(Object args)
{
    printf("set-cdr! called\n");
    return Object::UnBound;
}

Object scheme::sysDisplayEx(Object args)
{
    const Object arg1 = args.first();
    if (args.cdr().isNil()) {
        theVM->getOutputPort().display(arg1);
    } else {
        const Object arg2 = args.second();
        if (!arg2.isTextualOutputPort()) {
            VM_RAISE1("display <textual-output-port> required, but got ~a\n", arg2);
        }
        arg2.toTextualOutputPort()->display(arg1);
    }
    return Object::Undef;
}

Object scheme::rxmatchEx(Object args)
{
    const Object arg1 = args.first();
    if (!arg1.isRegexp()) {
        VM_RAISE1("rxmatch rxmatch required, but got ~a\n", arg1);
    }

    if (Pair::length(args) == 2) {
        const Object arg2 = args.second();
        if (!arg2.isString()) {
            VM_RAISE1("rxmatch rxmatch required, but got ~a\n", arg2);
        }
        return arg1.toRegexp()->match(arg2.toString()->data());
    } else {
        VM_RAISE1("wrong number of arguments for rxmatch (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::regexpPEx(Object args)
{
    RETURN_BOOL(args.first().isRegexp());
}

Object scheme::regexpTostringEx(Object args)
{
    const Object arg1 = args.first();
    if (!arg1.isRegexp()) {
        VM_RAISE1("regexp->string regexp required, but got ~a\n", arg1);
    }
    return Object::makeString(arg1.toRegexp()->pattern());
}

Object scheme::rxmatchStartEx(Object args)
{
    const Object r = args.first();
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-start regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (!args.cdr().isNil()) {
        const Object i = args.second();
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-start number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return Object::makeInt(r.toRegMatch()->matchStart(index));
}

Object scheme::rxmatchEndEx(Object args)
{
    const Object r = args.first();
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-end regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (!args.cdr().isNil()) {
        const Object i = args.second();
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-end number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return Object::makeInt(r.toRegMatch()->matchEnd(index));
}

Object scheme::rxmatchAfterEx(Object args)
{
    const Object r = args.first();
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-after regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (!args.cdr().isNil()) {
        const Object i = args.second();
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-after number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return r.toRegMatch()->matchAfter(index);
}

Object scheme::rxmatchBeforeEx(Object args)
{
    const Object r = args.first();
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-before regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (!args.cdr().isNil()) {
        const Object i = args.second();
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-before number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return r.toRegMatch()->matchBefore(index);
}

Object scheme::rxmatchSubstringEx(Object args)
{
    const Object r = args.first();
    if (r.isFalse())  return Object::False;
    if (!r.isRegMatch()) {
        VM_RAISE1("rxmatch-substring regexp required, but got ~a\n", r);
    }

    int index = 0;
    if (!args.cdr().isNil()) {
        const Object i = args.second();
        if (!i.isInt()) {
            VM_RAISE1("rxmatch-substring number required, but got ~a\n", i);
        }
        index = i.toInt();
    }
    return r.toRegMatch()->matchSubString(index);
}

Object scheme::regMatchProxy(Object args)
{
    const Object match = args.first();
    const Object rest  = args.cdr();
    if (rest.isPair() && rest.car() == Symbol::AFTER) {
        return rxmatchAfterEx(Object::cons(match, rest.cdr()));
    } else if (rest.isPair() && rest.car() == Symbol::BEFORE) {
        return rxmatchBeforeEx(Object::cons(match, rest.cdr()));
    } else {
        return rxmatchSubstringEx(args);
    }
}

Object scheme::makeStringEx(Object args)
{
    const Object arg1 = args.first();
    if (!arg1.isInt()) {
        VM_RAISE1("make-string number required, but got ~a\n", arg1);
    }

    if (Pair::length(args) == 2) {
        const Object arg2 = args.second();
        if (!arg2.isChar()) {
            VM_RAISE1("make-string char required, but got ~a\n", arg2);
        }
        return Object::makeString(arg1.toInt(), arg2.toChar());
    } else {
        return Object::makeString(arg1.toInt());
    }
}

Object scheme::stringSetEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 3) {
        VM_RAISE1("string-set! 3 arguments required, but got ~d\n", Object::makeInt(length));
    } else if (!args.first().isString()) {
        VM_RAISE1("string-set! string required, but got ~a\n", args.first());
    } else if (!args.second().isInt()) {
        VM_RAISE1("string-set! number required, but got ~a\n", args.second());
    } else if (!args.third().isChar()) {
        VM_RAISE1("string-set! char required, but got ~a\n", args.second());
    }
    args.first().toString()->data()[args.second().toInt()] = args.third().toChar();
    return Object::Undef;
}

Object scheme::stringLengthEx(Object args)
{
    if (args.first().isString()) {
        return Object::makeInt(args.first().toString()->data().length());
    } else {
        VM_RAISE1("string-length string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::stringTosymbolEx(Object args)
{
    if (args.first().isString()) {
        return Symbol::intern(args.first().toString()->data().c_str());
    } else {
        VM_RAISE1("string->symbol string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::stringTonumberEx(Object args)
{
    if (args.first().isString()) {
        return Object::makeInt(atoi(args.first().toString()->data().ascii_c_str()));
    } else {
        VM_RAISE1("string->number string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::stringAppendEx(Object args)
{
    ucs4string ret;
    for (Object s = args; !s.isNil(); s = s.cdr()) {
        if (!s.car().isString()) {
            VM_RAISE1("string-append string required, but got ~a\n", s.car());
        }
        ret += s.car().toString()->data();
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

Object scheme::stringSplitEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("string-split 2 arguments required, but got ~d\n", Object::makeInt(length));
    } else if (!args.first().isString()) {
        VM_RAISE1("string-split string required, but got ~a\n", args.first());
    } else if (!args.second().isChar()) {
        VM_RAISE1("string-split char required, but got ~a\n", args.second());
    }

    gc_vector<ucs4string> v;
    args.first().toString()->data().split(args.second().toChar(), v);
    return makeList(v, 0);
}

Object scheme::numberTostringEx(Object args)
{
    const int length = Pair::length(args);
    if (length == 2) {
        Object arg1 = args.first();
        Object arg2 = args.second();
        if (arg1.isInt() && arg2.isInt() && arg2.toInt() == 16) {
            static char buf[32];
            snprintf(buf, 32, "%lx", args.first().toInt());
            return Object::makeString(buf);
        } else {
            VM_RAISE1("number->string number required, but got ~a\n", args.first());
        }
    }
    else if (args.first().isInt()) {
        static char buf[32];
        snprintf(buf, 32, "%ld", args.first().toInt());
        return Object::makeString(buf);
    } else {
        VM_RAISE1("number->string number required, but got ~a\n", args.first());
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

Object scheme::reverseEx(Object args)
{
    return Pair::reverse(args.first());
//    return reverseIter(args.car(), Object::Nil);
}

Object scheme::eofObjectPEx(Object args)
{
    RETURN_BOOL(args.first().isEof());
}

Object scheme::readCharEx(Object args)
{
    // todo
    if (args.first().isTextualInputPort()) {
        const ucs4char c = args.first().toTextualInputPort()->getChar();
        return c == EOF ? Object::Eof : Object::makeChar(c);
    } else {
        VM_RAISE1("read-char port required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::readEx(Object args)
{
    if (args.isNil()) {
        return theVM->currentInputPort().toTextualInputPort()->getDatum();
    } else if (args.first().isTextualInputPort()) {
        return args.first().toTextualInputPort()->getDatum();
    } else {
        VM_RAISE1("read required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::charEqPEx(Object args)
{
    const Object start = args.first();
    if (!start.isChar()) {
        return Object::False;
    }

    for (Object c = args.cdr(); !c.isNil(); c = c.cdr()) {
        if (!c.car().isChar()) {
            return Object::False;
        }
        if (start != c.car()) {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::stringPEx(Object args)
{
    return Object::makeBool(args.car().isString());
}

Object scheme::sysGetenvEx(Object args)
{
    if (args.first().isString()) {
        const char* str = getenv(args.first().toString()->data().ascii_c_str());
        return str == NULL ? Object::False : Object::makeString(str);
    } else {
        VM_RAISE1("getenv string required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::equalPEx(Object args)
{
    if (Pair::length(args) == 2) {
        return args.first().equal(args.second());
    } else {
        VM_RAISE1("wrong number of arguments for equal? (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::openStringInputPortEx(Object args)
{
    if (args.first().isString()) {
        return Object::makeStringInputPort(args.first().toString()->data());
    } else {
        VM_RAISE1("open-string-input-port string required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::sysOpenOutputStringEx(Object args)
{
    return Object::makeStringOutputPort();
}

Object scheme::sysPortSeekEx(Object args)
{

//todo
    return Object::UnBound;
}


Object scheme::openOutputFileEx(Object args)
{
    printf("open-output-file called\n");
    return Object::UnBound;
}

Object scheme::closeOutputPortEx(Object args)
{
    const Object port = args.first();
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

Object scheme::closeInputPortEx(Object args)
{
    const Object port = args.first();
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
Object scheme::digitTointegerEx(Object args)
{
    const ucs4char ch = args.first().toChar();
    const int radix  =args.second().toInt();
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

Object scheme::getRemainingInputStringEx(Object args)
{
    printf("get-remaining-input-string called\n");
    return Object::UnBound;
}

Object scheme::sysReaddirEx(Object args)
{
    if (!args.first().isString()) {
        VM_RAISE1("sys-readdir string required, but got ~a\n", args.first());
    }

   DIR* dir;
   if (NULL == (dir = opendir(args.first().toString()->data().ascii_c_str()))) {
       VM_RAISE1("couldn't open dir: ~s", args.first());
   }
   Object ret = Object::Nil;
   for (struct dirent* entry = readdir(dir); entry != NULL; entry = readdir(dir))
   {
       ret = Object::cons(Object::makeString(entry->d_name), ret);
   }
   return ret;
}

Object scheme::fileExistsPEx(Object args)
{
    if (!args.first().isString()) {
        VM_RAISE1("file-exists? string required, but got ~a\n", args.first());
    }

    FILE* stream = fopen(args.first().toString()->data().ascii_c_str(), "rb");
    if (NULL == stream) {
        return Object::False;
    } else {
        fclose(stream);
        return Object::True;;
    }
}


// string-output-port only
Object scheme::sysGetOutputStringEx(Object args)
{
    StringTextualOutputPort* p = reinterpret_cast<StringTextualOutputPort*>(args.first().toTextualOutputPort());
    return Object::makeString(p->getString());
}

Object scheme::stringToregexpEx(Object args)
{
    if (args.first().isString()) {
        return Object::makeRegexp(args.first().toString()->data());
    } else {
        VM_RAISE1("string->regexp string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::charTointegerEx(Object args)
{
    if (args.first().isChar()) {
        return Object::makeInt(args.first().toChar());
    } else {
        VM_RAISE1("char->integer char required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::integerTocharEx(Object args)
{
    if (args.first().isInt()) {
        return Object::makeChar(args.first().toInt());
    } else {
        VM_RAISE1("integer->char number required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::errorfEx(Object args)
{
    theVM->raiseFormat(args.car().toString()->data().c_str(), args.cdr());
    return Object::Undef;
}

Object scheme::formatEx(Object args)
{
    const Object arg1 = args.first();
    if (arg1.isTextualOutputPort()) {
        if (args.cdr().isNil() || !args.second().isString()) {
            VM_RAISE1("format string required, but got ~a\n", args.second());
        }
        return arg1.toTextualOutputPort()->format(args.second().toString()->data(), args.cdr().cdr());
    } else if (arg1.isTrue()) {
        if (args.cdr().isNil() || !args.second().isString()) {
            VM_RAISE1("format string required, but got ~a\n", args.second());
        }
        return theVM->getOutputPort().format(args.second().toString()->data(), args.cdr().cdr());
    } else if (arg1.isFalse()) {
        if (args.cdr().isNil() || !args.second().isString()) {
            VM_RAISE1("format string required, but got ~a\n", args.second());
        }
        const Object port = Object::makeStringOutputPort();
        StringTextualOutputPort* const p = static_cast<StringTextualOutputPort*>(port.toTextualOutputPort());
        p->format(args.second().toString()->data(), args.cdr().cdr());
        return Object::makeString(p->getString());
    } else if (arg1.isString()) {
        const Object port = Object::makeStringOutputPort();
        StringTextualOutputPort* const p = static_cast<StringTextualOutputPort*>(port.toTextualOutputPort());
        p->format(arg1.toString()->data(), args.cdr());
        return Object::makeString(p->getString());
    } else {
        VM_RAISE1("format port and string required, but got ~a\n", args);
    }
    return Object::Undef;
}

Object scheme::currentInputPortEx(Object args)
{
    return theVM->currentInputPort();
}

Object scheme::currentOutputPortEx(Object args)
{
    printf("current-output-port called\n");
    return Object::UnBound;
}

Object scheme::setCurrentInputPortEx(Object args)
{
    if (args.first().isTextualInputPort()) {
        theVM->setInputPort(args.first());
    } else {
        VM_RAISE1("set-current-input-port! <textual-port> required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::setCurrentOutputPortEx(Object args)
{
    if (args.first().isTextualOutputPort()) {
        theVM->setOutputPort(*(args.first().toTextualOutputPort()));
    } else {
        VM_RAISE1("set-current-output-port! <textual-port> required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::charPEx(Object args)
{
    RETURN_BOOL(args.first().isChar());
}

Object scheme::writeEx(Object args)
{
    const Object arg1 = args.first();
    if (args.cdr().isNil()) {
        theVM->getOutputPort().putDatum(arg1);
    } else {
        const Object arg2 = args.second();
        if (!arg2.isTextualOutputPort()) {
            VM_RAISE1("write <textual-output-port> required, but got ~a\n", arg2);
        }
        arg2.toTextualOutputPort()->putDatum(arg1);
    }
    return Object::Undef;
}

Object scheme::gensymEx(Object args)
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

Object scheme::stringEqPEx(Object args)
{
    printf("string=? called\n");
    return Object::UnBound;
}

Object scheme::vectorPEx(Object args)
{
    printf("vector? called\n");
    return Object::UnBound;
}

Object scheme::listPEx(Object args)
{
    Object obj = args.first();
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

Object scheme::memqEx(Object args)
{
    const Object arg1 = args.first();
    if (Pair::length(args) == 2) {
        const Object arg2 = args.second();
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
        VM_RAISE1("wrong number of arguments for memq (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::memvEx(Object args)
{
    const Object arg1 = args.first();
    if (Pair::length(args) == 2) {
        const Object arg2 = args.second();
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
        VM_RAISE1("wrong number of arguments for memq (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}


Object scheme::eqPEx(Object args)
{
    if (Pair::length(args) == 2) {
        RETURN_BOOL(args.first() == args.second());
    } else {
        VM_RAISE1("wrong number of arguments for eq? (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::memberEx(Object args)
{
    const Object arg1 = args.first();
    if (Pair::length(args) == 2) {
        const Object arg2 = args.second();
        if (!arg2.isPair() && !arg2.isNil()) {
            VM_RAISE1("member pair required, but got ~a\n", arg2);
        }
        for (Object o = arg2; o != Object::Nil; o = o.cdr()) {
            if (o.car().equal(arg1).isTrue()) {
                return o;
            }
        }
        return Object::False;
    } else {
        VM_RAISE1("wrong number of arguments for member (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::booleanPEx(Object args)
{
    const Object arg1 = args.first();
    if (arg1.isFalse() || arg1.isTrue()) {
        return Object::True;
    } else {
        return Object::False;
    }
}

Object scheme::symbolTostringEx(Object args)
{
    if (args.first().isSymbol()) {
        return Object::makeString(args.first().toSymbol()->c_str());
    } else {
        VM_RAISE1("symbol->string symbol required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::stringRefEx(Object args)
{
    const int length = Pair::length(args);
    if (length < 2) {
        VM_RAISE1("wrong number of arguments for string-ref (required 2, got ~d)\n", Object::makeInt(length));
    }

    Object arg1 = args.first();
    Object arg2 = args.second();
    if (arg1.isString() && arg2.isInt()) {
        return Object::makeChar(arg1.toString()->charAt(arg2.toInt()));
    } else {
        VM_RAISE2("wrong arguments for string-ref required (string number), got (~a ~a)\n", arg1, arg2);
    }
    return Object::UnBound;
}

Object scheme::errorEx(Object args)
{
    VM_RAISE1("error : ~a", args);
    return Object::UnBound;
}

Object scheme::getTimeofdayEx(Object args)
{
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return Object::cons(Object::makeInt(tv.tv_sec), Object::makeInt(tv.tv_usec));
}

// Object scheme::valuesEx(Object args)
// {
// //    VM_LOG1("values=~d ", Object::makeInt(Pair::length(args)));
//     fflush(stderr);
//     return Object::makeValues(args);
// }

Object scheme::vmApplyEx(Object a)
{
    return theVM->apply(a.first(), a.second());
}

Object scheme::pairPEx(Object args)
{
    RETURN_BOOL(args.first().isPair());
}

Object scheme::initLibraryTableEx(Object args)
{
    theVM->initLibraryTable();
    return Object::Undef;
}

Object mapIter(Object proc, Object lst)
{
    if (lst.isNil()) {
        return Object::Nil;
    } else {
        return Object::cons(theVM->callClosure(proc, lst.car()), mapIter(proc, lst.cdr()));
    }
}

// まだ正しく動いていない（全てのtestが通らない）
Object scheme::map10Ex(Object args)
{
    const Object proc = args.first();
    if (!proc.isClosure() && !proc.isCProcedure()) {
        VM_RAISE1("map closure required, but got ~a\n", proc);
    }
    const int length = Pair::length(args);
    if (length == 2) {
        const Object lst = args.second();
        if (lst.isNil()) return lst;
        if (!lst.isPair()) {
            VM_RAISE1("map pair required, but got ~a\n", lst);
        }
        return mapIter(proc, lst);
    } else {
        VM_RAISE1("wrong number of arguments for map (required 2, got ~d)\n", Object::makeInt(length));
    }
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

Object scheme::find10Ex(Object args)
{
    const Object proc = args.first();
    if (!proc.isClosure() && !proc.isCProcedure()) {
        VM_RAISE1("find closure required, but got ~a\n", proc);
    }
    const int length = Pair::length(args);
    if (length == 2) {
        const Object lst = args.second();
        if (lst.isNil()) return Object::False;
        if (!lst.isPair()) {
            VM_RAISE1("map pair required, but got ~a\n", lst);
        }
        return findIter(proc, lst);
    } else {
        VM_RAISE1("wrong number of arguments for find (required 2, got ~d)\n", Object::makeInt(length));
    }
    return Object::False;
}

// todo incomplete
// (make-custom-binary-input-port id read! get-position set-position! close)
Object scheme::makeCustomBinaryInputPortEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 5) {
        VM_RAISE1("wrong number of arguments for make-custom-binary-input-port (required 5, got ~d)\n", Object::makeInt(length));
    }

    const Object id = args.first();
    if (!id.isString()) {
        VM_RAISE1("make-custom-binary-input-port string required, but got ~a\n", id);
    }

    const Object readProc = args.second();
    if (!readProc.isClosure() && !readProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc required, but got ~a\n", readProc);
    }

    const Object getPositionProc = args.third();
    if (!getPositionProc.isFalse() && !getPositionProc.isClosure() && !getPositionProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc or #f required, but got ~a\n", getPositionProc);
    }

    const Object setPositionProc = args.fourth();
    if (!setPositionProc.isFalse() && !setPositionProc.isClosure() && !setPositionProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc or #f required, but got ~a\n", setPositionProc);
    }

    const Object closeProc = args.fifth();
    if (!closeProc.isFalse() && !closeProc.isClosure() && !closeProc.isCProcedure()) {
        VM_RAISE1("make-custom-binary-input-port proc or #f required, but got ~a\n", closeProc);
    }

    // todo
    return Object::makeCustomBinaryInputPort(readProc);
}

Object scheme::getU8Ex(Object args)
{
    if (args.first().isBinaryInputPort()) {
        return Object::makeInt(args.first().toBinaryInputPort()->getU8());
    } else {
        VM_RAISE1("get-u8 <binary-input-port> required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::bytevectorU8SetEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 3) {
        VM_RAISE1("wrong number of arguments for bytevector-u8-set (required 3, got ~d)\n", Object::makeInt(length));
    }

    const Object bv = args.first();
    if (!bv.isByteVector()) {
        VM_RAISE1("bytevector-u8-set! byte-vector required, but got ~a\n", bv);
    }

    const Object i = args.second();
    if (!i.isInt()) {
        VM_RAISE1("bytevector-u8-set! number required, but got ~a\n", i);
    }

    const Object v = args.third();
    if (!v.isInt()) {
        VM_RAISE1("bytevector-u8-set! number required, but got ~a\n", v);
    }

    bv.toByteVector()->u8set(i.toInt(), v);
    return Object::Undef;
}

Object scheme::transcodedPortEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for transcoded-port (required 2, got ~d)\n", Object::makeInt(length));
    }
    const Object port = args.first();
    const Object transcoder = args.second();
    if (port.isBinaryInputPort() && transcoder.isTranscoder()) {
        return Object::makeTextualInputPort(port.toBinaryInputPort(), transcoder.toTranscoder());
    } else {
        VM_RAISE2("transcoded-port (binary-input-port transcoder) required, but got (~a ~a)\n", port, transcoder);
    }
    return Object::Undef;
}

Object scheme::utf8CodecEx(Object args)
{
    return Object::makeUTF8Codec();
}

Object scheme::makeTranscoderEx(Object args)
{
    if (args.first().isCodec()) {
        return Object::makeTranscoder(args.first().toCodec());
    } else {
        VM_RAISE1("make-transcoder code required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::eofObjectEx(Object args)
{
    return Object::Eof;
}

Object scheme::sysOpenBytevectorOutputPortEx(Object args)
{
    if (args.isNil() || args.first().isFalse()) {
        printf("not implemented binary port %s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
    } else if (args.first().isTranscoder()) {
        return Object::makeTextualByteVectorOuputPort(args.first().toTranscoder());
    } else {
        VM_RAISE1("open-bytevector-output-port transcoder required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::sysGetBytevectorEx(Object args)
{
    TextualByteVectorOutputPort* p = reinterpret_cast<TextualByteVectorOutputPort*>(args.first().toTextualOutputPort());
    return Object::makeByteVector(p->getByteVector());
}

Object scheme::bytevectorU8RefEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for bytevector-u8-ref (required 2, got ~d)\n", Object::makeInt(length));
    }
    const Object bv = args.first();
    const Object i = args.second();
    if (bv.isByteVector() && i.isInt()) {
        return bv.toByteVector()->u8Ref(i.toInt());
    } else {
        VM_RAISE2("bytevector-u8-ref (bytevector number) required, but got (~a ~a)\n", bv, i);
    }
    return Object::Undef;
}

Object scheme::bytevectorLengthEx(Object args)
{
    if (args.first().isByteVector()) {
        return Object::makeInt(args.first().toByteVector()->length());
    } else {
        VM_RAISE1("bytevector-length bytevector required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::standardInputPortEx(Object args)
{
    return theVM->standardInputPort();
}

Object scheme::getBytevectorNEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for get-byte-vector-n (required 2, got ~d)\n", Object::makeInt(length));
    }

    const Object p = args.first();
    if (!p.isBinaryInputPort()) {
        VM_RAISE1("get-byte-vector-n binary-input-port required, but got ~a\n", p);
    }

    const Object count = args.second();
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

Object scheme::utf8TostringEx(Object args)
{
    const Object bv = args.first();
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

Object scheme::openFileOutputPortEx(Object args)
{
    const Object file = args.first();
    if (!file.isString()) {
        VM_RAISE1("open-file-output-port string required, but got ~a\n", file);
    }

    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    Object ret = Object::makeTextualOutputPort(new FileBinaryOutputPort(file.toString()->data()), transcoder);
    return ret;
}

Object scheme::openFileInputPortEx(Object args)
{
    const Object file = args.first();
    if (!file.isString()) {
        VM_RAISE1("open-file-input-port string required, but got ~a\n", file);
    }
    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    return Object::makeTextualInputPort(new FileBinaryInputPort(file.toString()->data()), transcoder);
}

Object scheme::vectorEx(Object args)
{
    const int length = Pair::length(args);
    const Object vec = Object::makeVector(length);
    Vector* const v = vec.toVector();
    int i = 0;
    for (Object s = args; !s.isNil(); s = s.cdr(), i++) {
        v->set(i, s.car());
    }
    return vec;
}

Object scheme::regexpReplaceEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 3) {
        VM_RAISE1("wrong number of arguments for regexp-replace (required 3, got ~d)\n", Object::makeInt(length));
    }

    const Object reg = args.first();
    const Object str = args.second();
    const Object sub = args.third();

    if (reg.isRegexp() && str.isString() && sub.isString()) {
        return reg.toRegexp()->replace(str, sub);
    } else {
        VM_RAISE3("regexp-replace (regexp string string) required, but got (~a ~a ~a)\n", reg, str, sub);
    }
    return Object::Undef;
}

Object scheme::regexpReplaceAllEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 3) {
        VM_RAISE1("wrong number of arguments for regexp-replace-all (required 3, got ~d)\n", Object::makeInt(length));
    }

    const Object reg = args.first();
    const Object str = args.second();
    const Object sub = args.third();

    if (reg.isRegexp() && str.isString() && sub.isString()) {
        return reg.toRegexp()->replaceAll(str, sub);
    } else {
        VM_RAISE3("regexp-replace (regexp string string) required, but got (~a ~a ~a)\n", reg, str, sub);
    }
    return Object::Undef;
}

Object scheme::evalEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for eval (required 2, got ~d)\n", Object::makeInt(length));
    }

    return theVM->eval(args.first(), args.second());
}

Object scheme::raiseEx(Object args)
{
    theVM->raise(args.first());
    return Object::Undef;
}

Object scheme::raiseContinuableEx(Object args)
{
    return theVM->raiseContinuable(args.first());
}


Object scheme::withExceptionHandlerEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for with-exception-handler (required 2, got ~d)\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
    if (arg1.isClosure() && arg2.isClosure()) {
        return theVM->withExceptionHandler(arg1, arg2);
    } else {
        VM_RAISE2("wrong arguments for with-exception-handler required (closure, closure), got (~a ~a)\n", arg1, arg2);
    }
    return Object::Undef;
}

Object scheme::makeVectorTypeEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 5) {
        VM_RAISE1("wrong number of arguments for make-vector-type (required 5, got ~d)\n", Object::makeInt(length));
    }
    const Object name = args.first();
    const Object supertype = args.second();
    const Object data = args.third();
    const Object fieldMutability = args.fourth();

    if (name.isSymbol() && (supertype.isFalse() || supertype.isTypedVectorDesc()) && fieldMutability.isPair()) {
        return Object::makeTypedVectorDesc(name, supertype, data, fieldMutability);
    } else {
        VM_RAISE0("wrong arguments for make-vector-type");
        return Object::Undef;
    }
}

Object scheme::vectorTypePEx(Object args)
{
    return Object::makeBool(args.first().isTypedVectorDesc());
}

Object scheme::vectorTypeDataEx(Object args)
{
    const Object vt = args.first();
    if (vt.isTypedVectorDesc()) {
        return vt.toTypedVectorDesc()->data;
    } else {
        VM_RAISE1("wrong argument for vector-type-data required vector-type, got ~a\n", vt);
        return Object::Undef;
    }
}

Object scheme::vectorTypeInstanceOfPEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for vector-type-instance-of? (required 2, got ~d)\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
    if (arg1.isTypedVector() && arg2.isTypedVectorDesc()) {
        return arg1.toTypedVector()->instanceOf(arg2);
    } else {
        VM_RAISE2("wrong arguments for vector-type-instance-of? required (vector-type-instance, vector-type), got (~a ~a)\n", arg1, arg2);
        return Object::Undef;
    }
}

Object scheme::makeTypedVectorEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for make-typed-vector (required 2, got ~d)\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
    if (arg1.isTypedVectorDesc() && arg2.isPair()) {
        return Object::makeTypedVector(arg1, arg2);
    } else {
        VM_RAISE2("wrong arguments for make-typed-vector required (vector-type-instance, list of args), got (~a ~a)\n", arg1, arg2);
        return Object::Undef;
    }
}

Object scheme::typedVectorGetNthEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for typed-vector-get-nth (required 2, got ~d)\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
    if (arg1.isTypedVector() && arg2.isInt()) {
        return arg1.toTypedVector()->getNth(arg2);
    } else {
        VM_RAISE2("wrong arguments for typed-vector-get-nth required (vector-type-instance, number), got (~a ~a)\n", arg1, arg2);
        return Object::Undef;
    }
}

Object scheme::typedVectorSetNthEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 3) {
        VM_RAISE1("wrong number of arguments for typed-vector-set-nth (required 3, got ~d)\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
    const Object arg3 = args.third();
    if (arg1.isTypedVector() && arg2.isInt()) {
        arg1.toTypedVector()->setNth(arg2, arg3);
    } else {
        VM_RAISE3("wrong arguments for typed-vector-set-nth required (vector-type-instance, number, any object), got (~a ~a ~a)\n", arg1, arg2, arg3);
    }
    return Object::Undef;
}

Object scheme::typedVectorTypeEx(Object args)
{
    const Object vt = args.first();
    if (vt.isTypedVector()) {
        return vt.toTypedVector()->desc;
    } else {
        VM_RAISE1("wrong arguments for typed-vector-type required vector-type-instance, got ~a", vt);
        return Object::Undef;
    }
}

Object scheme::typedVectorPEx(Object args)
{
    return Object::makeBool(args.first().isTypedVector());
}

Object scheme::applyEx(Object args)
{
    const int length = Pair::length(args);
    if (length < 2) {
        VM_RAISE1("wrong number of arguments for apply (required at least 2, got ~d)\n", Object::makeInt(length));
    }

    const Object proc = args.first();
    if (!proc.isCallable()) {
        VM_RAISE1("wrong arguments for apply required closure, got ~a\n", proc);
    }

    Object rest = args.cdr();
    const Object lastPair = Pair::getLastPair(rest);
    const Object last = lastPair.car();

    Object argsAsList = Object::Nil;
    for (int i = 0; i < length - 1; i++) {
        if (i == length - 2) {
            argsAsList = Pair::appendD(argsAsList, rest.car());
        } else {
            argsAsList = Pair::appendD(argsAsList, Pair::list1(rest.car()));
        }
        rest = rest.cdr();
    }
    return theVM->applyClosure(proc, argsAsList);
}

Object scheme::valuesEx(Object args)
{
    return theVM->values(args);
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

Object scheme::modEx(Object args)
{
    const int length = Pair::length(args);
    if (length < 2) {
        VM_RAISE1("wrong number of arguments for mod (required 2, got ~d)\n", Object::makeInt(length));
    }

    Object arg1 = args.first();
    Object arg2 = args.second();
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

Object scheme::divEx(Object args)
{
    const int length = Pair::length(args);
    if (length < 2) {
        VM_RAISE1("wrong number of arguments for div (required 2, got ~d)\n", Object::makeInt(length));
    }

    Object arg1 = args.first();
    Object arg2 = args.second();
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

Object scheme::assqEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for assq (required 2, got ~d)\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
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

Object scheme::exitEx(Object args)
{
    if (args.isNil()) {
        exit(EXIT_SUCCESS);
    }

    const Object arg1 = args.first();
    if (arg1.isInt()) {
        exit(arg1.toInt());
    } else {
        VM_RAISE1("wrong arguments for exit required number, got ~a\n", arg1);
    }
}

Object scheme::macroexpand1Ex(Object args)
{
    static Object proc = Symbol::intern(UC("pass1/macroexpand"));
    return theVM->callClosureByName(proc, args.first());
}

Object scheme::procedurePEx(Object args)
{
    const Object arg1 = args.first();
    return Object::makeBool(arg1.isClosure() || arg1.isCProcedure());
}

Object scheme::loadEx(Object args)
{
    if (args.first().isString()) {
        theVM->loadFile(args.first().toString()->data());
    } else {
        VM_RAISE1("load string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::symbolPEx(Object args)
{
    return Object::makeBool(args.car().isSymbol());
}

Object scheme::dynamicWindEx(Object args)
{
    const int length = Pair::length(args);
    if (length != 3) {
        VM_RAISE1("wrong number of arguments for dynamic-wind (required 3, got ~d)\n", Object::makeInt(length));
    }
    const Object arg1 = args.first();
    const Object arg2 = args.second();
    const Object arg3 = args.third();
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

Object scheme::charGePEx(Object args)
{
   const int length = Pair::length(args);
   if (length < 2) {
       VM_RAISE1("wrong number of arguments for char>=? required at least 2, got ~d)\n", Object::makeInt(length));
   }
   for (Object p = args;;) {
       const Object o = p.car();
       if (!o.isChar()) {
           VM_RAISE1("char>=? char required, but got ~a\n", o);
       }
       if (p.cdr().isNil()) break;
       const Object n = p.cdr().car();
       if (!n.isChar()) {
           VM_RAISE1("char>=? char required, but got ~a\n", n);
       }
       if (o.toChar() < n.toChar()) {
           return Object::False;
       }
       p = p.cdr();
   }
   return Object::True;
}

Object scheme::charGtPEx(Object args)
{
   const int length = Pair::length(args);
   if (length < 2) {
       VM_RAISE1("wrong number of arguments for char>? required at least 2, got ~d)\n", Object::makeInt(length));
   }
   for (Object p = args;;) {
       const Object o = p.car();
       if (!o.isChar()) {
           VM_RAISE1("char>? char required, but got ~a\n", o);
       }
       if (p.cdr().isNil()) break;
       const Object n = p.cdr().car();
       if (!n.isChar()) {
           VM_RAISE1("char>? char required, but got ~a\n", n);
       }
       if (o.toChar() <= n.toChar()) {
           return Object::False;
       }
       p = p.cdr();
   }
   return Object::True;
}

Object scheme::charLePEx(Object args)
{
   const int length = Pair::length(args);
   if (length < 2) {
       VM_RAISE1("wrong number of arguments for char<=? required at least 2, got ~d)\n", Object::makeInt(length));
   }
   for (Object p = args;;) {
       const Object o = p.car();
       if (!o.isChar()) {
           VM_RAISE1("char<=? char required, but got ~a\n", o);
       }
       if (p.cdr().isNil()) break;
       const Object n = p.cdr().car();
       if (!n.isChar()) {
           VM_RAISE1("char<=? char required, but got ~a\n", n);
       }
       if (o.toChar() > n.toChar()) {
           return Object::False;
       }
       p = p.cdr();
   }
   return Object::True;
}

Object scheme::charLtPEx(Object args)
{
   const int length = Pair::length(args);
   if (length < 2) {
       VM_RAISE1("wrong number of arguments for char<? required at least 2, got ~d)\n", Object::makeInt(length));
   }
   for (Object p = args;;) {
       const Object o = p.car();
       if (!o.isChar()) {
           VM_RAISE1("char<? char required, but got ~a\n", o);
       }
       if (p.cdr().isNil()) break;
       const Object n = p.cdr().car();
       if (!n.isChar()) {
           VM_RAISE1("char<? char required, but got ~a\n", n);
       }
       if (o.toChar() >= n.toChar()) {
           return Object::False;
       }
       p = p.cdr();
   }
   return Object::True;
}

Object scheme::vectorTolistEx(Object args)
{
   const int length = Pair::length(args);
   if (length != 1) {
       VM_RAISE1("wrong number of arguments for vector->list required 1, got ~d)\n", Object::makeInt(length));
   }
   const Object arg = args.first();
   if (!arg.isVector()) {
       VM_RAISE1("vector->list vector required, but got ~a\n", arg);
   }
   Vector* const v = arg.toVector();
   const int vLength = v->length();
   Object ret = Object::Nil;
   for (int i = vLength - 1; i >= 0; i--) {
       ret = Object::cons(v->ref(i), ret);
   }
   return ret;
}

Object scheme::callProcessEx(Object args)
{
    const int BUFFER_SIZE = 1024;
    const Object cmd = args.first();
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

Object scheme::internalgetClosureNameEx(Object args)
{
    return theVM->getClosureName(args.first());
}

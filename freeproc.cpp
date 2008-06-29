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

#define checkArgLength(required, argc, proc)   \
    if (argc != required) { \
        VM_RAISE1("wrong number of argument for " proc " required " #required ", got ~d\n", Object::makeInt(argc)); \
    } \



// Object scheme::dummy(Object obj)
// {
//     return Object::False;
// }

Object scheme::currentErrorPortEx(Object args, int argc, Object* argv)
{
    printf("current-error-port called\n");
    return Object::UnBound;

}

// Object scheme::hashtableKeysEx(Object args, int argc, Object* argv)
// {
//     const Object ht = args.first();
//     if (!ht.isEqHashTable()) {
//         VM_RAISE1("hashtable-keys hash-table required, but got ~a\n", ht);
//     }
//     return ht.toEqHashTable()->keys();
// }
// Object scheme::hashtableSetDEx(Object args, int argc, Object* argv)
// {
//     const Object ht = args.first();
//     if (!ht.isEqHashTable()) {
//         VM_RAISE1("hashtable-set! hash-table required, but got ~a\n", ht);
//     }

//     if (Pair::length(args) == 3) {
//         const Object key = args.second();
//         const Object val = args.third();
//         ht.toEqHashTable()->set(key, val);
//         return Object::Undef;
//     } else {
//         VM_RAISE1("wrong number of arguments for hashtable-set! (required 3, got ~d)\n", Object::makeInt(Pair::length(args)));
//     }
//     return Object::UnBound;
// }

// Object scheme::hashtableRefEx(Object args, int argc, Object* argv)
// {
//     const Object ht = args.first();
//     if (!ht.isEqHashTable()) {
//         VM_RAISE1("hashtable-ref hash-table required, but got ~a\n", ht);
//     }

//     const int length = Pair::length(args);
//     if (length == 3) {
//         const Object key = args.second();
//         const Object defaultVal = args.third();
//         return ht.toEqHashTable()->ref(key, defaultVal);
//     } else if (length == 2) {
//         const Object key = args.second();
//         return ht.toEqHashTable()->ref(key, Object::False);
//     } else {
//         VM_RAISE1("wrong number of arguments for hash-table-get (required 2 or 3, got ~d)\n", Object::makeInt(Pair::length(args)));
//     }
//     return Object::UnBound;
// }

Object scheme::makeEqHashtableEx(Object args, int argc, Object* argv)
{
    return Object::makeEqHashTable();
}

Object scheme::hashtableKeysEx(Object args, int argc, Object* argv)
{
    checkArgLength(1, argc, "hash-table-keys");
    const Object ht = argv[0];
    if (!ht.isEqHashTable()) {
        VM_RAISE1("hashtable-keys hash-table required, but got ~a\n", ht);
    }
    return ht.toEqHashTable()->keys();
}
Object scheme::hashtableSetDEx(Object args, int argc, Object* argv)
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

Object scheme::hashtableRefEx(Object args, int argc, Object* argv)
{
    if (argc == 2 || argc == 3) {
        const Object ht = argv[0];
        if (!ht.isEqHashTable()) {
            VM_RAISE1("hashtable-ref hash-table required, but got ~a\n", ht);
        }
        const Object key = argv[1];
        const Object defaultVal = (argc == 3 ? argv[2] : Object::False);
        return ht.toEqHashTable()->ref(key, defaultVal);
//     } else if (argc == 2) {
//         const Object ht = argv[0];
//         if (!ht.isEqHashTable()) {
//             VM_RAISE1("hashtable-ref hash-table required, but got ~a\n", ht);
//         }
//         const Object key = argv[1];
//         return ht.toEqHashTable()->ref(key, Object::False);
    } else {
        VM_RAISE1("wrong number of arguments for hash-table-get (required 2 or 3, got ~d)\n", Object::makeInt(argc));
    }
    return Object::UnBound;
}



// Object scheme::makeEqHashtableEx(Object args, int argc, Object* argv)
// {
// //    checkArgLength(0, argc, "make-eq-hashtable");
//     return Object::makeEqHashTable();
// }



Object scheme::numberPEx(Object args, int argc, Object* argv)
{
//    checkArgLength(1, argc, "number?");
    RETURN_BOOL(args.first().isInt());
}

Object scheme::consEx(Object args, int argc, Object* argv)
{
//    checkArgLength(2, argc, "cons");
    return Object::cons(args.first(), args.second());
}

Object scheme::carEx(Object args, int argc, Object* argv)
{
//    checkArgLength(1, argc, "car");
    if (args.first().isPair()) {
        return args.first().car();
    } else {
        VM_RAISE1("car pair required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::cdrEx(Object args, int argc, Object* argv)
{
    if (args.first().isPair()) {
        return args.first().cdr();
    } else {
        VM_RAISE1("cdr pair required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::sourceInfoEx(Object args, int argc, Object* argv)
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

Object scheme::setSourceInfoDEx(Object args, int argc, Object* argv)
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

Object scheme::nullPEx(Object args, int argc, Object* argv)
{
    if (args.isPair()) {
        return Object::makeBool(args.first().isNil());
    } else {
        return Object::False;
    }
}

Object scheme::setCarDEx(Object args, int argc, Object* argv)
{
    printf("set-car! called\n");
    return Object::UnBound;
}

Object scheme::setCdrDEx(Object args, int argc, Object* argv)
{
    printf("set-cdr! called\n");
    return Object::UnBound;
}

Object scheme::sysDisplayEx(Object args, int argc, Object* argv)
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

Object scheme::rxmatchEx(Object args, int argc, Object* argv)
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

Object scheme::regexpPEx(Object args, int argc, Object* argv)
{
    RETURN_BOOL(args.first().isRegexp());
}

Object scheme::regexpTostringEx(Object args, int argc, Object* argv)
{
    const Object arg1 = args.first();
    if (!arg1.isRegexp()) {
        VM_RAISE1("regexp->string regexp required, but got ~a\n", arg1);
    }
    return Object::makeString(arg1.toRegexp()->pattern());
}

Object scheme::rxmatchStartEx(Object args, int argc, Object* argv)
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

Object scheme::rxmatchEndEx(Object args, int argc, Object* argv)
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

Object scheme::rxmatchAfterEx(Object args, int argc, Object* argv)
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

Object scheme::rxmatchBeforeEx(Object args, int argc, Object* argv)
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

Object scheme::rxmatchSubstringEx(Object args, int argc, Object* argv)
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

Object scheme::regMatchProxy(Object args, int argc, Object* argv)
{
    const Object match = args.first();
    const Object rest  = args.cdr();
    if (rest.isPair() && rest.car() == Symbol::AFTER) {
        return rxmatchAfterEx(Object::cons(match, rest.cdr()), 0, NULL);
    } else if (rest.isPair() && rest.car() == Symbol::BEFORE) {
        return rxmatchBeforeEx(Object::cons(match, rest.cdr()), 0, NULL);
    } else {
        return rxmatchSubstringEx(args, 0, NULL);
    }
}

Object scheme::makeStringEx(Object args, int argc, Object* argv)
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

Object scheme::stringSetDEx(Object args, int argc, Object* argv)
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

Object scheme::stringLengthEx(Object args, int argc, Object* argv)
{
    if (args.first().isString()) {
        return Object::makeInt(args.first().toString()->data().length());
    } else {
        VM_RAISE1("string-length string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::stringTosymbolEx(Object args, int argc, Object* argv)
{
    if (args.first().isString()) {
        return Symbol::intern(args.first().toString()->data().c_str());
    } else {
        VM_RAISE1("string->symbol string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::stringTonumberEx(Object args, int argc, Object* argv)
{
    if (args.first().isString()) {
        return Object::makeInt(atoi(args.first().toString()->data().ascii_c_str()));
    } else {
        VM_RAISE1("string->number string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::stringAppendEx(Object args, int argc, Object* argv)
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

Object scheme::stringSplitEx(Object args, int argc, Object* argv)
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

Object scheme::numberTostringEx(Object args, int argc, Object* argv)
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

Object scheme::reverseEx(Object args, int argc, Object* argv)
{
    return Pair::reverse(args.first());
//    return reverseIter(args.car(), Object::Nil);
}

Object scheme::eofObjectPEx(Object args, int argc, Object* argv)
{
    RETURN_BOOL(args.first().isEof());
}

Object scheme::readCharEx(Object args, int argc, Object* argv)
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

Object scheme::readEx(Object args, int argc, Object* argv)
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

Object scheme::charEqPEx(Object args, int argc, Object* argv)
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

Object scheme::stringPEx(Object args, int argc, Object* argv)
{
    return Object::makeBool(args.car().isString());
}

Object scheme::sysGetenvEx(Object args, int argc, Object* argv)
{
    if (args.first().isString()) {
        const char* str = getenv(args.first().toString()->data().ascii_c_str());
        return str == NULL ? Object::False : Object::makeString(str);
    } else {
        VM_RAISE1("getenv string required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::equalPEx(Object args, int argc, Object* argv)
{
    if (Pair::length(args) == 2) {
        return args.first().equal(args.second());
    } else {
        VM_RAISE1("wrong number of arguments for equal? (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::openStringInputPortEx(Object args, int argc, Object* argv)
{
    if (args.first().isString()) {
        return Object::makeStringInputPort(args.first().toString()->data());
    } else {
        VM_RAISE1("open-string-input-port string required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::sysOpenOutputStringEx(Object args, int argc, Object* argv)
{
    return Object::makeStringOutputPort();
}

Object scheme::sysPortSeekEx(Object args, int argc, Object* argv)
{

//todo
    return Object::UnBound;
}


Object scheme::openOutputFileEx(Object args, int argc, Object* argv)
{
    printf("open-output-file called\n");
    return Object::UnBound;
}

Object scheme::closeOutputPortEx(Object args, int argc, Object* argv)
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

Object scheme::closeInputPortEx(Object args, int argc, Object* argv)
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
Object scheme::digitTointegerEx(Object args, int argc, Object* argv)
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

Object scheme::getRemainingInputStringEx(Object args, int argc, Object* argv)
{
    printf("get-remaining-input-string called\n");
    return Object::UnBound;
}

Object scheme::sysReaddirEx(Object args, int argc, Object* argv)
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

Object scheme::fileExistsPEx(Object args, int argc, Object* argv)
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
Object scheme::sysGetOutputStringEx(Object args, int argc, Object* argv)
{
    StringTextualOutputPort* p = reinterpret_cast<StringTextualOutputPort*>(args.first().toTextualOutputPort());
    return Object::makeString(p->getString());
}

Object scheme::stringToregexpEx(Object args, int argc, Object* argv)
{
    if (args.first().isString()) {
        return Object::makeRegexp(args.first().toString()->data());
    } else {
        VM_RAISE1("string->regexp string required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::charTointegerEx(Object args, int argc, Object* argv)
{
    if (args.first().isChar()) {
        return Object::makeInt(args.first().toChar());
    } else {
        VM_RAISE1("char->integer char required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::integerTocharEx(Object args, int argc, Object* argv)
{
    if (args.first().isInt()) {
        return Object::makeChar(args.first().toInt());
    } else {
        VM_RAISE1("integer->char number required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::errorfEx(Object args, int argc, Object* argv)
{
    theVM->raiseFormat(args.car().toString()->data().c_str(), args.cdr());
    return Object::Undef;
}

Object scheme::formatEx(Object args, int argc, Object* argv)
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

Object scheme::currentInputPortEx(Object args, int argc, Object* argv)
{
    return theVM->currentInputPort();
}

Object scheme::currentOutputPortEx(Object args, int argc, Object* argv)
{
    printf("current-output-port called\n");
    return Object::UnBound;
}

Object scheme::setCurrentInputPortDEx(Object args, int argc, Object* argv)
{
    if (args.first().isTextualInputPort()) {
        theVM->setInputPort(args.first());
    } else {
        VM_RAISE1("set-current-input-port! <textual-port> required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::setCurrentOutputPortDEx(Object args, int argc, Object* argv)
{
    if (args.first().isTextualOutputPort()) {
        theVM->setOutputPort(*(args.first().toTextualOutputPort()));
    } else {
        VM_RAISE1("set-current-output-port! <textual-port> required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::charPEx(Object args, int argc, Object* argv)
{
    RETURN_BOOL(args.first().isChar());
}

Object scheme::writeEx(Object args, int argc, Object* argv)
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

Object scheme::gensymEx(Object args, int argc, Object* argv)
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

Object scheme::stringEqPEx(Object args, int argc, Object* argv)
{
    printf("string=? called\n");
    return Object::UnBound;
}

Object scheme::vectorPEx(Object args, int argc, Object* argv)
{
    printf("vector? called\n");
    return Object::UnBound;
}

Object scheme::listPEx(Object args, int argc, Object* argv)
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

Object scheme::memqEx(Object args, int argc, Object* argv)
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

Object scheme::memvEx(Object args, int argc, Object* argv)
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


Object scheme::eqPEx(Object args, int argc, Object* argv)
{
    if (Pair::length(args) == 2) {
        RETURN_BOOL(args.first() == args.second());
    } else {
        VM_RAISE1("wrong number of arguments for eq? (required 2, got ~d)\n", Object::makeInt(Pair::length(args)));
    }
    return Object::UnBound;
}

Object scheme::memberEx(Object args, int argc, Object* argv)
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

Object scheme::booleanPEx(Object args, int argc, Object* argv)
{
    const Object arg1 = args.first();
    if (arg1.isFalse() || arg1.isTrue()) {
        return Object::True;
    } else {
        return Object::False;
    }
}

Object scheme::symbolTostringEx(Object args, int argc, Object* argv)
{
    if (args.first().isSymbol()) {
        return Object::makeString(args.first().toSymbol()->c_str());
    } else {
        VM_RAISE1("symbol->string symbol required, but got ~a\n", args.first());
    }
    return Object::UnBound;
}

Object scheme::stringRefEx(Object args, int argc, Object* argv)
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

Object scheme::errorEx(Object args, int argc, Object* argv)
{
    VM_RAISE1("error : ~a", args);
    return Object::UnBound;
}

Object scheme::getTimeofdayEx(Object args, int argc, Object* argv)
{
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return Object::cons(Object::makeInt(tv.tv_sec), Object::makeInt(tv.tv_usec));
}

// Object scheme::valuesEx(Object args, int argc, Object* argv)
// {
// //    VM_LOG1("values=~d ", Object::makeInt(Pair::length(args)));
//     fflush(stderr);
//     return Object::makeValues(args);
// }

Object scheme::vmApplyEx(Object args, int argc, Object* argv)
{
    return theVM->apply(args.first(), args.second());
}

Object scheme::pairPEx(Object args, int argc, Object* argv)
{
    RETURN_BOOL(args.first().isPair());
}

Object scheme::initLibraryTableEx(Object args, int argc, Object* argv)
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
Object scheme::map10Ex(Object args, int argc, Object* argv)
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

Object scheme::find10Ex(Object args, int argc, Object* argv)
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
Object scheme::makeCustomBinaryInputPortEx(Object args, int argc, Object* argv)
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

Object scheme::getU8Ex(Object args, int argc, Object* argv)
{
    if (args.first().isBinaryInputPort()) {
        return Object::makeInt(args.first().toBinaryInputPort()->getU8());
    } else {
        VM_RAISE1("get-u8 <binary-input-port> required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::bytevectorU8SetDEx(Object args, int argc, Object* argv)
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

Object scheme::transcodedPortEx(Object args, int argc, Object* argv)
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

Object scheme::utf8CodecEx(Object args, int argc, Object* argv)
{
    return Object::makeUTF8Codec();
}

Object scheme::makeTranscoderEx(Object args, int argc, Object* argv)
{
    if (args.first().isCodec()) {
        return Object::makeTranscoder(args.first().toCodec());
    } else {
        VM_RAISE1("make-transcoder code required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::eofObjectEx(Object args, int argc, Object* argv)
{
    return Object::Eof;
}

Object scheme::sysOpenBytevectorOutputPortEx(Object args, int argc, Object* argv)
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

Object scheme::sysGetBytevectorEx(Object args, int argc, Object* argv)
{
    TextualByteVectorOutputPort* p = reinterpret_cast<TextualByteVectorOutputPort*>(args.first().toTextualOutputPort());
    return Object::makeByteVector(p->getByteVector());
}

Object scheme::bytevectorU8RefEx(Object args, int argc, Object* argv)
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

Object scheme::bytevectorLengthEx(Object args, int argc, Object* argv)
{
    if (args.first().isByteVector()) {
        return Object::makeInt(args.first().toByteVector()->length());
    } else {
        VM_RAISE1("bytevector-length bytevector required, but got ~a\n", args.first());
    }
    return Object::Undef;
}

Object scheme::standardInputPortEx(Object args, int argc, Object* argv)
{
    return theVM->standardInputPort();
}

Object scheme::getBytevectorNEx(Object args, int argc, Object* argv)
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

Object scheme::utf8TostringEx(Object args, int argc, Object* argv)
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

Object scheme::openFileOutputPortEx(Object args, int argc, Object* argv)
{
    const Object file = args.first();
    if (!file.isString()) {
        VM_RAISE1("open-file-output-port string required, but got ~a\n", file);
    }

    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    Object ret = Object::makeTextualOutputPort(new FileBinaryOutputPort(file.toString()->data()), transcoder);
    return ret;
}

Object scheme::openFileInputPortEx(Object args, int argc, Object* argv)
{
    const Object file = args.first();
    if (!file.isString()) {
        VM_RAISE1("open-file-input-port string required, but got ~a\n", file);
    }
    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    return Object::makeTextualInputPort(new FileBinaryInputPort(file.toString()->data()), transcoder);
}

Object scheme::vectorEx(Object args, int argc, Object* argv)
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

Object scheme::regexpReplaceEx(Object args, int argc, Object* argv)
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

Object scheme::regexpReplaceAllEx(Object args, int argc, Object* argv)
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

Object scheme::evalEx(Object args, int argc, Object* argv)
{
    const int length = Pair::length(args);
    if (length != 2) {
        VM_RAISE1("wrong number of arguments for eval (required 2, got ~d)\n", Object::makeInt(length));
    }

    return theVM->eval(args.first(), args.second());
}

Object scheme::raiseEx(Object args, int argc, Object* argv)
{
    theVM->raise(args.first());
    return Object::Undef;
}

Object scheme::raiseContinuableEx(Object args, int argc, Object* argv)
{
    return theVM->raiseContinuable(args.first());
}


Object scheme::withExceptionHandlerEx(Object args, int argc, Object* argv)
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

Object scheme::makeVectorTypeEx(Object args, int argc, Object* argv)
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

Object scheme::vectorTypePEx(Object args, int argc, Object* argv)
{
    return Object::makeBool(argv[0].isTypedVectorDesc());
}

Object scheme::vectorTypeDataEx(Object args, int argc, Object* argv)
{
    const Object vt = argv[0];
    if (vt.isTypedVectorDesc()) {
        return vt.toTypedVectorDesc()->data;
    } else {
        VM_RAISE1("wrong argument for vector-type-data required vector-type, got ~a\n", vt);
        return Object::Undef;
    }
}

Object scheme::vectorTypeInstanceOfPEx(Object args, int argc, Object* argv)
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

Object scheme::makeTypedVectorEx(Object args, int argc, Object* argv)
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

Object scheme::typedVectorGetNthEx(Object args, int argc, Object* argv)
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

Object scheme::typedVectorSetNthEx(Object args, int argc, Object* argv)
{
    checkArgLength(3, argc, "typed-vector-set-nth");
//     const int length = Pair::length(args);
//     if (length != 3) {
//         VM_RAISE1("wrong number of arguments for typed-vector-set-nth (required 3, got ~d)\n", Object::makeInt(length));
//     }
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

Object scheme::typedVectorTypeEx(Object args, int argc, Object* argv)
{
    const Object vt = argv[0];
    if (vt.isTypedVector()) {
        return vt.toTypedVector()->desc;
    } else {
        VM_RAISE1("wrong arguments for typed-vector-type required vector-type-instance, got ~a", vt);
        return Object::Undef;
    }
}

Object scheme::typedVectorPEx(Object args, int argc, Object* argv)
{
    return Object::makeBool(argv[0].isTypedVector());
}

Object scheme::applyEx(Object args, int argc, Object* argv)
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
//     const int length = Pair::length(args);
//     if (length < 2) {
//         VM_RAISE1("wrong number of arguments for apply (required at least 2, got ~d)\n", Object::makeInt(length));
//     }

//     const Object proc = args.first();
//     if (!proc.isCallable()) {
//         VM_RAISE1("wrong arguments for apply required closure, got ~a\n", proc);
//     }

//     Object rest = args.cdr();
//     const Object lastPair = Pair::getLastPair(rest);
//     const Object last = lastPair.car();

//     Object argsAsList = Object::Nil;
//     for (int i = 0; i < length - 1; i++) {
//         if (i == length - 2) {
//             argsAsList = Pair::appendD(argsAsList, rest.car());
//         } else {
//             argsAsList = Pair::appendD(argsAsList, Pair::list1(rest.car()));
//         }
//         rest = rest.cdr();
//     }
//     return theVM->applyClosure(proc, argsAsList);

}

Object scheme::valuesEx(Object args, int argc, Object* argv)
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

Object scheme::modEx(Object args, int argc, Object* argv)
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

Object scheme::divEx(Object args, int argc, Object* argv)
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

Object scheme::assqEx(Object args, int argc, Object* argv)
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

Object scheme::exitEx(Object args, int argc, Object* argv)
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

Object scheme::macroexpand1Ex(Object args, int argc, Object* argv)
{
    static Object proc = Symbol::intern(UC("pass1/macroexpand"));
    return theVM->callClosureByName(proc, argv[0]);
}

Object scheme::procedurePEx(Object args, int argc, Object* argv)
{
    checkArgLength(1, argc, "procedure");
    const Object arg1 = argv[0];
    return Object::makeBool(arg1.isClosure() || arg1.isCProcedure());
}

Object scheme::loadEx(Object args, int argc, Object* argv)
{
    checkArgLength(1, argc, "load");
    if (argv[0].isString()) {
        theVM->loadFile(argv[0].toString()->data());
    } else {
        VM_RAISE1("load string required, but got ~a\n", argv[0]);
    }
    return Object::Undef;
}

Object scheme::symbolPEx(Object args, int argc, Object* argv)
{
    checkArgLength(1, argc, "symbol?");
    return Object::makeBool(argv[0].isSymbol());
}

Object scheme::dynamicWindEx(Object args, int argc, Object* argv)
{
    checkArgLength(3, argc, "dynamic-wind");
//     const int length = Pair::length(args);
//     if (length != 3) {
//         VM_RAISE1("wrong number of arguments for dynamic-wind (required 3, got ~d)\n", Object::makeInt(length));
//     }
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

Object scheme::charGePEx(Object args, int argc, Object* argv)
{
//    const int length = Pair::length(args);
//    if (length < 2) {
//        VM_RAISE1("wrong number of arguments for char>=? required at least 2, got ~d)\n", Object::makeInt(length));
//    }
//    for (Object p = args;;) {
//        const Object o = p.car();
//        if (!o.isChar()) {
//            VM_RAISE1("char>=? char required, but got ~a\n", o);
//        }
//        if (p.cdr().isNil()) break;
//        const Object n = p.cdr().car();
//        if (!n.isChar()) {
//            VM_RAISE1("char>=? char required, but got ~a\n", n);
//        }
//        if (o.toChar() < n.toChar()) {
//            return Object::False;
//        }
//        p = p.cdr();
//    }
//    return Object::True;
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

Object scheme::charGtPEx(Object args, int argc, Object* argv)
{
//    const int length = Pair::length(args);
//    if (length < 2) {
//        VM_RAISE1("wrong number of arguments for char>? required at least 2, got ~d)\n", Object::makeInt(length));
//    }
//    for (Object p = args;;) {
//        const Object o = p.car();
//        if (!o.isChar()) {
//            VM_RAISE1("char>? char required, but got ~a\n", o);
//        }
//        if (p.cdr().isNil()) break;
//        const Object n = p.cdr().car();
//        if (!n.isChar()) {
//            VM_RAISE1("char>? char required, but got ~a\n", n);
//        }
//        if (o.toChar() <= n.toChar()) {
//            return Object::False;
//        }
//        p = p.cdr();
//    }
//   return Object::True;
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

Object scheme::charLePEx(Object args, int argc, Object* argv)
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

//    const int length = Pair::length(args);
//    if (length < 2) {
//        VM_RAISE1("wrong number of arguments for char<=? required at least 2, got ~d)\n", Object::makeInt(length));
//    }
//    for (Object p = args;;) {
//        const Object o = p.car();
//        if (!o.isChar()) {
//            VM_RAISE1("char<=? char required, but got ~a\n", o);
//        }
//        if (p.cdr().isNil()) break;
//        const Object n = p.cdr().car();
//        if (!n.isChar()) {
//            VM_RAISE1("char<=? char required, but got ~a\n", n);
//        }
//        if (o.toChar() > n.toChar()) {
//            return Object::False;
//        }
//        p = p.cdr();
//    }
//    return Object::True;
}

Object scheme::charLtPEx(Object args, int argc, Object* argv)
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

//    const int length = Pair::length(args);
//    if (length < 2) {
//        VM_RAISE1("wrong number of arguments for char<? required at least 2, got ~d)\n", Object::makeInt(length));
//    }
//    for (Object p = args;;) {
//        const Object o = p.car();
//        if (!o.isChar()) {
//            VM_RAISE1("char<? char required, but got ~a\n", o);
//        }
//        if (p.cdr().isNil()) break;
//        const Object n = p.cdr().car();
//        if (!n.isChar()) {
//            VM_RAISE1("char<? char required, but got ~a\n", n);
//        }
//        if (o.toChar() >= n.toChar()) {
//            return Object::False;
//        }
//        p = p.cdr();
//    }
//    return Object::True;
}

Object scheme::vectorTolistEx(Object args, int argc, Object* argv)
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
//    const int length = Pair::length(args);
//    if (length != 1) {
//        VM_RAISE1("wrong number of arguments for vector->list required 1, got ~d)\n", Object::makeInt(length));
//    }
//    const Object arg = args.first();
//    if (!arg.isVector()) {
//        VM_RAISE1("vector->list vector required, but got ~a\n", arg);
//    }
//    Vector* const v = arg.toVector();
//    const int vLength = v->length();
//    Object ret = Object::Nil;
//    for (int i = vLength - 1; i >= 0; i--) {
//        ret = Object::cons(v->ref(i), ret);
//    }
//    return ret;

}

Object scheme::callProcessEx(Object args, int argc, Object* argv)
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
//     const int BUFFER_SIZE = 1024;
//     const Object cmd = args.first();
//     if (!cmd.isString()) {
//        VM_RAISE1("call-process string required, but got ~a\n", cmd);
//     }

//     FILE* in = popen(cmd.toString()->data().ascii_c_str(), "r");
//     char buffer[BUFFER_SIZE];
//     if (in == NULL) {
//         VM_RAISE1("call-process failed: ~a\n", cmd);
//     }

//     memset(buffer, '\0', BUFFER_SIZE);

//     ucs4string ret;
//     int size;
//     while ((size = fread(buffer, sizeof(char), BUFFER_SIZE, in)) > 0) {
//         ret += ucs4string::from_c_str(buffer, size);
//     }
//     if (pclose(in) != 0) {
//         VM_RAISE0("call-process failed\n");
//     }
//     return Object::makeString(ret);

}

Object scheme::internalgetClosureNameEx(Object args, int argc, Object* argv)
{
    return theVM->getClosureName(argv[0]);
//    return theVM->getClosureName(args.first());
}

Object scheme::appendEx(Object args, int argc, Object* argv)
{
//     if (args.isNil()) return Object::Nil;
//         gc_vector<Object> lists;
//         for (Object p = args; p.isPair(); p = p.cdr()) {
//             lists.push_back(p.car());
//         }
//         Object ret = lists[lists.size() - 1];
//         for (int i = lists.size() - 2; i >= 0; i--) {
//             if (!lists[i].isPair()) {
//                 // error
//             }
//             ret = Pair::append2(lists[i], ret);
//         }
//         return ret;
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

Object scheme::append2Ex(Object args, int argc, Object* argv)
{
    checkArgLength(2, argc, "append2");
    return Pair::append2(argv[0], argv[1]);
}

Object scheme::appendAEx(Object args, int argc, Object* argv)
{
    checkArgLength(2, argc, "appendA");
    return Pair::append2(argv[0], argv[1]);
}


Object scheme::appendDEx(Object args, int argc, Object* argv)
{
//     if (args.isNil()) return Object::Nil;
//     gc_vector<Object> lists;
//     for (Object p = args; p.isPair(); p = p.cdr()) {
//         lists.push_back(p.car());
//     }
//     Object ret = lists[lists.size() - 1];
//     for (int i = lists.size() - 2; i >= 0; i--) {
//         if (!lists[i].isPair()) {
//             // error
//         }
//         ret = Pair::appendD2(lists[i], ret);
//     }
//     return ret;
    if (0 == argc) return Object::Nil;
//     gc_vector<Object> lists;
//     for (Object p = args; p.isPair(); p = p.cdr()) {
//         lists.push_back(p.car());
//     }
    Object ret = argv[argc - 1];
    for (int i = argc - 2; i >= 0; i--) {
        if (!argv[i].isPair()) {
            // error
        }
        ret = Pair::appendD2(argv[i], ret);
    }
    return ret;

}

// Object scheme::internalsetUnionEx(Object args, int argc, Object* argv)
// {
//     static long seenTime = 0;
//     static long restTime = 0;

//     const Object list1 = args.first();
//     const Object list2 = args.second();
// //    printf("set-union %d %d\n",Pair::length(list1), Pair::length(list2));

//     if (list1.isNil()) {
//         return list2;
//     } else if (list2.isNil()) {
//         return list1;
//     }
//     Object ret = list2;
//     EqHashTable seen;

//     struct timeval tv1, tv2;
//     struct timezone tz1, tz2;

//     for (Object p = ret; p.isPair(); p = p.cdr()) {
//         seen.set(p.car(), Object::True);
//     }


//     static const Object notFound = Symbol::intern(UC("%%NOTFOUND%%"));
// //    gettimeofday(&tv1, &tz1);

//     for (Object p = list1; p.isPair(); p = p.cdr()) {
//         const Object o = p.car();

//         const Object found = seen.ref(o, notFound);
//         if (found == notFound) {
// //    gettimeofday(&tv1, &tz1);
//             ret = Object::cons(o, ret);
// //    gettimeofday(&tv2, &tz2);
// //    seenTime += (tv2.tv_sec - tv1.tv_sec) * 1000 * 1000 + (tv2.tv_usec - tv1.tv_usec);

// //    gettimeofday(&tv1, &tz1);

//             seen.set(o, Object::True);
// //     gettimeofday(&tv2, &tz2);
// //     restTime += (tv2.tv_sec - tv1.tv_sec) * 1000 * 1000 + (tv2.tv_usec - tv1.tv_usec);

//         }

//     }
// //    printf("seen = %ld rest = %ld %d\n", seenTime, restTime, i);
//     return ret;
// }

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

Object scheme::pass3FindFreeEx(Object args, int argc, Object* argv)
{
//    return findFree(args.first(), args.second(), args.third());
    return findFree(argv[0], argv[1], argv[2]);
}

Object scheme::pass3FindSetsEx(Object args, int argc, Object* argv)
{
//    return findSets(args.first(), args.second());
    return findSets(argv[0], argv[1]);
}

Object scheme::findFree(Object iform, Object locals, Object canFrees)
{
    const Object ret = findFreeRec(iform, locals, canFrees, Object::Nil);
//    VM_LOG1("find-free = ~a\n", uniq(ret));
    return uniq(ret);
//   (uniq (rec iform locals '())))
}


Object scheme::findFreeRec(Object i, Object l, Object canFrees, Object labelsSeen)
{
    const int CONST = 0;
    const int LET = 2;
    const int SEQ = 3;
    const int LAMBDA = 4;
    const int LOCAL_REF = 5;
    const int LOCAL_ASSIGN = 6;
    const int GLOBAL_REF = 7;
    const int GLOBAL_ASSIGN = 8;
    const int UNDEF = 9;
    const int IF = 10;
    const int ASM = 11;
    const int DEFINE = 12;
    const int CALL_CC = 13;
    const int CALL = 14;
    const int LABEL = 15;
    const int LIST = 16;
    const int LIBRARY = 17;
    const int IMPORT = 18;
    const int IT = 20;
    const int RECEIVE = 21;

    Vector* v = i.toVector();
    switch(v->ref(0).toInt()) {
    case CONST:
        return Object::Nil;
    case LET:
    {
//        [(= $LET t)
//         (append ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($let.inits i))
//                 (rec ($let.body i) ($let.lvars i) labels-seen))]

        const Object letLvars = v->ref(2);
        const Object letInits = v->ref(3);
        const Object letBody = v->ref(4);
        return Pair::append2(findFreeRecMap(l, canFrees, labelsSeen, letInits),
                             findFreeRec(letBody, letLvars, canFrees, labelsSeen));
    }
    case RECEIVE:
    {
//        [(= $RECEIVE t)
//         (append (rec ($receive.vals i) l labels-seen)
//                 (rec ($receive.body i) ($receive.lvars i) labels-seen))]
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        const Object receiveLVars = v->ref(1);
        return Pair::append2(findFreeRec(receiveVals, l, canFrees, labelsSeen),
                             findFreeRec(receiveBody, receiveLVars, canFrees, labelsSeen));

    }
    case SEQ:
    {
//        [(= $SEQ t)
//         ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($seq.body i))]
        const Object seqBody = v->ref(1);
        return findFreeRecMap(l, canFrees, labelsSeen, seqBody);
    }
    case LAMBDA:
    {
//        [(= $LAMBDA t)
//         (rec ($lambda.body i) ($lambda.lvars i) labels-seen)]
        const Object lambdaBody = v->ref(6);
        const Object lambdaLvars = v->ref(5);
        return findFreeRec(lambdaBody, lambdaLvars, canFrees, labelsSeen);
    }
    case LOCAL_ASSIGN:
    {
//        [(= $LOCAL-ASSIGN t)
//         (let1 lvar ($local-assign.lvar i)
//           (append (if (memq lvar can-frees) (list lvar) '())
//                   (rec ($local-assign.val i) l labels-seen)))]
        const Object lvar = v->ref(1);
        const Object val = v->ref(2);
        if (memq(lvar, canFrees).isFalse()) {
            return findFreeRec(val, l, canFrees, labelsSeen);
        } else {
            return Object::cons(lvar, findFreeRec(val, l, canFrees, labelsSeen));
        }
//         return Pair::append2(!memq(lvar, canFrees).isFalse() ? Pair::list1(lvar) : Object::Nil,
//                              findFreeRec(val, l, canFrees, labelsSeen));
    }
    case LOCAL_REF:
    {
//        [(= $LOCAL-REF t)
//         (let1 lvar ($local-ref.lvar i)
//           (cond [(memq lvar l) '()]
//                 [(memq lvar can-frees) (list lvar)]
//                 [else '()]))]
        const Object lvar = v->ref(1);
        if (!memq(lvar, l).isFalse()) {
            return Object::Nil;
        } else if (!memq(lvar, canFrees).isFalse()) {
            return Pair::list1(lvar);
        } else {
            return Object::Nil;
        }
    }
    case GLOBAL_REF:
    {
//        [(= $GLOBAL-REF t)
//         (let* ([sym ($global-ref.sym i)]
//                [found (find10 (lambda (x) (eq? ($lvar.sym x) sym)) can-frees)])
//           (if found (list found) '()))]
        const Object sym = v->ref(2);
        Object found = Object::False;
        for (Object p = canFrees; p.isPair(); p = p.cdr()) {
            const Object lvarSym = p.car().toVector()->ref(1);
            if (lvarSym == sym) {
                found = p.car();
                break;
            }
        }
        return !found.isFalse() ? Pair::list1(found) : Object::Nil;
    }
    case UNDEF:
//        [(= $UNDEF t)      '()]
        return Object::Nil;
    case IF:
    {
//        [(= $IF t)
//         (append (rec ($if.test i) l labels-seen)
//                 (rec ($if.then i) l labels-seen)
//                 (rec ($if.else i) l labels-seen))]
        const Object testF = findFreeRec(v->ref(1), l, canFrees, labelsSeen);
        const Object thenF = findFreeRec(v->ref(2), l, canFrees, labelsSeen);
        const Object elseF = findFreeRec(v->ref(3), l, canFrees, labelsSeen);
        return Pair::append2(testF, Pair::append2(thenF, elseF));
    }
    case ASM:
    {
//        [(= $ASM t)
//         ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($asm.args i))]
        const Object asmArgs = v->ref(2);
        return findFreeRecMap(l, canFrees, labelsSeen, asmArgs);
    }
    case DEFINE:
    {
//        [(= $DEFINE t)
//         (rec ($define.val i) l labels-seen)]
        const Object defineVal = v->ref(3);
        return findFreeRec(defineVal, l, canFrees, labelsSeen);
    }
    case CALL:
    {
//        [(= $CALL t)
//         ;; N.B.
//         ;; (proc args)
//         ;;   args are evaluate before proc, so you should find free variables of args at first.
//        (append
//          ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($call.args i))
//          (rec ($call.proc i) l labels-seen)
//                 )]
        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findFreeRecMap(l, canFrees, labelsSeen, callArgs),
                             findFreeRec(callProc, l, canFrees, labelsSeen));
    }
    case CALL_CC:
    {
//        [(= $CALL-CC t)
//         (rec ($call-cc.proc i) l labels-seen)]
        const Object callccProc = v->ref(1);
        return findFreeRec(callccProc, l, canFrees, labelsSeen);
    }
    case GLOBAL_ASSIGN:
    {
//        [(= $GLOBAL-ASSIGN t)
//         (rec ($global-assign.val i) l labels-seen)]
        const Object globalAssignVal = v->ref(3);
        return findFreeRec(globalAssignVal, l, canFrees, labelsSeen);
    }
    case LIST:
    {
//        [(= $LIST t)
//         ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($list.args i))]
        const Object listArgs = v->ref(1);
        return findFreeRecMap(l, canFrees, labelsSeen, listArgs);
    }
    case LABEL:
    {
//        [(= $LABEL t)
//         (if (memq i labels-seen)
//             '()
//             (rec ($label.body i) l (cons i labels-seen)))]
        const Object labelBody = v->ref(2);
        if (!memq(i, labelsSeen).isFalse()) {
            return Object::Nil;
        } else {
            findFreeRec(labelBody, l, canFrees, Object::cons(i, labelsSeen));
        }
    }
    case IMPORT:
//        [(= $IMPORT t)
//         '() ;; todo 本当?
//         ]
        return Object::Nil;
    case LIBRARY:
//        [(= $LIBRARY t)
//         '() ;; todo 本当?
//         ]

        return Object::Nil;
    case IT:
//        [(= $IT t) '()]
        return Object::Nil;
    default:
//        [else
//         (error "pass3/find-free unknown iform:" (tag i))])))
        VM_RAISE1("pass3/find-free unknown iform: ~a", v->ref(0));
        break;
    }
    return Object::Undef;
}

// callee should check <list>.
Object scheme::memq(Object o, Object list)
{
    for (Object p = list; p != Object::Nil; p = p.cdr()) {
        if (p.car() == o) {
            return p;
        }
    }
    return Object::False;
}


Object scheme::findFreeRecMap(Object l, Object canFrees, Object labelsSeen, Object list)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        ret = Pair::append2(ret, findFreeRec(p.car(), l, canFrees, labelsSeen));
    }
    return ret;
}

Object scheme::findSetsRecMap(Object lvars, Object list)
{
    Object ret = Object::Nil;
    for (Object p = list; p.isPair(); p = p.cdr()) {
        ret = Pair::append2(ret, findSetsRec(p.car(), lvars));
    }
    return ret;
}

Object scheme::findSets(Object iform, Object lvars)
{
    return uniq(findSetsRec(iform, lvars));
}

Object scheme::findSetsRec(Object i, Object lvars)
{
    const int CONST = 0;
    const int LET = 2;
    const int SEQ = 3;
    const int LAMBDA = 4;
    const int LOCAL_REF = 5;
    const int LOCAL_ASSIGN = 6;
    const int GLOBAL_REF = 7;
    const int GLOBAL_ASSIGN = 8;
    const int UNDEF = 9;
    const int IF = 10;
    const int ASM = 11;
    const int DEFINE = 12;
    const int CALL_CC = 13;
    const int CALL = 14;
    const int LABEL = 15;
    const int LIST = 16;
    const int LIBRARY = 17;
    const int IMPORT = 18;
    const int IT = 20;
    const int RECEIVE = 21;

    Vector* v = i.toVector();
    switch(v->ref(0).toInt()) {
    case CONST:
//        [(= $CONST t) '()]
        return Object::Nil;
    case LET:
    {
//        [(= $LET t)
//         (append ($append-map1 rec ($let.inits i))
//                 (rec ($let.body i)))]
        const Object letInits = v->ref(3);
        const Object letBody = v->ref(4);
        return Pair::append2(findSetsRecMap(lvars, letInits),
                             findSetsRec(letBody, lvars));
    }
    case RECEIVE:
    {
//        [(= $RECEIVE t)
//         (append (rec ($receive.vals i))
//                 (rec ($receive.body i)))]
        const Object receiveVals = v->ref(4);
        const Object receiveBody = v->ref(5);
        return Pair::append2(findSetsRec(receiveVals, lvars),
                             findSetsRec(receiveBody, lvars));
    }
    case SEQ:
    {
//        [(= $SEQ t)
//         ($append-map1 rec ($seq.body i))]
        const Object seqBody = v->ref(1);
        return findSetsRecMap(lvars, seqBody);
    }
    case LAMBDA:
    {
//        [(= $LAMBDA t)
//         (rec ($lambda.body i))]
        const Object lambdaBody = v->ref(6);
        return findSetsRec(lambdaBody, lvars);
    }
    case LOCAL_ASSIGN:
    {
//        [(= $LOCAL-ASSIGN t)
//         (let1 lvar ($local-assign.lvar i)
//           (append (if (memq lvar lvars) (list lvar) '())
//                   (rec ($local-assign.val i))))]
        const Object localAssignLvar = v->ref(1);
        const Object localAssignVal = v->ref(2);
        if (memq(localAssignLvar, lvars).isFalse()) {
            return findSetsRec(localAssignVal, lvars);
        } else {
            return Object::cons(localAssignLvar, findSetsRec(localAssignVal, lvars));
        }
//         return Pair::append2(!memq(localAssignLvar, lvars).isFalse() ? Pair::list1(localAssignLvar) : Object::Nil,
//                              findSetsRec(localAssignVal, lvars));
    }
    case LOCAL_REF:
    {
//        [(= $LOCAL-REF t)  '()]
        return Object::Nil;
    }
    case GLOBAL_REF:
    {
//        [(= $GLOBAL-REF t) '()]
        return Object::Nil;
    }
    case UNDEF:
//        [(= $UNDEF t)      '()]
        return Object::Nil;
    case IF:
    {
//        [(= $IF t)
//         (append (rec ($if.test i))
//                 (rec ($if.then i))
//                 (rec ($if.else i)))]
        const Object testF = findSetsRec(v->ref(1), lvars);
        const Object thenF = findSetsRec(v->ref(2), lvars);
        const Object elseF = findSetsRec(v->ref(3), lvars);
        return Pair::append2(testF, Pair::append2(thenF, elseF));
    }
    case ASM:
    {
//        [(= $ASM t)
//         ($append-map1 rec ($asm.args i))]
        const Object asmArgs = v->ref(2);
        return findSetsRecMap(lvars, asmArgs);
    }
    case DEFINE:
    {
//        [(= $DEFINE t)
//         (rec ($define.val i))]
        const Object defineVal = v->ref(3);
        return findSetsRec(defineVal, lvars);
    }
    case CALL:
    {
//        [(= $CALL t)
//         (append
//          ($append-map1 rec ($call.args i))
//          (rec ($call.proc i))
//                 )]
        const Object callArgs = v->ref(2);
        const Object callProc = v->ref(1);
        return Pair::append2(findSetsRecMap(lvars, callArgs),
                             findSetsRec(callProc, lvars));
    }
    case CALL_CC:
    {
//        [(= $CALL-CC t)
//         (rec ($call-cc.proc i))]
        const Object callccProc = v->ref(1);
        return findSetsRec(callccProc, lvars);
    }
    case GLOBAL_ASSIGN:
    {
//        [(= $GLOBAL-ASSIGN t)
//         (rec ($global-assign.val i))]
        const Object globalAssignVal = v->ref(3);
        return findSetsRec(globalAssignVal, lvars);
    }
    case LIST:
    {
//        [(= $LIST t)
//         ($append-map1 rec ($list.args i))]
        const Object listArgs = v->ref(1);
        return findSetsRecMap(lvars, listArgs);
    }
    case LABEL:
    {
//        [(= $LABEL t)
//         '() ;; todo 本当
//         ]
        return Object::Nil;
    }
    case IMPORT:
//        [(= $IMPORT t)
//         '() ;; todo 本当?
//         ]
        return Object::Nil;
    case LIBRARY:
//        [(= $LIBRARY t)
//         '() ;; todo 本当?
//         ]

        return Object::Nil;
    case IT:
//        [(= $IT t) '()]
        return Object::Nil;
    default:
        VM_RAISE1("pass3/find-sets unknown iform: ~a", v->ref(0));
        break;
    }
    return Object::Undef;
}

// Object scheme::pass4FixupLabelCollect(Object vec)
// {
//     static const Object NOP                   = Symbol::intern(UC("NOP"));
//     static const Object UNFIXED_JUMP          = Symbol::intern(UC("UNFIXED_JUMP"));
//     static const Object TEST                  = Symbol::intern(UC("TEST"));
//     static const Object NUMBER_LE_TEST        = Symbol::intern(UC("NUMBER_LE_TEST"));
//     static const Object NOT_TEST              = Symbol::intern(UC("NOT_TEST"));
//     static const Object REFER_LOCAL0_EQV_TEST = Symbol::intern(UC("REFER_LOCAL0_EQV_TEST"));
//     static const Object FRAME                 = Symbol::intern(UC("FRAME"));
//     static const Object PUSH_FRAME            = Symbol::intern(UC("PUSH_FRAME"));
//     static const Object CLOSURE               = Symbol::intern(UC("CLOSURE"));
//     const int LABEL = 15;

//     const Vector* const v = vec.toVector();
//     const int length = v->length();
//     const Object ret = Object::makeVector(length, NOP);
//     Vector* const rv= ret.toVector();
//     Object labels = Object::Nil;
//     for (int i = 0, j = 0; i < length;) {
//         const Object insn = v->ref(i);
//         if (insn == UNFIXED_JUMP          ||
//             insn == TEST                  ||
//             insn == NUMBER_LE_TEST        ||
//             insn == NOT_TEST              ||
//             insn == REFER_LOCAL0_EQV_TEST ||
//             insn == FRAME                 ||
//             insn == PUSH_FRAME            ||
//             insn == CLOSURE) {
//             rv->set(j, insn);
//             rv->set(j + 1, v->ref(i + 1));
//             i += 2;
//             j += 2;
//         } else if (insn.isVector() && insn.toVector()->length() > 0 &&
//                    insn.toVector()->ref(0).toInt() == LABEL) {
//             i++;
//             labels = Object::cons(Object::cons(insn, Object::makeInt(j)), labels);
//         } else {
//             rv->set(j, insn);
//             i++;
//             j++;
//         }
//     }
//     return Object::cons(ret, labels);
// }

Object scheme::assq(Object o, Object alist)
{
    for (Object p = alist; p.isPair(); p = p.cdr()) {
        if (p.car().car() == o) {
            return p.car();
        }
    }
    return Object::False;
}

// // コンパイラはインストラクションをシンボルで持ってなかった。。
// Object scheme::pass4FixupLabel(Object vec)
// {
//     static const Object UNFIXED_JUMP          = Symbol::intern(UC("UNFIXED_JUMP"));
//     static const Object TEST                  = Symbol::intern(UC("TEST"));
//     static const Object NUMBER_LE_TEST        = Symbol::intern(UC("NUMBER_LE_TEST"));
//     static const Object NOT_TEST              = Symbol::intern(UC("NOT_TEST"));
//     static const Object REFER_LOCAL0_EQV_TEST = Symbol::intern(UC("REFER_LOCAL0_EQV_TEST"));
//     static const Object FRAME                 = Symbol::intern(UC("FRAME"));
//     static const Object PUSH_FRAME            = Symbol::intern(UC("PUSH_FRAME"));
//     static const Object CLOSURE               = Symbol::intern(UC("CLOSURE"));
//     static const Object LOCAL_JMP             = Symbol::intern(UC("LOCAL_JMP"));

//     const Object collected = pass4FixupLabelCollect(vec);
//     Vector* const code = collected.car().toVector();
//     const Object labels = collected.cdr();
//     const int length = code->length();
//     printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//     VM_LOG1("labels = ~a", labels);
// //   (receive (code labels) (collect-labels)
// //     (let1 len (vector-length code)
// //     (let loop ([i 0])
// //       (cond
// //        [(= i len) code]
// //        [else
// //         (let1 insn (vector-ref code i)
// //           (cond
// //            [(eq? insn 'UNFIXED_JUMP)          (pass4/fixup-labels-insn 'LOCAL_JMP)]
// //            [(eq? insn 'CLOSURE)               (pass4/fixup-labels-insn 'CLOSURE)]
// //            [(eq? insn 'TEST)                  (pass4/fixup-labels-insn 'TEST)]
// //            [(eq? insn 'NUMBER_LE_TEST)        (pass4/fixup-labels-insn 'NUMBER_LE_TEST)]
// //            [(eq? insn 'NOT_TEST)              (pass4/fixup-labels-insn 'NOT_TEST)]
// //            [(eq? insn 'REFER_LOCAL0_EQV_TEST) (pass4/fixup-labels-insn 'REFER_LOCAL0_EQV_TEST)]
// //            [(eq? insn 'FRAME)                 (pass4/fixup-labels-insn 'FRAME)]
// //            [(eq? insn 'PUSH_FRAME)            (pass4/fixup-labels-insn 'PUSH_FRAME)]
// //            [else (loop (+ i 1))]))])))))


// //   `(let1 label (assq (vector-ref code (+ i 1)) labels)
// //      (cond
// //       [label
// //        (vector-set! code i ,insn)
// //        (vector-set! code (+ i 1) (- (cdr label) i 1)) ;; jump point
// //        (loop (+ i 2))]
// //       [else
// //        (loop (+ i 1))])))


//     for (int i = 0; i < length;) {
//         const Object insn = code->ref(i);
//             VM_LOG1("insn = ~a\n", insn);
//         if (insn == UNFIXED_JUMP) {
//             const Object label = assq(code->ref(i + 1), labels);
//             if (!labels.isFalse()) {
//                 code->set(i, LOCAL_JMP);
//                 code->set(i + 1, Object::makeInt(label.cdr().toInt() - i - 1));
//                 i += 2;
//             } else {
//                 i++;
//             }
//         } else if (insn == TEST                  ||
//                    insn == NUMBER_LE_TEST        ||
//                    insn == NOT_TEST              ||
//                    insn == REFER_LOCAL0_EQV_TEST ||
//                    insn == FRAME                 ||
//                    insn == PUSH_FRAME            ||
//                    insn == CLOSURE) {
//             const Object label = assq(code->ref(i + 1), labels);
//             if (!labels.isFalse()) {
//                 code->set(i, insn);
//                 code->set(i + 1, Object::makeInt(label.cdr().toInt() - i - 1));
//                 i += 2;
//             } else {
//                 i++;
//             }
//         } else {
//             i++;
//         }
//     }
//     printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//     return collected.car();
// }

// Object scheme::pass4FixupLabelsEx(Object args, int argc, Object* argv)
// {
//     printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//     return pass4FixupLabel(args.first());
// }

Object scheme::makeCodeBuilderEx(Object args, int argc, Object* argv)
{
    return Object::makeCodeBuilder();
}

// Object scheme::codeBuilderPut1DEx(Object args, int argc, Object* argv)
// {
//    const int length = Pair::length(args);
//    if (length != 2) {
//        VM_RAISE1("wrong number of arguments for code-builder-put! required 2, got ~d)\n", Object::makeInt(length));
//    }
//    const Object arg = args.first();
//    if (!arg.isCodeBuilder()) {
//        VM_RAISE1("code-builder required, but got ~a\n", arg);
//    }
//    arg.toCodeBuilder()->put(args.second());
//    return Object::Undef;
// }

// Object scheme::codeBuilderPut2DEx(Object args, int argc, Object* argv)
// {
//    const int length = Pair::length(args);
//    if (length != 3) {
//        VM_RAISE1("wrong number of arguments for code-builder-put2! required 3, got ~d)\n", Object::makeInt(length));
//    }
//    const Object arg = args.first();
//    if (!arg.isCodeBuilder()) {
//        VM_RAISE1("code-builder required, but got ~a\n", arg);
//    }
//    arg.toCodeBuilder()->put(args.second(), args.third());
//    return Object::Undef;
// }

// Object scheme::codeBuilderPut3DEx(Object args, int argc, Object* argv)
// {
//    const int length = Pair::length(args);
//    if (length != 4) {
//        VM_RAISE1("wrong number of arguments for code-builder-put3! required 4, got ~d)\n", Object::makeInt(length));
//    }
//    const Object arg = args.first();
//    if (!arg.isCodeBuilder()) {
//        VM_RAISE1("code-builder required, but got ~a\n", arg);
//    }
//    arg.toCodeBuilder()->put(args.second(), args.third(), args.cdr().cdr().cdr().car());
//    return Object::Undef;
// }

// Object scheme::codeBuilderPut4DEx(Object args, int argc, Object* argv)
// {
//    const int length = Pair::length(args);
//    if (length != 5) {
//        VM_RAISE1("wrong number of arguments for code-builder-put4! required 5, got ~d)\n", Object::makeInt(length));
//    }
//    const Object arg = args.first();
//    if (!arg.isCodeBuilder()) {
//        VM_RAISE1("code-builder required, but got ~a\n", arg);
//    }
//    arg.toCodeBuilder()->put(args.second(), args.third(), args.cdr().cdr().cdr().car(), args.cdr().cdr().cdr().cdr().car());
//    return Object::Undef;
// }
Object scheme::codeBuilderPut1DEx(Object args, int argc, Object* argv)
{
    checkArgLength(2, argc, "code-builder-put1!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1]);
    return Object::Undef;
}


Object scheme::codeBuilderPut2DEx(Object args, int argc, Object* argv)
{
    checkArgLength(3, argc, "code-builder-put2!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1],
                            argv[2]);
    return Object::Undef;
}

Object scheme::codeBuilderPut3DEx(Object args, int argc, Object* argv)
{
    checkArgLength(4, argc, "code-builder-put3!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1],
                            argv[2],
                            argv[3]);
    return Object::Undef;
}

Object scheme::codeBuilderPut4DEx(Object args, int argc, Object* argv)
{
    checkArgLength(5, argc, "code-builder-put4!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1],
                            argv[2],
                            argv[3],
                            argv[4]);
    return Object::Undef;
}


Object scheme::codeBuilderPut5DEx(Object args, int argc, Object* argv)
{
    checkArgLength(6, argc, "code-builder-put5!");
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~a\n", cb);
    }
    cb.toCodeBuilder()->put(argv[1],
                            argv[2],
                            argv[3],
                            argv[4],
                            argv[5]);
    return Object::Undef;
}



Object scheme::codeBuilderAppendDEx(Object args, int argc, Object* argv)
{
    checkArgLength(2, argc, "code-builder-append!");
//    const int length = Pair::length(args);
//    if (length != 2) {
//        VM_RAISE1("wrong number of arguments for code-builder-append! required 2, got ~d)\n", Object::makeInt(length));
//    }
    const Object cbDst = argv[0];
   const Object cbSrc = argv[1];
   if (!cbDst.isCodeBuilder() || !cbSrc.isCodeBuilder()) {
       VM_RAISE2("code-builder required, but got ~a, ~a\n", cbDst, cbSrc);
   }
   cbDst.toCodeBuilder()->append(cbSrc.toCodeBuilder());
   return Object::Undef;
//    const Object arg1 = args.first();
//    const Object arg2 = args.second();
//    if (!arg1.isCodeBuilder() || !arg2.isCodeBuilder()) {
//        VM_RAISE2("code-builder required, but got ~a, ~a\n", arg1, arg2);
//    }
//    arg1.toCodeBuilder()->append(arg2.toCodeBuilder());
//    return Object::Undef;

}

Object scheme::codeBuilderEmitEx(Object args, int argc, Object* argv)
{
    checkArgLength(1, argc, "code-builder-emit2");
//    const int length = Pair::length(args);
//    if (length != 1) {
//        VM_RAISE1("wrong number of arguments for code-builder-emit required 1, got ~d)\n", Object::makeInt(length));
//    }
    const Object cb = argv[0];
    if (!cb.isCodeBuilder()) {
        VM_RAISE1("code-builder required, but got ~an", cb);
    }
   return cb.toCodeBuilder()->emit();
//    const int length = Pair::length(args);
//    if (length != 1) {
//        VM_RAISE1("wrong number of arguments for code-builder-emit required 1, got ~d)\n", Object::makeInt(length));
//    }
//    const Object arg1 = args.first();
//    if (!arg1.isCodeBuilder()) {
//        VM_RAISE1("code-builder required, but got ~an", arg1);
//    }
//    return arg1.toCodeBuilder()->emit();

}

Object scheme::eqHashtableCopyEx(Object args, int argc, Object* argv)
{
    checkArgLength(1, argc, "eq-hashtable-copy");
    const Object ht = argv[0];
    if (!ht.isEqHashTable()) {
        VM_RAISE1("eq-hashtable required, but got ~an", ht);
    }
    return ht.toEqHashTable()->copy();
//     const int length = Pair::length(args);
//     if (length != 1) {
//         VM_RAISE1("wrong number of argument for eq-hashtable-copy required 1, got ~d)\n", Object::makeInt(length));
//    }
//    const Object arg1 = args.first();
//    if (!arg1.isEqHashTable()) {
//        VM_RAISE1("eq-hashtable required, but got ~an", arg1);
//    }
//    return arg1.toEqHashTable()->copy();
}


Object scheme::doNothingEx(Object args, int argc, Object* argv)
{
//    argv[0];
//    printf("argc=%d\n", argc);
    asm volatile("\t # minowa");
    VM_LOG1("arg1=~a", argv[0]);
    VM_LOG1("arg2=~a", argv[1]);
    VM_LOG1("arg3=~a", argv[2]);
    asm volatile("\t # taro");
//    VM_LOG1("arg2=~a", argv[-2]);
//    VM_LOG1("arg3=~a", argv[-1]);


}
// {
//     printf("%d %s %s:%d\n", argc, __func__, __FILE__, __LINE__);fflush(stdout);// debug
//     return Object::True;
//     const int length = Pair::length(args);
//     if (length != 3) {
//         VM_RAISE1("wrong number of argument for eq-hashtable-copy required 1, got ~d)\n", Object::makeInt(length));
//    }

//     const Object arg1 = args.first();
//     const Object arg2 = args.second();
//     const Object arg3 = args.third();
//     if (arg1.isInt() && arg2.isInt() && arg3.isInt()) {
//     return Object::makeInt(arg1.toInt() + arg2.toInt() + arg3.toInt());
//     }
//}

// (define (%set-intersect lst1 lst2)
//   (if (null? lst1)
//       '()
//       (if (memq2 (car lst1) lst2)
//           (cons (car lst1) (%set-intersect (cdr lst1) lst2))
//           (%set-intersect (cdr lst1) lst2))))


// Object setIntersectRec(Object lst1, Object lst2)
// {
//     if (lst1.isNil()) {
//         return Object::Nil;
//     }
//     if (memq(lst1.car(), lst2).isFalse()) {
//         return setIntersectRec(lst1.cdr(), lst2);
//     } else {
//         return Object::cons(lst1.car(), setIntersectRec(lst1.cdr(), lst2));
//     }
// }

// Object setIntersectRec(Object lst1, EqHashTable* seen)
// {
//     static const Object notFound = Symbol::intern(UC("%%NOTFOUND%%"));
//     if (lst1.isNil()) {
//         return Object::Nil;
//     }
//     if (seen->ref(lst1.car(), notFound) != notFound) {
//         return setIntersectRec(lst1.cdr(), seen);
//     } else {
//         return Object::cons(lst1.car(), setIntersectRec(lst1.cdr(), seen));
//     }
// }


// Object scheme::internalsetIntersectEx(Object args, int argc, Object* argv)
// {
//     const Object list2 = args.second();
// //     EqHashTable seen;
// //     for (Object p = list2; p.isPair(); p = p.cdr()) {
// //         seen.set(p.car(), Object::True);
// //     }

//     return setIntersectRec(args.first(), list2);
//     if (list1.isNil()) {
//         return Object::Nil;
//     } else if (list2.isNil()) {
//         return Object::Nil;
//     }
//     EqHashTable seen;
//     for (Object p = list2; p.isPair(); p = p.cdr()) {
//         seen.set(p.car(), Object::True);
//     }

//     Object ret = Object::Nil;
//     const Object notFound = Symbol::intern(UC("%%NOTFOUND%%"));
//     for (Object p = list1; p.isPair(); p = p.cdr()) {
//         const Object o = p.car();
//         if (seen.ref(o, notFound) != notFound) {
//             ret = Object::cons(o, ret);
//         }
//     }
//     return ret;//Pair::reverse(ret);
//}

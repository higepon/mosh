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

#ifdef MONA
#include <monapi.h>
#endif

#ifdef _WIN32
#include <windows.h>
#include <shlwapi.h>
#pragma comment(lib, "shlwapi.lib")
#else
#include <unistd.h>
#endif
#ifdef _MSC_VER
    #include <windows.h> // for FILETIME
    #include "gettimeofday.h"
#else
#include <sys/time.h>
#endif
#ifndef _WIN32
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "scheme.h"
#include "OSCompatThread.h"
#include "EqHashTable.h"
#include "VM.h"
#include "Symbol.h"
#include "Closure.h"
#include "Gloc.h"
#include "VM-inl.h"
#include "UtilityProcedures.h"
#include "ProcedureMacro.h"
#include "PortProcedures.h"
#include "StringProcedures.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Equivalent.h"
#include "ByteArrayBinaryInputPort.h"
#include "UTF8Codec.h"
#include "UTF16Codec.h"
#include "Transcoder.h"
#include "SString.h"
#include "Vector.h"
#include "ByteVector.h"
#include "TextualOutputPort.h"
#include "Closure.h"
#include "Fixnum.h"
#include "Arithmetic.h"
#include "Bignum.h"
#include "OSCompat.h"
#include "FFI.h"

#ifdef _WIN32
    #define popen _popen
    #define pclose _pclose
#endif
using namespace scheme;

Object scheme::osConstantEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("os-constant");
    checkArgumentLength(1);
    argumentCheckSymbol(0, key);
    bool isFound = false;
    const Object ret = getOSConstant(key, isFound);
    if (isFound) {
        return ret;
    } else {
        return Object::False;
    }
}

Object scheme::moshExecutablePathEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("mosh-executable-path");
    checkArgumentLength(0);
    bool isErrorOccured = false;
    ucs4string path = getMoshExecutablePath(isErrorOccured);
    if (isErrorOccured) {
        return Object::False;
    } else {
        return Object(path);
    }
// #if defined(_WIN32)
// //    char path[MAX_PATH];
// //    if (GetModuleFileNameA(NULL, path, sizeof(path))) {
// //        if (PathRemoveFileSpecA(path)) {
// //            PathAddBackslashA(path);
// //            return Object::makeString(path);
// //        }
// //    }
//     TCHAR tmp[MAX_PATH]; /* may be Unicoded */
//     if (GetModuleFileNameW(NULL,tmp,MAX_PATH)) {
//         TCHAR* trm = _tcsinc(_tcsrchr(tmp,_T('\\')));
//         *trm = _T('\0');
//         ByteArrayBinaryInputPort name((uint8_t *)tmp,_tcslen(tmp)*sizeof(TCHAR));
//         UTF16Codec codec(UTF16Codec::UTF_16LE);
//         Transcoder tcoder(&codec);
//         return Object::makeString(tcoder.getString(&name));
//     }
//     return Object::False;
// #elif defined(__linux__)
//     char path[4096];
//     int ret = readlink("/proc/self/exe", path, sizeof(path));
//     if (ret != -1) {
//         std::string chop(path, ret);
//         int pos = chop.find_last_of('/');
//         if (pos > 0) {
//             return Object::makeString(chop.substr(0, pos + 1).c_str());
//         }
//     }
//     return Object::False;
// #elif defined(__FreeBSD__)
//     Dl_info info;
//     char path[PATH_MAX + 1];

//     if (dladdr( (const void*)&main, &info) == 0) {
//         return Object::False;
//     }

//     strncpy(path, info.dli_fname, PATH_MAX + 1);
//     path[PATH_MAX + 1] = '\0';
//     char base[PATH_MAX];
//     if (NULL== realpath(path, base)) {
//         return Object::False;
//     }
//     std::string p = base;
//     int pos = p.find_last_of('/');
//     if (pos > 0) {
//         return Object::makeString(p.substr(0, pos + 1).c_str());
//     }
//     return Object::False;
// #elif defined(__APPLE__)
//     char path[MAXPATHLEN];
//     uint32_t pathLen = MAXPATHLEN;
//     if (_NSGetExecutablePath(path, &pathLen) == 0) {
//         std::string chop(path, pathLen);
//         int pos = chop.find_last_of('/');
//         if (pos > 0) {
//             return Object::makeString(chop.substr(0, pos + 1).c_str());
//         }
//     }
//     return Object::False;
// #elif defined(__sun)
//     char path[4096];
//     char procpath[64];
//     pid_t my_pid = getpid();
//     sprintf(procpath, "/proc/%d/path/a.out", (int)my_pid);
//     int ret = readlink(procpath, path, sizeof(path));
//     if (ret != -1) {
//         std::string chop(path, ret);
//         int pos = chop.find_last_of('/');
//         if (pos > 0) {
//             return Object::makeString(chop.substr(0, pos + 1).c_str());
//         }
//     }
//     return Object::Undef;
// #else
//     return Object::False;
// #endif
}

Object scheme::hostOsEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("host-os");
    checkArgumentLengthAtLeast(0);
// TODO
#ifdef MONA
    return "mona";
#else
    return MOSH_HOST_OS;
#endif
}

Object scheme::booleanEqPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("boolean=?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentCheckBoolean(i, s1);
        argumentCheckBoolean(i + 1, s2);
        if (s1.eq(s2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::symbolEqPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("symbol=?");
    checkArgumentLengthAtLeast(2);
    for (int i = 0; i < argc - 1; i++) {
        argumentCheckSymbol(i, s1);
        argumentCheckSymbol(i + 1, s2);
        if (s1.eq(s2)) {
            continue;
        } else {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::unGenSyms(Object symbols)
{
    MOSH_ASSERT(symbols.isList());
    Object ret = Object::Nil;
    for (Object p = symbols; !p.isNil(); p = p.cdr()) {
        if (p.car().isSymbol()) {
            ret = Object::cons(unGenSym(p.car()), ret);
        } else {
            ret = Object::cons(p.car(), ret);
        }
    }
    return Pair::reverse(ret);
}

Object scheme::unGenSym(Object symbol)
{
    MOSH_ASSERT(symbol.isSymbol());
    ucs4string symbolString = symbol.toSymbol()->c_str();
    gc_vector<ucs4string> splitted;
    symbolString.split('@', splitted);
    if (splitted.size() == 2) {
        return Symbol::intern(splitted[1].strdup());
    } else {
        return symbol;
    }
}


Object scheme::ungensymEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("ungensym");
    checkArgumentLength(1);
    argumentCheckSymbol(0, symbol);
    return unGenSym(symbol);
}

Object scheme::makeCompilerInstructionEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-compiler-instruction");
    checkArgumentLength(1);
    argumentAsFixnum(0, n);
    return Object::makeCompilerInstruction(n);
}

Object scheme::makeInstructionEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-instruction");
    checkArgumentLength(1);
    argumentAsFixnum(0, n);
    return Object::makeInstruction(n);
}

Object scheme::bytevectorPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isByteVector());
}

Object scheme::numberTostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("number->string");
    checkArgumentLengthBetween(1, 3);
    argumentCheckNumber(0, z);
    if (3 == argc) {
        argumentAsPositiveFixnum(2, precision);
    }

    if (2 == argc || 3 == argc) {
        // we ignore precision parameter
        argumentAsFixnum(1, radix);
        if (z.isFlonum()) {
            if (radix == 10) {
                return Arithmetic::numberToString(z, 10);
            } else {
                callAssertionViolationAfter(theVM, procedureName, "radix should be 10 for flonum", L1(argv[1]));
                return Object::Undef;
            }
        } else {
            if (radix == 2 || radix == 8 || radix == 10 || radix == 16) {
                return Arithmetic::numberToString(z, radix);
            } else {
                callAssertionViolationAfter(theVM, procedureName, "radix should be 2, 8, 10 ro 16", L1(argv[1]));
                return Object::Undef;
            }
        }
    } else {
        return Arithmetic::numberToString(z, 10);
    }
}

Object scheme::charEqPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char=?");
    checkArgumentLengthAtLeast(2);
    argumentAsChar(0, startCharacter);

    for (int i = 1; i < argc; i++) {
        argumentAsChar(i, currentCharacter);
        if (startCharacter != currentCharacter) {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::stringPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isString());
}

Object scheme::getEnvironmentVariableEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-environment-variable");
    checkArgumentLength(1);
    argumentAsString(0, text);
    const ucs4char* value = getEnv(text->data());
    return nullptr == value ? Object::False : Object::makeString(value);
}

Object scheme::getEnvironmentVariablesEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-environment-variables");
    checkArgumentLength(0);
    return getEnvAlist();
}

Object scheme::equalPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("equal?");
    checkArgumentLength(2);
    Equal e;
    return Object::makeBool(e.equal(argv[0], argv[1]));
}

Object scheme::fastEqualPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("fast-equal?");
    checkArgumentLength(2);
    return Object::makeBool(fastEqual(argv[0], argv[1]));
}


// todo from gauche
Object scheme::digitTointegerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("digit->integer");
    argumentAsChar(0, ch);
    argumentAsFixnum(1, radix);
    if (ch < '0') {
        return Object::False;
    }
    if (radix <= 10) {
        if (ch < '0' + radix) {
            return Object::makeFixnum(ch - '0');
        }
    } else {
        if (ch <= '9') return Object::makeFixnum(ch - '0');
        if (ch < 'A') return Object::False;
        if (ch < 'A' + radix - 10) return Object::makeFixnum(ch - 'A' + 10);
        if (ch < 'a') return Object::False;
        if (ch < 'a' + radix - 10) return Object::makeFixnum(ch - 'a' + 10);
    }
    return Object::False;
}

Object scheme::getRemainingInputStringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-remaning-input-string");
    callNotImplementedAssertionViolationAfter(theVM, procedureName);
    return Object::UnBound;
}

Object scheme::charTointegerEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char->integer");
    checkArgumentLength(1);
    argumentAsChar(0, ch);
    return Object::makeFixnum(ch);
}

Object scheme::integerTocharEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("integer->char");
    checkArgumentLength(1);
    argumentAsFixnum(0, integer);
    if (!ucs4string::isValidScalar(integer)) {
        callAssertionViolationAfter(theVM, procedureName, "code point out of range", L1(argv[0]));
        return Object::Undef;
    }

    return Object::makeChar(integer);
}

Object scheme::charPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isChar());
}

static const char* gensymPrefix = "a";

Object scheme::gensymPrefixSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("gensym-prefix-set!");
    checkArgumentLength(1);
    argumentAsSymbol(0, prefix);
    gensymPrefix = ucs4string(prefix->c_str()).ascii_c_str();
    return Object::Undef;
}

// This gensym returns "interned symbol"
// See psyntax-r6rs/rev10_to_10/psyntax/compat.ss
Object scheme::gensymEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("gensym");
    checkArgumentLengthBetween(0, 1);

    static int next = 0;
    char ubuf[32];
#ifdef USE_BOEHM_GC
    ucs4char* ibuf = new(PointerFreeGC) ucs4char[32];
#else
    ucs4char* ibuf = new ucs4char[32];
#endif

    if (0 == argc) {
        sprintf(ubuf, "%s%x", gensymPrefix, next++);
        const int len = strlen(ubuf) + 1;
        for (int i = 0; i < len; i++) {
            ibuf[i] = ubuf[i];
        }

        return Symbol::intern(ibuf);
    } else {
        sprintf(ubuf, "%s%x@", gensymPrefix, next++);
        const int len = strlen(ubuf) + 1;
        for (int i = 0; i < len; i++) {
            ibuf[i] = ubuf[i];
        }
        ucs4string val = ibuf;
        const Object sym = argv[1];
        if (sym.isSymbol()) {
            val += sym.toSymbol()->c_str();
            return Symbol::intern(val.strdup());
        } else {
            sprintf(ubuf, "%s%x", gensymPrefix, next++);
            const int len = strlen(ubuf) + 1;
            for (int i = 0; i < len; i++) {
                ibuf[i] = ubuf[i];
            }

            return Symbol::intern(ibuf);
        }
    }
//     if (1 == argc) {
//         if (argv[1].isSymbol()) {
//             return Object::makeSymbol(format(UC("~a@~a"), Pair::list2(ibuf, argv[1])).toString()->data().c_str());
//         } else {
//             return Object::makeSymbol(ibuf);
//         }
//     } else {
//         return Object::makeSymbol(ibuf);
//     }
}

Object scheme::vectorPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vector?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isVector());
}

Object scheme::vectorFillDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vector-fill!");
    checkArgumentLength(2);
    argumentAsVector(0, v);
    v->fill(argv[1]);
    return Object::Undef;

}

Object scheme::eqPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eq?");
    checkArgumentLength(2);
    return Object::makeBool(argv[0] == argv[1]);
}

Object scheme::eqvPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eqv?");
    checkArgumentLength(2);
    return Object::makeBool(eqv(argv[0], argv[1]));
}

Object scheme::booleanPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("boolean?");
    checkArgumentLength(1);
    const Object arg1 = argv[0];
    if (arg1.isFalse() || arg1.isTrue()) {
        return Object::True;
    } else {
        return Object::False;
    }
}

Object scheme::symbolTostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("symbol->string");
    checkArgumentLength(1);
    argumentAsSymbol(0, symbol);
    return Object::makeString(symbol->c_str());
}

// // todo
// Object scheme::errorEx(VM* theVM, int argc, const Object* argv)
// {
//     DeclareProcedureName("error");
//     checkArgumentLengthAtLeast(1);

//     const Object stringPort = Object::makeStringOutputPort();
//     TextualOutputPort* port = stringPort.toTextualOutputPort();
//     for (int i = argc - 1; i >= 0; i--) {
//         port->display(argv[i]);
//         port->display(" ");
//     }
//     VM_RAISE1("error : ~a", sysGetOutputStringEx(1, &stringPort));
//     return Object::UnBound;
// }


Object scheme::getTimeofdayEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-timeofday");
    checkArgumentLength(0);
#ifdef MONA
    int64_t nowMsec = MonAPI::Date::nowInMsec();
    return Object::cons(Object::makeBignum(nowMsec/1000), Object::makeBignum((nowMsec - (nowMsec/1000) * 1000) * 1000));
#else
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return Object::cons(Object::makeBignum(tv.tv_sec), Object::makeBignum(tv.tv_usec));
#endif
}

Object scheme::vmApplyEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vm-apply");
    checkArgumentLength(2);
    return theVM->vmapply(argv[0], argv[1]);
}

Object scheme::pairPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("pair?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isPair());
}

Object scheme::vectorEx(VM* theVM, int argc, const Object* argv)
{
    const Object vec = Object::makeVector(argc);
    Vector* const v = vec.toVector();
    for (int i = 0; i < argc; i++) {
        v->set(i, argv[i]);
    }
    return vec;
}


Object scheme::evalEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eval");
    checkArgumentLength(2);
    return theVM->evalAfter(argv[0]);
}

Object scheme::evalCompiledEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eval-compiled");
    checkArgumentLength(1);
    return theVM->evalCompiledAfter(argv[0]);
}


Object scheme::applyEx(VM* theVM, int argc, const Object* argv)
{
    MOSH_FATAL("you should not call apply directory");
    return Object::Undef;
}

Object scheme::valuesEx(VM* theVM, int argc, const Object* argv)
{
    // We don't use DeclareProcedureName here,
    // since we can't call theVM->setNumValues1 here.
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

Object scheme::modEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("mod");
    checkArgumentLength(2);

    argumentAsFixnum(0, number1);
    argumentAsFixnum(1, number2);

    if (0 == number2) {
        callAssertionViolationAfter(theVM, procedureName, "Dividing by zero");
        return Object::Undef;
    }
    return Object::makeFixnum(mod(number1, number2));
}

// Object scheme::divEx(VM* theVM, int argc, const Object* argv)
// {
//     DeclareProcedureName("div");
//     checkArgumentLength(2);

//     argumentAsFixnum(0, number1);
//     argumentAsFixnum(1, number2);
//     if (0 == number2) {
//         callAssertionViolationAfter(procedureName, "Dividing by zero");
//         return Object::Undef;
//     }
//     return Object::makeFixnum(div(number1, number2));
// }


Object scheme::exitEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("exit");
    checkArgumentLengthBetween(0, 1);

    theVM->flushAllPorts();

    if (theVM->isMainThread()) {
        if (0 == argc) {
            exit(EXIT_SUCCESS);
        }
        const Object exitValue = argv[0];
        if (exitValue.isFixnum()) {
            exit(exitValue.toFixnum());
        } else if (exitValue.isFalse()) {
            exit(EXIT_FAILURE);
        } else {
            exit(EXIT_FAILURE);
        }
    } else { // return value of vm-join!
        Thread::exit(new Object(argv[0]));
    }
    return Object::Undef;
}

Object scheme::macroexpand1Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("macroexpand1");
    checkArgumentLength(1);
    static Object proc = Symbol::intern(UC("pass1/macroexpand"));
    return theVM->callClosureByName(proc, argv[0]);
}

Object scheme::procedurePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("procedure");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isProcedure());
}

Object scheme::loadEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("load");
    checkArgumentLength(1);
    argumentAsString(0, path);
    theVM->loadFileUnsafe(path->data());
    return Object::Undef;
}

Object scheme::symbolPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("symbol?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isSymbol());
}

Object scheme::charGePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char>=?");
    checkArgumentLengthAtLeast(2);

    for (int i = 0; i < argc; i++) {
        argumentAsChar(i, ch);
        if (i == argc - 1) break;

        argumentAsChar(i + 1, chNext);
        if (ch < chNext) {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::charGtPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char>?");
    checkArgumentLengthAtLeast(2);

    for (int i = 0; i < argc; i++) {
        argumentAsChar(i, ch);
        if (i == argc - 1) break;

        argumentAsChar(i + 1, chNext);
        if (ch <= chNext) {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::charLePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char<=?");
    checkArgumentLengthAtLeast(2);

    for (int i = 0; i < argc; i++) {
        argumentAsChar(i, ch);
        if (i == argc - 1) break;

        argumentAsChar(i + 1, chNext);
        if (ch > chNext) {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::charLtPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char<?");
    checkArgumentLengthAtLeast(2);

    for (int i = 0; i < argc; i++) {
        argumentAsChar(i, ch);
        if (i == argc - 1) break;

        argumentAsChar(i + 1, chNext);
        if (ch >= chNext) {
            return Object::False;
        }
    }
    return Object::True;
}

Object scheme::vectorTolistEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vector->list");
    checkArgumentLength(1);

    argumentAsVector(0, v);

    const int vLength = v->length();
    Object ret = Object::Nil;
    for (int i = vLength - 1; i >= 0; i--) {
        ret = Object::cons(v->ref(i), ret);
    }
    return ret;
}

// (start-process command-line . args)
//    Unlike call-process, start-process just starts a process, doen't wait it's termination.
//    On Mona (start-process comand-line standard-stream-haneld).
Object scheme::internalStartProcessEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%start-process");
    checkArgumentLengthBetween(1, 2);
    argumentAsString(0, cmd);
#ifdef MONA
    uint32_t stdoutHandle = NULL;
    if (argc == 1) {
        stdoutHandle = MonAPI::System::getProcessStdoutID();
    } else {
        argumentAsPointer(1, s);
        MonAPI::Stream* stream = (MonAPI::Stream*)(s->pointer());
        stdoutHandle = stream->handle();
    }
    uint32_t tid;
    int result = monapi_process_execute_file_get_tid(cmd->data().ascii_c_str(),
                                                     MONAPI_TRUE,
                                                     &tid,
                                                     MonAPI::System::getProcessStdinID(),
                                                     stdoutHandle);
    if (result != M_OK) {
        callAssertionViolationAfter(theVM, procedureName, "can't execute process", L1(argv[0]));
    }
    return Object::Undef;
#else
    (void) cmd;
    return callAssertionViolationAfter(theVM, procedureName, "not implmented", L1(argv[0]));
#endif
}

Object scheme::internalCallProcessEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%call-process");
    checkArgumentLengthBetween(1, 2);

    argumentAsString(0, cmd);
#ifdef _WIN32
        callAssertionViolationAfter(theVM, procedureName, "not supported", L1(argv[0]));
        return Object::Undef;
#elif defined(MONA)
    uint32_t tid;
    uint32_t outHandle;
    if (argc == 2) {
        argumentAsPointer(1, s);
        MonAPI::Stream* stream = (MonAPI::Stream*)(s->pointer());
        outHandle = stream->handle();
    } else {
        outHandle = MonAPI::System::getProcessStdoutID();
    }
    MonAPI::Stream* s = MonAPI::System::getStdoutStream();
    int result = monapi_process_execute_file_get_tid(cmd->data().ascii_c_str(),
                                                     MONAPI_TRUE,
                                                     &tid,
                                                     MonAPI::System::getProcessStdinID(),
                                                     outHandle);
    if (result != M_OK) {
        return callAssertionViolationAfter(theVM, procedureName, "can't execute process", L1(argv[0]));
    }
    // todo values
    return Bignum::makeIntegerFromSigned<intptr_t>(monapi_process_wait_terminated(tid));
#else
    const int BUFFER_SIZE = 1024;
    FILE* in = popen(cmd->data().ascii_c_str(), "r");
    char buffer[BUFFER_SIZE];
    if (nullptr == in) {
        callAssertionViolationAfter(theVM, procedureName, "failed", L1(argv[0]));
        return Object::Undef;
    }

    memset(buffer, '\0', BUFFER_SIZE);

    ucs4string ret;
    int size;
    while ((size = fread(buffer, sizeof(char), BUFFER_SIZE, in)) > 0) {
        ret += ucs4string::from_c_str(buffer, size);
    }

    int status = pclose(in);
    if (status == -1) {
        callAssertionViolationAfter(theVM, procedureName, "failed. pclose returned error", L1(argv[0]));
        return Object::Undef;
    }

    return theVM->values3(Object::makeString(ret),
                          processExitValue(status),
                          processTerminationSignal(status));
#endif
}

Object scheme::internalGetClosureNameEx(VM* theVM, int argc, const Object* argv)
{
#ifdef ENABLE_PROFILER
    DeclareProcedureName("%get-closure-name");
    checkArgumentLength(1);
    return theVM->getClosureName(argv[0]);
#else
    return UC("<unknown>");
#endif
}


// for psyntax
Object scheme::setSymbolValueDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-symbol-value");
    checkArgumentLength(2);

    argumentCheckSymbol(0, id);
    const Object val = argv[1];
    theVM->setValueSymbol(id, val);
    return Object::Undef;
}

// for psyntax
Object scheme::symbolValueEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("symbol-value");
    checkArgumentLength(1);
    argumentCheckSymbol(0, id);
    return theVM->getGlobalValue(id);
}

// for srfi-19
Object scheme::microsecondsEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("microseconds");
    checkArgumentLength(0);
#ifdef MONA
    int64_t msec = MonAPI::Date::nowInMsec();
    uint64_t usec = msec * 1000;
#else
    struct timeval tv;
    gettimeofday(&tv, nullptr);
    const uint64_t usec = static_cast<uint64_t>(tv.tv_sec) * 1000000 + static_cast<uint64_t>(tv.tv_usec);
#endif
    return Bignum::makeIntegerFromU64(usec);

}
// for srfi-19
Object scheme::localTzOffsetEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("local-tz-offset");
    checkArgumentLength(0);
    struct tm localTime;
    struct tm utcTime;
    time_t current = time(nullptr);
#if defined(_WIN32) && !defined(MOSH_MINGW32)
    localtime_s(&localTime, &current);
    time_t l = mktime(&localTime);
    gmtime_s(&utcTime, &l);
#elif defined(MOSH_MINGW32)
    localTime = *localtime(&current);
    time_t l = mktime(&localTime);
    utcTime = *gmtime(&l);
#elif defined(MONA)
    callAssertionViolationAfter(theVM, procedureName, "not implmented", L1(argv[0]));
    return Object::Undef;
#else
    localtime_r(&current, &localTime);
    time_t l = mktime(&localTime);
    gmtime_r(&l,  &utcTime);
#endif
    return Bignum::makeIntegerFromU64(static_cast<uint64_t>(mktime(&localTime) - mktime(&utcTime)));
}

Object scheme::timeUsageEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("time-usage");
#ifdef _WIN32
    FILETIME real_time;
    FILETIME creation_time;
    FILETIME exit_time;
    FILETIME kernel_time;
    FILETIME user_time;
    GetSystemTimeAsFileTime(&real_time);
    if (GetProcessTimes(GetCurrentProcess(), &creation_time, &exit_time, &kernel_time, &user_time)) {
      return Pair::list3(Object::makeFlonum(((double)real_time.dwLowDateTime
                                             + (double)real_time.dwHighDateTime
                                             * (double)UINT_MAX) / 10000000.0),
                         Object::makeFlonum(((double)user_time.dwLowDateTime
                                             + (double)user_time.dwHighDateTime
                                             * (double)UINT_MAX) / 10000000.0),
                         Object::makeFlonum(((double)kernel_time.dwLowDateTime
                                             + (double)kernel_time.dwHighDateTime
                                             * (double)UINT_MAX) / 10000000.0));
    } else {
      return Object::False;
    }
    return Object::makeString(UC("<not-supported>"));
#elif defined(MONA)
    uint64_t msec = MonAPI::Date::nowInMsec();
    return Pair::list3(Object::makeFlonum((double)msec / 1000.0),
                       Object::makeFlonum(0.0),
                       Object::makeFlonum(0.0));

#else
    checkArgumentLength(0);
    struct timeval tv;
    struct rusage ru;
    gettimeofday(&tv, nullptr);
    getrusage(RUSAGE_SELF, &ru);

    return Pair::list3(Object::makeFlonum((double)tv.tv_sec + tv.tv_usec / 1000000.0),
                       Object::makeFlonum((double)ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1000000.0),
                       Object::makeFlonum((double)ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1000000.0));
#endif
}

Object scheme::currentDynamicWindersEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("current-dynamic-winders");
    checkArgumentLengthBetween(0, 1);
    if (argc == 0) {
        return theVM->dynamicWinders();
    } else {
        theVM->setDynamicWinders(argv[0]);
        return Object::Undef;
    }
}

// rtds are shared between multiple VMs.
static EqHashTable nongenerativeRtds;

Object scheme::nongenerativeRtdSetDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("nongenerative-rtd-set!");
    checkArgumentLength(2);
    argumentCheckSimpleStruct(1, rtd);
    nongenerativeRtds.set(argv[0], rtd);
    return Object::Undef;
}

Object scheme::lookupNongenerativeRtdEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("lookup-nongenerative-rtd");
    checkArgumentLength(1);
    return nongenerativeRtds.ref(argv[0], Object::False);
}

Object scheme::internalConfstrEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%confstr");
#ifdef MONA
    return callImplementationRestrictionAfter(theVM, procedureName, "not implmented", Pair::list1(argv[0]));
#else
    checkArgumentLength(1);
    argumentAsFixnum(0, name);

#ifndef _WIN32
    Object val;    // return value of this procedure
    int save_errno  = errno;

    size_t size = confstr(name, nullptr, 0);
    ucs4string result = ucs4string(size);
    char *buf = result.ascii_c_str();
    size_t ret = confstr(name, buf, size);

    // Handle two error cases which we need to distinguish thru errno
    if (ret == 0) {
        // error
        val = Object::False;
        if (errno != save_errno) {
            switch(errno){
                case EINVAL:
                    callErrorAfter(theVM, procedureName, "invalid name", 
                            L1(argv[0]));
                    break;
                default:
                    callErrorAfter(theVM, procedureName, "unknown error", 
                            L1(argv[0]));
                    break;
            }
        }
    } else {
        // Can't call makeString on 'result' as 'buf' is now a separate piece
        // of memory
        val = Object::makeString(buf);
    }
    
    return val;
#else
    (void) name;
    return Object::False;
#endif
#endif
}

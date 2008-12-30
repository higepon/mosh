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

#include <sys/time.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "scheme.h"
#include "VM.h"
#include "UtilityProcedures.h"
#include "ProcedureMacro.h"
#include "PortProcedures.h"
#include "StringProcedures.h"
#include "Record.h"
#include "Equivalent.h"
#include "ByteArrayBinaryInputPort.h"
#include "UTF8Codec.h"
#include "SString.h"
#include "Vector.h"
#include "EqHashTable.h"
#include "Symbol.h"
#include "ByteVector.h"
#include "TextualOutputPort.h"
#include "Closure.h"
#include "Fixnum.h"
#include "Arithmetic.h"

using namespace scheme;

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

    if (2 == argc || 3 == argc) {
        // we ignore precision parameter
        argumentAsFixnum(1, radix);
        if (radix == 2 || radix == 8 || radix == 10 || radix == 16) {
            return Arithmetic::numberToString(theVM, z, radix);
        } else {
            callAssertionViolationAfter(theVM, procedureName, "radix should be 2, 8, 10 ro 16", L1(argv[1]));
            return Object::Undef;
        }
    } else {
        return Arithmetic::numberToString(theVM, z, 10);
    }
}

Object scheme::charEqPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("char=?");
    checkArgumentLengthAtLeast(1);
    argumentAsChar(0, startCharacter);

    for (int i = 0; i < argc; i++) {
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
    const char* value = getenv(text->data().ascii_c_str());
    return NULL == value ? Object::False : Object::makeString(value);
}

extern char** environ;
Object scheme::getEnvironmentVariablesEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-environment-variables");
    checkArgumentLength(0);
    Object ret = Object::Nil;
    char ** env = environ;
    while(*env) {
        char* equalPostion = strchr(*env, '=');
        ucs4string key = ucs4string::from_c_str(*env, equalPostion - *env);
        ucs4string value = ucs4string::from_c_str(equalPostion + 1, strlen(equalPostion + 1));
        ret = Object::cons(Object::cons(Object::makeString(key),
                                        Object::makeString(value)),
                           ret);
        env++;
    }
    return ret;
}

Object scheme::equalPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("equal?");
    checkArgumentLength(2);
    return Object::makeBool(equal(theVM, argv[0], argv[1], new EqHashTable()));
//    return argv[0].equal(argv[1]);
}

Object scheme::fastEqualPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("fast-equal?");
    checkArgumentLength(2);
    return Object::makeBool(fastEqual(theVM, argv[0], argv[1]));
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

Object scheme::gensymEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("gen-sym");
    checkArgumentLengthBetween(0, 1);

    static int next = 0;
    char ubuf[32];
#ifdef USE_BOEHM_GC
    ucs4char* ibuf = new(PointerFreeGC) ucs4char[32];
#else
    ucs4char* ibuf = new ucs4char[32];
#endif

    sprintf(ubuf, "G%x", next++);
    const int len = strlen(ubuf) + 1;
    for (int i = 0; i < len; i++) {
        ibuf[i] = ubuf[i];
    }
    if (1 == argc) {
        if (argv[1].isSymbol()) {
            return Object::makeSymbol(format(UC("~a@~a"), Pair::list2(ibuf, argv[1])).toString()->data().c_str());
        } else {
            return Object::makeSymbol(ibuf);
        }
    } else {
        return Object::makeSymbol(ibuf);
    }
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
    return Object::makeBool(eqv(theVM, argv[0], argv[1]));
}

Object scheme::booleanPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bool?");
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
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return Object::cons(Object::makeBignum(tv.tv_sec), Object::makeBignum(tv.tv_usec));
}

Object scheme::vmApplyEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("vm-apply");
    checkArgumentLength(2);
    return theVM->apply(argv[0], argv[1]);
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

Object scheme::applyEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("apply");
    checkArgumentLengthAtLeast(2);

    argumentCheckProcedure(0, proc);

    Object argsAsList = Object::Nil;
    for (int i = 1; i < argc; i++) {
        if (i == argc - 1) {
            argumentCheckList(i, lastPair);
            argsAsList = Pair::appendD(argsAsList, lastPair);
        } else {
            argsAsList = Pair::appendD(argsAsList, Pair::list1(argv[i]));
        }
    }
    theVM->applyClosure(proc, argsAsList);
    return Object::Undef;
}

Object scheme::valuesEx(VM* theVM, int argc, const Object* argv)
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
    theVM->loadFile(path->data());
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

Object scheme::callProcessEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("call-process");
    checkArgumentLength(1);

    argumentAsString(0, cmd);

    const int BUFFER_SIZE = 1024;
    FILE* in = popen(cmd->data().ascii_c_str(), "r");
    char buffer[BUFFER_SIZE];
    if (NULL == in) {
        callAssertionViolationAfter(theVM, procedureName, "failed", L1(argv[0]));
        return Object::Undef;
    }

    memset(buffer, '\0', BUFFER_SIZE);

    ucs4string ret;
    int size;
    while ((size = fread(buffer, sizeof(char), BUFFER_SIZE, in)) > 0) {
        ret += ucs4string::from_c_str(buffer, size);
    }
    if (pclose(in) != 0) {
        callAssertionViolationAfter(theVM, procedureName, "failed. pclose returned error", L1(argv[0]));
        return Object::Undef;
    }
    return Object::makeString(ret);
}

Object scheme::internalgetClosureNameEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("%get-closure-name");
    checkArgumentLength(1);
    return theVM->getClosureName(argv[0]);
}


// for psyntax
Object scheme::setSymbolValueDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-symbol-value");
    checkArgumentLength(2);

    argumentCheckSymbol(0, id);
    const Object val = argv[1];
    theVM->setTopLevelGlobalValue(id, val);
    return Object::Undef;
}

// for psyntax
Object scheme::symbolValueEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("symbol-value");
    checkArgumentLength(1);
    argumentCheckSymbol(0, id);
    fflush(stderr);
    return theVM->getTopLevelGlobalValue(id);
}

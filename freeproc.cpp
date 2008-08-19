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
#include "ProcedureMacro.h"
#include "PortProcedures.h"
#include "Equivalent.h"
using namespace scheme;

Object scheme::numberPEx(int argc, const Object* argv)
{
    DeclareProcedureName("number?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isInt());
}


Object scheme::numberTostringEx(int argc, const Object* argv)
{
    DeclareProcedureName("number->string");
    checkArgumentLengthBetween(1, 2);
    argumentAsInt(0, number);
    if (2 == argc) {
        argumentAsInt(1, radix);

        if (16 == radix) {
            static char buf[32];
            snprintf(buf, 32, "%x", number);
            return Object::makeString(buf);
        } else {
            callAssertionViolationAfter(procedureName, "unsupported radix", L1(argv[1]));
            return Object::Undef;
        }
    }
    else {
        static char buf[32];
        snprintf(buf, 32, "%d", number);
        return Object::makeString(buf);
    }
}

Object scheme::charEqPEx(int argc, const Object* argv)
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

Object scheme::stringPEx(int argc, const Object* argv)
{
    DeclareProcedureName("string?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isString());
}

Object scheme::getEnvironmentVariableEx(int argc, const Object* argv)
{
    DeclareProcedureName("get-environment-variable");
    checkArgumentLength(1);
    argumentAsString(0, text);
    const char* value = getenv(text->data().ascii_c_str());
    return NULL == value ? Object::False : Object::makeString(value);
}

extern char** environ;
Object scheme::getEnvironmentVariablesEx(int argc, const Object* argv)
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

Object scheme::equalPEx(int argc, const Object* argv)
{
    DeclareProcedureName("equal?");
    checkArgumentLength(2);
    return Object::makeBool(equal(argv[0], argv[1], new EqHashTable()));
//    return argv[0].equal(argv[1]);
}

// todo from gauche
Object scheme::digitTointegerEx(int argc, const Object* argv)
{
    DeclareProcedureName("digit->integer");
    argumentAsChar(0, ch);
    argumentAsInt(1, radix);
    if (ch < '0') {
        return Object::False;
    }
    if (radix <= 10) {
        if (ch < '0' + radix) {
            return Object::makeInt(ch - '0');
        }
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
    DeclareProcedureName("get-remaning-input-string");
    callNotImplementedAssertionViolationAfter(procedureName);
    return Object::UnBound;
}

Object scheme::charTointegerEx(int argc, const Object* argv)
{
    DeclareProcedureName("char->integer");
    checkArgumentLength(1);
    argumentAsChar(0, ch);
    return Object::makeInt(ch);
}

Object scheme::integerTocharEx(int argc, const Object* argv)
{
    DeclareProcedureName("integer->char");
    checkArgumentLength(1);
    argumentAsInt(0, integer);
    return Object::makeChar(integer);
}

Object scheme::charPEx(int argc, const Object* argv)
{
    DeclareProcedureName("char?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isChar());
}

Object scheme::gensymEx(int argc, const Object* argv)
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

    sprintf(ubuf, "G%d", next++);
    const int len = strlen(ubuf) + 1;
    for (int i = 0; i < len; i++) {
        ibuf[i] = ubuf[i];
    }
    return Symbol::intern(ibuf);
}

Object scheme::vectorPEx(int argc, const Object* argv)
{
    DeclareProcedureName("vector?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isVector());
}


Object scheme::eqPEx(int argc, const Object* argv)
{
    DeclareProcedureName("eq?");
    checkArgumentLength(2);
    return Object::makeBool(argv[0] == argv[1]);
}

Object scheme::eqvPEx(int argc, const Object* argv)
{
    DeclareProcedureName("eqv?");
    checkArgumentLength(2);
    return Object::makeBool(argv[0].eqv(argv[1]));
}

Object scheme::booleanPEx(int argc, const Object* argv)
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

Object scheme::symbolTostringEx(int argc, const Object* argv)
{
    DeclareProcedureName("symbol->string");
    checkArgumentLength(1);
    argumentAsSymbol(0, symbol);
    return Object::makeString(symbol->c_str());
}

// // todo
// Object scheme::errorEx(int argc, const Object* argv)
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


Object scheme::getTimeofdayEx(int argc, const Object* argv)
{
    DeclareProcedureName("get-timeofday");
    checkArgumentLength(1);
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return Object::cons(Object::makeInt(tv.tv_sec), Object::makeInt(tv.tv_usec));
}

Object scheme::vmApplyEx(int argc, const Object* argv)
{
    DeclareProcedureName("vm-apply");
    checkArgumentLength(2);
    return theVM->apply(argv[0], argv[1]);
}

Object scheme::pairPEx(int argc, const Object* argv)
{
    DeclareProcedureName("pair?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isPair());
}

Object scheme::initLibraryTableEx(int argc, const Object* argv)
{
    DeclareProcedureName("init-library-table");
    checkArgumentLength(0);
    theVM->initLibraryTable();
    return Object::Undef;
}

Object scheme::bytevectorU8RefEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u8-ref");
    checkArgumentLength(2);

    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    return bytevector->u8Ref(index);
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

Object scheme::utf8TostringEx(int argc, const Object* argv)
{
    DeclareProcedureName("utf8->string");
    checkArgumentLength(1);

    argumentAsByteVector(0, bytevector);

    BinaryInputPort* in = new ByteArrayBinaryInputPort(reinterpret_cast<uint8_t*>(const_cast<int8_t*>(bytevector->data())), bytevector->length());
    ucs4string ret;
    UTF8Codec codec;
    for (ucs4char c = codec.in(in); c != EOF; c = codec.in(in)) {
        ret += c;
    }
    return Object::makeString(ret);
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


Object scheme::evalEx(int argc, const Object* argv)
{
    DeclareProcedureName("eval");
    checkArgumentLength(2);
    return theVM->eval(argv[0], argv[1]);
}

Object scheme::applyEx(int argc, const Object* argv)
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
    DeclareProcedureName("mod");
    checkArgumentLength(2);

    argumentAsInt(0, number1);
    argumentAsInt(1, number2);

    if (0 == number2) {
        callAssertionViolationAfter(procedureName, "Dividing by zero");
        return Object::Undef;
    }
    return Object::makeInt(mod(number1, number2));
}

Object scheme::divEx(int argc, const Object* argv)
{
    DeclareProcedureName("div");
    checkArgumentLength(2);

    argumentAsInt(0, number1);
    argumentAsInt(1, number2);
    if (0 == number2) {
        callAssertionViolationAfter(procedureName, "Dividing by zero");
        return Object::Undef;
    }
    return Object::makeInt(div(number1, number2));
}


Object scheme::exitEx(int argc, const Object* argv)
{
    DeclareProcedureName("exit");
    checkArgumentLengthBetween(0, 1);
    if (0 == argc) {
        exit(EXIT_SUCCESS);
    }

    const Object exitValue = argv[0];
    if (exitValue.isInt()) {
        exit(exitValue.toInt());
    } else if (exitValue.isFalse()) {
        exit(EXIT_FAILURE);
    } else {
        exit(EXIT_FAILURE);
    }
    return Object::Undef;
}

Object scheme::macroexpand1Ex(int argc, const Object* argv)
{
    DeclareProcedureName("macroexpand1");
    checkArgumentLength(1);
    static Object proc = Symbol::intern(UC("pass1/macroexpand"));
    return theVM->callClosureByName(proc, argv[0]);
}

Object scheme::procedurePEx(int argc, const Object* argv)
{
    DeclareProcedureName("procedure");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isProcedure());
}

Object scheme::loadEx(int argc, const Object* argv)
{
    DeclareProcedureName("load");
    checkArgumentLength(1);

    argumentAsString(0, path);
    theVM->loadFile(path->data());
    return Object::Undef;
}

Object scheme::symbolPEx(int argc, const Object* argv)
{
    DeclareProcedureName("symbol?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isSymbol());
}

Object scheme::charGePEx(int argc, const Object* argv)
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

Object scheme::charGtPEx(int argc, const Object* argv)
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

Object scheme::charLePEx(int argc, const Object* argv)
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

Object scheme::charLtPEx(int argc, const Object* argv)
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

Object scheme::vectorTolistEx(int argc, const Object* argv)
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

Object scheme::callProcessEx(int argc, const Object* argv)
{
    DeclareProcedureName("call-process");
    checkArgumentLength(1);

    argumentAsString(0, cmd);

    const int BUFFER_SIZE = 1024;
    FILE* in = popen(cmd->data().ascii_c_str(), "r");
    char buffer[BUFFER_SIZE];
    if (NULL == in) {
        callAssertionViolationAfter(procedureName, "failed", L1(argv[0]));
        return Object::Undef;
    }

    memset(buffer, '\0', BUFFER_SIZE);

    ucs4string ret;
    int size;
    while ((size = fread(buffer, sizeof(char), BUFFER_SIZE, in)) > 0) {
        ret += ucs4string::from_c_str(buffer, size);
    }
    if (pclose(in) != 0) {
        callAssertionViolationAfter(procedureName, "failed. pclose returned error", L1(argv[0]));
        return Object::Undef;
    }
    return Object::makeString(ret);
}

Object scheme::internalgetClosureNameEx(int argc, const Object* argv)
{
    DeclareProcedureName("%get-closure-name");
    checkArgumentLength(1);
    return theVM->getClosureName(argv[0]);
}


// for psyntax
Object scheme::setSymbolValueDEx(int argc, const Object* argv)
{
    DeclareProcedureName("set-symbol-value");
    checkArgumentLength(2);

    argumentCheckSymbol(0, id);
    const Object val = argv[1];
    theVM->setTopLevelGlobalValue(id, val);
    return Object::Undef;
}

// for psyntax
Object scheme::symbolValueEx(int argc, const Object* argv)
{
    DeclareProcedureName("symbol-value");
    checkArgumentLength(1);
    argumentCheckSymbol(0, id);
    return theVM->getTopLevelGlobalValue(id);
}

/*
 * PortProcedures.cpp - <port> procedures.
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

#include <dirent.h>
#include <unistd.h> // getcwd
#include <sys/stat.h> // stat
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ByteVector.h"
#include "PortProcedures.h"
#include "ProcedureMacro.h"
#include "TextualOutputPort.h"
#include "TextualInputPort.h"
#include "StringTextualOutputPort.h"
#include "TextualByteVectorOutputPort.h"
#include "Transcoder.h"
#include "UTF8Codec.h"
#include "FileBinaryOutputPort.h"
#include "FileBinaryInputPort.h"
#include "Symbol.h"
#include "EqHashTable.h"
#include "Bignum.h"
#include "Fasl.h"
#include "Arithmetic.h"
#include "ByteVector.h"


using namespace scheme;

Object scheme::putBytevectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("put-bytevector");
    checkArgumentLength(2); // todo more arguments
    argumentAsBinaryOutputPort(0, outputPort);
    argumentAsByteVector(1, bv);
    outputPort->putByteVector(bv);
    return Object::Undef;
}

Object scheme::outputPortPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("output-port?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isOutputPort());
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

Object scheme::statMtimeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("stat-mtime");
    checkArgumentLength(1);
    argumentAsString(0, path);
    struct stat sb;
    if (-1 == stat(path->data().ascii_c_str(), &sb)) {
        callAssertionViolationAfter(theVM, procedureName, "failed", L1(argv[0]));
        return Object::Undef;
    } else {
        return Object::makeFixnum(sb.st_mtime);
    }
}

Object scheme::getStringNEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-string-n");
    checkArgumentLength(2);
    argumentAsTextualInputPort(0, inputPort);
    argumentAsFixnum(1, size);
    return inputPort->getStringN(size);
}

Object scheme::faslWriteEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("fasl-write");
    checkArgumentLength(2);
    argumentAsBinaryOutputPort(1, outputPort);
    FaslWriter writer(outputPort);
    TRY_IO {
        writer.put(argv[0]);
    } CATCH_IO {
        callAssertionViolationAfter(theVM, procedureName, IO_ERROR_MESSAGE, L1(argv[0]));
        return Object::Undef;
    }
    return Object::Undef;
}

Object scheme::faslReadEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("fasl-read");
    checkArgumentLength(1);
    argumentAsBinaryInputPort(0, inputPort);
    FaslReader reader(theVM, inputPort);
    return reader.get();
}

Object scheme::getLineEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-line");
    checkArgumentLength(1);
    argumentAsTextualInputPort(0, inputPort);
    TRY_IO {
        return inputPort->getLine();
    } CATCH_IO {
        callAssertionViolationAfter(theVM, procedureName, IO_ERROR_MESSAGE, L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::closePortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("close-port");
    checkArgumentLength(1);
    const Object maybePort = argv[0];
    if (maybePort.isTextualInputPort()) {
        maybePort.toTextualInputPort()->close();
    } else if (maybePort.isBinaryInputPort()) {
        maybePort.toBinaryInputPort()->close();
    } else if (maybePort.isBinaryOutputPort()) {
        maybePort.toBinaryOutputPort()->close();
    } else if (maybePort.isTextualOutputPort()) {
        maybePort.toTextualOutputPort()->close();
    } else {
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "port", maybePort, L1(maybePort));
    }
    return Object::Undef;
}

Object scheme::standardLibraryPathEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("standard-library-path");
    checkArgumentLength(0);
    return Object::makeString(UC(MOSH_LIB_PATH));
}

Object scheme::lookaheadCharEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("lookahead-char");
    checkArgumentLengthBetween(1, 2);
    argumentAsTextualInputPort(0, textualInputPort);

    ucs4char ch;
    if (2 == argc) {
        // mosh only
        argumentAsFixnum(1, offset);
        ch = textualInputPort->lookaheadChar(offset);
    } else {
        ch = textualInputPort->lookaheadChar();
    }
    return ch == EOF ? Object::Eof : Object::makeChar(ch);
}

Object scheme::currentErrorPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("current-error-port");
    checkArgumentLength(0);
    return theVM->currentErrorPort();
}

Object scheme::sysDisplayEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("display");
    checkArgumentLengthBetween(1, 2);
    const Object obj = argv[0];
    if (1 == argc) {
        theVM->currentOutputPort().toTextualOutputPort()->display(obj);
        theVM->currentOutputPort().toTextualOutputPort()->flush();
    } else {
        argumentAsTextualOutputPort(1, textualOutputPort);
        textualOutputPort->display(obj);
        textualOutputPort->flush();
    }
    return Object::Undef;
}

Object scheme::eofObjectPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eof-object?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isEof());
}

Object scheme::readCharEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("read-char");
    checkArgumentLengthBetween(0, 1);

    if (0 == argc) {
        const ucs4char ch = theVM->currentInputPort().toTextualInputPort()->getChar();
        return ch == EOF ? Object::Eof : Object::makeChar(ch);
    } else {
        argumentAsTextualInputPort(0, textualInputPort);
        const ucs4char ch = textualInputPort->getChar();
        return ch == EOF ? Object::Eof : Object::makeChar(ch);
    }
}

Object scheme::readEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("read");
    checkArgumentLengthBetween(0, 1);

    bool errorOccured = false;
    TextualInputPort* inputPort = NULL;
    if (0 == argc) {
        inputPort = theVM->currentInputPort().toTextualInputPort();
    } else {
        argumentAsTextualInputPort(0, textualInputPort);
        inputPort = textualInputPort;
    }
    const Object object = inputPort->getDatum(errorOccured);
    if (errorOccured) {
        callLexicalViolationAfter(theVM, procedureName, inputPort->error());
        return Object::Undef;
    }
    return object;
}

Object scheme::openStringInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string-input-port");
    argumentAsString(0, text);
    return Object::makeStringInputPort(text->data());
}

Object scheme::openOutputStringEx(VM* theVM, int argc, const Object* argv)
{
    return Object::makeStringOutputPort();
}

Object scheme::sysPortSeekEx(VM* theVM, int argc, const Object* argv)
{
    // todo
    return Object::UnBound;
}

Object scheme::openOutputFileEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-output-file");
    checkArgumentLength(1);

    argumentAsString(0, file);

    Transcoder* transcoder = Transcoder::nativeTranscoder();
    FileBinaryOutputPort* const fileBinaryOutputPort = new FileBinaryOutputPort(file->data());

    if (MOSH_SUCCESS == fileBinaryOutputPort->open()) {
        return Object::makeTextualOutputPort(fileBinaryOutputPort, transcoder);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "can't open file", L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::closeOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("close-output-port");
    checkArgumentLength(1);

    const Object port = argv[0];
    if (port.isTextualOutputPort()) {
        port.toTextualOutputPort()->close();
        return Object::Undef;
    } else if (port.isBinaryOutputPort()) {
        port.toBinaryOutputPort()->close();
        return Object::Undef;
    } else  {
        callAssertionViolationAfter(theVM, procedureName, "port required", L1(port));
        return Object::Undef;
    }
}

Object scheme::closeInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("close-input-port");
    checkArgumentLength(1);

    const Object port = argv[0];
    if (port.isTextualInputPort()) {
        port.toTextualInputPort()->close();
        return Object::Undef;
    } else if (port.isBinaryInputPort()) {
        port.toBinaryInputPort()->close();
        return Object::Undef;
    } else  {
        callAssertionViolationAfter(theVM, procedureName, "port required", L1(port));
        return Object::Undef;
    }
}

Object scheme::getOutputStringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-output-string");
    checkArgumentLength(1);
    argumentAsTextualOutputPort(0, textualOutputPort);
    StringTextualOutputPort* p = reinterpret_cast<StringTextualOutputPort*>(textualOutputPort);
    return Object::makeString(p->getString());
}

Object scheme::deleteFileEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("delete-file");
    checkArgumentLength(1);
    argumentAsString(0, text);
    if (-1 == unlink(text->data().ascii_c_str())) {
        callIoFileNameErrorAfter(theVM, procedureName,
                                 "can't delete file",
                                 L1(argv[0]));
        return Object::Undef;
    } else {
        return Object::Undef;
    }
}

Object scheme::fileExistsPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("file-exists?");
    checkArgumentLength(1);
    argumentAsString(0, path);
    return Object::makeBool(access(path->data().ascii_c_str(), F_OK) == 0);
}

// todo cleanup
Object scheme::formatEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("format");
    const Object arg1 = argv[0];
    if (arg1.isTextualOutputPort()) {
        checkArgumentLengthAtLeast(2);
        argumentAsTextualOutputPort(0, textualOutputPort);
        argumentAsString(1, formatString);

        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 2; i--) {
            lst = Object::cons(argv[i], lst);
        }
        textualOutputPort->format(formatString->data(), lst);
        if (textualOutputPort->isErrorOccured()) {
            callAssertionViolationAfter(theVM, procedureName,
                                        textualOutputPort->errorMessage(),
                                        textualOutputPort->irritants());
            return Object::Undef;
        } else {
            return Object::Undef;
        }
    } else if (arg1.isTrue()) {
        checkArgumentLengthAtLeast(2);
        argumentAsString(1, formatString);

        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 2; i--) {
            lst = Object::cons(argv[i], lst);
        }
        TextualOutputPort* const outputPort = theVM->currentOutputPort().toTextualOutputPort();
        outputPort->format(formatString->data(), lst);
        if (outputPort->isErrorOccured()) {
            callAssertionViolationAfter(theVM, procedureName,
                                        outputPort->errorMessage(),
                                        outputPort->irritants());
            return Object::Undef;
        } else {
            return Object::Undef;
        }
    } else if (arg1.isFalse()) {
        checkArgumentLengthAtLeast(2);
        argumentAsString(1, formatString);

        const Object port = Object::makeStringOutputPort();
        StringTextualOutputPort* const p = static_cast<StringTextualOutputPort*>(port.toTextualOutputPort());
        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 2; i--) {
            lst = Object::cons(argv[i], lst);
        }

        p->format(formatString->data(), lst);
        if (p->isErrorOccured()) {
            callAssertionViolationAfter(theVM, procedureName,
                                        p->errorMessage(),
                                        p->irritants());
            return Object::Undef;
        } else {
            return Object::makeString(p->getString());
        }
    } else if (arg1.isString()) {
        const Object port = Object::makeStringOutputPort();
        StringTextualOutputPort* const p = static_cast<StringTextualOutputPort*>(port.toTextualOutputPort());
        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 1; i--) {
            lst = Object::cons(argv[i], lst);
        }
        p->format(arg1.toString()->data(), lst);
        if (p->isErrorOccured()) {
            callAssertionViolationAfter(theVM, procedureName,
                                        p->errorMessage(),
                                        p->irritants());
            return Object::Undef;
        } else {
            return Object::makeString(p->getString());
        }
    } else {
        callAssertionViolationAfter(theVM, procedureName, "port and format string required");
        return Object::Undef;
    }
}

Object scheme::writeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("write");
    checkArgumentLengthBetween(1, 2);
    const Object obj = argv[0];
    if (1 == argc) {
        theVM->currentOutputPort().toTextualOutputPort()->putDatum(obj);
    } else {
        argumentAsTextualOutputPort(1, textualOutputPort);
        textualOutputPort->putDatum(obj);
    }
    return Object::Undef;
}

// todo incomplete
// (make-custom-binary-input-port id read! get-position set-position! close)
Object scheme::makeCustomBinaryInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-binary-input-port");
    checkArgumentLength(5);

    argumentCheckString(0, id);
    argumentCheckProcedure(1, readProc);
    argumentCheckProcedureOrFalse(2, getPositionProc);
    argumentCheckProcedureOrFalse(3, setPositionProc);
    argumentCheckProcedureOrFalse(3, closeProc);

    return Object::makeCustomBinaryInputPort(theVM, readProc);
}

Object scheme::getU8Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-u8");
    checkArgumentLength(1);
    argumentAsBinaryInputPort(0, binaryInputPort);
    const int b = binaryInputPort->getU8();
    if (EOF == b) {
        return Object::Eof;
    } else {
        return Object::makeFixnum(b);
    }
}

Object scheme::lookaheadU8Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("lookahead-u8");
    checkArgumentLength(1);
    argumentAsBinaryInputPort(0, binaryInputPort);
    const int b = binaryInputPort->lookaheadU8();
    if (EOF == b) {
        return Object::Eof;
    } else {
        return Object::makeFixnum(b);
    }
}

Object scheme::getBytevectorNEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-bytevector-n");
    checkArgumentLength(2);

    argumentAsBinaryInputPort(0, binaryInputPort);
    argumentCheckExactInteger(1, count);
    if (!Arithmetic::fitsU32(count)) {
        callAssertionViolationAfter(theVM, procedureName, "value out of range", L1(argv[1]));
        return Object::Undef;
    }

    const uint32_t u32Count = Arithmetic::toU32(count);
    ByteVector* ret = binaryInputPort->getByteVector(u32Count);
    if (ret->length() == 0) {
        return Object::Eof;
    } else {
        return Object::makeByteVector(ret);
    }
}

//Object scheme::getBytevectorNDEx(VM* theVM, int argc, const Object* argv)
//Object scheme::getBytevectorSomeEx(VM* theVM, int argc, const Object* argv)
//Object scheme::getBytevectorAllEx(VM* theVM, int argc, const Object* argv)

Object scheme::transcodedPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("transcoded-port");
    checkArgumentLength(2);

    argumentAsBinaryInputPort(0, binaryInputPort);
    argumentAsTranscoder(1, transcoder);
    return Object::makeTextualInputPort(binaryInputPort, transcoder);
}

Object scheme::latin1CodecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("latin-1-codec");
    checkArgumentLength(0);
    return Object::makeLatin1Codec();
}

Object scheme::utf8CodecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("utf-8-codec");
    checkArgumentLength(0);
    return Object::makeUTF8Codec();
}

Object scheme::utf16CodecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("utf-16-codec");
    checkArgumentLength(0);
    return Object::makeUTF16Codec();
}

Object scheme::nativeEolStyleEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("native-eol-style");
    checkArgumentLength(0);

#if LINE_FEED_CODE_LF
    return Symbol::LF;
#elif LINE_FEED_CODE_CRLF
    return Symbol::CRLF;
#elif LINE_FEED_CODE_CR
    return Symbol::CR;
#else
    MOSH_FATAL("not found platform native eol style\n");
#endif
}

Object scheme::makeTranscoderEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-transcoder");
    checkArgumentLengthBetween(1, 3);

    argumentAsCodec(0, codec);
    if (argc == 1) {
        return Object::makeTranscoder(codec);
    }
    argumentCheckSymbol(1, eolStyle);
    if (argc == 2) {
        return Object::makeTranscoder(codec, eolStyle);
    }
    argumentCheckSymbol(2, errorHandlingMode);
    return Object::makeTranscoder(codec, eolStyle, errorHandlingMode);
}

Object scheme::nativeTranscoderEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("native-transcoder");
    checkArgumentLength(0);
    return Object::makeTranscoder(Transcoder::nativeTranscoder());
}

Object scheme::eofObjectEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("eof-object");
    checkArgumentLength(0);
    return Object::Eof;
}

Object scheme::sysOpenBytevectorOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-bytevector-output-port");
    checkArgumentLengthBetween(0, 1);
    if (0 == argc || argv[0].isFalse()) {
        callNotImplementedAssertionViolationAfter(theVM, procedureName);
        return Object::Undef;
    } else {
        argumentAsTranscoder(0, transcoder);
        return Object::makeTextualByteVectorOuputPort(transcoder);
    }
}

Object scheme::sysGetBytevectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-bytevector");
    checkArgumentLength(1);
    argumentAsTextualOutputPort(0, textualOutputPort);
    TextualByteVectorOutputPort* p = reinterpret_cast<TextualByteVectorOutputPort*>(textualOutputPort);
    return Object::makeByteVector(p->getByteVector());
}

Object scheme::openFileOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-file-output-port");
    checkArgumentLength(1);

    argumentAsString(0, file);

    FileBinaryOutputPort* fileBinaryOutputPort = new FileBinaryOutputPort(file->data());

    if (MOSH_SUCCESS == fileBinaryOutputPort->open()) {
        return Object::makeBinaryOutputPort(fileBinaryOutputPort);
    } else {
        callAssertionViolationAfter(theVM, procedureName, "can't open file", L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::openInputFileEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-input-file");
    checkArgumentLength(1);

    argumentAsString(0, path);

    Transcoder* transcoder = Transcoder::nativeTranscoder();
    FileBinaryInputPort* const fileBinaryInputPort = new FileBinaryInputPort(path->data());
    if (MOSH_SUCCESS == fileBinaryInputPort->open()) {
        return Object::makeTextualInputPort(fileBinaryInputPort, transcoder);
    } else {
        callErrorAfter(theVM, procedureName, "can't open file", L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::openFileInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-file-input-port");
    checkArgumentLengthBetween(1, 4);
    FileBinaryInputPort* fileBinaryInputPort = NULL;
    Transcoder* transcoder = NULL;

    if (argc == 1) {
        argumentAsString(0, path);
        fileBinaryInputPort = new FileBinaryInputPort(path->data(), Object::Nil, Symbol::BLOCK);
    } else if (argc == 2) {
        argumentAsString(0, path);
        argumentCheckList(1, fileOptions);
        fileBinaryInputPort = new FileBinaryInputPort(path->data(), fileOptions, Symbol::BLOCK);
    } else if (argc == 3) {
        argumentAsString(0, path);
        argumentCheckList(1, fileOptions);
        argumentCheckSymbol(2, bufferMode);
        if ((bufferMode != Symbol::NONE) &&
            (bufferMode != Symbol::LINE) &&
            (bufferMode != Symbol::BLOCK)) {
            callErrorAfter(theVM, procedureName, "ignore buffer-mode option", L1(argv[2]));
            return Object::Undef;
        }
        fileBinaryInputPort = new FileBinaryInputPort(path->data(), fileOptions, bufferMode);
    } else if (argc == 4) {
        argumentAsString(0, path);
        argumentCheckList(1, fileOptions);
        argumentCheckSymbol(2, bufferMode);
        if ((bufferMode != Symbol::NONE) &&
            (bufferMode != Symbol::LINE) &&
            (bufferMode != Symbol::BLOCK)) {
            callErrorAfter(theVM, procedureName, "ignore buffer-mode option", L1(argv[2]));
            return Object::Undef;
        }
        fileBinaryInputPort = new FileBinaryInputPort(path->data(), fileOptions, bufferMode);
        argumentCheckTranscoderOrFalse(3, maybeTranscoder);
        if (maybeTranscoder != Object::False) {
            transcoder = maybeTranscoder.toTranscoder();
        }
    }

    if ((fileBinaryInputPort != NULL) && (MOSH_SUCCESS == fileBinaryInputPort->open())) {
        if (transcoder == NULL) {
            return Object::makeBinaryInputPort(fileBinaryInputPort);
        } else {
            return Object::makeTextualInputPort(fileBinaryInputPort, transcoder);
        }
    } else {
        callErrorAfter(theVM, procedureName, "can't open file", L1(argv[0]));
        return Object::Undef;
    }

//     Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
//     FileBinaryInputPort* const fileBinaryInputPort = new FileBinaryInputPort(path->data());
//     if (MOSH_SUCCESS == fileBinaryInputPort->open()) {
//         return Object::makeTextualInputPort(fileBinaryInputPort, transcoder);
//     } else {
//         callAssertionViolationAfter(theVM, procedureName, "can't open file", L1(argv[0]));
//         return Object::Undef;
//     }
}

Object scheme::currentInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("current-input-port");
    checkArgumentLength(0);
    return theVM->currentInputPort();
}

Object scheme::currentOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("current-output-port");
    checkArgumentLength(0);
    return theVM->currentOutputPort();
}

Object scheme::setCurrentInputPortDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-current-input-port!");
    checkArgumentLength(1);

    argumentCheckTextualInputPort(0, textualInputPort);
    theVM->setCurrentInputPort(textualInputPort);
    return Object::UnBound;
}

Object scheme::setCurrentOutputPortDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-current-output-port!");
    checkArgumentLength(1);

    argumentCheckTextualOutputPort(0, textualOutputPort);
    theVM->setCurrentOutputPort(textualOutputPort);
    return Object::Undef;
}

Object scheme::standardInputPortEx(VM* theVM, int argc, const Object* argv)
{
    static const Object port = Object::makeBinaryInputPort(fileno(stdin));
    DeclareProcedureName("starndard-input-port");
    checkArgumentLength(0);
    return port;
}

Object scheme::standardOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    static const Object port = Object::makeBinaryOutputPort(fileno(stdout));
    DeclareProcedureName("starndard-output-port");
    checkArgumentLength(0);
    return port;
}

Object scheme::standardErrorPortEx(VM* theVM, int argc, const Object* argv)
{
    static const Object port = Object::makeBinaryOutputPort(fileno(stderr));
    DeclareProcedureName("starndard-error-port");
    checkArgumentLength(0);
    return port;
}


Object scheme::readdirEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("readdir");
    checkArgumentLength(1);
    argumentAsString(0, path);

    DIR* dir;
    if (NULL == (dir = opendir(path->data().ascii_c_str()))) {
        callAssertionViolationAfter(theVM, procedureName, "could't open dir", L1(argv[0]));
        return Object::Undef;
    }
    Object ret = Object::Nil;
    for (struct dirent* entry = readdir(dir); entry != NULL; entry = readdir(dir))
    {
        ret = Object::cons(Object::makeString(entry->d_name), ret);
    }
    return ret;
}

Object scheme::bufferModePEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("buffer-mode?");
    checkArgumentLength(1);
    argumentCheckSymbol(0, bufferMode);

    if (bufferMode == Symbol::NONE ||
        bufferMode == Symbol::LINE ||
        bufferMode == Symbol::BLOCK) {
        return Object::True;
    }

    return Object::False;
}

Object scheme::inputPortPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("input-port?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isInputPort());
}

Object scheme::binaryPortPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("binary-port?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isBinaryPort());
}

Object scheme::textualPortPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("textual-port?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isTextualPort());
}

Object scheme::portPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("port?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isPort());
}

Object scheme::portTranscoderEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("port-transcoder");
    checkArgumentLength(1);
    argumentCheckPort(0, port);

    if (port.isTextualOutputPort()) {
        return Object::makeTranscoder(port.toTextualOutputPort()->transcoder());
    } else if (port.isTextualInputPort()) {
        return Object::makeTranscoder(port.toTextualInputPort()->transcoder());
    } else {
        return Object::False;
    }
}

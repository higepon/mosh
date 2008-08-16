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
 *  $Id: PortProcedures.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "PortProcedures.h"
#include "ProcedureMacro.h"

using namespace scheme;

extern scheme::VM* theVM;

Object scheme::currentErrorPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("current-error-port");
    checkArgumentLength(0);
    return theVM->getErrorPort();
}

Object scheme::sysDisplayEx(int argc, const Object* argv)
{
    DeclareProcedureName("display");
    checkArgumentLengthBetween(1, 2);
    const Object obj = argv[0];
    if (1 == argc) {
        theVM->getOutputPort().display(obj);
    } else {
        argumentAsTextualOutputPort(1, textualOutputPort);
        textualOutputPort->display(obj);
    }
    fflush(stdout);// temp
    return Object::Undef;
}

Object scheme::eofObjectPEx(int argc, const Object* argv)
{
    DeclareProcedureName("eof-object?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isEof());
}

Object scheme::readCharEx(int argc, const Object* argv)
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

Object scheme::readEx(int argc, const Object* argv)
{
    DeclareProcedureName("read");
    checkArgumentLengthBetween(0, 1);

    if (0 == argc) {
        return theVM->currentInputPort().toTextualInputPort()->getDatum();
    } else {
        argumentAsTextualInputPort(0, textualInputPort);
        return textualInputPort->getDatum();
    }
}

Object scheme::openStringInputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("string-input-port");
    argumentAsString(0, text);
    return Object::makeStringInputPort(text->data());
}

Object scheme::sysOpenOutputStringEx(int argc, const Object* argv)
{
    return Object::makeStringOutputPort();
}

Object scheme::sysPortSeekEx(int argc, const Object* argv)
{
    // todo
    return Object::UnBound;
}

Object scheme::openOutputFileEx(int argc, const Object* argv)
{
    MOSH_ASSERT(false);
    return Object::UnBound;
}

Object scheme::closeOutputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("close-output-port");
    checkArgumentLength(0);

    const Object port = argv[0];
    if (port.isTextualOutputPort()) {
        port.toTextualOutputPort()->close();
        return Object::Undef;
    } else if (port.isBinaryOutputPort()) {
        port.toBinaryOutputPort()->close();
        return Object::Undef;
    } else  {
        callAssertionViolationAfter(procedureName, "port required", L1(port));
        return Object::Undef;
    }
}

Object scheme::closeInputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("close-input-port");
    checkArgumentLength(0);

    const Object port = argv[0];
    if (port.isTextualInputPort()) {
        port.toTextualInputPort()->close();
        return Object::Undef;
    } else if (port.isBinaryInputPort()) {
        port.toBinaryInputPort()->close();
        return Object::Undef;
    } else  {
        callAssertionViolationAfter(procedureName, "port required", L1(port));
        return Object::Undef;
    }
}

Object scheme::sysGetOutputStringEx(int argc, const Object* argv)
{
    DeclareProcedureName("get-output-string");
    checkArgumentLength(1);
    argumentAsTextualOutputPort(0, textualOutputPort);
    StringTextualOutputPort* p = reinterpret_cast<StringTextualOutputPort*>(textualOutputPort);
    return Object::makeString(p->getString());
}

Object scheme::fileExistsPEx(int argc, const Object* argv)
{
    DeclareProcedureName("file-exists?");
    checkArgumentLength(1);

    argumentAsString(0, text);
    FILE* stream = fopen(text->data().ascii_c_str(), "rb");
    if (NULL == stream) {
        return Object::False;
    } else {
        fclose(stream);
        return Object::True;;
    }
}

// todo cleanup
Object scheme::formatEx(int argc, const Object* argv)
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
        return textualOutputPort->format(formatString->data(), lst);
    } else if (arg1.isTrue()) {
        checkArgumentLengthAtLeast(2);
        argumentAsString(1, formatString);

        Object lst = Object::Nil;
        for (int i = argc - 1; i >= 2; i--) {
            lst = Object::cons(argv[i], lst);
        }
        return theVM->getOutputPort().format(formatString->data(), lst);
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
        callAssertionViolationAfter(procedureName, "port and format string required");
        return Object::Undef;
    }
}

Object scheme::writeEx(int argc, const Object* argv)
{
    DeclareProcedureName("write");
    checkArgumentLengthBetween(1, 2);
    const Object obj = argv[0];
    if (1 == argc) {
        theVM->getOutputPort().putDatum(obj);
    } else {
        argumentAsTextualOutputPort(1, textualOutputPort);
        textualOutputPort->putDatum(obj);
    }
    return Object::Undef;
}

// todo incomplete
// (make-custom-binary-input-port id read! get-position set-position! close)
Object scheme::makeCustomBinaryInputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-binary-input-port");
    checkArgumentLength(5);

    argumentAsString(0, id);
    argumentCheckProcedure(1, readProc);
    argumentCheckProcedureOrFalse(2, getPositionProc);
    argumentCheckProcedureOrFalse(3, setPositionProc);
    argumentCheckProcedureOrFalse(3, closeProc);

    return Object::makeCustomBinaryInputPort(readProc);
}

Object scheme::getU8Ex(int argc, const Object* argv)
{
    DeclareProcedureName("get-u8");
    checkArgumentLength(1);
    argumentAsBinaryInputPort(0, binaryInputPort);
    return Object::makeInt(binaryInputPort->getU8());
}

Object scheme::bytevectorU8SetDEx(int argc, const Object* argv)
{
    DeclareProcedureName("bytevector-u8-set!");
    checkArgumentLength(3);

    argumentAsByteVector(0, bytevector);
    argumentAsInt(1, index);
    argumentAsInt(2, value);

    bytevector->u8set(index, static_cast<uint8_t>(value));
    return Object::Undef;
}

Object scheme::transcodedPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("transcoded-port");
    checkArgumentLength(2);

    argumentAsBinaryInputPort(0, binaryInputPort);
    argumentAsTranscoder(1, transcoder);
    return Object::makeTextualInputPort(binaryInputPort, transcoder);
}

Object scheme::utf8CodecEx(int argc, const Object* argv)
{
    DeclareProcedureName("utf8-codec");
    checkArgumentLength(0);
    return Object::makeUTF8Codec();
}

Object scheme::makeTranscoderEx(int argc, const Object* argv)
{
    DeclareProcedureName("make-trans-coder");
    checkArgumentLength(1);
    argumentAsCodec(0, codec);
    return Object::makeTranscoder(codec);
}

Object scheme::eofObjectEx(int argc, const Object* argv)
{
    DeclareProcedureName("eof-object");
    checkArgumentLength(0);
    return Object::Eof;
}

Object scheme::sysOpenBytevectorOutputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("open-bytevector-output-port");
    checkArgumentLengthBetween(0, 1);
    if (0 == argc || argv[0].isFalse()) {
        callNotImplementedAssertionViolationAfter(procedureName);
        return Object::Undef;
    } else {
        argumentAsTranscoder(0, transcoder);
        return Object::makeTextualByteVectorOuputPort(transcoder);
    }
}

Object scheme::sysGetBytevectorEx(int argc, const Object* argv)
{
    DeclareProcedureName("get-bytevector");
    checkArgumentLength(1);
    argumentAsTextualOutputPort(0, textualOutputPort);
    TextualByteVectorOutputPort* p = reinterpret_cast<TextualByteVectorOutputPort*>(textualOutputPort);
    return Object::makeByteVector(p->getByteVector());
}

Object scheme::openFileOutputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("open-file-output-port");
    checkArgumentLength(1);

    argumentAsString(0, file);

    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    Object ret = Object::makeTextualOutputPort(new FileBinaryOutputPort(file->data()), transcoder);
    return ret;
}

Object scheme::openFileInputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("open-file-input-port");
    checkArgumentLength(1);
    const Object file = argv[0];
    if (!file.isString()) {
        VM_RAISE1("open-file-input-port string required, but got ~a\n", file);
    }
    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    return Object::makeTextualInputPort(new FileBinaryInputPort(file.toString()->data()), transcoder);
}

Object scheme::currentInputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("current-input-port");
    checkArgumentLength(0);
    return theVM->currentInputPort();
}

Object scheme::currentOutputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("current-output-port");
    callNotImplementedAssertionViolationAfter(procedureName);
    return Object::UnBound;
}

Object scheme::setCurrentInputPortDEx(int argc, const Object* argv)
{
    DeclareProcedureName("set-current-input-port!");
    checkArgumentLength(1);

    argumentCheckTextualInputPort(0, textualInputPort);
    theVM->setInputPort(textualInputPort);
    return Object::UnBound;
}

Object scheme::setCurrentOutputPortDEx(int argc, const Object* argv)
{
    DeclareProcedureName("set-current-output-port!");
    checkArgumentLength(1);

    argumentAsTextualOutputPort(0, textualOutputPort);
    theVM->setOutputPort(*textualOutputPort);
    return Object::Undef;
}

Object scheme::standardInputPortEx(int argc, const Object* argv)
{
    DeclareProcedureName("starndard-input-port");
    checkArgumentLength(0);
    return theVM->standardInputPort();
}

Object scheme::readdirEx(int argc, const Object* argv)
{
    DeclareProcedureName("readdir");
    checkArgumentLength(1);
    argumentAsString(0, path);

    DIR* dir;
    if (NULL == (dir = opendir(path->data().ascii_c_str()))) {
        callAssertionViolationAfter(procedureName, "could't open dir", L1(argv[0]));
        return Object::Undef;
    }
    Object ret = Object::Nil;
    for (struct dirent* entry = readdir(dir); entry != NULL; entry = readdir(dir))
    {
        ret = Object::cons(Object::makeString(entry->d_name), ret);
    }
    return ret;
}

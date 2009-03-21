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

#ifdef _WIN32
    #include <io.h>
#else
#include <dirent.h>
#include <unistd.h> // getcwd
#endif
#include <sys/stat.h> // stat
#include <sys/types.h>
#include <fcntl.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "Record.h"
#include "ByteVector.h"
#include "PortProcedures.h"
#include "ProcedureMacro.h"
#include "TextualOutputPort.h"
#include "TextualInputPort.h"
#include "StringTextualOutputPort.h"
#include "Transcoder.h"
#include "Latin1Codec.h"
#include "UTF8Codec.h"
#include "UTF16Codec.h"
#include "StandardOutputPort.h"
#include "StandardErrorPort.h"
#include "StandardInputPort.h"
#include "ByteArrayBinaryInputPort.h"
#include "Symbol.h"
#include "EqHashTable.h"
#include "Bignum.h"
#include "Fasl.h"
#include "Arithmetic.h"
#include "ByteVector.h"
#include "CustomBinaryInputPort.h"
#include "CustomBinaryOutputPort.h"
#include "CustomTextualInputPort.h"
#include "CustomTextualOutputPort.h"
#include "CustomTextualInputOutputPort.h"
#include "BufferedFileBinaryInputPort.h"
#include "BufferedFileBinaryOutputPort.h"
#include "BinaryInputOutputPort.h"
#include "BufferedFileBinaryInputOutputPort.h"
#include "BlockBufferedFileBinaryOutputPort.h"
#include "FileBinaryInputOutputPort.h"
#include "BlockBufferedFileBinaryInputOutputPort.h"
#include "LineBufferedFileBinaryInputOutputPort.h"
#include "TranscodedTextualInputOutputPort.h"
#include "TranscodedTextualInputPort.h"
#include "TranscodedTextualOutputPort.h"
#include "BinaryInputOutputPort.h"
#include "ListProcedures.h"
#include "CustomBinaryInputOutputPort.h"
#include "ByteArrayBinaryOutputPort.h"

#ifdef _WIN32
    #define F_OK 0
    #define W_OK 2
    #define R_OK 4
#endif

using namespace scheme;

int scheme::readFromFd(int fd, uint8_t* buf, size_t size)
{
    MOSH_ASSERT(fd != BinaryPort::INVALID_FILENO);
    for (;;) {
        const int result = read(fd, buf, size);
        if (result < 0 && errno == EINTR) {
            // read again
            errno = 0;
        } else {
            if (result < 0) {
                throwIOError2(IOError::READ, strerror(errno));
                return result;
            } else {
                return result;
            }
        }
    }
}

int scheme::writeToFd(int fd, uint8_t* buf, size_t count)
{
    MOSH_ASSERT(fd != BinaryPort::INVALID_FILENO);

    for (;;) {
        const int result = write(fd, buf, count);
        if (result < 0 && errno == EINTR) {
            // write again
            errno = 0;
        } else {
            if (result < 0) {
                throwIOError2(IOError::WRITE, strerror(errno));
                return result;
            } else {
                return result;
            }
        }
    }
}


bool scheme::fileExistsP(const ucs4string& path)
{
    return access(path.ascii_c_str(), F_OK) == 0;
}

bool scheme::fileWritableP(const ucs4string& path)
{
    return access(path.ascii_c_str(), W_OK | R_OK) == 0;
}

bool scheme::fileReadableP(const ucs4string& path)
{
    return access(path.ascii_c_str(), R_OK) == 0;
}


static bool isExistOption(Record* fileOptions, Object option)
{
    MOSH_ASSERT(fileOptions->fieldsLength() == 2);
    Object members = fileOptions->fieldAt(1);
    MOSH_ASSERT(members.isList());
    return !memq(option, members).isFalse();
}
static bool isNoFail(Record* fileOptions)
{
    return isExistOption(fileOptions, Symbol::NO_FAIL);
}

static bool isNoCreate(Record* fileOptions)
{
    return isExistOption(fileOptions, Symbol::NO_CREATE);
}

static bool isNoTruncate(Record* fileOptions)
{
    return isExistOption(fileOptions, Symbol::NO_TRUNCATE);
}

static bool isEmpty(Record* fileOptions)
{
    MOSH_ASSERT(fileOptions->fieldsLength() == 2);
    return fileOptions->fieldAt(1).isNil();
}

Object scheme::makeCustomTextualInputOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-textual-input/output-port");
    checkArgumentLength(6);

    argumentAsString(0, id);
    argumentCheckProcedure(1, readProc);
    argumentCheckProcedure(2, writeDProc);
    argumentCheckProcedureOrFalse(3, getPositionProc);
    argumentCheckProcedureOrFalse(4, setPositionDProc);
    argumentCheckProcedureOrFalse(5, closeProc);

    return Object::makeCustomTextualInputOutputPort(theVM, id->data(), readProc, writeDProc, getPositionProc, setPositionDProc, closeProc);
}

Object scheme::makeCustomBinaryInputOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-binary-input/output-port");
    checkArgumentLength(6);

    argumentAsString(0, id);
    argumentCheckProcedure(1, readProc);
    argumentCheckProcedure(2, writeProc);
    argumentCheckProcedureOrFalse(3, getPositionProc);
    argumentCheckProcedureOrFalse(4, setPositionProc);
    argumentCheckProcedureOrFalse(5, closeProc);

    return Object::makeCustomBinaryInputOutputPort(theVM, id->data(), readProc, writeProc, getPositionProc, setPositionProc, closeProc);
}

/*
    file-options

    (file-options)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
    (file-options no-create)
      If file exists:     truncate
      If does not exist:  raise &file-does-not-exist
    (file-options no-fail)
      If file exists:     truncate
      If does not exist:  create new file
    (file-options no-truncate)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
    (file-options no-create no-fail)
      If file exists:     truncate
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist
    (file-options no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  create new file
    (file-options no-create no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  raise &file-does-not-exist
    (file-options no-create no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist

*/
Object scheme::openFileInputOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-file-input/output-port");
    checkArgumentLengthBetween(1, 4);
    BinaryInputOutputPort* port = NULL;
    Transcoder* transcoder = NULL;
    int openFlags = 0;

    argumentAsString(0, path);
    const bool isFileExist = fileExistsP(path->data());
    const bool isReadable = fileReadableP(path->data());

    if (argc == 1) {
        if (isFileExist) {
            return callIoFileAlreadyExistAfter(theVM, procedureName, argv[0], "file already exists", L1(argv[0]));
        }

        // default buffer mode is Block
        port = new BlockBufferedFileBinaryInputOutputPort(path->data(), openFlags);
    } else {
        argumentAsRecord(1, fileOptions);

        const bool emptyP = isEmpty(fileOptions);
        const bool noCreateP = isNoCreate(fileOptions);
        const bool noTruncateP = isNoTruncate(fileOptions);
        const bool noFailP = isNoFail(fileOptions);

//        printf("emptyP=%d noCreateP=%d noTruncateP=%d noFailP=%d\n", emptyP, noCreateP, noTruncateP, noFailP);

        if (isFileExist && emptyP) {
            return callIoFileAlreadyExistAfter(theVM, argv[0], procedureName, "file already exists", L1(argv[0]));
        } else if (noCreateP && noTruncateP) {
            if (!isFileExist) {
                return callIoFileNotExistAfter(theVM, argv[0], procedureName, "file-options no-create: file not exist", L1(argv[0]));
            }
        } else if (noCreateP) {
            if (isFileExist) {
                openFlags |= O_TRUNC;
            } else {
                return callIoFileNotExistAfter(theVM, argv[0], procedureName, "file-options no-create: file not exist", L1(argv[0]));
            }
        } else if (noFailP && noTruncateP) {
            if (!isFileExist) {
                openFlags |= O_TRUNC;
            }
        } else if (noFailP) {
            openFlags |= O_TRUNC;
        } else if (noTruncateP) {
            if (isFileExist) {
                return callIoFileAlreadyExistAfter(theVM, argv[0], procedureName, "file-options no-trucate: file already exists", L1(argv[0]));
            } else {
                openFlags |= O_TRUNC;
            }
        }

        if (argc == 2) {
            port = new BlockBufferedFileBinaryInputOutputPort(path->data(), openFlags);
        } else {
            argumentCheckSymbol(2, bufferMode);

            if (bufferMode == Symbol::BLOCK) {
                port = new BlockBufferedFileBinaryInputOutputPort(path->data(), openFlags);
            } else if (bufferMode == Symbol::LINE) {
                port = new LineBufferedFileBinaryInputOutputPort(path->data(), openFlags);
            } else if (bufferMode == Symbol::NONE) {
                port = new FileBinaryInputOutputPort(path->data(), openFlags);
            } else {
                callErrorAfter(theVM, procedureName, "invalid buffer-mode option", L1(argv[2]));
                return Object::Undef;
            }
            if (argc == 4) {
                argumentCheckTranscoderOrFalse(3, maybeTranscoder);
                if (maybeTranscoder != Object::False) {
                    transcoder = maybeTranscoder.toTranscoder();
                }
            }
        }
    }

    if ((port != NULL) && (MOSH_SUCCESS == port->open())) {
        if (transcoder == NULL) {
            return Object::makeBinaryInputOutputPort(port);
        } else {
            return Object::makeTextualInputOutputPort(port, transcoder);
        }
    } else {
        switch(errno) {
        case EACCES:
            if (isReadable) {
                return callIoFileReadOnlyAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
            } else {
                return callIoFileProtectionAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
            }
        default:
            callErrorAfter(theVM, procedureName, strerror(errno), L1(argv[0]));
            return Object::Undef;
        }
    }
}

Object scheme::peekCharEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("peek-char");
    checkArgumentLengthBetween(0, 1);
    TRY_WITHOUT_DSTR {
        if (0 == argc) {
            const ucs4char ch = theVM->currentInputPort().toTextualInputPort()->lookaheadChar();
            return ch == EOF ? Object::Eof : Object::makeChar(ch);
        } else {
            argumentAsTextualInputPort(0, textualInputPort);
            const ucs4char ch = textualInputPort->lookaheadChar();
            return ch == EOF ? Object::Eof : Object::makeChar(ch);
        }
    } CATCH(ioError) {
        ioError.arg1 = (0 == argc) ? theVM->currentInputPort() : argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::getDatumEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-datum");
    checkArgumentLength(1);
    bool errorOccured = false;
    argumentAsTextualInputPort(0, in);
    TRY_WITHOUT_DSTR {
        const Object object = in->getDatum(errorOccured);
        if (errorOccured) {
            callLexicalAndIOReadAfter(theVM, procedureName, in->error());
            return Object::Undef;
        }
        return object;
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::getStringAllEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-string-all");
    argumentAsTextualInputPort(0, in);
    TRY_WITHOUT_DSTR {
        ucs4string text = in->getStringAll();
        if (text.size() == 0) {
            return Object::Undef;
        } else {
            return Object::makeString(text);
        }
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::getStringNDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-string-n!");
    argumentAsTextualInputPort(0, in);
    argumentAsString(1, dest);
    argumentCheckExactInteger(2, start);
    argumentCheckExactInteger(3, count);

    if (!Arithmetic::fitsU32(start)) {
        callAssertionViolationAfter(theVM, procedureName, "start value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (!Arithmetic::fitsU32(count)) {
        callAssertionViolationAfter(theVM, procedureName, "count value out of range", L1(argv[3]));
        return Object::Undef;
    }

    const uint32_t u32Start  = Arithmetic::toU32(start);
    const uint32_t u32Count = Arithmetic::toU32(count);

    if ((uint32_t)dest->length() < u32Count + u32Start) {
        callAssertionViolationAfter(theVM, procedureName, "string must be a string with at least start + count elements.", L2(argv[2], argv[3]));
        return Object::Undef;
    }

    TRY_WITHOUT_DSTR {
        ucs4string text = in->getString(u32Count);
        if (text.size() == 0) {
            return Object::Eof;
        } else {
            ucs4string& s = dest->data();
            for (int i = 0; i < (int)text.size(); i++) {
                s[u32Start + i] = text[i];
            }
            return Bignum::makeInteger(text.size());
        }
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::getCharEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-char");
    checkArgumentLength(1);
    argumentAsTextualInputPort(0, textualInputPort);
    TRY_WITHOUT_DSTR {
        const ucs4char ch = textualInputPort->getChar();
        return ch == EOF ? Object::Eof : Object::makeChar(ch);
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::getStringNEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-string-n");
    checkArgumentLength(2);
    argumentAsTextualInputPort(0, inputPort);
    argumentAsFixnum(1, size);
    TRY_WITHOUT_DSTR {
        ucs4string text = inputPort->getString(size);

        if (text.size() == 0) {
            return Object::Eof;
        } else {
            return Object::makeString(text);
        }
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::portHasPortPositionPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("port-has-port-position?");
    checkArgumentLength(1);
    argumentAsPort(0, port);
    return Object::makeBool(port->hasPosition());
}

Object scheme::portHasSetPortPositionDPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("port-has-set-port-position!?");
    checkArgumentLength(1);
    argumentAsPort(0, port);
    return Object::makeBool(port->hasSetPosition());
}

Object scheme::setPortPositionDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("set-port-position!");
    checkArgumentLength(2);
    argumentAsPort(0, port);
    argumentAsFixnum(1, position);
    if (port->hasSetPosition()) {
        if (port->setPosition(position)) {
            return Object::Undef;
        } else {
            return callIOInvalidPositionAfter(theVM, procedureName, "invalid port position", L2(argv[0], argv[1]), argv[1]);
        }
    } else {
        callAssertionViolationAfter(theVM, procedureName, "port doesn't support set-port-position!", L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::portPositionEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("port-position");
    checkArgumentLength(1);
    argumentAsPort(0, port);
    if (port->hasPosition()) {
        return port->position();
    } else {
        callAssertionViolationAfter(theVM, procedureName, "port doesn't support port-position", L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::openBytevectorInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-bytevector-input-port");
    checkArgumentLengthBetween(1, 2);
    argumentAsByteVector(0, bv);
    if (1 == argc) {
        return Object::makeBinaryInputPort(new ByteArrayBinaryInputPort(bv->data(), bv->length()));
    } else { // 2 == argc
        argumentCheckTranscoderOrFalse(1, maybeTranscoder);
        BinaryInputPort* in = new ByteArrayBinaryInputPort(bv->data(), bv->length());
        if (maybeTranscoder.isFalse()) {
            return Object::makeBinaryInputPort(in);
        } else {
            return Object::makeTextualInputPort(in, maybeTranscoder.toTranscoder());
        }
    }
}

Object scheme::portEofPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("port-eof?");
    checkArgumentLength(1);
    const Object port = argv[0];
    TRY_WITHOUT_DSTR {
        if (port.isBinaryInputPort()) {
            return Object::makeBool(port.toBinaryInputPort()->lookaheadU8() == EOF);
        } else if (port.isBinaryInputOutputPort()) {
            return Object::makeBool(port.toBinaryInputOutputPort()->lookaheadU8() == EOF);
        } else if (port.isTextualInputPort()) {
            return Object::makeBool(port.toTextualInputPort()->lookaheadChar() == EOF);
        } else if (port.isTextualInputOutputPort()) {
            return Object::makeBool(port.toTextualInputOutputPort()->lookaheadChar() == EOF);
        } else {
            callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "port", port, L1(port));
            return Object::Undef;
        }
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::putBytevectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("put-bytevector");
    checkArgumentLengthBetween(2, 4);
    argumentAsBinaryOutputPort(0, outputPort);
    argumentAsByteVector(1, bv);
    if (argc < 3) {
        outputPort->putByteVector(bv);
        return Object::Undef;
    }

    argumentCheckExactInteger(2, startObj);
    int start;
    if (startObj.isFixnum()) {
        start = startObj.toFixnum();
    } else { // startObj.isBignum()
        start = startObj.toBignum()->toS32();
    }
    if (argc < 4) {
        outputPort->putByteVector(bv, start);
        return Object::Undef;
    }

    argumentCheckExactInteger(3, countObj);
    int count;
    if (countObj.isFixnum()) {
        count = countObj.toFixnum();
    } else { // countObj.isBignum()
        count = countObj.toBignum()->toS32();
    }
    outputPort->putByteVector(bv, start, count);
    return Object::Undef;
}

Object scheme::putCharEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("put-char");
    checkArgumentLength(2);
    argumentAsTextualOutputPort(0, textualOutputPort);
    argumentAsChar(1, ch);
    TRY_WITHOUT_DSTR {
        textualOutputPort->putChar(ch);
        return Object::Undef;
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::putDatumEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("put-datum");
    checkArgumentLength(2);
    argumentAsTextualOutputPort(0, textualOutputPort);
    TRY_WITHOUT_DSTR {
        textualOutputPort->putDatum(argv[1]);
        return Object::Undef;
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}


Object scheme::outputPortPEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("output-port?");
    checkArgumentLength(1);
    return Object::makeBool(argv[0].isOutputPort());
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

Object scheme::faslWriteEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("fasl-write");
    checkArgumentLength(2);
    argumentAsBinaryOutputPort(1, outputPort);
    FaslWriter writer(outputPort);
    TRY_WITHOUT_DSTR {
        writer.put(argv[0]);
    } CATCH(ioError) {
        ioError.arg1 = argv[1];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
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
    TRY_WITHOUT_DSTR {
        return inputPort->getLine();
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::closePortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("close-port");
    checkArgumentLength(1);
    argumentAsPort(0, port);
    TRY_WITHOUT_DSTR {
        port->close();
//         if (port.isBinaryOutputPort()) {
//             port.toBinaryOutputPort()->close();
//         } else if (port.isBinaryInputOutputPort()) {
//             port.toBinaryInputOutputPort()->close();
//         } else if (port.isTextualOutputPort()) {
//             port.toTextualOutputPort()->close();
//         } else if (port.isTextualInputOutputPort()) {
//             port.toTextualInputOutputPort()->close();
//         } else if (port.isBinaryInputPort()) {
//             port.toBinaryInputPort()->close();
//         } else if (port.isTextualInputPort()) {
//             port.toTextualInputPort()->close();
//         } else {
//             callAssertionViolationAfter(theVM, procedureName, "port required", L1(port));
//         }
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
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
    checkArgumentLength(1);
    argumentAsTextualInputPort(0, textualInputPort);
    TRY_WITHOUT_DSTR {
        ucs4char ch;
        ch = textualInputPort->lookaheadChar();
        return ch == EOF ? Object::Eof : Object::makeChar(ch);
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
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
    TRY_WITHOUT_DSTR {
        if (1 == argc) {
            theVM->currentOutputPort().toTextualOutputPort()->display(obj);
            theVM->currentOutputPort().toTextualOutputPort()->flush();
        } else {
            argumentAsTextualOutputPort(1, textualOutputPort);
            textualOutputPort->display(obj);
            textualOutputPort->flush();
        }
        return Object::Undef;
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::writeCharEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("write-char");
    checkArgumentLengthBetween(1, 2);
    argumentAsChar(0, ch);
    TRY_WITHOUT_DSTR {
        if (1 == argc) {
            theVM->currentOutputPort().toTextualOutputPort()->putChar(ch);
            theVM->currentOutputPort().toTextualOutputPort()->flush();
        } else {
            argumentAsTextualOutputPort(1, textualOutputPort);
            textualOutputPort->putChar(ch);
            textualOutputPort->flush();
        }
        return Object::Undef;
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
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
    TRY_WITHOUT_DSTR {
        if (0 == argc) {
            const ucs4char ch = theVM->currentInputPort().toTextualInputPort()->getChar();
            return ch == EOF ? Object::Eof : Object::makeChar(ch);
        } else {
            argumentAsTextualInputPort(0, textualInputPort);
            const ucs4char ch = textualInputPort->getChar();
            return ch == EOF ? Object::Eof : Object::makeChar(ch);
        }
    } CATCH(ioError) {
        ioError.arg1 = (0 == argc) ? theVM->currentInputPort() : argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
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
    TRY_WITHOUT_DSTR {
        const Object object = inputPort->getDatum(errorOccured);
        if (errorOccured) {
            callLexicalAndIOReadAfter(theVM, procedureName, inputPort->error());
            return Object::Undef;
        }
        return object;
    } CATCH(ioError) {
        ioError.arg1 = (0 == argc) ? theVM->currentInputPort() : argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
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
    BlockBufferedFileBinaryOutputPort* const fileBinaryOutputPort = new BlockBufferedFileBinaryOutputPort(file->data());

    if (MOSH_SUCCESS == fileBinaryOutputPort->open()) {
        return Object::makeTextualOutputPort(fileBinaryOutputPort, transcoder);
    } else {
        const bool isReadable = fileReadableP(file->data());
        switch(errno) {
        case EACCES:
            if (isReadable) {
                return callIoFileReadOnlyAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
            } else {
                return callIoFileProtectionAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
            }
        default:
            callErrorAfter(theVM, procedureName, strerror(errno), L1(argv[0]));
            return Object::Undef;
        }
    }
}

Object scheme::closeOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("close-output-port");
    checkArgumentLength(1);

    if (argv[0].isTextualOutputPort() || argv[0].isBinaryOutputPort()) {
        argumentAsPort(0, port);
        port->close();
        return Object::Undef;
    } else {
        callAssertionViolationAfter(theVM, procedureName, "output port required", L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::closeInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("close-input-port");
    checkArgumentLength(1);
    if (argv[0].isTextualInputPort() || argv[0].isBinaryInputPort()) {
        argumentAsPort(0, port);
        port->close();
        return Object::Undef;
    } else {
        callAssertionViolationAfter(theVM, procedureName, "input port required", L1(argv[0]));
        return Object::Undef;
    }
}

Object scheme::getOutputStringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-output-string");
    checkArgumentLength(1);
    argumentAsTextualOutputPort(0, textualOutputPort);
    StringTextualOutputPort* p = reinterpret_cast<StringTextualOutputPort*>(textualOutputPort);
    const Object ret = Object::makeString(p->getString());
    p->reset();
    return ret;
}

Object scheme::deleteFileEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("delete-file");
    checkArgumentLength(1);
    argumentAsString(0, text);
    if (-1 == unlink(text->data().ascii_c_str())) {
        callIoFileNameErrorAfter(theVM, argv[0], procedureName,
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
    return Object::makeBool(fileExistsP(path->data()));
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
    TRY_WITHOUT_DSTR {
        if (1 == argc) {
            theVM->currentOutputPort().toTextualOutputPort()->putDatum(obj);
        } else {
            argumentAsTextualOutputPort(1, textualOutputPort);
            textualOutputPort->putDatum(obj);
        }
        return Object::Undef;
    } CATCH(ioError) {
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::makeCustomBinaryInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-binary-input-port");
    checkArgumentLength(5);

    argumentAsString(0, id);
    argumentCheckProcedure(1, readProc);
    argumentCheckProcedureOrFalse(2, getPositionProc);
    argumentCheckProcedureOrFalse(3, setPositionProc);
    argumentCheckProcedureOrFalse(4, closeProc);

    return Object::makeCustomBinaryInputPort(theVM, id->data(), readProc, getPositionProc, setPositionProc, closeProc);
}

Object scheme::makeCustomBinaryOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-binary-output-port");
    checkArgumentLength(5);

    argumentAsString(0, id);
    argumentCheckProcedure(1, writeDProc);
    argumentCheckProcedureOrFalse(2, getPositionProc);
    argumentCheckProcedureOrFalse(3, setPositionDProc);
    argumentCheckProcedureOrFalse(4, closeProc);

    return Object::makeCustomBinaryOutputPort(theVM, id->data(), writeDProc, getPositionProc, setPositionDProc, closeProc);
}

Object scheme::makeCustomTextualInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-textual-input-port");
    checkArgumentLength(5);

    argumentAsString(0, id);
    argumentCheckProcedure(1, readProc);
    argumentCheckProcedureOrFalse(2, getPositionProc);
    argumentCheckProcedureOrFalse(3, setPositionProc);
    argumentCheckProcedureOrFalse(4, closeProc);

    return Object::makeCustomTextualInputPort(theVM, id->data(), readProc, getPositionProc, setPositionProc, closeProc);
}

Object scheme::makeCustomTextualOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("make-custom-textual-output-port");
    checkArgumentLength(5);

    argumentAsString(0, id);
    argumentCheckProcedure(1, writeDProc);
    argumentCheckProcedureOrFalse(2, getPositionProc);
    argumentCheckProcedureOrFalse(3, setPositionDProc);
    argumentCheckProcedureOrFalse(4, closeProc);

    return Object::makeCustomTextualOutputPort(theVM, id->data(), writeDProc, getPositionProc, setPositionDProc, closeProc);
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
    uint8_t* buffer = allocatePointerFreeU8Array(u32Count);
    bool isErrorOccured = false;
    const int ret = binaryInputPort->readBytes(buffer, u32Count, isErrorOccured);
    if (isErrorOccured) {
        callAssertionViolationAfter(theVM, procedureName, "read error");
        return Object::Undef;
    } else if (ret == 0) {
        return Object::Eof;
    } else {
        return Object::makeByteVector(new ByteVector(ret, buffer));
    }
}

Object scheme::getBytevectorAllEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-bytevector-all");
    checkArgumentLength(1);

    argumentAsBinaryInputPort(0, binaryInputPort);

    bool isErrorOccured = false;
    uint8_t* dest;
    const int ret = binaryInputPort->readAll(&dest, isErrorOccured);
    if (isErrorOccured) {
        callAssertionViolationAfter(theVM, procedureName, "read error");
        return Object::Undef;
    } else if (ret == 0) {
        return Object::Eof;
    } else {
        return Object::makeByteVector(new ByteVector(ret, dest));
    }
}

Object scheme::getBytevectorSomeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-bytevector-some");
    checkArgumentLength(1);

    argumentAsBinaryInputPort(0, binaryInputPort);

    bool isErrorOccured = false;
    uint8_t* dest;
    const int ret = binaryInputPort->readSome(&dest, isErrorOccured);
    if (isErrorOccured) {
        callAssertionViolationAfter(theVM, procedureName, "read error");
        return Object::Undef;
    } else if (ret == 0) {
        return Object::Eof;
    } else {
        return Object::makeByteVector(new ByteVector(ret, dest));
    }
}

Object scheme::getBytevectorNDEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-bytevector-n!");
    checkArgumentLength(4);

    argumentAsBinaryInputPort(0, binaryInputPort);
    argumentAsByteVector(1, bv);
    argumentCheckExactInteger(2, start);
    argumentCheckExactInteger(3, count);

    if (!Arithmetic::fitsU32(start)) {
        callAssertionViolationAfter(theVM, procedureName, "start value out of range", L1(argv[2]));
        return Object::Undef;
    }

    if (!Arithmetic::fitsU32(count)) {
        callAssertionViolationAfter(theVM, procedureName, "count value out of range", L1(argv[3]));
        return Object::Undef;
    }

    const uint32_t u32Start  = Arithmetic::toU32(start);
    const uint32_t u32Count = Arithmetic::toU32(count);

    if (bv->length() < u32Count + u32Start) {
        callAssertionViolationAfter(theVM, procedureName, "bytevector must be a bytevector with at least start + count elements.", L2(argv[2], argv[3]));
        return Object::Undef;
    }

    bool isErrorOccured = false;
    const int ret = binaryInputPort->readBytes(bv->data() + u32Start, u32Count, isErrorOccured);
    if (isErrorOccured) {
        callAssertionViolationAfter(theVM, procedureName, "read error");
        return Object::Undef;
    } else if (ret == 0) {
        return Object::Eof;
    } else {
        return Bignum::makeInteger(ret);
    }
}

Object scheme::transcodedPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("transcoded-port");
    checkArgumentLength(2);

    argumentAsTranscoder(1, transcoder);
    const Object port = argv[0];
    if (port.isBinaryInputPort()) {
        return Object::makeTextualInputPort(port.toBinaryInputPort(), transcoder);
    } else if (port.isBinaryOutputPort()) {
        return Object::makeTextualOutputPort(port.toBinaryOutputPort(), transcoder);
    } else {
        callWrongTypeOfArgumentViolationAfter(theVM, procedureName, "binary port", port, L1(port));
        return Object::Undef;
    }
}

Object scheme::latin1CodecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("latin-1-codec");
    checkArgumentLength(0);
    return Object::makeCodec(new Latin1Codec());
}

Object scheme::utf8CodecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("utf-8-codec");
    checkArgumentLength(0);
    return Object::makeCodec(new UTF8Codec());
}

Object scheme::utf16CodecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("utf-16-codec");
    checkArgumentLength(0);
    return Object::makeCodec(new UTF16Codec);
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
    argumentCheckSymbol(1, style);
    EolStyle eolStyle;
    if (!Transcoder::validateEolStyle(style, eolStyle)) {
        callAssertionViolationAfter(theVM, procedureName, "invalid eol-style", L1(argv[1]));
        return Object::Undef;
    }
    if (argc == 2) {
        return Object::makeTranscoder(codec, eolStyle);
    }
    argumentCheckSymbol(2, errorHandlingMode);
    ErrorHandlingMode mode;
    if (!Transcoder::validateErrorHandlingMode(errorHandlingMode, mode)) {
        callAssertionViolationAfter(theVM, procedureName, "invalid error-handling-mode", L1(argv[2]));
        return Object::Undef;
    }
    return Object::makeTranscoder(codec, eolStyle, mode);
}

Object scheme::nativeTranscoderEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("native-transcoder");
    checkArgumentLength(0);
    return Object::makeTranscoder(Transcoder::nativeTranscoder());
}

Object scheme::transcoderCodecEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("transcoder-codec");
    checkArgumentLength(1);
    argumentAsTranscoder(0, transcoder);
    return transcoder->codec();
}

Object scheme::transcoderEolStyleEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("transcoder-eol-style");
    checkArgumentLength(1);
    argumentAsTranscoder(0, transcoder);
    return transcoder->eolStyleSymbol();
}

Object scheme::transcoderErrorHandlingModeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("transcoder-error-handling-mode");
    checkArgumentLength(1);
    argumentAsTranscoder(0, transcoder);
    return transcoder->errorHandlingModeSymbol();
}

Object scheme::bytevectorTostringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("bytevector->string");
    checkArgumentLength(2);

    argumentAsByteVector(0, bytevector);
    argumentAsTranscoder(1, transcoder);

    BinaryInputPort* port = new ByteArrayBinaryInputPort(bytevector->data(), bytevector->length());
    TRY_WITHOUT_DSTR {
        return Object::makeString(transcoder->getString(port));
    } CATCH(ioError) {
        ioError.arg1 = Object::makeBinaryInputPort(port);
        ioError.who = procedureName;
        ioError.irritants = Object::cons(argv[1], ioError.irritants);
        return callIOErrorAfter(theVM, ioError);
    }
}

Object scheme::stringTobytevectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("string->bytevector");
    checkArgumentLength(2);
    argumentAsString(0, text);
    argumentAsTranscoder(1, transcoder);

    ByteArrayBinaryOutputPort accum;
    TranscodedTextualOutputPort out(&accum, transcoder);

    for (ucs4string::const_iterator it = text->data().begin();
         it != text->data().end(); ++it) {
        TRY_WITHOUT_DSTR {
            out.putChar(*it);
        } CATCH(ioError) {
            ioError.arg1 = Object::Nil;
            ioError.who = procedureName;
            return callIOErrorAfter(theVM, ioError);
        }
    }
    return Object::makeByteVector(accum.toByteVector());
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
        return Object::makeBinaryOutputPort(new ByteArrayBinaryOutputPort);
    } else {
        argumentAsTranscoder(0, transcoder);
        return Object::makeTextualOutputPort(new ByteArrayBinaryOutputPort(), transcoder);
    }
}

Object scheme::sysGetBytevectorEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("get-bytevector");
    checkArgumentLength(1);
    const Object port = argv[0];
    if (port.isBinaryOutputPort()) {
        return Object::makeByteVector(reinterpret_cast<ByteArrayBinaryOutputPort*>(port.toBinaryOutputPort())->toByteVector());
    } else if (port.isTextualOutputPort()) {
        BinaryOutputPort* out = reinterpret_cast<TranscodedTextualOutputPort*>(port.toTextualOutputPort())->binaryPort();
        return Object::makeByteVector(reinterpret_cast<ByteArrayBinaryOutputPort*>(out)->toByteVector());
    } else {
        callAssertionViolationAfter(theVM, procedureName, "bytevector-port required", L1(argv[0]));
        return Object::Undef;
    }
}

/*
    file-options

    (file-options)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
    (file-options no-create)
      If file exists:     truncate
      If does not exist:  raise &file-does-not-exist
    (file-options no-fail)
      If file exists:     truncate
      If does not exist:  create new file
    (file-options no-truncate)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
    (file-options no-create no-fail)
      If file exists:     truncate
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist
    (file-options no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  create new file
    (file-options no-create no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  raise &file-does-not-exist
    (file-options no-create no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist

*/
Object scheme::openFileOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-file-output-port");
    checkArgumentLengthBetween(1, 4);
    BinaryOutputPort* port = NULL;
    Transcoder* transcoder = NULL;
    int openFlags = 0;

    argumentAsString(0, path);
    const bool isFileExist = fileExistsP(path->data());
    const bool isReadable = fileReadableP(path->data());


    if (argc == 1) {
        if (isFileExist) {
            return callIoFileAlreadyExistAfter(theVM, argv[0], procedureName, "file already exists", L1(argv[0]));
        }
        // default buffer mode is Block
        port = new BlockBufferedFileBinaryOutputPort(path->data(), openFlags);
    } else {
        argumentAsRecord(1, fileOptions);

        const bool emptyP = isEmpty(fileOptions);
        const bool noCreateP = isNoCreate(fileOptions);
        const bool noTruncateP = isNoTruncate(fileOptions);
        const bool noFailP = isNoFail(fileOptions);

        if (isFileExist && emptyP) {
            return callIoFileAlreadyExistAfter(theVM, argv[0], procedureName, "file already exists", L1(argv[0]));
        } else if (noCreateP && noTruncateP) {
            if (!isFileExist) {
                return callIoFileNotExistAfter(theVM, argv[0], procedureName, "file-options no-create: file not exist", L1(argv[0]));
            }
        } else if (noCreateP) {
            if (isFileExist) {
                openFlags |= O_TRUNC;
            } else {
                return callIoFileNotExistAfter(theVM, argv[0], procedureName, "file-options no-create: file not exist", L1(argv[0]));
            }
        } else if (noFailP && noTruncateP) {
            if (!isFileExist) {
                openFlags |= O_TRUNC;
            }
        } else if (noFailP) {
            openFlags |= O_TRUNC;
        } else if (noTruncateP) {
            if (isFileExist) {
                return callIoFileAlreadyExistAfter(theVM, argv[0], procedureName, "file-options no-trucate: file already exists", L1(argv[0]));
            } else {
                openFlags |= O_TRUNC;
            }
        }

        if (argc == 2) {
            port = new BlockBufferedFileBinaryOutputPort(path->data(), openFlags);
        } else {
            argumentCheckSymbol(2, bufferMode);

            if (bufferMode == Symbol::BLOCK) {
                port = new BlockBufferedFileBinaryOutputPort(path->data(), openFlags);
            } else if (bufferMode == Symbol::LINE) {
                port = new LineBufferedFileBinaryOutputPort(path->data(), openFlags);
            } else if (bufferMode == Symbol::NONE) {
                port = new FileBinaryOutputPort(path->data(), openFlags);
            } else {
                callErrorAfter(theVM, procedureName, "invalid buffer-mode option", L1(argv[2]));
                return Object::Undef;
            }
            if (argc == 4) {
                argumentCheckTranscoderOrFalse(3, maybeTranscoder);
                if (maybeTranscoder != Object::False) {
                    transcoder = maybeTranscoder.toTranscoder();
                }
            }
        }
    }

    if ((port != NULL) && (MOSH_SUCCESS == port->open())) {
        if (transcoder == NULL) {
            return Object::makeBinaryOutputPort(port);
        } else {
            return Object::makeTextualOutputPort(port, transcoder);
        }
    } else {
        switch(errno) {
        case EACCES:
            if (isReadable) {
                return callIoFileReadOnlyAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
            } else {
                return callIoFileProtectionAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
            }
        default:
            callErrorAfter(theVM, procedureName, strerror(errno), L1(argv[0]));
            return Object::Undef;
        }
    }
}

Object scheme::openInputFileEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-input-file");
    checkArgumentLength(1);

    argumentAsString(0, path);

    Transcoder* transcoder = Transcoder::nativeTranscoder();
    // we choose buffered port
    BufferedFileBinaryInputPort* const fileBinaryInputPort = new BufferedFileBinaryInputPort(path->data());
    if (MOSH_SUCCESS == fileBinaryInputPort->open()) {
        return Object::makeTextualInputPort(fileBinaryInputPort, transcoder);
    } else {
        switch(errno) {
        case EACCES:
            return callIoFileProtectionAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
        default:
            callErrorAfter(theVM, procedureName, strerror(errno), L1(argv[0]));
            return Object::Undef;
        }
    }
}

Object scheme::openFileInputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("open-file-input-port");
    checkArgumentLengthBetween(1, 4);
    BinaryInputPort* in = NULL;
    Transcoder* transcoder = NULL;

    // N.B. As R6RS says, we ignore "file-options" for input-port.
    if (argc == 1) {
        argumentAsString(0, path);
        // default buffer mode is Block
        in = new BufferedFileBinaryInputPort(path->data());
    } else if (argc == 2) {
        argumentAsString(0, path);
        argumentCheckRecord(1, fileOptions);
        // default buffer mode is Block
        in = new BufferedFileBinaryInputPort(path->data());
    } else if (argc == 3) {
        argumentAsString(0, path);
        argumentCheckRecord(1, fileOptions);
        argumentCheckSymbol(2, bufferMode);

        // N.B. On Mosh, buffer mode BLOCK == LINE.
        if (bufferMode == Symbol::BLOCK || bufferMode == Symbol::LINE) {
            in = new BufferedFileBinaryInputPort(path->data());
        } else if (bufferMode == Symbol::NONE) {
            in = new FileBinaryInputPort(path->data());
        } else {
            callErrorAfter(theVM, procedureName, "invalid buffer-mode option", L1(argv[2]));
            return Object::Undef;
        }
    } else if (argc == 4) {
        argumentAsString(0, path);
        argumentCheckRecord(1, fileOptions);
        argumentCheckSymbol(2, bufferMode);
        // N.B. On Mosh, buffer mode BLOCK == LINE.
        if (bufferMode == Symbol::BLOCK || bufferMode == Symbol::LINE) {
            in = new BufferedFileBinaryInputPort(path->data());
        } else if (bufferMode == Symbol::NONE) {
            in = new FileBinaryInputPort(path->data());
        } else {
            callErrorAfter(theVM, procedureName, "invalid buffer-mode option", L1(argv[2]));
            return Object::Undef;
        }
        argumentCheckTranscoderOrFalse(3, maybeTranscoder);
        if (maybeTranscoder != Object::False) {
            transcoder = maybeTranscoder.toTranscoder();
        }
    }

    if ((in != NULL) && (MOSH_SUCCESS == in->open())) {
        if (transcoder == NULL) {
            return Object::makeBinaryInputPort(in);
        } else {
            return Object::makeTextualInputPort(in, transcoder);
        }
    } else {
        switch(errno) {
        case EACCES:
            return callIoFileProtectionAfter(theVM, argv[0], procedureName, strerror(errno), L1(argv[0]));
        default:
            callErrorAfter(theVM, procedureName, strerror(errno), L1(argv[0]));
            return Object::Undef;
        }
    }
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
    static const Object port = Object::makeBinaryInputPort(new StandardInputPort());
    DeclareProcedureName("starndard-input-port");
    checkArgumentLength(0);
    return port;
}

Object scheme::standardOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    static const Object port = Object::makeBinaryOutputPort(new StandardOutputPort());
    DeclareProcedureName("starndard-output-port");
    checkArgumentLength(0);
    return port;
}

Object scheme::standardErrorPortEx(VM* theVM, int argc, const Object* argv)
{
    static const Object port = Object::makeBinaryOutputPort(new StandardErrorPort());
    DeclareProcedureName("starndard-error-port");
    checkArgumentLength(0);
    return port;
}


Object scheme::readdirEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("readdir");
#ifdef _WIN32
    return Object::makeString(UC("<not-supported>"));
#else
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
#endif
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


Object scheme::putU8Ex(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("put-u8");
    checkArgumentLength(2);
    argumentAsBinaryOutputPort(0, binaryOutputPort);
    argumentAsOctet(1, octet);

    binaryOutputPort->putU8(octet);

    return Object::Undef;
}

Object scheme::putStringEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("put-string");
    checkArgumentLengthBetween(2, 4);
    argumentAsTextualOutputPort(0, textualOutputPort);
    argumentAsString(1, stringObj);
    const ucs4string string = stringObj->data();
    if (argc < 3) {
        TRY_WITHOUT_DSTR {
            textualOutputPort->putString(string);
        } CATCH(IOError){
            ioError.arg1 = argv[0];
            ioError.who = procedureName;
            return callIOErrorAfter(theVM, ioError);
        }
        return Object::Undef;
    }

    argumentCheckExactInteger(2, startObj);
    int start;
    if (startObj.isFixnum()) {
        start = startObj.toFixnum();
    } else { // startObj.isBignum()
        start = startObj.toBignum()->toS32();
    }
    if (argc < 4) {
        TRY_WITHOUT_DSTR {
            textualOutputPort->putString(string.substr(start, string.length()-start));
        } CATCH(IOError){
            ioError.arg1 = argv[0];
            ioError.who = procedureName;
            return callIOErrorAfter(theVM, ioError);
        }
        return Object::Undef;
    }

    argumentCheckExactInteger(3, countObj);
    int count;
    if (countObj.isFixnum()) {
        count = countObj.toFixnum();
    } else { // countObj.isBignum()
        count = countObj.toBignum()->toS32();
    }
    TRY_WITHOUT_DSTR {
        textualOutputPort->putString(string.substr(start, count));
    } CATCH(IOError){
        ioError.arg1 = argv[0];
        ioError.who = procedureName;
        return callIOErrorAfter(theVM, ioError);
    }
    return Object::Undef;
}

Object scheme::flushOutputPortEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("flush-output-port");
    checkArgumentLength(1);
    const Object outputPort = argv[0];
    if (outputPort.isBinaryOutputPort()) {
        outputPort.toBinaryOutputPort()->flush();
    } else if (outputPort.isBinaryInputOutputPort()) {
        outputPort.toBinaryInputOutputPort()->flush();
    } else if (outputPort.isTextualOutputPort()) {
        outputPort.toTextualOutputPort()->flush();
    } else if (outputPort.isTextualInputOutputPort()) {
        outputPort.toTextualInputOutputPort()->flush();
    } else {
        callAssertionViolationAfter(theVM, procedureName, "output-port required", L1(outputPort));
    }
    return Object::Undef;
}

Object scheme::outputPortBufferModeEx(VM* theVM, int argc, const Object* argv)
{
    DeclareProcedureName("output-port-buffer-mode");
    checkArgumentLength(1);

    argumentCheckOutputPort(0, outputPort);
    enum OutputPort::bufferMode bufferMode;
    if (outputPort.isBinaryPort()) {
        bufferMode = outputPort.toBinaryOutputPort()->bufferMode();
    } else if (outputPort.isTextualPort()) {
        bufferMode = outputPort.toTextualOutputPort()->bufferMode();
    } else {
        callAssertionViolationAfter(theVM, procedureName, "output-port required", L1(outputPort));
        return Object::Undef;
    }

    if (bufferMode == OutputPort::BLOCK) {
        return Symbol::BLOCK;
    } else if (bufferMode == OutputPort::LINE) {
        return Symbol::LINE;
    } else {
        return Symbol::NONE;
    }
}

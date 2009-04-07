/*
 * OSCompat.cpp - OS compatibility functions.
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: OScompat.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include "scheme.h"
#include "Object.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Transcoder.h"
#include "UTF8Codec.h"
#include "ByteArrayBinaryInputPort.h"
#include "ByteArrayBinaryOutputPort.h"
#include "ErrorProcedures.h"
#include "OSCompat.h"
#include "SString.h"
#include "ByteVector.h"
#include <dirent.h>

using namespace scheme;
//
// N.B Dont't forget to add tests to OScompatTest.cpp.
//

// TODO: This funcion should be placed on File Class ?.
int scheme::openFd(const ucs4string& file, int flags, mode_t mode)
{
#ifdef _WIN32
    // Add O_BINARY flag!
    // TODO file should be encoded
    return open(file.ascii_c_str(), O_BINARY | flags, mode);
#else
    return open((char*)utf32toUtf8(file)->data(), flags, mode);
#endif
}

// TODO: This funcion should be placed on File Class ?.
// N.B. This funcion can raise I/O error, caller should handle it.
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
                throwIOError2(IOError::READ, stringError(errno));
                return result;
            } else {
                return result;
            }
        }
    }
}

// TODO: This funcion should be placed on File Class ?.
// N.B. This funcion can raise I/O error, caller should handle it.
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
                throwIOError2(IOError::WRITE, stringError(errno));
                return result;
            } else {
                return result;
            }
        }
    }
}


#ifdef _WIN32
    #define F_OK 0
    #define W_OK 2
    #define R_OK 4
#endif

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

ucs4string scheme::utf8ToUtf32(const char* s, int len)
{
    ByteArrayBinaryInputPort in((uint8_t*)s, len);
    UTF8Codec codec;
    Transcoder transcoderr(&codec, EolStyle(LF), ErrorHandlingMode(IGNORE_ERROR));
    return transcoderr.getString(&in);
}

// output is NULL terminated
ByteVector* scheme::utf32toUtf8(const ucs4string& s)
{
    ByteArrayBinaryOutputPort out;
    UTF8Codec codec;
    Transcoder transcoderr(&codec, EolStyle(LF), ErrorHandlingMode(IGNORE_ERROR));
    transcoderr.putString(&out, s);
    if (!s.empty()) {
        transcoderr.putChar(&out, '\0');
    }
    return out.toByteVector();
}

ucs4string scheme::stringError(int num)
{
    const char* text = strerror(num);
    return ucs4string::from_c_str(text, strlen(text));
    // use _wcserror_s on Windows ?
}

ucs4char* scheme::getEnv(const ucs4string& key)
{
    const char* value = getenv(key.ascii_c_str());
    if (NULL == value) {
        return NULL;
    }
    return utf8ToUtf32(value, strlen(value)).strdup();
}

#ifdef _WIN32
#include <stdlib.h>
#define environ _environ
#else
extern char** environ;
#endif

Object scheme::getEnvAlist()
{
    Object ret = Object::Nil;
    char** env = environ;
    while(*env) {
        char* equalPostion = strchr(*env, '=');
        ucs4string key = utf8ToUtf32(*env, equalPostion - *env);
        ucs4string value = utf8ToUtf32(equalPostion + 1, strlen(equalPostion + 1));
        ret = Object::cons(Object::cons(Object::makeString(key),
                                        Object::makeString(value)),
                           ret);
        env++;
    }
    return ret;
}

Object scheme::readDirectory(const ucs4string& path)
{
    DIR* dir;
    if (NULL == (dir = opendir((char*)utf32toUtf8(path)->data()))) {
        return Object::False;
    }
    Object ret = Object::Nil;
    for (struct dirent* entry = readdir(dir); entry != NULL; entry = readdir(dir))
    {
        ret = Object::cons(Object::makeString(entry->d_name), ret);
    }
    return ret;
}

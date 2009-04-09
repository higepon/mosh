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

#ifndef _WIN32
#include <sys/resource.h>
#define _LARGEFILE64_SOURCE
#include <sys/types.h>
#include <unistd.h>
#endif
#ifdef _WIN32
#include <windows.h>
#include <shlwapi.h>
#include <tchar.h>
#pragma comment(lib, "shlwapi.lib")
#else
#include <unistd.h>
#endif
#ifndef _MSC_VER
#include <dirent.h>
#endif
#ifdef __APPLE__
#include <sys/param.h>
#include <mach-o/dyld.h> /* _NSGetExecutablePath */
#include <string.h>
#endif /* __APPLE__ */

#ifdef __FreeBSD__
#include <dlfcn.h>
extern int main(int argc, char *argv[]);
#endif /* __FreeBSD__ */
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
#include "UTF16Codec.h"
#include "ByteArrayBinaryInputPort.h"
#include "ByteArrayBinaryOutputPort.h"
#include "ErrorProcedures.h"
#include "OSCompat.h"
#include "SString.h"
#include "ByteVector.h"
#include "PortProcedures.h"

using namespace scheme;
//
// N.B Dont't forget to add tests to OScompatTest.cpp.
//

File::File() : desc_(-1), isOpen_(false)
{
}

// For stdin/stdout/stderr
File::File(int desc) : desc_(desc), isOpen_(true)
{
}

File::~File()
{
}

bool File::open(const ucs4string& file, int flags, int mode)
{
#ifdef _WIN32
    // Add O_BINARY flag!
    // TODO file should be encoded
    desc_ = ::open(file.ascii_c_str(), O_BINARY | flags, mode);
    isOpen_ = desc_ != -1;
    return isOpen_;
#else
    desc_ = ::open((char*)utf32toUtf8(file)->data(), flags, mode);
    isOpen_ = desc_ != -1;
    return isOpen_;
#endif
}

int File::dup(int target)
{
    return dup2(desc_, target);
    // TODO windows
}

void File::close()
{
    // TODO windows
    if (isOpen_) {
        ::close(desc_);
    }
}

int64_t File::size() const
{
    // TODO windows
    struct stat st;
    const int result = fstat(desc_, &st);
    MOSH_ASSERT(result == 0); // will never happen?
    return st.st_size;
}

// N.B. This funcion can raise I/O error, caller should handle it.
int File::write(uint8_t* buf, size_t size)
{
    MOSH_ASSERT(desc_ != BinaryPort::INVALID_FILENO);

    for (;;) {
        const int result = ::write(desc_, buf, size);
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
    return 0;
}

int File::read(uint8_t* buf, size_t size)
{
    MOSH_ASSERT(desc_ != BinaryPort::INVALID_FILENO);
    for (;;) {
        const int result = ::read(desc_, buf, size);
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
    return 0;
}

bool File::isOpen() const
{
    return isOpen_;
}

int64_t File::seek(int64_t offset, int whence)
{
#if defined(_WIN32) // TODO
    return lseek(desc_, offset, whence);
#elif defined(__APPLE__)
    return lseek(desc_, offset, whence);
#else
    // TODO handle 64bit lseek64?
    return lseek64(desc_, offset, whence);
#endif
}

bool File::isExists(const ucs4string& path)
{
    return false;
}


int64_t scheme::lseekFd(int fd, int64_t offset, int whence)
{
#if defined(_WIN32) // TODO
    return lseek(fd, offset, whence);
#elif defined(__APPLE__)
    return lseek(fd, offset, whence);
#else
    // TODO handle 64bit lseek64?
    return lseek64(fd, offset, whence);
#endif
}

ucs4char** scheme::getCommandLine(int argc, char* argv[])
{
    // TODO: Windows
    ucs4char** argvU = new(GC) ucs4char*[argc + 1];
    argvU[argc] = NULL;
    for (int i = 0; i < argc; i++) {
        argvU[i] = utf8ToUtf32(argv[i], strlen(argv[i])).strdup();
    }
    return argvU;
}

ucs4string scheme::getMoshExecutablePath(bool& isErrorOccured)
{
#if defined(_WIN32)
    wchar_t tmp[MAX_PATH]; 
    if (GetModuleFileNameW(NULL,tmp,MAX_PATH)) {
        if(PathRemoveFileSpecW(tmp)){
            PathAddBackslashW(tmp);
            ByteArrayBinaryInputPort name((uint8_t *)tmp,wcslen((const wchar_t*)tmp)*sizeof(wchar_t)); //FIXME: check wcslen behavior when the path includes any charactor outside of BMP.
            UTF16Codec codec(UTF16Codec::UTF_16LE);
            Transcoder tcoder(&codec);
            return tcoder.getString(&name);
        }
    }
    isErrorOccured = true;
    return UC("");
#elif defined(__linux__)
    char path[4096];
    int ret = readlink("/proc/self/exe", path, sizeof(path));
    if (ret != -1) {
        std::string chop(path, ret);
        int pos = chop.find_last_of('/');
        if (pos > 0) {
            const char* v = chop.substr(0, pos + 1).c_str();
            return ucs4string::from_c_str(v, strlen(v));
        }
    }
    isErrorOccured = true;
    return UC("");
#elif defined(__FreeBSD__)
    Dl_info info;
    char path[PATH_MAX + 1];

    if (dladdr( (const void*)&main, &info) == 0) {
        isErrorOccured = true;
        return UC("");
    }

    strncpy(path, info.dli_fname, PATH_MAX + 1);
    path[PATH_MAX + 1] = '\0';
    char base[PATH_MAX];
    if (NULL== realpath(path, base)) {
        isErrorOccured = true;
        return UC("");
    }
    std::string p = base;
    int pos = p.find_last_of('/');
    if (pos > 0) {
        return Object::makeString(p.substr(0, pos + 1).c_str());
    }
    isErrorOccured = true;
    return UC("");
#elif defined(__APPLE__)
    char path[MAXPATHLEN];
    uint32_t pathLen = MAXPATHLEN;
    if (_NSGetExecutablePath(path, &pathLen) == 0) {
        std::string chop(path, pathLen);
        int pos = chop.find_last_of('/');
        if (pos > 0) {
            const char* execPath = chop.substr(0, pos + 1).c_str();
            return ucs4string::from_c_str(execPath, strlen(execPath));
        }
    }
    isErrorOccured = true;
    return UC("");
#elif defined(__sun)
    char path[4096];
    char procpath[64];
    pid_t my_pid = getpid();
    sprintf(procpath, "/proc/%d/path/a.out", (int)my_pid);
    int ret = readlink(procpath, path, sizeof(path));
    if (ret != -1) {
        std::string chop(path, ret);
        int pos = chop.find_last_of('/');
        if (pos > 0) {
            return ucs4string::from_c_str(chop.substr(0, pos + 1).c_str());
        }
    }
    isErrorOccured = true;
    return UC("");
#else
    isErrorOccured = true;
    return UC("");
#endif
}

// TODO: This funcion should be placed on File Class ?.
int scheme::openFd(const ucs4string& file, int flags, int mode)
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
#ifdef _MSC_VER
    // TODO
	return Object::False;
#else
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
#endif
}

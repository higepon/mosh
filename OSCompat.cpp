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
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <assert.h>
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

#ifdef _WIN32
const HANDLE File::STANDARD_IN  = GetStdHandle(STD_INPUT_HANDLE);
const HANDLE File::STANDARD_OUT = GetStdHandle(STD_OUTPUT_HANDLE);
const HANDLE File::STANDARD_ERR = GetStdHandle(STD_ERROR_HANDLE);

#else
const int File::STANDARD_IN  = 0;
const int File::STANDARD_OUT = 1;
const int File::STANDARD_ERR = 2;
#endif

#ifdef _WIN32
wchar_t* utf32ToUtf16(const ucs4string& s)
{
    ByteArrayBinaryOutputPort out;
    UTF16Codec codec(UTF16Codec::UTF_16LE);
    Transcoder tcoder(&codec);
    tcoder.putString(&out, s);
    tcoder.putChar(&out, '\0');
    return (wchar_t*)out.toByteVector()->data();
}
std::wstring utf8ToUtf16(const uint8_t* s, int len)
{
    /* sorry, I don't know how to use UTF8Codec */
    std::wstring out;
    size_t outSize = MultiByteToWideChar(CP_UTF8, 0, reinterpret_cast<const char*>(s), len, 0, 0);
    if (outSize > 0) {
        out.resize(outSize);
        MultiByteToWideChar(CP_UTF8, 0, reinterpret_cast<const char*>(s), len, &out[0], outSize);
    } else {
//      printf("ERR %08x\n", GetLastError());
    }
    return out;
}
#endif // _WIN32

#ifdef _WIN32
File::File(HANDLE desc /* = INVALID_HANDLE_VALUE */)
    : desc_(desc)
    , prevC_(-1)
    {}
#else
File::File(int desc /* = -1 */)
    : desc_(desc) {}
#endif


bool File::open(const ucs4string& file, int flags)
{
#ifdef _WIN32
    DWORD access = 0, share = 0, disposition = 0;

    if (isOpen()) {
        return false;
    }
#if 1
    share = FILE_SHARE_READ;
    switch (flags) {
    case Read | Write | Create:
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = OPEN_ALWAYS;
        break;
    case Read | Write | Create | Truncate:
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = TRUNCATE_EXISTING;
        break;
    case Read:
        access = GENERIC_READ;
        disposition = OPEN_EXISTING;
        break;
    case Write | Create:
        access = GENERIC_WRITE;
        disposition = OPEN_ALWAYS;
        break;
    case Write | Create | Truncate:
        access = GENERIC_WRITE;
        disposition = TRUNCATE_EXISTING;
        break;
    default:
        MOSH_ASSERT(0);
    }
#else
    if (flags & Read) {
        access |= GENERIC_READ;
    }
    if (flags & Write) {
        access |= GENERIC_WRITE;
    }
    if (flags & Truncate) {
        disposition = TRUNCATE_EXISTING;
    }
    if (flags & Create) {
        disposition = CREATE_ALWAYS;
    }
    if (disposition == 0) disposition = OPEN_EXISTING; // is this correct ?
#endif
    desc_ = CreateFile(utf32ToUtf16(file), access, share, NULL, disposition, FILE_ATTRIBUTE_NORMAL, NULL);
if (desc_ == (HANDLE)-1) {
FILE *fp = fopen("c:/tmp/ttt.log", "wb");
if (fp) {
	fprintf(fp, "\nQQQ\nfile=%S, access=%08x, disposition=%x, flags=%08x\nQQQ\n", utf32ToUtf16(file), access, disposition, flags);
	fclose(fp);
}
}
    return isOpen();
#else
    if (isOpen()) {
        return false;
    }

    int mode = 0;
    if ((flags & Read) && (flags & Write)) {
        mode |= O_RDWR;
    } else {
        if (flags & Write) {
            mode |= O_WRONLY;
        }
        if (flags & Read) {
            mode |= O_RDONLY;
        }
    }
    if (flags & Create) {
        mode |= O_CREAT;
    }
    if (flags & Truncate) {
        mode |= O_TRUNC;
    }
    desc_ = ::open((char*)utf32toUtf8(file)->data(), mode, 0644);
    return isOpen();
#endif
}

File::~File()
{
    close();
}

#ifdef _WIN32
int File::dup(HANDLE target)
{
    MOSH_ASSERT(isOpen());
    // TODO windows
    return -1;
}
#else
int File::dup(int target)
{
    MOSH_ASSERT(isOpen());
    return dup2(desc_, target);
}
#endif

bool File::close()
{
#ifdef _WIN32
    if (isOpen()) {
        const bool isOK = CloseHandle(desc_) != 0;
        desc_ = INVALID_HANDLE_VALUE;
        return isOK;
    }
    return false;
#else
    if (isOpen()) {
        const bool isOK = ::close(desc_) != 0;
        desc_ = -1;
        return isOK;
    }
#endif
}

int64_t File::size() const
{
#ifdef _WIN32
    LARGE_INTEGER size;
    int isOK = GetFileSizeEx(desc_, &size);
    if (isOK) {
        return size.QuadPart;
    } else {
        return -1;
    }
#else
    struct stat st;
    const int result = fstat(desc_, &st);
    if (result != 0) {
        return -1;
    } else {
        return st.st_size;
    }
#endif
}

// N.B. This funcion can raise I/O error, caller should handle it.
int File::write(uint8_t* buf, size_t size)
{
#ifdef _WIN32
    MOSH_ASSERT(isOpen());
    DWORD writeSize;
    int isOK;
    // Writing to console is automatically converted into encoding of console.
    if (desc_ == STANDARD_OUT || desc_ == STANDARD_ERR) {
        isOK = WriteConsole(desc_, buf, size / 2, &writeSize, NULL);
        writeSize *= 2;
    } else {
        isOK = WriteFile(desc_, buf, size, &writeSize, NULL);
    }
    if (isOK) {
        return writeSize;
    } else {
        return -1;
    }
#else
    MOSH_ASSERT(isOpen());
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
#endif
}

int File::read(uint8_t* buf, size_t size)
{
#ifdef _WIN32
    DWORD readSize;
    int isOK;
    if (desc_ == STANDARD_IN) {
#if 1
        assert(size == 1); // temporary restriction
        if (prevC_ != -1) {
            isOK = true;
            readSize = 1;
            *buf = static_cast<uint8_t>(prevC_);
            prevC_ = -1;
        } else {
            wchar_t wc;
            isOK = ReadConsole(desc_, &wc, 1, &readSize, NULL);
            if (isOK) {
                readSize = 1;
                *buf = static_cast<uint8_t>(wc);
                prevC_ = wc >> 8;
            }
        }
#else
        isOK = ReadConsole(desc_, buf, size, &readSize, NULL);
#endif
    } else {
        isOK = ReadFile(desc_, buf, size, &readSize, NULL);
    }
    if (isOK) {
        return readSize;
    } else {
        return -1;
    }
#else
    MOSH_ASSERT(isOpen());
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
#endif
}

bool File::isOpen() const
{
#ifdef _WIN32
    return desc_ != INVALID_HANDLE_VALUE;
#else
    return desc_ != -1;
#endif
}

int64_t File::seek(int64_t offset, Whence whence /* = Begin */)
{
#ifdef _WIN32
    LARGE_INTEGER largePos;
    largePos.QuadPart = offset;
    DWORD posMode = FILE_BEGIN;
    switch (whence) {
    case Begin:
        posMode = FILE_BEGIN;
        break;
    case Current:
        posMode = FILE_CURRENT;
        break;
    case End:
        posMode = FILE_END;
        break;
    }
    LARGE_INTEGER resultPos;
    const BOOL isOK = SetFilePointerEx(desc_, largePos, &resultPos, posMode);
    if (isOK) {
        return resultPos.QuadPart;
    } else {
        return -1;
    }
#else
    // Don't use lseek64.
    // We handle 64bit offset With -D _FILE_OFFSET_BITS=64 and lseek.
    // See http://www.linux.or.jp/JM/html/LDP_man-pages/man7/feature_test_macros.7.html
    int w = SEEK_SET;
    switch (whence) {
    case Begin:
        w = SEEK_SET;
        break;
    case Current:
        w = SEEK_CUR;
        break;
    case End:
        w = SEEK_END;
        break;
    }
    return lseek(desc_, offset, w);
#endif
}

#ifdef _WIN32
    #define F_OK 0
    #define W_OK 2
    #define R_OK 4
#endif

bool File::isExist(const ucs4string& path)
{
    return access((char*)utf32toUtf8(path)->data(), F_OK) == 0;
}

bool File::isWritable(const ucs4string& path)
{
    return access((char*)utf32toUtf8(path)->data(), W_OK | R_OK) == 0;
}
bool File::isReadable(const ucs4string& path)
{
    return access((char*)utf32toUtf8(path)->data(), R_OK) == 0;
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
        std::string chop(path);
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

ucs4string scheme::stringError(int num)
{
    const char* text = strerror(num);
    return ucs4string::from_c_str(text, strlen(text));
    // use _wcserror_s on Windows ?
}

ucs4char* scheme::getEnv(const ucs4string& key)
{
    const char* value = getenv((char*)utf32toUtf8(key)->data());
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

Transcoder* scheme::nativeConsoleTranscoder()
{
#ifdef _WIN32
    return new Transcoder(new UTF16Codec(UTF16Codec::UTF_16LE), Transcoder::nativeEolStyle(), ErrorHandlingMode(IGNORE_ERROR));
#else
    return new Transcoder(new UTF8Codec(), Transcoder::nativeEolStyle(), ErrorHandlingMode(IGNORE_ERROR));
#endif
}

// Default Reading/Writing encoding is UTF8.(compatible with ASCII)
Transcoder* scheme::nativeTranscoder()
{
    return new Transcoder(new UTF8Codec(), Transcoder::nativeEolStyle(), ErrorHandlingMode(IGNORE_ERROR));
}

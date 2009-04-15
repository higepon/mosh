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

#ifdef _WIN32
    #include <stdlib.h>
    #include <io.h>
    #include <direct.h>
    #include <process.h>
    #define PATH_MAX _MAX_PATH
    #define dup2 _dup2
#endif


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
ucs4string utf16ToUtf32(const std::wstring& s)
{
    // use UTF16Codec
    struct local {
        static inline bool isLead(ucs4char c) { return (c & 0xfffffc00) == 0xd800; }
        static inline bool isTrail(ucs4char c) { return (c & 0xfffffc00) == 0xdc00; }
    };
    size_t i = 0, n = s.size();
    ucs4string out;
    while (i < n) {
        ucs4char c0 = s[i++];
        if (local::isLead(c0)) {
            ucs4char c1;
            if (i < n && local::isTrail((c1 = s[i]))) {
                i++;
                const ucs4char offset = (0xd800 << 10UL) + 0xdc00 - 0x10000;
                c0 = (c0 << 10) + c1 - offset;
            } else {
                return ucs4string::from_c_str("bad char", 8);
            }
        }
        out.push_back(c0);
    }
    return out;
}
#endif // _WIN32

#ifdef _WIN32
static ucs4string getLastErrorMessageInternal(DWORD e)
{
    const int msgSize = 128;
    wchar_t msg[msgSize];
    int size = FormatMessageW(
        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        0,
        e,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        msg,
        msgSize,
        NULL
    );
    // remove last "\r\n"
    if (size > 2 && msg[size - 2] == '\r') {
        msg[size - 2] = 0;
        size -= 2;
    }
    return utf16ToUtf32(msg);
}
#else
static ucs4string getLastErrorMessageInternal(int e)
{
    const char* message = strerror(e);
    return ucs4string::from_c_str(message, strlen(message));
}
#endif

#ifdef _WIN32
File::File(HANDLE desc /* = INVALID_HANDLE_VALUE */)
    : desc_(desc)
    , prevC_(-1)
    , lastError_(0)
    {}
#else
File::File(int desc /* = -1 */)
    : desc_(desc)
    , lastError_(0)
    {}
#endif


bool File::open(const ucs4string& file, int flags)
{
#ifdef _WIN32

    if (isOpen()) {
        return false;
    }
    DWORD access = 0, disposition = 0;
    DWORD share = FILE_SHARE_READ | FILE_SHARE_WRITE;
    switch (flags) {
    case Read | Write | Create:
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = OPEN_ALWAYS;
        break;
    case Read | Write | Create | Truncate:
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = CREATE_ALWAYS;
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
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = CREATE_ALWAYS;
        break;
    default:
        MOSH_ASSERT(0);
    }
    desc_ = CreateFile(utf32ToUtf16(file), access, share, NULL, disposition, FILE_ATTRIBUTE_NORMAL, NULL);
    lastError_ = GetLastError();
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
    lastError_ = errno;
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
        lastError_ = GetLastError();
        desc_ = INVALID_HANDLE_VALUE;
        return isOK;
    }
#else
    if (isOpen()) {
        const bool isOK = ::close(desc_) != 0;
        lastError_ = errno;
        desc_ = -1;
        return isOK;
    }
#endif
    return false;
}

ucs4string File::getLastErrorMessage() const
{
    return getLastErrorMessageInternal(lastError_);
}

int64_t File::size()
{
#ifdef _WIN32
    LARGE_INTEGER size;
    int isOK = GetFileSizeEx(desc_, &size);
    lastError_ = GetLastError();
    if (isOK) {
        return size.QuadPart;
    } else {
        return -1;
    }
#else
    struct stat st;
    const int result = fstat(desc_, &st);
    lastError_ = errno;
    if (result != 0) {
        return -1;
    } else {
        return st.st_size;
    }
#endif
}

// N.B. This funcion can raise I/O error, caller should handle it.
int64_t File::write(uint8_t* buf, int64_t _size)
{
    MOSH_ASSERT(isInSize_t(_size)); // loop is better if !isInSize_t(_size) on 32-bit
    const size_t size = static_cast<size_t>(_size);
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
    lastError_ = GetLastError();
    if (isOK) {
        return writeSize;
    } else {
        return -1;
    }
#else
    MOSH_ASSERT(isOpen());
    for (;;) {
        const int result = ::write(desc_, buf, size);
        lastError_ = errno;
        if (result < 0 && errno == EINTR) {
            // write again
            errno = 0;
        } else {
            if (result < 0) {
                throwIOError2(IOError::WRITE, getLastErrorMessage());
                return result;
            } else {
                return result;
            }
        }
    }
    return 0;
#endif
}

int64_t File::read(uint8_t* buf, int64_t _size)
{
    MOSH_ASSERT(isInSize_t(_size)); // loop is better if !isInSize_t(_size) on 32-bit
    const size_t size = static_cast<size_t>(_size);
#ifdef _WIN32
    DWORD readSize;
    int isOK;
    if (desc_ == STANDARD_IN) {
#if 1
        MOSH_ASSERT(size == 1); // temporary restriction
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
    lastError_ = GetLastError();
    if (isOK) {
        return readSize;
    } else {
        return -1;
    }
#else
    MOSH_ASSERT(isOpen());
    for (;;) {
        const int result = ::read(desc_, buf, size);
        lastError_ = errno;
        if (result < 0 && errno == EINTR) {
            // read again
            errno = 0;
        } else {
            if (result < 0) {
                throwIOError2(IOError::READ, getLastErrorMessage());
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
    lastError_ = GetLastError();
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
    const int64_t ret = lseek(desc_, offset, w);
    lastError_ = errno;
    return ret;
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

bool File::isLastErrorAcessError() const
{
    // TODO: Windows
#ifdef _WIN32
    return false;
#else
    return lastError_ == EACCES;
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


ucs4string scheme::getLastErrorMessage()
{
#ifdef _WIN32
    return getLastErrorMessageInternal(GetLastError());
#else
    return getLastErrorMessageInternal(errno);
#endif
}

Object scheme::getCurrentDirectory()
{
// TODO Windows
    char buf[PATH_MAX];
    if (getcwd(buf, PATH_MAX) == NULL) {
        return Object::False;
    } else {
        return Object::makeString(buf);
   }
}

bool scheme::setCurrentDirectory(const ucs4string& dir)
{
    return (-1 != chdir((char*)utf32toUtf8(dir)->data()));
}


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
#include <dirent.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> // necesary for os-constant procedure.
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
#include "Object-inl.h"
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
#include "EqHashTable.h"
#include "Symbol.h"
#include "Bignum.h"
#include "PortProcedures.h"

#ifdef _WIN32
    #include <stdlib.h>
    #include <io.h>
    #include <direct.h>
    #include <process.h>
    #include <shellapi.h>
	#include <winsock2.h> // for OSConstants
	#include <ws2tcpip.h> // for OSConstants

	#define SHUT_RD SD_RECEIVE
	#define SHUT_WR SD_SEND
    #define SHUT_RDWR SD_BOTH
    #define PATH_MAX _MAX_PATH
    #define dup2 _dup2
    #ifdef _MSC_VER
#define IPPROTO_UDP 17
#define IPPROTO_TCP 6
#define IPPROTO_RAW 255
        #pragma comment(lib, "shell32.lib")
    #endif
#endif

using namespace scheme;
//
// N.B Dont't forget to add tests to OScompatTest.cpp.
//

static EqHashTable* osConstants = NULL;

void scheme::initOSConstants()
{
    osConstants = new EqHashTable;
#include "OSConstants.h"
}

Object scheme::getOSConstant(Object key, bool& found)
{
    static const Object NOT_FOUND = Symbol::intern(UC("*not-found*"));
    MOSH_ASSERT(osConstants);
    const Object value = osConstants->ref(key, NOT_FOUND);
    if (value == NOT_FOUND) {
        found = false;
        return Object::False;
    } else {
        found = true;
        return value;
    }
}

namespace {

#ifdef _WIN32
const wchar_t* utf32ToUtf16(const ucs4string& s)
{
    ByteArrayBinaryOutputPort out;
    UTF16Codec codec(UTF16Codec::UTF_16LE);
    Transcoder tcoder(&codec);
    tcoder.putString(&out, s);
    tcoder.putChar(&out, '\0');
    return (const wchar_t*)out.toByteVector()->data();
}
ucs4string my_utf16ToUtf32(const std::wstring& s)
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
                return ucs4string::from_c_str("bad char");
            }
        }
        out.push_back(c0);
    }
    return out;
}
#endif // _WIN32

#ifdef _WIN32
HANDLE getHandle(int id)
{
    const DWORD tbl[] = { STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE };
    return GetStdHandle(tbl[id]);
}
#else
int getHandle(int id) { return id; }
#endif

} // end of namespace

#ifdef _WIN32
ucs4string scheme::getLastErrorMessageInternal(DWORD e)
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
    return my_utf16ToUtf32(msg);
}
#else
ucs4string scheme::getLastErrorMessageInternal(int e)
{
    const char* message = strerror(e);
    return ucs4string::from_c_str(message);
}
#endif


File File::STANDARD_IN(getHandle(0));
File File::STANDARD_OUT(getHandle(1));
File File::STANDARD_ERR(getHandle(2));

bool File::isUTF16Console() const
{
#ifdef _WIN32
    return GetFileType(desc_) == FILE_TYPE_CHAR;
#else
    return false;
#endif
}

bool File::open(const ucs4string& file, int flags)
{
    if (isOpen()) {
        return false;
    }
#ifdef _WIN32
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
#else
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
#endif
    setLastError();
    return isOpen();
}

bool File::dup(File& target)
{
#ifdef _WIN32
    MOSH_ASSERT(isOpen());
    // TODO windows
    return false;
#else
    MOSH_ASSERT(isOpen());
    return dup2(desc_, target.desc_) != -1;
#endif
}

bool File::close()
{
    if (isOpen()) {
#ifdef _WIN32
        const bool isOK = CloseHandle(desc_) != 0;
#else
        const bool isOK = ::close(desc_) != 0;
#endif
        setLastError();
        desc_ = INVALID_HANDLE_VALUE;
        return isOK;
    }
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
    setLastError();
    if (isOK) {
        return size.QuadPart;
    } else {
        return -1;
    }
#else
    struct stat st;
    const int result = fstat(desc_, &st);
    setLastError();
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
    if (isUTF16Console()) {
        isOK = WriteConsole(desc_, buf, size / 2, &writeSize, NULL);
        writeSize *= 2;
    } else {
        isOK = WriteFile(desc_, buf, size, &writeSize, NULL);
    }
    setLastError();
    if (isOK) {
        return writeSize;
    } else {
        return -1;
    }
#else
    MOSH_ASSERT(isOpen());
    int64_t result;
    do {
        result = ::write(desc_, buf, size);
    } while (result < 0 && errno == EINTR);
    setLastError();
    if (result < 0) {
        throwIOError2(IOError::WRITE, getLastErrorMessage());
    }
    return result;
#endif
}

int64_t File::read(uint8_t* buf, int64_t _size)
{
    MOSH_ASSERT(isInSize_t(_size)); // loop is better if !isInSize_t(_size) on 32-bit
    const size_t size = static_cast<size_t>(_size);
#ifdef _WIN32
    DWORD readSize;
    int isOK;
    if (isUTF16Console()) {
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
    setLastError();
    if (isOK) {
        return readSize;
    } else {
        return -1;
    }
#else
    MOSH_ASSERT(isOpen());
    int64_t result;
    do {
        result = ::read(desc_, buf, size);
    } while (result < 0 && errno == EINTR);
    setLastError();
    if (result < 0) {
        throwIOError2(IOError::READ, getLastErrorMessage());
    }
    return result;
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
    setLastError();
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
    setLastError();
    return ret;
#endif
}

#ifdef _WIN32
    #define F_OK 0
    #define W_OK 2
    #define R_OK 4
#endif

namespace {

bool wrapped_access(const ucs4string& path, int mode)
{
#ifdef _WIN32
    return _waccess(utf32ToUtf16(path), mode) == 0;
#else
    return access((char*)utf32toUtf8(path)->data(), mode) == 0;
#endif
}

}

bool File::isExist(const ucs4string& path)
{
    return wrapped_access(path, F_OK);
}

bool File::isWritable(const ucs4string& path)
{
    return wrapped_access(path, W_OK | F_OK);
}
bool File::isReadable(const ucs4string& path)
{
    return wrapped_access(path, R_OK);
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
#ifdef _WIN32
    wchar_t **argvw = CommandLineToArgvW(GetCommandLineW(), &argc);
    ucs4char** argvU = new(GC) ucs4char*[argc + 1];
    argvU[argc] = NULL;
    for (int i = 0; i < argc; i++) {
        argvU[i] = my_utf16ToUtf32(argvw[i]).strdup();
    }
    LocalFree(argvw);
    return argvU;
#else
    ucs4char** argvU = new(GC) ucs4char*[argc + 1];
    argvU[argc] = NULL;
    for (int i = 0; i < argc; i++) {
        argvU[i] = utf8ToUtf32(argv[i], strlen(argv[i])).strdup();
    }
    return argvU;
#endif
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
            return ucs4string::from_c_str(v);
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
        const char* ret = p.substr(0, pos + 1).c_str();
        return ucs4string::from_c_str(ret);
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
            return ucs4string::from_c_str(execPath);
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
#ifdef _WIN32
    const int valueSize = 1024;
    wchar_t value[valueSize];
    int size = GetEnvironmentVariableW(utf32ToUtf16(key), value, valueSize);
    if (size == 0 || size > valueSize) {
        return NULL;
    }
    return my_utf16ToUtf32(value).strdup();
#else
    const char* value = getenv((char*)utf32toUtf8(key)->data());
    if (NULL == value) {
        return NULL;
    }
    return utf8ToUtf32(value, strlen(value)).strdup();
#endif
}

#ifdef _WIN32
#include <stdlib.h>
#define environ _environ
#else
extern char** environ;
#endif

Object scheme::getEnvAlist()
{
#ifdef _WIN32
    Object ret = Object::Nil;
    const wchar_t *env = GetEnvironmentStringsW();
    for (;;) {
        const wchar_t *p = wcschr(env + (*env == L'=' ? 1 : 0), L'=');
        if (p) {
            ucs4string key = my_utf16ToUtf32(std::wstring(env, p));
            size_t len = wcslen(p + 1);
            ucs4string value = my_utf16ToUtf32(std::wstring(p + 1, len));
            env = p + 1 + len + 1;
            ret = Object::cons(Object::cons(Object::makeString(key),
                                            Object::makeString(value)),
                               ret);
        } else {
            MOSH_ASSERT(0);
            break;
        }
        if (*env == 0) break;
    }
    return ret;
#else
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
#endif
}

Object scheme::readDirectory(const ucs4string& path)
{
#ifdef _WIN32
    WIN32_FIND_DATA data;
    HANDLE hdl;

    Object ret = Object::Nil;
    ucs4string path2(path);
    const ucs4char suf[] = { '\\', '*', 0 };
    path2.append(suf);
    hdl = FindFirstFileW(utf32ToUtf16(path2), &data);
    if (hdl != INVALID_HANDLE_VALUE) {
        do {
            ret = Object::cons(Object::makeString(my_utf16ToUtf32(data.cFileName)), ret);
        } while (FindNextFileW(hdl, &data));
        FindClose(hdl);
    }
    return ret;
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
    closedir(dir);
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
#ifdef _WIN32
    wchar_t buf[PATH_MAX];
    int size = GetCurrentDirectoryW(PATH_MAX, buf);
    if (size > 0) {
        return Object::makeString(my_utf16ToUtf32(buf));
    } else {
        return Object::False;
    }
#else
    char buf[PATH_MAX];
    if (getcwd(buf, PATH_MAX) == NULL) {
        return Object::False;
    } else {
        return Object::makeString(buf);
    }
#endif
}

bool scheme::setCurrentDirectory(const ucs4string& dir)
{
#ifdef _WIN32
    return SetCurrentDirectoryW(utf32ToUtf16(dir)) != 0;
#else
    return (-1 != chdir((char*)utf32toUtf8(dir)->data()));
#endif
}


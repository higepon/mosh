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
#include <dirent.h>
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
#include "PortProcedures.h"

using namespace scheme;
//
// N.B Dont't forget to add tests to OScompatTest.cpp.
//

#ifndef HAVE_INT_OPTRESET
int         optreset;
#endif

#define BADCH   '?'
#define BADARG  ':'
#define EMSG    (ucs4char*)(UC(""))

int  opterr;
int  optind = 1;
int  optopt;
ucs4char *optarg4;

static size_t strcspn_utf32(const ucs4char* str1, const ucs4char* str2) {

    const ucs4char* const head = str1;
    const ucs4char* t;
    for (; *str1; str1++) {

        for (t = str2; *t; t++) if (*t == *str1) return str1 - head;
    }
    return str1 - head;
}

static int strncmp_utf32(const ucs4char *s1, const ucs4char *s2, size_t n)
{
    for(size_t i = 0; i < n ; i++ ) {
        if( s1[i] > s2[i] ) {
            return 1;
        }
        if( s1[i] < s2[i] ) {
            return -1;
        }
        if( s1[i] == s2[i] ) {
            continue;
        }
        if( s1[i] == '\0' || s2[i] == '\0' ) {
            break;
        }
    }
    return 0;
}

static size_t strlen_utf32(const ucs4char *s)
{
    int c;
    while(*s++)
        c++;
    return c;
}

// strchr from OpenBSD
static ucs4char* strchr_utf32(const ucs4char *p, int ch)
{
    for (;; ++p) {
        if (*p == ch)
            return((ucs4char *)p);
        if (!*p)
            return((ucs4char *)NULL);
    }
    /* NOTREACHED */
}

// static ucs4char* strchr_utf32(const ucs4char *s, ucs4char c)
// {
//     while( *s++ != '\0' ) {
//         printf("<%c = %c>", *s, c);
//         if( *s == c )
//             return (ucs4char*)s;
//     }
//     return NULL;
// }

// Ported from pgsql/src/port/getopt_long.c
int scheme::getopt_long_utf32(int argc, ucs4char *const argv[],
            const ucs4char *optstring,
            const struct option_utf32 * longopts, int *longindex)
{
    static ucs4char *place = EMSG;  /* option letter processing */
    ucs4char       *oli;            /* option letter list index */

    if (optreset || !*place)
    {                           /* update scanning pointer */
        optreset = 0;

        if (optind >= argc)
        {
            place = EMSG;
            return -1;
        }

        place = argv[optind];

        if (place[0] != '-')
        {
            place = EMSG;
            return -1;
        }

        place++;

        if (place[0] && place[0] == '-' && place[1] == '\0')
        {                       /* found "--" */
            ++optind;
            place = EMSG;
            return -1;
        }

        if (place[0] && place[0] == '-' && place[1])
        {
            /* long option */
            size_t      namelen;
            int         i;

            place++;

            namelen = strcspn_utf32(place, UC("="));

            for (i = 0; longopts[i].name != NULL; i++)
            {
                if (strlen_utf32(longopts[i].name) == namelen
                    && strncmp_utf32(place, longopts[i].name, namelen) == 0)
                {
                    if (longopts[i].has_arg)
                    {
                        if (place[namelen] == '=')
                            optarg4 = place + namelen + 1;
                        else if (optind < argc - 1)
                        {
                            optind++;
                            optarg4 = argv[optind];
                        }
                        else
                        {
                            if (optstring[0] == ':')
                                return BADARG;
                            if (opterr)
                                fprintf(stderr,
                                   "%s: option requires an argument -- %s\n",
                                        argv[0], place);
                            place = EMSG;
                            optind++;
                            return BADCH;
                        }
                    }
                    else
                    {
                        optarg4 = NULL;
                        if (place[namelen] != 0)
                        {
                            /* XXX error? */
                        }
                    }

                    optind++;

                    if (longindex)
                        *longindex = i;

                    place = EMSG;

                    if (longopts[i].flag == NULL)
                        return longopts[i].val;
                    else
                    {
                        *longopts[i].flag = longopts[i].val;
                        return 0;
                    }
                }
            }

            if (opterr && optstring[0] != ':')
                fprintf(stderr,
                        "%s: illegal option -- %s\n", argv[0], place);
            place = EMSG;
            optind++;
            return BADCH;
        }
    }

    /* short option */
    optopt = (int) *place++;

    oli = strchr_utf32(optstring, optopt);
    for (int i = 0; ;i++) {
        if (optstring[i] == '\0') {
            break;
        }
        printf("<%c>", optstring[i]);
    }
    printf("optopt = %c", optopt);
    printf("oli=%d\n", oli);
    if (!oli)
    {
        if (!*place)
            ++optind;
        if (opterr && *optstring != ':') {
            fprintf(stderr,
                    "%s: illegal option -- %c\n", argv[0], optopt);
        }
        return BADCH;
    }

    if (oli[1] != ':')
    {                           /* don't need argument */
        optarg4 = NULL;
        if (!*place)
            ++optind;
    }
    else
    {                           /* need an argument */
        if (*place)             /* no white space */
            optarg4 = place;
        else if (argc <= ++optind)
        {                       /* no arg */
            place = EMSG;
            if (*optstring == ':')
                return BADARG;
            if (opterr)
                fprintf(stderr,
                        "%s: option requires an argument -- %c\n",
                        argv[0], optopt);
            return BADCH;
        }
        else
            /* white space */
            optarg4 = argv[optind];
        place = EMSG;
        ++optind;
    }
    return optopt;
}


ucs4string scheme::getMoshExecutablePath(bool& isErrorOccured)
{
#if defined(_WIN32)
    TCHAR tmp[MAX_PATH]; /* may be Unicoded */
    if (GetModuleFileNameW(NULL,tmp,MAX_PATH)) {
        TCHAR* trm = _tcsinc(_tcsrchr(tmp,_T('\\')));
        *trm = _T('\0');
        ByteArrayBinaryInputPort name((uint8_t *)tmp,_tcslen(tmp)*sizeof(TCHAR));
        UTF16Codec codec(UTF16Codec::UTF_16LE);
        Transcoder tcoder(&codec);
        return tcoder.getString(&name);
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
            return ucs4string::from_c_str(chop.substr(0, pos + 1).c_str());
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

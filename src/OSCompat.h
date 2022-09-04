/*
 * OSCompat.h -
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
 *  $Id: OScompat.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_OSCOMPAT_
#define SCHEME_OSCOMPAT_

#ifdef _WIN32
//#define UNICODE
//#define _UNICODE
#include <stdio.h>
#include <windows.h>
#include <shlwapi.h>
#include <tchar.h>
#include <sys/types.h>
#ifdef _MSC_VER
#pragma comment(lib, "shlwapi.lib")
#endif

typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;

#else // NOT Windows
#include <sys/resource.h>
#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif // _LARGEFILE64_SOURCE
#include <sys/types.h>
#include <unistd.h>
#endif
#include "scheme.h"

namespace scheme {

    ucs4char** getCommandLine(int argc, char* argv[]);
    ucs4char* getEnv(const ucs4string& key);
    void setEnv(const ucs4string& key, const ucs4string& value);
    Object getEnvAlist();
    Object readDirectory(const ucs4string& dir);
    bool createDirectory(const ucs4string& path);
    bool isDirectory(const ucs4string& path);
    Object getCurrentDirectory();
    bool setCurrentDirectory(const ucs4string& dir);
    ucs4string getLastErrorMessage();
    ucs4string getMoshExecutablePath(bool& isErrorOccured);
    Transcoder* createNativeConsoleTranscoder();
    Transcoder* createNativeTranscoder();

#ifdef _WIN32
    ucs4string getLastErrorMessageInternal(DWORD e);
#else
    ucs4string getLastErrorMessageInternal(int e);
#endif

    void initOSConstants();
    Object getOSConstant(Object key, bool& found);

    Object processExitValue(int statVal);
    Object processTerminationSignal(int statVal);

    class File EXTEND_GC
    {
#ifdef _WIN32
        typedef HANDLE Handle;
        typedef DWORD ErrorCode;
#else
        typedef int Handle;
        typedef int ErrorCode;
        enum {
            INVALID_HANDLE_VALUE = -1
        };
#endif
        void setLastError()
        {
#ifdef _WIN32
            lastError_ = GetLastError();
#else
            lastError_ = errno;
#endif
        }

        void setLastError(int e)
        {
            lastError_ = e;
        }

    public:
        void operator=(const File&) = delete;
        File(const File& rhs) = delete;

        enum Mode {
            Read            = 0x00000001,
            Write           = 0x00000002,
            Create          = 0x00000010,
            Truncate        = 0x00000020,
            FORBIT_EXTRA_COMMA
        };
        enum Whence {
            Begin,
            Current,
            End
        };
        File(Handle desc = INVALID_HANDLE_VALUE)
            : desc_(desc)
#ifdef _WIN32
            , prevC_(-1)
#endif
        {
        }
        bool open(const ucs4string& file, int flags);

        virtual ~File()
        {
            close();
        }

        bool isOpen() const
        {
#ifdef MONA
            return desc_ > 0;
#else
            return desc_ != INVALID_HANDLE_VALUE;
#endif
        }
        bool close();
        bool isUTF16Console() const;
        int64_t write(uint8_t* buf, int64_t size);
        int64_t read(uint8_t* buf, int64_t size);
        int64_t seek(int64_t offset, Whence whece = Begin);
        bool dup(File& target);
        int64_t size();

        ucs4string getLastErrorMessage() const;
        bool isLastErrorAcessError() const;

        static Object size(const ucs4string& path);
        static bool isExist(const ucs4string& path);
        static bool isWritable(const ucs4string& path);
        static bool isReadable(const ucs4string& path);
        static bool isExecutable(const ucs4string& path);
        static bool isRegular(const ucs4string& path);
        static bool isSymbolicLink(const ucs4string& path);
        static bool rename(const ucs4string& oldPath, const ucs4string& newPath);
        static bool createSymbolicLink(const ucs4string& oldPath, const ucs4string& newPath);
        static bool deleteFileOrDirectory(const ucs4string& path);
        static Object modifyTime(const ucs4string& path);
        static Object accessTime(const ucs4string& path);
        static Object changeTime(const ucs4string& path);
        static File STANDARD_IN;
        static File STANDARD_OUT;
        static File STANDARD_ERR;

    private:
        Handle desc_;
        ErrorCode lastError_{0};
#ifdef _WIN32
        int prevC_; // to keep half of wchar_t
#endif
    };
}; // namespace scheme

#endif // SCHEME_OSCOMPAT_

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
#pragma comment(lib, "shlwapi.lib")

typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;

#else // NOT Windows
#include <sys/resource.h>
#define _LARGEFILE64_SOURCE
#include <sys/types.h>
#include <unistd.h>
#endif
#include "scheme.h"

namespace scheme {

    ucs4char** getCommandLine(int argc, char* argv[]);
    ucs4char* getEnv(const ucs4string& key);
    Object getEnvAlist();
    Object readDirectory(const ucs4string& dir);
    ucs4string stringError(int num);
    ucs4string getMoshExecutablePath(bool& isErrorOccured);
    Transcoder* nativeConsoleTranscoder();
    Transcoder* nativeTranscoder();

    class File EXTEND_GC
    {
    public:
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
#ifdef _WIN32
        File(HANDLE desc = INVALID_HANDLE_VALUE);
#else
        File(int desc = -1);
#endif
        bool open(const ucs4string& file, int flags);

        virtual ~File();


        bool isOpen() const;
        bool close();
        int64_t write(uint8_t* buf, int64_t size);
        int64_t read(uint8_t* buf, int64_t size);
        int64_t seek(int64_t offset, Whence whece = Begin);
#ifdef _WIN32
        int dup(HANDLE target);
#else
        int dup(int target);
#endif
        int64_t size();

        ucs4string getLastErrorMessage() const;

        static bool isExist(const ucs4string& path);
        static bool isWritable(const ucs4string& path);
        static bool isReadable(const ucs4string& path);

#ifdef _WIN32
        static const HANDLE STANDARD_IN;
        static const HANDLE STANDARD_OUT;
        static const HANDLE STANDARD_ERR;
#else
        static const int STANDARD_IN;
        static const int STANDARD_OUT;
        static const int STANDARD_ERR;
#endif

    private:
#ifdef _WIN32
        HANDLE desc_;
        int prevC_; // to keep half of wchar_t
        DWORD lastError_;
#else
        int desc_;
        int lastError_;
#endif
    };
}; // namespace scheme

#endif // SCHEME_OSCOMPAT_

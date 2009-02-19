/*
 * main.cpp - Interpreter main.
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

#include <time.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <getopt.h>

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "scheme.h"
#include "Vector.h"
#include "VM.h"
#include "Closure.h"
#include "Symbol.h"
#include "EqHashTable.h"
#include "VM-inl.h"
#include "ErrorProcedures.h"
#include "BinaryOutputPort.h"
#include "TextualOutputPort.h"
#include "StandardOutputPort.h"
#include "StandardInputPort.h"
#include "TextualInputPort.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "SString.h"
#include "Symbol.h"
#include "EqHashTable.h"
#include "Record.h"
#include "Equivalent.h"
#include "Ratnum.h"
#include "Flonum.h"

using namespace scheme;

static VM* theVM;

Object argsToList(int argc, int optind, char* argv[])
{
    Object p = Object::Nil;
    for (int i = optind; i < argc; i++) {
        p = Object::cons(Object::makeString(argv[i]), p);
    }
    return Pair::reverse(p);
}

void showVersion()
{
    printf("Mosh R6RS scheme interpreter, version %s \n", PACKAGE_VERSION);
    exit(0);
}

void showUsage()
{
    fprintf(stderr,
            "Usage: mosh <options> [file]\n"
            "options:\n"
            "  -5                Run with safe mode (Almost R5RS).\n"
            "  -V                Prints version and exits.\n"
            "  -v                Prints version and exits.\n"
            "  -h                Prints this help.\n"
#ifdef ENABLE_PROFILER
            "  -p                Executes with profiler.\n"
#endif
            "  -t                Executes test.\n"
            "  --help            Prints this help.\n"
            "  --loadpath=<path> Add libary loadpath.\n\n"
            " MOSH_LOADPATH\n"
            "  You can add library loadpath by using environment variable MOSH_LOADPATH, with \':\' separated paths.\n\n"
            "bug report:\n"
            "  http://code.google.com/p/mosh-scheme/\n"
            "  higepon@users.sourceforge.jp\n\n"
        );
    exit(EXIT_FAILURE);
}

#ifdef ENABLE_PROFILER
void signal_handler(int signo)
{
    theVM->collectProfile();
}
#endif

int main(int argc, char *argv[])
{
    int opt;
    int optionIndex = 0;
    bool isTestOption    = false;
    bool isCompileString = false;
    bool isProfiler      = false;
    bool isR6RSBatchMode = true;
    bool isDebugExpand   = false; // show the result of psyntax expansion.
    char* initFile = NULL;
    char* loadPath = NULL;

   static struct option long_options[] = {
       {"loadpath", optional_argument, 0, 'L'},
       {"help", 0, 0, 'h'},
       {0, 0, 0, 0}
   };

    while ((opt = getopt_long(argc, argv, "htvpVcl:5rze", long_options, &optionIndex)) != -1) {
        switch (opt) {
        case 'h':
            showUsage();
            break;
        case 'l':
            initFile = optarg;
            break;
        case 'L':
            loadPath = optarg;
            break;
        case 'b':
            isR6RSBatchMode = true;
            break;
        case 'v':
            showVersion();
            break;
        case 'V':
            showVersion();
            break;
        case 't':
            isTestOption = true;
            break;
        case 'p':
            isProfiler = true;
            break;
        case 'c':
            isCompileString = true;
            break;
        case 'e':
            isDebugExpand = true;
            break;
        case '5':
            isR6RSBatchMode = false;
            break;
        default:
            fprintf(stderr, "invalid option %c", opt);
            showUsage();
            exit(EXIT_FAILURE);
        }
    }

    if (isProfiler && argc == optind) {
        fprintf(stderr, "[file] not specified\n");
        showUsage();
        exit(EXIT_FAILURE);
    }

    mosh_init();

    // for Shell mode.
    // VM(=parent) ignores SIGINT, but child use default handler. (See %fork)
    signal(SIGINT, SIG_IGN);

    Transcoder* transcoder = Transcoder::nativeTranscoder();
    const Object inPort    = Object::makeTextualInputPort(new StandardInputPort(), transcoder);
    const Object outPort   = Object::makeTextualOutputPort(new StandardOutputPort(fileno(stdout)), transcoder);
    const Object errorPort = Object::makeTextualOutputPort(new StandardOutputPort(fileno(stderr)), transcoder);

    theVM = new VM(10000, outPort, errorPort, inPort, isProfiler);
    theVM->loadCompiler();
    theVM->setValueString(UC("*command-line-args*"), argsToList(argc, optind, argv));
//     if (initFile != NULL) {
//         theVM->load(Object::makeString(initFile).toString()->data());
//     }

    if (isTestOption) {
        theVM->loadFileWithGuard(UC("all-tests.scm"));
    } else if (isCompileString) {
        const Object port = Object::makeStringInputPort((const uint8_t*)argv[optind], strlen(argv[optind]));
        bool errorOccured = false;
        const Object code = port.toTextualInputPort()->getDatum(errorOccured);
        if (errorOccured) {
            callLexicalViolationImmidiaImmediately(theVM, "read", port.toTextualInputPort()->error());
        } else {
            const Object compiled = theVM->compile(code);
            theVM->currentOutputPort().toTextualOutputPort()->display(compiled);
        }
    } else if (isR6RSBatchMode) {
        if (NULL == loadPath) {
            theVM->setValueString(UC("%loadpath"), Object::False);
        } else {
            theVM->setValueString(UC("%loadpath"), Object::makeString(loadPath));
        }
        theVM->activateR6RSMode(isDebugExpand);
    } else if (optind < argc) {
        theVM->setValueString(UC("debug-expand"), Object::makeBool(isDebugExpand));
        theVM->loadFileWithGuard(Object::makeString(argv[optind]).toString()->data());
    } else {
        showUsage();
    }

#ifdef ENABLE_PROFILER
    if (isProfiler) {
        const Object result = theVM->getProfileResult();
        theVM->callClosureByName(Symbol::intern(UC("show-profile")), result);
    }
#endif
    exit(EXIT_SUCCESS);
}

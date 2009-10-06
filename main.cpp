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
 *  $Id: main.cpp 2013 2009-08-11 03:46:06Z higepon $
 */

#include <time.h>
#include <signal.h>
#ifdef _WIN32
#else
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "scheme.h"
#include "VM.h"
#include "SString.h"
#include "Symbol.h"
#include "Record.h"
#include "getoptU.h"
#include "OSCompat.h"
#include "OSCompatThread.h"
#include "MultiVMProcedures.h"
#include "VMFactory.h"
#include "Gloc.h"
#include "Closure.h"
#include "VM-inl.h"

bool debug_on;
using namespace scheme;

static VM* theVM;



Object argsToList(int argc, int optind, ucs4char** argvU)
{
    Object p = Object::Nil;
    for (int i = optind; i < argc; i++) {
        p = Object::cons(Object::makeString(argvU[i]), p);
    }
    return Pair::reverse(p);
}

void showVersion()
{
    ucs4string revision("$Revision$");
//     gc_vector<ucs4string> v;
//     revision.split(':', v);
    // todo
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
//            "  -t                Executes test.\n"
            "  --help            Prints this help.\n"
            "  --disable-acc     disables auto-compile-cache.\n"
            "  --clean-acc       cleans auto-compile-cache.\n"
            "  --loadpath=<path> Add library loadpath.\n\n"
            " MOSH_LOADPATH\n"
            "  You can add library loadpath by using environment variable MOSH_LOADPATH, with \':\'(use \';\' for Windows) separated paths.\n\n"
            "bug report:\n"
            "  http://code.google.com/p/mosh-scheme/issues\n"
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

#ifndef _WIN32
#include <sys/types.h>
#include <sys/wait.h>
#endif

int main(int argc, char *argv[])
{
    // call this before any allocation.
    mosh_init();

    ucs4char opt;
    int optionIndex = 0;
    bool isTestOption    = false;
    bool isCompileString = false;
    bool isProfilerOn      = false;
    bool isR6RSBatchMode = true;
    bool disableAcc = false;
    bool cleanAcc = false;
    bool isDebugExpand   = false; // show the result of psyntax expansion.
    ucs4char* initFile = NULL;
    ucs4char* loadPath = NULL;

   static struct optionU long_options[] = {
       {UC("loadpath"), optional_argument, 0, 'L'},
       {UC("help"), 0, 0, 'h'},
       {UC("disable-acc"), 0, 0, 'd'},
       {UC("clean-acc"), 0, 0, 'C'},
       {0, 0, 0, 0}
   };

   ucs4char** argvU = getCommandLine(argc, argv);

   while ((opt = getopt_longU(argc, argvU, UC("htvpVcl:5rze"), long_options, &optionIndex)) != -1) {
        switch (opt) {
        case 'h':
            showUsage();
            break;
        case 'd':
            disableAcc = true;
            break;
        case 'l':
            initFile = optargU;
            break;
        case 'L':
            loadPath = optargU;
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
            isProfilerOn = true;
            break;
        case 'c':
            isCompileString = true;
            break;
        case 'C':
            cleanAcc = true;
            disableAcc = true;
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

    if (isProfilerOn && argc == optindU) {
        fprintf(stderr, "[file] not specified\n");
        showUsage();
        exit(EXIT_FAILURE);
    }


    // for Shell mode.
    // VM(=parent) ignores SIGINT, but child use default handler. (See %fork)
//    signal(SIGINT, SIG_IGN);

#ifndef _WIN32
    signal(SIGPIPE, SIG_IGN);
#endif

    VMFactory factory;
    const int INITIAL_STACK_SIZE = 10000;

    // N.B.
    // We store the VM instance in thread specific storage.
    // Used for storing yylex and re2c which has only global interfaces.
    theVM = factory.create(INITIAL_STACK_SIZE, isProfilerOn);

    if (!setCurrentVM(theVM)) {
        fprintf(stderr, "fatal vm specific failure\n");
        exit(-1);
    }

    theVM->setValueString(UC("*command-line-args*"), argsToList(argc, optindU, argvU));

    if (isTestOption) {
        theVM->loadFileWithGuard(UC("all-tests.scm"));
//     } else if (isCompileString) {
//         ucs4string text
//         const Object port = Object::makeStringInputPort((const uint8_t*)argvU[optindU], strlen(argv[optindU]));
//         bool errorOccured = false;
//         const Object code = port.toTextualInputPort()->getDatum(errorOccured);
//         if (errorOccured) {
//             callLexicalViolationImmidiaImmediately(theVM, "read", port.toTextualInputPort()->error());
//         } else {
//             const Object compiled = theVM->compile(code);
//             theVM->currentOutputPort().toTextualOutputPort()->display(compiled);
//         }
    } else if (isR6RSBatchMode) {
        if (NULL == loadPath) {
            theVM->setValueString(UC("%loadpath"), Object::False);
        } else {
            theVM->setValueString(UC("%loadpath"), Object::makeString(loadPath));
        }
        theVM->setValueString(UC("%disable-acc"), Object::makeBool(disableAcc));
        theVM->setValueString(UC("%clean-acc"), Object::makeBool(cleanAcc));
        theVM->activateR6RSMode(isDebugExpand);
    } else if (optindU < argc) {
        theVM->setValueString(UC("debug-expand"), Object::makeBool(isDebugExpand));
        theVM->loadFileWithGuard(Object::makeString(argvU[optindU]).toString()->data());
    } else {
        showUsage();
    }
#ifdef ENABLE_PROFILER
    if (isProfilerOn) {
        const Object result = theVM->getProfileResult();
        theVM->callClosureByName(Symbol::intern(UC("show-profile")), result);
    }
#endif
    theVM->flushAllPorts();

    // N.B.
    // static destructor will be called.
    // this means that static member *can be freed*.
    // Don't rely on static initializer and destructor on multithreads.
    // See Symbol::symbols for more detailed information.
    exit(EXIT_SUCCESS);
}

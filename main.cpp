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
 *  $Id: main.cpp 5323 2008-05-09 09:06:26Z higepon $
 */

#include "scheme.h"
#include "VM.h"

using namespace scheme;

VM* theVM;
#ifdef TRACE_INSN
FILE* errOut;
#endif


Object argsToList(int argc, int optind, char* argv[])
{
    Object p = Object::Nil;
    for (int i = optind; i < argc; i++) {
        p = Object::cons(Object::makeString(argv[i]), p);
    }
    return Pair::reverse(p);
}

// for precompiled code
Object arrayToList(Object* array, int size)
{
    Object p = Object::Nil;
    for (int i = size - 1; i >= 0; i--) {
        p = Object::cons(array[i], p);
    }
    return p;
}

#ifdef DUMP_ALL_INSTRUCTIONS
FILE* stream;
#endif

void showVersion()
{
    printf("Mosh R6RS scheme interpreter, version %s \n", PACKAGE_VERSION);
    exit(0);
}

void showUsage()
{
    fprintf(stderr,
            "Usage: mosh [-vhpV] [file]\n"
            "options:\n"
            "  -l       Loads <file> before executing the script file or entering repl.\n"
            "  -V       Prints version and exits.\n"
            "  -v       Prints version and exits.\n"
            "  -h       Prints this help.\n"
#ifdef ENABLE_PROFILER
            "  -p       Executes with profiler.\n"
#endif
            "  -t       Executes test.\n"
            "bug report:\n"
            "  http://code.google.com/p/mosh-scheme/\n"
            "  higepon@users.sourceforge.jp\n"
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
    bool isTestOption    = false;
    bool isCompileString = false;
    bool isProfiler      = false;
    char* initFile = NULL;

    while ((opt = getopt(argc, argv, "htvpVcl:")) != -1) {
        switch (opt) {
        case 'h':
            showUsage();
            break;
        case 'l':
            initFile = optarg;
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

#ifdef DUMP_ALL_INSTRUCTIONS
    stream = fopen("./instruction.log", "a+");
#endif

#ifdef USE_BOEHM_GC
    GC_INIT();
#endif

    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);
    TextualOutputPort outPort(TextualOutputPort(new FileBinaryOutputPort(stdout), transcoder));
#ifdef TRACE_INSN
    errOut = fopen(INSN_LOG_FILE, "w");
    TextualOutputPort errorPort(TextualOutputPort(new FileBinaryOutputPort(errOut), transcoder));
#else
    TextualOutputPort errorPort(TextualOutputPort(new FileBinaryOutputPort(stderr), transcoder));
#endif
    Object inPort = Object::makeTextualInputPort(new FileBinaryInputPort(stdin), transcoder);;

    theVM = new VM(2000000, outPort, errorPort, inPort, isProfiler);

    // Do not call Symbol::intern before you load precompiled compiler!
    const Object compiler = getCompiler();
    Symbol::initBuitinSymbols();
    theVM->importTopLevel();
    theVM->defineGlobal(Symbol::intern(UC("top level :$:*command-line-args*")), argsToList(argc, optind, argv));

#ifdef ENABLE_PROFILER
    if (isProfiler) {
        theVM->initProfiler();
    }
#endif

    theVM->evaluate(compiler);

    if (initFile != NULL) {
        theVM->load(Object::makeString(initFile).toString()->data());
    }

    if (isTestOption) {
        theVM->load(UC("all-tests.scm"));
    } else if (isCompileString) {
        const Object port = Object::makeStringInputPort((const uint8_t*)argv[optind], strlen(argv[optind]));
        const Object code = port.toTextualInputPort()->getDatum();
        sysDisplayEx(L1(theVM->compile(code)), 0, NULL); // temp
    } else if (optind < argc) {
        theVM->load(Object::makeString(argv[optind]).toString()->data());
    } else {
        theVM->load(UC("repl.scm"));
    }

#ifdef DUMP_ALL_INSTRUCTIONS
    fclose(stream);
#endif

#ifdef ENABLE_PROFILER
    if (isProfiler) {
        const Object result = theVM->getProfileResult();
        theVM->callClosureByName(Symbol::intern(UC("show-profile")), result);
    }
#endif
#ifdef TRACE_INSN
    fclose(errOut);
#endif
    exit(EXIT_SUCCESS);
}

extern "C" void dont_free(void* p)
{
}

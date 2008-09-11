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
#include <sys/time.h>
#include <sys/resource.h>


#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "scheme.h"
#include "VM.h"
#include "ErrorProcedures.h"
#include "BinaryOutputPort.h"
#include "TextualOutputPort.h"
#include "FileBinaryOutputPort.h"
#include "FileBinaryInputPort.h"
#include "TextualInputPort.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "Builtin.h"
#include "SString.h"
#include "Symbol.h"
#include "EqHashTable.h"
#include "Equivalent.h"

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
            "  -b       [experimental] R6RS batch mode. Loads R6RS top-level program file\n"
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

void compareRead(const char* file);

int main(int argc, char *argv[])
{
    int opt;
    bool isTestOption    = false;
    bool isCompileString = false;
    bool isProfiler      = false;
    bool isR6RSBatchMode = false;
    bool isCompareRead   = false;
    char* initFile = NULL;


    while ((opt = getopt(argc, argv, "htvpVcl:br")) != -1) {
        switch (opt) {
        case 'h':
            showUsage();
            break;
        case 'l':
            initFile = optarg;
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
        case 'r':
            isCompareRead = true;
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

#ifdef TRACE_INSN
    errOut = fopen(INSN_LOG_FILE, "w");
    TextualOutputPort errorPort(TextualOutputPort(new FileBinaryOutputPort(errOut), transcoder));
#else

#endif
    Object inPort = Object::makeTextualInputPort(new FileBinaryInputPort(stdin), transcoder);;
    Object outPort = Object::makeTextualOutputPort(new FileBinaryOutputPort(stdout), transcoder);

    Object errorPort = Object::makeTextualOutputPort(new FileBinaryOutputPort(stderr), transcoder);;
    theVM = new VM(2000000, outPort, errorPort, inPort, isProfiler);


    // Do not call Symbol::intern before you load precompiled compiler!
    const Object compiler = getBuiltinCompiler();
    Symbol::initBuitinSymbols();
    theVM->importTopLevel();
    theVM->setTopLevelGlobalValue(Symbol::intern(UC("*command-line-args*")), argsToList(argc, optind, argv));
#ifdef ENABLE_PROFILER
    if (isProfiler) {
        theVM->initProfiler();
    }
#endif

    theVM->evaluate(compiler);
    theVM->evaluate(getBuiltinMatch());

    if (initFile != NULL) {
        theVM->load(Object::makeString(initFile).toString()->data());
    }

    if (isTestOption) {
        theVM->load(UC("all-tests.scm"));
    } else if (isCompileString) {
        const Object port = Object::makeStringInputPort((const uint8_t*)argv[optind], strlen(argv[optind]));
        bool errorOccured = false;
        const Object code = port.toTextualInputPort()->getDatum(errorOccured);
        if (errorOccured) {
            callLexicalViolationImmidiaImmediately("read", port.toTextualInputPort()->error());
        } else {
            const Object compiled = theVM->compile(code);
            theVM->getOutputPort().toTextualOutputPort()->display(compiled);
        }
    } else if (isR6RSBatchMode) {
        theVM->activateR6RSMode();
    } else if (isCompareRead) {
        compareRead(argv[optind]);
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

// compare "old read" and "new read"
void compareRead(const char* file)
{
    Transcoder* transcoder = new Transcoder(new UTF8Codec, Transcoder::LF, Transcoder::IGNORE_ERROR);

    FILE* fp1 = fopen(file, "r");
    FILE* fp2 = fopen(file, "r");
    TextualInputPort* const in1 = Object::makeTextualInputPort(new FileBinaryInputPort(fp1), transcoder).toTextualInputPort();
    TextualInputPort* const in2 = Object::makeTextualInputPort(new FileBinaryInputPort(fp2), transcoder).toTextualInputPort();


    long long a1 = 0;
    long long a2 = 0;

    struct timeval tv1, tv2, tv3;

    TextualOutputPort* const port = theVM->getOutputPort().toTextualOutputPort();
    bool isErrorOccured = false;
    for (;;) {
        gettimeofday(&tv1, NULL);
        const Object o1 = in1->getDatum(isErrorOccured);
        gettimeofday(&tv2, NULL);
        Object o2 = in2->getDatum2(isErrorOccured);
        gettimeofday(&tv3, NULL);
        a1 += (tv2.tv_sec * 1000 * 1000 + tv2.tv_usec) - (tv1.tv_sec * 1000 * 1000 + tv1.tv_usec);
        a2 += (tv3.tv_sec * 1000 * 1000 + tv3.tv_usec) - (tv2.tv_sec * 1000 * 1000 + tv2.tv_usec);

        if (o2.isEof()) {
            break;
        }

//        port->putDatum(o2);

              if (!equal(o1, o2)) {
            printf("======= error ==============================================================\n");
             port->putDatum(o1);
            printf("\n\n");
            port->putDatum(o2);
            break;
        } else {
//              outPort.toTextualOutputPort()->putDatum(o1);
//              outPort.toTextualOutputPort()->putDatum(o2);
//             printf("=====================================================================\n");
//             outPort.toTextualOutputPort()->putDatum(o1);
//             printf("\n");
        }
    }
    printf("%lld:%lld :", a1, a2);


}

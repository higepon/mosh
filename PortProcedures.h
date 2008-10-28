/*
 * PortProcedures.h - <port> procedures.
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
 *  $Id: PortProcedures.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef __SCHEME_PORT_PROCEDURES__
#define __SCHEME_PORT_PROCEDURES__

#include "scheme.h"

namespace scheme {

    bool fileExistsP(const ucs4string& file);
    Object getStringNEx(int argc, const Object* argv);
    Object faslWriteEx(int argc, const Object* argv);
    Object openInputFileEx(int argc, const Object* argv);
    Object closePortEx(int argc, const Object* argv);
    Object getLineEx(int argc, const Object* argv);
    Object currentDirectoryEx(int argc, const Object* argv);
    Object standardLibraryPathEx(int argc, const Object* argv);
    Object standardInputPortEx(int argc, const Object* argv);
    Object bytevectorU8SetDEx(int argc, const Object* argv);
    Object lookaheadCharEx(int argc, const Object* argv);
    Object readEx(int argc, const Object* argv);
    Object sysDisplayEx(int argc, const Object* argv);
    Object currentErrorPortEx(int argc, const Object* argv);
    Object openStringInputPortEx(int argc, const Object* argv);
    Object sysOpenOutputStringEx(int argc, const Object* argv);
    Object openOutputFileEx(int argc, const Object* argv);
    Object closeOutputPortEx(int argc, const Object* argv);
    Object closeInputPortEx(int argc, const Object* argv);
    Object readdirEx(int argc, const Object* argv);
    Object fileExistsPEx(int argc, const Object* argv);
    Object deleteFileEx(int argc, const Object* argv);
    Object sysGetOutputStringEx(int argc, const Object* argv);
    Object formatEx(int argc, const Object* argv);
    Object currentInputPortEx(int argc, const Object* argv);
    Object currentOutputPortEx(int argc, const Object* argv);
    Object setCurrentInputPortDEx(int argc, const Object* argv);
    Object setCurrentOutputPortDEx(int argc, const Object* argv);
    Object readCharEx(int argc, const Object* argv);
    Object writeEx(int argc, const Object* argv);
    Object sysPortSeekEx(int argc, const Object* argv);
    Object makeCustomBinaryInputPortEx(int argc, const Object* argv);
    Object getU8Ex(int argc, const Object* argv);
    Object transcodedPortEx(int argc, const Object* argv);
    Object utf8CodecEx(int argc, const Object* argv);
    Object makeTranscoderEx(int argc, const Object* argv);
    Object eofObjectEx(int argc, const Object* argv);
    Object sysOpenBytevectorOutputPortEx(int argc, const Object* argv);
    Object sysGetBytevectorEx(int argc, const Object* argv);
    Object openFileOutputPortEx(int argc, const Object* argv);
    Object openFileInputPortEx(int argc, const Object* argv);
    Object eofObjectPEx(int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_PORT_PROCEDURES__

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
    Object openFileInputOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object peekCharEx(VM* theVM, int argc, const Object* argv);
    Object getDatumEx(VM* theVM, int argc, const Object* argv);
    Object getStringAllEx(VM* theVM, int argc, const Object* argv);
    Object getStringNDEx(VM* theVM, int argc, const Object* argv);
    Object getCharEx(VM* theVM, int argc, const Object* argv);
    Object getBytevectorAllEx(VM* theVM, int argc, const Object* argv);
    Object getBytevectorSomeEx(VM* theVM, int argc, const Object* argv);
    Object getBytevectorNDEx(VM* theVM, int argc, const Object* argv);
    Object portHasPortPositionPEx(VM* theVM, int argc, const Object* argv);
    Object portHasSetPortPositionDPEx(VM* theVM, int argc, const Object* argv);
    Object setPortPositionDEx(VM* theVM, int argc, const Object* argv);
    Object portPositionEx(VM* theVM, int argc, const Object* argv);
    Object openBytevectorInputPortEx(VM* theVM, int argc, const Object* argv);
    Object portEofPEx(VM* theVM, int argc, const Object* argv);
    Object writeCharEx(VM* theVM, int argc, const Object* argv);
    Object putBytevectorEx(VM* theVM, int argc, const Object* argv);
    Object outputPortPEx(VM* theVM, int argc, const Object* argv);
    Object getStringNEx(VM* theVM, int argc, const Object* argv);
    Object statMtimeEx(VM* theVM, int argc, const Object* argv);
    Object faslWriteEx(VM* theVM, int argc, const Object* argv);
    Object faslReadEx(VM* theVM, int argc, const Object* argv);
    Object openInputFileEx(VM* theVM, int argc, const Object* argv);
    Object closePortEx(VM* theVM, int argc, const Object* argv);
    Object getLineEx(VM* theVM, int argc, const Object* argv);
    Object standardLibraryPathEx(VM* theVM, int argc, const Object* argv);
    Object standardInputPortEx(VM* theVM, int argc, const Object* argv);
    Object standardOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object standardErrorPortEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorU8SetDEx(VM* theVM, int argc, const Object* argv);
    Object lookaheadCharEx(VM* theVM, int argc, const Object* argv);
    Object readEx(VM* theVM, int argc, const Object* argv);
    Object sysDisplayEx(VM* theVM, int argc, const Object* argv);
    Object currentErrorPortEx(VM* theVM, int argc, const Object* argv);
    Object openStringInputPortEx(VM* theVM, int argc, const Object* argv);
    Object openOutputStringEx(VM* theVM, int argc, const Object* argv);
    Object openOutputFileEx(VM* theVM, int argc, const Object* argv);
    Object closeOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object closeInputPortEx(VM* theVM, int argc, const Object* argv);
    Object readdirEx(VM* theVM, int argc, const Object* argv);
    Object fileExistsPEx(VM* theVM, int argc, const Object* argv);
    Object deleteFileEx(VM* theVM, int argc, const Object* argv);
    Object getOutputStringEx(VM* theVM, int argc, const Object* argv);
    Object formatEx(VM* theVM, int argc, const Object* argv);
    Object currentInputPortEx(VM* theVM, int argc, const Object* argv);
    Object currentOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object setCurrentInputPortDEx(VM* theVM, int argc, const Object* argv);
    Object setCurrentOutputPortDEx(VM* theVM, int argc, const Object* argv);
    Object readCharEx(VM* theVM, int argc, const Object* argv);
    Object writeEx(VM* theVM, int argc, const Object* argv);
    Object sysPortSeekEx(VM* theVM, int argc, const Object* argv);
    Object makeCustomBinaryInputPortEx(VM* theVM, int argc, const Object* argv);
    Object makeCustomBinaryOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object makeCustomTextualInputPortEx(VM* theVM, int argc, const Object* argv);
    Object makeCustomTextualOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object getU8Ex(VM* theVM, int argc, const Object* argv);
    Object lookaheadU8Ex(VM* theVM, int argc, const Object* argv);
    Object getBytevectorNEx(VM* theVM, int argc, const Object* argv);
    //Object getBytevectorNDEx(VM* theVM, int argc, const Object* argv);
    //Object getBytevectorSomeEx(VM* theVM, int argc, const Object* argv);
    //Object getBytevectorAllEx(VM* theVM, int argc, const Object* argv);
    Object transcodedPortEx(VM* theVM, int argc, const Object* argv);
    Object latin1CodecEx(VM* theVM, int argc, const Object* argv);
    Object utf8CodecEx(VM* theVM, int argc, const Object* argv);
    Object utf16CodecEx(VM* theVM, int argc, const Object* argv);
    Object nativeEolStyleEx(VM* theVM, int argc, const Object* argv);
    Object makeTranscoderEx(VM* theVM, int argc, const Object* argv);
    Object nativeTranscoderEx(VM* theVM, int argc, const Object* argv);
    Object transcoderCodecEx(VM* theVM, int argc, const Object* argv);
    Object transcoderEolStyleEx(VM* theVM, int argc, const Object* argv);
    Object transcoderErrorHandlingModeEx(VM* theVM, int argc, const Object* argv);
    Object bytevectorTostringEx(VM* theVM, int argc, const Object* argv);
    Object stringTobytevectorEx(VM* theVM, int argc, const Object* argv);
    Object eofObjectEx(VM* theVM, int argc, const Object* argv);
    Object sysOpenBytevectorOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object sysGetBytevectorEx(VM* theVM, int argc, const Object* argv);
    Object openFileOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object openFileInputPortEx(VM* theVM, int argc, const Object* argv);
    Object eofObjectPEx(VM* theVM, int argc, const Object* argv);
    Object bufferModePEx(VM* theVM, int argc, const Object* argv);
    Object inputPortPEx(VM* theVM, int argc, const Object* argv);
    Object binaryPortPEx(VM* theVM, int argc, const Object* argv);
    Object textualPortPEx(VM* theVM, int argc, const Object* argv);
    Object portPEx(VM* theVM, int argc, const Object* argv);
    Object portTranscoderEx(VM* theVM, int argc, const Object* argv);
    Object putU8Ex(VM* theVM, int argc, const Object* argv);
    Object putStringEx(VM* theVM, int argc, const Object* argv);
    Object flushOutputPortEx(VM* theVM, int argc, const Object* argv);
    Object outputPortBufferModeEx(VM* theVM, int argc, const Object* argv);

}; // namespace scheme

#endif // __SCHEME_PORT_PROCEDURES__

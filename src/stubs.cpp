#include "Object.h"                                                                         
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ByteVector.h"
#include "VM.h"
#include "ByteVectorProcedures.h"
#include "ErrorProcedures.h"
#include "PortProcedures.h"
#include "ByteArrayBinaryInputPort.h"
#include "BinaryInputPort.h"
#include "Symbol.h"
#include "ProcedureMacro.h"
#include "Transcoder.h"
#include "UTF8Codec.h"
#include "UTF16Codec.h"
#include "UTF32Codec.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Arithmetic.h"
#include "TextualOutputPort.h"
#include "FFI.h"

using namespace scheme;

#ifdef _WIN32
Object stub_win32_process_pipe(VM* theVM, int argc, const Object* argv);
Object stub_win32_process_redirected_child(VM* theVM, int argc, const Object* argv);
Object stub_win32_handle_read(VM* theVM, int argc, const Object* argv);
Object stub_win32_handle_write(VM* theVM, int argc, const Object* argv);
Object stub_win32_handle_close(VM* theVM, int argc, const Object* argv);
Object stub_win32_process_wait(VM* theVM, int argc, const Object* argv);
#endif

void
register_stubs(VM* theVM){
#ifdef _WIN32
    theVM->setValueString(UC("%win32_process_pipe"),Object::makeCProcedure(stub_win32_process_pipe));
    theVM->setValueString(UC("%win32_process_redirected_child"),Object::makeCProcedure(stub_win32_process_redirected_child));
    theVM->setValueString(UC("%win32_handle_read"),Object::makeCProcedure(stub_win32_handle_read));
    theVM->setValueString(UC("%win32_handle_write"),Object::makeCProcedure(stub_win32_handle_write));
    theVM->setValueString(UC("%win32_handle_close"),Object::makeCProcedure(stub_win32_handle_close));
    theVM->setValueString(UC("%win32_process_wait"),Object::makeCProcedure(stub_win32_process_wait));
#endif
}

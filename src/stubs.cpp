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
#include "Gloc.h"
#include "Closure.h"
#include "VM-inl.h"

#if !defined(_WIN32) && !defined(MONA)
#define HAVE_TERMINAL // this should be done in configure..
#endif

#ifdef _WIN32
#define HAVE_AIO_WIN32
#include "aio_win32.h"
#endif

#ifdef HAVE_TERMINAL
#include "mosh_terminal.h"
#endif

using namespace scheme;

#ifdef _WIN32
Object stub_win32_process_pipe(VM* theVM, int argc, const Object* argv);
Object stub_win32_process_redirected_child(VM* theVM, int argc, const Object* argv);
Object stub_win32_handle_read(VM* theVM, int argc, const Object* argv);
Object stub_win32_handle_write(VM* theVM, int argc, const Object* argv);
Object stub_win32_handle_close(VM* theVM, int argc, const Object* argv);
Object stub_win32_process_wait(VM* theVM, int argc, const Object* argv);
Object stub_win32_named_pipe_create(VM* theVM, int argc, const Object* argv);
Object stub_win32_named_pipe_wait(VM* theVM, int argc, const Object* argv);
#endif

#define NIL Object::Nil
#define CONS(x,y) Object::cons((x),(y))
#define SYM(x) Symbol::intern(UC(x))
#define PTR(x) Object::makePointer((void*)x)
#define FUNC(x,y) CONS(SYM(x),PTR(y))

#ifdef HAVE_TERMINAL
#define LIBDATA_TERMINAL CONS(SYM("terminal"), \
CONS(FUNC("terminal_acquire",terminal_acquire), \
CONS(FUNC("terminal_release",terminal_release), \
CONS(FUNC("terminal_getsize",terminal_getsize), \
CONS(FUNC("terminal_isatty",terminal_isatty),NIL)))))
#endif

#ifdef HAVE_AIO_WIN32
#define LIBDATA_AIO_WIN32 CONS(SYM("aio-win32"), \
CONS(FUNC("win32_iocp_create",win32_iocp_create), \
CONS(FUNC("win32_iocp_assoc",win32_iocp_assoc), \
CONS(FUNC("win32_iocp_pop",win32_iocp_pop), \
CONS(FUNC("win32_overlapped_alloc",win32_overlapped_alloc), \
CONS(FUNC("win32_overlapped_free",win32_overlapped_free), \
CONS(FUNC("win32_handle_read_async",win32_handle_read_async), \
CONS(FUNC("win32_handle_write_async",win32_handle_write_async), \
CONS(FUNC("win32_process_redirected_child2",win32_process_redirected_child2), \
CONS(FUNC("win32_create_named_pipe_async",win32_create_named_pipe_async), \
CONS(FUNC("win32_wait_named_pipe_async",win32_wait_named_pipe_async),NIL)))))))))))
#endif

Object
stub_get_pffi_feature_set(VM* theVM, int argc, const Object* argv){
    //DeclareProcedureName("%get-pffi-feature-set");
    Object tmp;

    tmp = Object::Nil;
#ifdef HAVE_TERMINAL
    tmp = Object::cons(LIBDATA_TERMINAL,tmp);
#endif
#ifdef HAVE_AIO_WIN32
	tmp = Pair::append2(tmp,LIBDATA_AIO_WIN32);
#endif
    return tmp;
}

#undef NIL
#undef CONS
#undef SYM
#undef PTR

void
register_stubs(VM* theVM){
    theVM->setValueString(UC("%get-pffi-feature-set"),Object::makeCProcedure(stub_get_pffi_feature_set));
#ifdef _WIN32
    theVM->setValueString(UC("%win32_process_pipe"),Object::makeCProcedure(stub_win32_process_pipe));
    theVM->setValueString(UC("%win32_process_redirected_child"),Object::makeCProcedure(stub_win32_process_redirected_child));
    theVM->setValueString(UC("%win32_handle_read"),Object::makeCProcedure(stub_win32_handle_read));
    theVM->setValueString(UC("%win32_handle_write"),Object::makeCProcedure(stub_win32_handle_write));
    theVM->setValueString(UC("%win32_handle_close"),Object::makeCProcedure(stub_win32_handle_close));
    theVM->setValueString(UC("%win32_process_wait"),Object::makeCProcedure(stub_win32_process_wait));
    theVM->setValueString(UC("%win32_named_pipe_create"),Object::makeCProcedure(stub_win32_named_pipe_create));
    theVM->setValueString(UC("%win32_named_pipe_wait"),Object::makeCProcedure(stub_win32_named_pipe_wait));
#endif
}

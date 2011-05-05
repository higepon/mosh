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

#ifndef MONA
#define HAVE_BDWGC_STUBS
#endif

#ifdef _WIN32
#define HAVE_AIO_WIN32
#include "aio_win32.h"
#endif

#ifdef HAVE_TERMINAL
#include "mosh_terminal.h"
#endif

#ifdef HAVE_BDWGC_STUBS
#include "boehmgc-stubs.h"
#endif 

#ifdef HAVE_KQUEUE
#include "bsd/kqueue_stubs.h"
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
CONS(FUNC("win32_handle_close",win32_handle_close), \
CONS(FUNC("win32_cancelioex",win32_cancelioex), \
CONS(FUNC("win32_iocp_create",win32_iocp_create), \
CONS(FUNC("win32_iocp_assoc",win32_iocp_assoc), \
CONS(FUNC("win32_iocp_pop",win32_iocp_pop), \
CONS(FUNC("win32_overlapped_alloc",win32_overlapped_alloc), \
CONS(FUNC("win32_overlapped_free",win32_overlapped_free), \
CONS(FUNC("win32_overlapped_setmydata",win32_overlapped_setmydata), \
CONS(FUNC("win32_overlapped_getmydata",win32_overlapped_getmydata), \
CONS(FUNC("win32_handle_read_async",win32_handle_read_async), \
CONS(FUNC("win32_handle_write_async",win32_handle_write_async), \
CONS(FUNC("win32_process_redirected_child2",win32_process_redirected_child2), \
CONS(FUNC("win32_create_named_pipe_async",win32_create_named_pipe_async), \
CONS(FUNC("win32_wait_named_pipe_async",win32_wait_named_pipe_async), \
CONS(FUNC("win32_process_wait_async",win32_process_wait_async), \
CONS(FUNC("win32_sockaddr_storage_size",win32_sockaddr_storage_size), \
CONS(FUNC("win32_socket_create",win32_socket_create), \
CONS(FUNC("win32_socket_close",win32_socket_close), \
CONS(FUNC("win32_addrinfoex_free",win32_addrinfoex_free), \
CONS(FUNC("win32_addrinfoex_read",win32_addrinfoex_read), \
CONS(FUNC("win32_socket_connect",win32_socket_connect), \
CONS(FUNC("win32_socket_accept",win32_socket_accept), \
CONS(FUNC("win32_socket_bind",win32_socket_bind), \
CONS(FUNC("win32_socket_listen",win32_socket_listen), \
CONS(FUNC("win32_getaddrinfo",win32_getaddrinfo), \
CONS(FUNC("win32_finalization_handler_get",win32_finalization_handler_get), \
CONS(FUNC("win32_finalization_handler_create",win32_finalization_handler_create), \
	NIL))))))))))))))))))))))))))))

#define LIBDATA_WIN32_GUI CONS(SYM("win32-gui"), \
CONS(FUNC("win32_messagebox",win32_messagebox) ,\
CONS(FUNC("win32_window_move",win32_window_move) ,\
CONS(FUNC("win32_window_show",win32_window_show) ,\
CONS(FUNC("win32_window_hide",win32_window_hide) ,\
CONS(FUNC("win32_window_settitle",win32_window_settitle) ,\
CONS(FUNC("win32_window_close",win32_window_close) ,\
CONS(FUNC("win32_window_destroy",win32_window_destroy) ,\
CONS(FUNC("win32_registerwindowclass",win32_registerwindowclass) ,\
CONS(FUNC("win32_window_alloc",win32_window_alloc) ,\
CONS(FUNC("win32_window_create",win32_window_create) ,\
CONS(FUNC("win32_getmonitorinfo",win32_getmonitorinfo) ,\
	NIL))))))))))))

#define LIBDATA_WIN32_MISC CONS(SYM("win32-misc"), \
CONS(FUNC("win32_get_processor_count",win32_get_processor_count), \
CONS(FUNC("win32_get_ansi_codepage",win32_get_ansi_codepage), \
CONS(FUNC("win32_multibyte_to_widechar",win32_multibyte_to_widechar), \
CONS(FUNC("win32_measure_multibyte_to_widechar",win32_measure_multibyte_to_widechar), \
	NIL)))))
#endif

#define LIBDATA_BOEHMGC_STUBS CONS(SYM("boehmgc-stubs"), \
CONS(FUNC("create_weak_vector",create_weak_vector), \
CONS(FUNC("weak_vector_ref",weak_vector_ref), \
CONS(FUNC("weak_vector_set",weak_vector_set), \
CONS(FUNC("register_disappearing_link_wv",register_disappearing_link_wv), \
CONS(FUNC("register_finalizer",register_finalizer), \
CONS(FUNC("register_disappearing_link",register_disappearing_link), \
CONS(FUNC("gcollect",gcollect),NIL))))))))

#ifdef HAVE_KQUEUE
#define LIBDATA_KQUEUE CONS(SYM("kqueue-stubs"), \
CONS(FUNC("kq_create",kq_create), \
CONS(FUNC("kevent_alloc",kevent_alloc), \
CONS(FUNC("kevent_offset",kevent_offset), \
CONS(FUNC("kevent_dispose",kevent_dispose), \
CONS(FUNC("kevent_set_readevent",kevent_set_readevent), \
CONS(FUNC("kevent_set_writeevent",kevent_set_writeevent), \
CONS(FUNC("kevent_set_enableuserevent",kevent_set_enableuserevent), \
CONS(FUNC("kevent_set_triggeruserevent",kevent_set_triggeruserevent), \
CONS(FUNC("kevent_ident",kevent_ident), \
CONS(FUNC("kevent_type",kevent_type), \
CONS(FUNC("kevent_decode_fd",kevent_decode_fd), \
CONS(FUNC("kevent_exec",kevent_exec), \
CONS(FUNC("socket_sizeof_sockaddr_storage",socket_sizeof_sockaddr_storage), \
CONS(FUNC("socket_getaddrinfo",socket_getaddrinfo), \
CONS(FUNC("socket_create",socket_create), \
CONS(FUNC("socket_freeaddrinfo",socket_freeaddrinfo), \
CONS(FUNC("socket_bind",socket_bind), \
CONS(FUNC("socket_accept",socket_accept), \
CONS(FUNC("socket_listen",socket_listen), \
CONS(FUNC("socket_connect",socket_connect), \
CONS(FUNC("socket_addrinfo_read",socket_addrinfo_read), \
CONS(FUNC("socket_setnodelay",socket_setnodelay), \
CONS(FUNC("fd_read",fd_read), \
CONS(FUNC("fd_write",fd_write), \
CONS(FUNC("fd_close",fd_close), \
CONS(FUNC("fd_setnonblock",fd_setnonblock), \
    NIL)))))))))))))))))))))))))))
#endif

Object
stub_get_pffi_feature_set(VM* theVM, int argc, const Object* argv){
    //DeclareProcedureName("%get-pffi-feature-set");
    Object tmp;

    tmp = Object::Nil;
#ifdef HAVE_KQUEUE
	tmp = Object::cons(LIBDATA_KQUEUE,tmp);
#endif
#ifdef HAVE_BDWGC_STUBS
	tmp = Object::cons(LIBDATA_BOEHMGC_STUBS,tmp);
#endif
#ifdef HAVE_TERMINAL
    tmp = Object::cons(LIBDATA_TERMINAL,tmp);
#endif
#ifdef HAVE_AIO_WIN32
	tmp = Object::cons(LIBDATA_AIO_WIN32,tmp);
	tmp = Object::cons(LIBDATA_WIN32_GUI,tmp);
    tmp = Object::cons(LIBDATA_WIN32_MISC,tmp);
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

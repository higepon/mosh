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
#include "posix/terminal/mosh_terminal.h"
#endif

#ifdef HAVE_BDWGC_STUBS
#include "generic/boehmgc-stubs.h"
#endif 

#ifdef HAVE_KQUEUE
#include "bsd/kqueue/kqueue_stubs.h"
#endif

#ifdef HAVE_PTRACE_COMMON
#include "posix/ptrace/ptrace_common.h"
#endif

#ifdef HAVE_POSIX_SPAWN
#include "posix/spawn/posixspawn.h"
#endif

#ifdef HAVE_FCNTL
#include "posix/fd/posix_fd.h"
#endif

#ifdef HAVE_POLL
#include "posix/poll/posix_poll.h"
#endif

#ifdef HAVE_SOCKET
#include "posix/socket/posix_socket.h"
#endif

using namespace scheme;

#define NIL Object::Nil
#define CONS(x,y) Object::cons((x),(y))
#define SYM(x) Symbol::intern(UC(x))
#define PTR(x) Object::makePointer((void*)x)
#define FUNC(x,y) CONS(SYM(x),PTR(y))
#define FN(x) FUNC(#x,x)


#ifdef HAVE_TERMINAL
#define LIBDATA_TERMINAL CONS(SYM("terminal"), \
CONS(FUNC("terminal_acquire",terminal_acquire), \
CONS(FUNC("terminal_release",terminal_release), \
CONS(FUNC("terminal_getsize",terminal_getsize), \
CONS(FUNC("terminal_isatty",terminal_isatty),NIL)))))
#endif

#ifdef HAVE_PTRACE_COMMON 
#define LIBDATA_PTRACE_COMMON CONS(SYM("ptrace-common"), \
CONS(FN(call_ptrace), \
CONS(FN(ptrace_traceme), \
CONS(FN(ptrace_write), \
CONS(FN(ptrace_read), \
CONS(FN(ptrace_continue), \
CONS(FN(ptrace_singlestep), \
CONS(FN(ptrace_attach), \
CONS(FN(ptrace_detatch), \
CONS(FN(ptrace_regsize), \
CONS(FN(ptrace_getregs), \
CONS(FN(ptrace_setregs), \
CONS(FN(ptrace_fpregsize), \
CONS(FN(ptrace_getfpregs), \
CONS(FN(ptrace_setfpregs), NIL))))))))))))))) 
#endif

#ifdef HAVE_AIO_WIN32
#define LIBDATA_AIO_WIN32 CONS(SYM("aio-win32"), \
CONS(FN(win32_handle_close), \
CONS(FN(win32_iocp_create), \
CONS(FN(win32_iocp_assoc), \
CONS(FN(win32_iocp_pop), \
CONS(FN(win32_overlapped_alloc), \
CONS(FN(win32_overlapped_free), \
CONS(FN(win32_overlapped_setmydata), \
CONS(FN(win32_overlapped_getmydata), \
CONS(FN(win32_handle_read_async), \
CONS(FN(win32_handle_write_async), \
CONS(FN(win32_process_redirected_child2), \
CONS(FN(win32_create_named_pipe_async), \
CONS(FN(win32_wait_named_pipe_async), \
CONS(FN(win32_process_wait_async), \
CONS(FN(win32_sockaddr_storage_size), \
CONS(FN(win32_socket_create), \
CONS(FN(win32_socket_close), \
CONS(FN(win32_addrinfoex_free), \
CONS(FN(win32_addrinfoex_read), \
CONS(FN(win32_socket_connect), \
CONS(FN(win32_socket_accept), \
CONS(FN(win32_socket_bind), \
CONS(FN(win32_socket_listen), \
CONS(FN(win32_getaddrinfo), \
CONS(FN(win32_finalization_handler_get), \
CONS(FN(win32_finalization_handler_create), \
	NIL)))))))))))))))))))))))))))

#define LIBDATA_WIN32_GUI CONS(SYM("win32-gui"), \
CONS(FN(win32_messagebox) ,\
CONS(FN(win32_window_move) ,\
CONS(FN(win32_window_show) ,\
CONS(FN(win32_window_hide) ,\
CONS(FN(win32_window_settitle) ,\
CONS(FN(win32_window_close) ,\
CONS(FN(win32_window_destroy) ,\
CONS(FN(win32_registerwindowclass) ,\
CONS(FN(win32_window_alloc) ,\
CONS(FN(win32_window_create) ,\
CONS(FN(win32_window_fitbuffer) ,\
CONS(FN(win32_getmonitorinfo) ,\
CONS(FN(win32_window_updaterects) ,\
CONS(FN(win32_window_createbitmap) ,\
CONS(FN(win32_window_getclientrect_x) ,\
CONS(FN(win32_window_getclientrect_y) ,\
CONS(FN(win32_dc_create) ,\
CONS(FN(win32_dc_dispose) ,\
CONS(FN(win32_dc_selectobject) ,\
CONS(FN(win32_dc_transform) ,\
CONS(FN(win32_dc_settransform) ,\
CONS(FN(win32_gdi_deleteobject) ,\
CONS(FN(win32_pen_create) ,\
CONS(FN(win32_brush_create) ,\
CONS(FN(win32_font_create) ,\
CONS(FN(win32_dc_draw) ,\
CONS(FN(win32_dc_measure_text) ,\
	NIL))))))))))))))))))))))))))))

#define LIBDATA_WIN32_MISC CONS(SYM("win32-misc"), \
CONS(FN(win32_get_processor_count), \
CONS(FN(win32_get_ansi_codepage), \
CONS(FN(win32_multibyte_to_widechar), \
CONS(FN(win32_measure_multibyte_to_widechar), \
CONS(FN(win32_mypath), \
	NIL))))))
#endif

#define LIBDATA_BOEHMGC_STUBS CONS(SYM("boehmgc-stubs"), \
CONS(FN(create_weak_vector), \
CONS(FN(weak_vector_ref), \
CONS(FN(weak_vector_set), \
CONS(FN(register_disappearing_link_wv), \
CONS(FN(register_finalizer), \
CONS(FN(register_disappearing_link), \
CONS(FN(gcollect),NIL))))))))

#ifdef HAVE_KQUEUE
#define LIBDATA_KQUEUE CONS(SYM("kqueue-stubs"), \
CONS(FN(kq_create), \
CONS(FN(kevent_alloc), \
CONS(FN(kevent_offset), \
CONS(FN(kevent_dispose), \
CONS(FN(kevent_set_readevent), \
CONS(FN(kevent_set_writeevent), \
CONS(FN(kevent_set_enableuserevent), \
CONS(FN(kevent_set_triggeruserevent), \
CONS(FN(kevent_ident), \
CONS(FN(kevent_type), \
CONS(FN(kevent_decode_fd), \
CONS(FN(kevent_exec), \
    NIL)))))))))))))
#endif

#ifdef HAVE_SOCKET
#define LIBDATA_POSIX_SOCKET CONS(SYM("posix-socket"), \
CONS(FN(socket_sizeof_sockaddr_storage), \
CONS(FN(socket_getaddrinfo), \
CONS(FN(socket_create), \
CONS(FN(socket_freeaddrinfo), \
CONS(FN(socket_bind), \
CONS(FN(socket_accept), \
CONS(FN(socket_listen), \
CONS(FN(socket_connect), \
CONS(FN(socket_addrinfo_read), \
CONS(FN(socket_setnodelay), \
    NIL)))))))))))
#endif

#ifdef HAVE_POLL
#define LIBDATA_POSIX_POLL CONS(SYM("posix-poll"), \
CONS(FN(poll_alloc), \
CONS(FN(poll_dispose), \
CONS(FN(poll_exec), \
CONS(FN(poll_set_fd), \
CONS(FN(poll_set_pollin), \
CONS(FN(poll_unset_pollin), \
CONS(FN(poll_set_pollout), \
CONS(FN(poll_unset_pollout), \
CONS(FN(poll_get_pollin), \
CONS(FN(poll_get_pollout), \
CONS(FN(poll_get_pollerr), \
CONS(FN(poll_get_pollhup), \
CONS(FN(poll_get_pollnval), \
    NIL))))))))))))))
#endif

#ifdef HAVE_FCNTL
#define LIBDATA_POSIX_FD CONS(SYM("posix-fd"), \
CONS(FN(fd_read), \
CONS(FN(fd_write), \
CONS(FN(fd_close), \
CONS(FN(fd_setnonblock), \
CONS(FN(fd_pipe), \
    NIL))))))
#endif

#ifdef HAVE_POSIX_SPAWN
#define LIBDATA_POSIX_SPAWN CONS(SYM("posixspawn"), \
CONS(FN(posixspawn_spawn), \
CONS(FN(posixspawn_fileactionssize), \
CONS(FN(posixspawn_fileactions_init), \
CONS(FN(posixspawn_fileactions_destroy), \
CONS(FN(posixspawn_fileactions_adddup2), \
CONS(FN(posixspawn_fileactions_addclose), \
    NIL)))))))
#endif

Object
stub_get_pffi_feature_set(VM* theVM, int argc, const Object* argv){
    //DeclareProcedureName("%get-pffi-feature-set");
    Object tmp;

    tmp = Object::Nil;
#ifdef HAVE_PTRACE_COMMON
	tmp = Object::cons(LIBDATA_PTRACE_COMMON,tmp);
#endif
#ifdef HAVE_KQUEUE
	tmp = Object::cons(LIBDATA_KQUEUE,tmp);
#endif
#ifdef HAVE_FCNTL
	tmp = Object::cons(LIBDATA_POSIX_FD,tmp);
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
#ifdef HAVE_POSIX_SPAWN
    tmp = Object::cons(LIBDATA_POSIX_SPAWN,tmp);
#endif
#ifdef HAVE_POLL
    tmp = Object::cons(LIBDATA_POSIX_POLL,tmp);
#endif
#ifdef HAVE_SOCKET
    tmp = Object::cons(LIBDATA_POSIX_SOCKET,tmp);
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
}

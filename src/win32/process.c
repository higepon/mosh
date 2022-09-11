#include <winsock2.h>
#include <mswsock.h>
#include <ws2tcpip.h>

#include <windows.h>
#include <config.h>

#include <process.h>
#include <stddef.h>
#include <stdio.h>

#define GC_NO_THREAD_REDIRECTS // we don't need override
#include <gc.h>


#include "aio_win32.h"

#ifndef WSAID_ACCEPTEX

// Took from libuv, MinGW64.


typedef struct addrinfoW {
  int ai_flags;
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  size_t ai_addrlen;
  PWSTR ai_canonname;
  struct sockaddr *ai_addr;
  struct addrinfoW *ai_next;
} ADDRINFOW,*PADDRINFOW;


DECLSPEC_IMPORT int WSAAPI GetAddrInfoW(const wchar_t* node,
                                        const wchar_t* service,
                                        const ADDRINFOW* hints,
                                        PADDRINFOW* result);

DECLSPEC_IMPORT void WSAAPI FreeAddrInfoW(PADDRINFOW pAddrInfo);


#define WSAID_ACCEPTEX                                \
{0xb5367df1, 0xcbac, 0x11cf,                          \
    {0x95, 0xca, 0x00, 0x80, 0x5f, 0x48, 0xa1, 0x92}}

#define WSAID_CONNECTEX                               \
{0x25a207b9, 0xddf3, 0x4660,                          \
    {0x8e, 0xe9, 0x76, 0xe5, 0x8c, 0x74, 0x06, 0x3e}}

#define WSAID_GETACCEPTEXSOCKADDRS                    \
{0xb5367df2, 0xcbac, 0x11cf,                          \
    {0x95, 0xca, 0x00, 0x80, 0x5f, 0x48, 0xa1, 0x92}}

#define WSAID_DISCONNECTEX                            \
{0x7fda2e11, 0x8630, 0x436f,                          \
    {0xa0, 0x31, 0xf5, 0x36, 0xa6, 0xee, 0xc1, 0x57}}

#define WSAID_TRANSMITFILE                            \
{0xb5367df0, 0xcbac, 0x11cf,                          \
    {0x95, 0xca, 0x00, 0x80, 0x5f, 0x48, 0xa1, 0x92}}

typedef BOOL PASCAL (*LPFN_ACCEPTEX)
    (SOCKET sListenSocket,
     SOCKET sAcceptSocket,
     PVOID lpOutputBuffer,
     DWORD dwReceiveDataLength,
     DWORD dwLocalAddressLength,
     DWORD dwRemoteAddressLength,
     LPDWORD lpdwBytesReceived,
     LPOVERLAPPED lpOverlapped);

typedef BOOL PASCAL (*LPFN_CONNECTEX)
    (SOCKET s,
     const struct sockaddr* name,
     int namelen,
     PVOID lpSendBuffer,
     DWORD dwSendDataLength,
     LPDWORD lpdwBytesSent,
     LPOVERLAPPED lpOverlapped);

typedef void PASCAL (*LPFN_GETACCEPTEXSOCKADDRS)
    (PVOID lpOutputBuffer,
     DWORD dwReceiveDataLength,
     DWORD dwLocalAddressLength,
     DWORD dwRemoteAddressLength,
     LPSOCKADDR* LocalSockaddr,
     LPINT LocalSockaddrLength,
     LPSOCKADDR* RemoteSockaddr,
     LPINT RemoteSockaddrLength);

typedef BOOL PASCAL (*LPFN_DISCONNECTEX)
    (SOCKET hSocket,
     LPOVERLAPPED lpOverlapped,
     DWORD dwFlags,
     DWORD reserved);

typedef BOOL PASCAL (*LPFN_TRANSMITFILE)
    (SOCKET hSocket,
     HANDLE hFile,
     DWORD nNumberOfBytesToWrite,
     DWORD nNumberOfBytesPerSend,
     LPOVERLAPPED lpOverlapped,
     LPTRANSMIT_FILE_BUFFERS lpTransmitBuffers,
     DWORD dwFlags);
#endif

char* errorpos; // FIXME: for debugging

#define OSERROR(x) errorpos = x

int
win32_process_pipe(HANDLE* ret){
	SECURITY_ATTRIBUTES sat;
	HANDLE tmp;

	sat.nLength = sizeof(SECURITY_ATTRIBUTES);
	sat.bInheritHandle = TRUE;
	sat.lpSecurityDescriptor = NULL;

	if(!CreatePipe(&tmp,&ret[0],&sat,0)){
		OSERROR("createpipe"); // handle Win32 Error Here
		return 0;
	}
	DuplicateHandle(GetCurrentProcess(),tmp,GetCurrentProcess(),&ret[1],0,FALSE,DUPLICATE_SAME_ACCESS);
	CloseHandle(tmp);
#if 0 // won't work..
	/* make non-inheritable */
	if(!SetHandleInformation(ret[1], HANDLE_FLAG_INHERIT, 0)){
		OSERROR("sethandleinformation");
		return 0;
	}
#endif

	return 1;
}

uintptr_t
win32_process_redirected_child(wchar_t* spec,wchar_t* dir, HANDLE std_in, HANDLE std_out, HANDLE std_err){
	PROCESS_INFORMATION pi;
	STARTUPINFOW si;
	BOOL r;

	ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
	ZeroMemory(&si, sizeof(STARTUPINFOW));

	si.cb = sizeof(STARTUPINFO);
	si.dwFlags = STARTF_USESTDHANDLES;
	if(std_in != 0){
		si.hStdInput = std_in;
	}else{
		si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	}
	if(std_out != 0){
		si.hStdOutput = std_out;
	}else{
		si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	}
	if(std_err != 0){
		si.hStdError = std_err;
	}else{
		si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	}

	r = CreateProcessW(NULL, spec, NULL, NULL, TRUE, 0, NULL, dir, &si, &pi);

	if(std_out != 0){
		CloseHandle(std_out);
	}

	if(std_err != 0){
		CloseHandle(std_err);
	}

	if (! r){
		OSERROR("CreateProcess");
		return 0;
	}
	CloseHandle(pi.hThread);
	return (uintptr_t)pi.hProcess;
}

// should be moved to handle.c...

int
win32_handle_read(HANDLE h,void* p,unsigned int len,unsigned int *res){
	BOOL r;
	r = ReadFile(h,p,len,res,NULL);
	if(!r){
		OSERROR("ReadFile");
		return 0;
	}
	return 1;
}

int
win32_handle_write(HANDLE h,void *p,unsigned int len,unsigned int *res){
	BOOL r;
	r = WriteFile(h,p,len,res,NULL);
	if(!r){
		OSERROR("WriteFile");
		return 0;
	}
	return 1;
}

int
win32_handle_close(HANDLE h){
	BOOL r;
    /* Phew! */
    closesocket((SOCKET)h);
	r = CloseHandle(h);
	if(!r){
		OSERROR("CloseHandle");
		return 0;
	}
	return 1;
}

int
win32_process_wait(HANDLE process){
	BOOL r;
	DWORD res;
	r = WaitForSingleObject(process,INFINITE);
	if(WAIT_FAILED == r){
		OSERROR("WaitForSingleObject");
		return -1;
	}
	GetExitCodeProcess(process,&res);
	CloseHandle(process);
	return res;
}

int
win32_handle_wait(HANDLE h){
	BOOL r;
	r = WaitForSingleObject(h,INFINITE);
	if(!r){
		OSERROR("WaitForSingleObject");
		return 0;
	}
	return 1;
}

uintptr_t
win32_create_named_pipe(wchar_t* name){
	HANDLE h;
	h = CreateNamedPipeW(name,PIPE_ACCESS_DUPLEX,PIPE_WAIT,PIPE_UNLIMITED_INSTANCES,4096,4096,0,NULL);
	if(h == INVALID_HANDLE_VALUE){
		OSERROR("CreateNamedPipeW");
		return -1;
	}
	return (uintptr_t)h;
}

int
win32_wait_named_pipe(HANDLE h){
	BOOL b;
	b = ConnectNamedPipe(h,NULL);
	if(!b){
		return 0;
	}
	return 1;
}

uintptr_t
win32_iocp_create(void){
	// FIXME: err?
	return (uintptr_t)CreateIoCompletionPort(INVALID_HANDLE_VALUE,NULL,0,0);
}

int
win32_iocp_assoc(uintptr_t iocp,uintptr_t in,uintptr_t key){
	HANDLE h = (HANDLE)in;
	HANDLE ret;
	int r;

	ret = CreateIoCompletionPort(h,(HANDLE)iocp,key,0);
	if(ret == NULL){
		// error
		r = GetLastError();
		return 0;
	}
	return 1;
}

int
win32_iocp_pop(uintptr_t iocp, intptr_t timeout_in,uintptr_t ret_bytestrans, uintptr_t ret_key, uintptr_t ret_overlapped){
	BOOL b;
	int r;
	DWORD timeout;
	timeout = (timeout_in == -1)?INFINITE:timeout_in;
	
	
	// GetQueuedCompletionStatus will return status of de-queued I/O ops.
	// So we should handle the result even if we get FALSE here..
	b = GetQueuedCompletionStatus((HANDLE)iocp,(LPDWORD)ret_bytestrans,(PULONG_PTR)ret_key,(LPOVERLAPPED *)ret_overlapped,timeout);
	if(!b){
		r = GetLastError();
		return 0;
	}else{
		return 1;
	}
}

// allocate and free OVERLAPPED. OVERLAPPED may passed to non-GC-managed threads.

typedef struct{
    // N.B. should synced with finalization handler and window handler
	OVERLAPPED ovl;
	void* p;
} OVPAIR;

void*
win32_overlapped_alloc(void){
	OVPAIR* p;
	p = (OVPAIR *)GC_MALLOC_UNCOLLECTABLE(sizeof(OVPAIR));
	ZeroMemory(p,sizeof(OVPAIR));
	return p; // FIXME: should be aligned ??
}

void
win32_overlapped_free(void* p){
	GC_FREE(p);
}

void
win32_overlapped_setmydata(void* p,void* data){
	OVPAIR* ov = (OVPAIR *)p;
	ov->p = data;
}

void*
win32_overlapped_getmydata(void* p){
	OVPAIR* ov = (OVPAIR *)p;
	return (void *)ov->p;
}

int
win32_overlapped_geterror(void* p){
	OVERLAPPED* ovl = (OVERLAPPED *)p;
	DWORD err = ovl->Internal;
	//ovl->Internal = 0;
	return err;
}

int
win32_handle_read_async(uintptr_t h,uintptr_t offsetL,uintptr_t offsetH,uintptr_t length,uintptr_t buf,uintptr_t ol){
	BOOL b;
	int err;
	DWORD out_len;
	OVERLAPPED* ovl = (OVERLAPPED *)ol;
	ZeroMemory(ovl,sizeof(OVERLAPPED));
	ovl->Offset = offsetL;
	ovl->OffsetHigh = offsetH;
	ovl->hEvent = NULL;
	b = ReadFile((HANDLE)h,(void*)buf,length,&out_len,ovl);
	if(!b){
		err = GetLastError();
		if(err == ERROR_IO_PENDING){
			return -1; // Queued
		}else{
			return -2;
		}
	}else{
		return out_len;
	}
}

int
win32_handle_write_async(uintptr_t h,uintptr_t offsetL,uintptr_t offsetH,uintptr_t length,uintptr_t buf,uintptr_t ol){
	BOOL b;
	int err;
	OVERLAPPED* ovl = (OVERLAPPED *)ol;
	DWORD out_len;
	ZeroMemory(ovl,sizeof(OVERLAPPED));
	ovl->Offset = offsetL;
	ovl->OffsetHigh = offsetH;
	ovl->hEvent = NULL;
	b = WriteFile((HANDLE)h,(void *)buf,length,&out_len,ovl);
	if(!b){
		err = GetLastError();
		if(err == ERROR_IO_PENDING){
			return -1; // Queued
		}else{
			return -2;
		}
	}else{
		return out_len;
	}
}

static HANDLE
open_for_input(wchar_t* path,int mode){
	SECURITY_ATTRIBUTES sat;

	sat.nLength = sizeof(SECURITY_ATTRIBUTES);
	sat.bInheritHandle = TRUE;
	sat.lpSecurityDescriptor = NULL;
	return CreateFileW(path,GENERIC_READ,FILE_SHARE_READ,&sat,mode,FILE_ATTRIBUTE_READONLY,NULL);
}

static HANDLE
open_for_output(wchar_t* path,int mode){
	SECURITY_ATTRIBUTES sat;

	sat.nLength = sizeof(SECURITY_ATTRIBUTES);
	sat.bInheritHandle = TRUE;
	sat.lpSecurityDescriptor = NULL;
	return CreateFileW(path,GENERIC_WRITE,mode,&sat,CREATE_ALWAYS,0,NULL);
}

// mode:
//  0 : /dev/null
//  1 : stdio
//  2 : CREATE_ALWAYS
//  3 : OPEN_EXISTING or TRUNCATE_EXISTING
//  4 : pass HANDLE
uintptr_t
win32_process_redirected_child2(wchar_t* spec,wchar_t* dir, wchar_t* std_in, wchar_t* std_out, wchar_t* std_err, int in_mode, int out_mode, int err_mode){
	PROCESS_INFORMATION pi;
	STARTUPINFOW si;
	int err;
	BOOL r;
	HANDLE h;
	HANDLE ex_in,ex_out, ex_err;
	ex_in = 0;
	ex_out = 0;
	ex_err = 0;

	ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
	ZeroMemory(&si, sizeof(STARTUPINFOW));

	si.cb = sizeof(STARTUPINFOW);
	si.dwFlags = STARTF_USESTDHANDLES;
	switch(in_mode){
	case 0:
		break;
	case 1:
		si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
		break;
	case 3:
		h = open_for_input(std_in,OPEN_EXISTING);
		if(h == INVALID_HANDLE_VALUE) return 0;
		si.hStdInput = h;
		ex_in = h;
		break;
        case 4:
        si.hStdInput = (HANDLE)std_in;
        break;
	}
	switch(out_mode){
	case 0:
		break;
	case 1:
		si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
		break;
	case 2:
		h = open_for_output(std_out,CREATE_ALWAYS);
		if(h == INVALID_HANDLE_VALUE) return 0;
		si.hStdOutput = h;
		ex_out = h;
		break;
	case 3:
		h = open_for_output(std_out,TRUNCATE_EXISTING);
		err = GetLastError();
		if(h == INVALID_HANDLE_VALUE) return 0;
		si.hStdOutput = h;
		ex_out = h;
		break;
        case 4:
        si.hStdOutput = (HANDLE)std_out;
        ex_out = (HANDLE)std_out;
        break;
	}
	switch(err_mode){
	case 0:
		break;
	case 1:
		si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
		break;
	case 2:
		h = open_for_output(std_err,CREATE_ALWAYS);
		if(h == INVALID_HANDLE_VALUE) return 0;
		si.hStdError = h;
		ex_err = h;
		break;
	case 3:
		h = open_for_output(std_err,TRUNCATE_EXISTING);
		if(h == INVALID_HANDLE_VALUE) return 0;
		si.hStdError = h;
		ex_err = h;
		break;
    case 4:
        si.hStdError = (HANDLE)std_err;
        ex_err = (HANDLE)std_err;
        break;
	}

	//FIXME: should we use CREATE_NO_WINDOW ?
	r = CreateProcessW(NULL, spec, NULL, NULL, TRUE, 0, NULL, dir, &si, &pi);

	if(ex_in != 0){
		CloseHandle(ex_in);
	}

	if(ex_out != 0){
		CloseHandle(ex_out);
	}

	if(ex_err != 0){
		CloseHandle(ex_err);
	}

	if (! r){
		r = GetLastError();
		OSERROR("CreateProcess");
		return 0;
	}
	CloseHandle(pi.hThread);
	return (uintptr_t)pi.hProcess;
}

uintptr_t
win32_create_named_pipe_async(wchar_t* name){
	HANDLE h;
	h = CreateNamedPipeW(name,PIPE_ACCESS_DUPLEX|FILE_FLAG_OVERLAPPED,PIPE_WAIT,PIPE_UNLIMITED_INSTANCES,4096,4096,NMPWAIT_WAIT_FOREVER,NULL);
	if(h == INVALID_HANDLE_VALUE){
		OSERROR("CreateNamedPipeW");
		return -1;
	}
	return (uintptr_t)h;
}
int
win32_wait_named_pipe_async(uintptr_t h, uintptr_t ovl){
	BOOL b;
	int err;
	ZeroMemory((OVERLAPPED*)ovl,sizeof(OVERLAPPED));
	b = ConnectNamedPipe((HANDLE)h,(OVERLAPPED *)ovl);
	err = GetLastError();
	if(!b){
		if (err == ERROR_IO_PENDING){
			return 1;
		}else if(err == ERROR_PIPE_CONNECTED){
			return 2; /* Already connected */
		}else{
			return 0;
		}
	}
	return 1;
}

typedef struct {
	HANDLE h;
	HANDLE iocp;
	uintptr_t key;
	uintptr_t overlapped;
} thread_waiter_param;


static void
emit_queue_event(HANDLE iocp,uintptr_t key,intptr_t res,uintptr_t overlapped){
	PostQueuedCompletionStatus(iocp,res,key,(LPOVERLAPPED)overlapped);
}

static void
thread_waiter(void* p){
	thread_waiter_param* param = (thread_waiter_param *)p;
	BOOL r;
	DWORD res;
	HANDLE h;
	HANDLE iocp;
	uintptr_t key;
	uintptr_t overlapped;
	h = param->h;
	iocp = param->iocp;
	key = param->key;
	overlapped = param->overlapped;
	free(param);

	r = WaitForSingleObject(h,INFINITE);
	if(WAIT_FAILED == r){
		OSERROR("WaitForSingleObject");
		emit_queue_event(iocp,key,-1,overlapped);
	}else{
		GetExitCodeProcess(h,&res);
		CloseHandle(h);
		emit_queue_event(iocp,key,res,overlapped);
	}
}

typedef uintptr_t (*ffithread_callback_t)(uintptr_t in0,uintptr_t in1,uintptr_t out0,uintptr_t out1);

typedef struct{
    HANDLE iocp;
    OVERLAPPED* ovl;
    ffithread_callback_t func;
    uintptr_t in0;
    uintptr_t in1;
} ffithread_data;

static void
thread_ffithread(void* p){
    uintptr_t out0;
    uintptr_t out1;
    uintptr_t continue_p;
    ffithread_data* x = (ffithread_data *)p;
    for(;;){
        continue_p = x->func(x->in0,x->in1,&out0,&out1);
        emit_queue_event(x->iocp,out0,out1,x->ovl);
        if(!continue_p){
            free(p);
            break;
        }
    }
}


void
win32_invoke_ffithread(HANDLE iocp,uintptr_t func,uintptr_t in0,uintptr_t in1,uintptr_t overlapped){
    ffithread_data* p = malloc(sizeof(ffithread_data));
    p->iocp = iocp;
    p->ovl = overlapped;
    p->in0 = in0;
    p->in1 = in1;
    p->func = func;
    _beginthread(thread_ffithread,0,p);
}

static void
invoke_thread_waiter(HANDLE h, HANDLE iocp, uintptr_t key,uintptr_t overlapped){
	thread_waiter_param* param;
	param = (thread_waiter_param *)malloc(sizeof(thread_waiter_param));
	param->h = h;
	param->iocp = iocp;
	param->key = key;
	param->overlapped = overlapped;
	_beginthread(thread_waiter,0,param);
}

typedef struct {
    // N.B. should synced with OVPAIR
    OVERLAPPED ovl;
    void* ptr;

    HANDLE iocp;
    void* key;
} finalization_handler_data;

static void
finalizer(void* obj, void* userdata){
    finalization_handler_data* d=(finalization_handler_data *)userdata;
    emit_queue_event(d->iocp,(uintptr_t)d->key,0,(uintptr_t)d->ptr);
}

void*
win32_finalization_handler_get(void){
    return finalizer;
}

void*
win32_finalization_handler_create(void* iocp, void* key, void* ptr){
    finalization_handler_data* d;
    d = (finalization_handler_data *)GC_MALLOC_UNCOLLECTABLE(sizeof(finalization_handler_data));
    ZeroMemory(&d->ovl,sizeof(OVERLAPPED));
    d->iocp = (HANDLE)iocp;
    d->key = key;
    d->ptr = ptr;
    return d;
}

int
win32_process_wait_async(uintptr_t h,uintptr_t iocp,uintptr_t key, uintptr_t overlapped){
	ZeroMemory((OVERLAPPED*)overlapped,sizeof(OVERLAPPED));
	invoke_thread_waiter((HANDLE)h,(HANDLE)iocp,key,overlapped);
	return 1;
}

#if 0
// Vista or later only...
int 
win32_cancelioex(void* h,void* ovl){
	return CancelIoEx((HANDLE)h,(OVERLAPPED *)ovl);
}
#endif

#if 0
int
win32_process_get_result(void* process){
	BOOL r;
	DWORD res;
	GetExitCodeProcess((HANDLE)process,&res);
	CloseHandle((HANDLE)process);
	return res;
}
#endif

/* windows sockets */
int
win32_sockaddr_storage_size(void){
	return sizeof(SOCKADDR_STORAGE);
}

// mode = 0 .. default
//        4 .. IPv4
//        6 .. IPv6
// proto = 0 .. ERR
//         1 .. TCP
//         2 .. UDP
uintptr_t
win32_socket_create(int mode,int proto,uintptr_t ret_connectex,uintptr_t ret_acceptex){
	int aaf;
	int atype;
	int aproto;
	GUID guidConnectEx = WSAID_CONNECTEX;
	GUID guidAcceptEx = WSAID_ACCEPTEX;
	DWORD bogus;
	DWORD one = 1;
	uintptr_t ret;
	switch(mode){
	case 0:
		aaf = AF_UNSPEC;
		break;
	case 4:
		aaf = AF_INET;
		break;
	case 6:
		aaf = AF_INET6;
		break;
	}
	switch(proto){
	case 0: // wrong
		atype = 0;
		aproto = 0;
		break;
	case 1:
		atype = SOCK_STREAM;
		aproto = IPPROTO_TCP;
		break;
	case 2:
		atype = SOCK_DGRAM;
		aproto = IPPROTO_UDP;
		break;
	}	
	ret = (uintptr_t)socket(aaf,atype,aproto);

	// Mark as non-blocking
	ioctlsocket(ret,FIONBIO,&one);

	if(atype == SOCK_STREAM){
		WSAIoctl(ret,SIO_GET_EXTENSION_FUNCTION_POINTER,&guidConnectEx,sizeof(GUID),(void *)ret_connectex,sizeof(void*),&bogus,NULL,NULL);
		WSAIoctl(ret,SIO_GET_EXTENSION_FUNCTION_POINTER,&guidAcceptEx,sizeof(GUID),(void *)ret_acceptex,sizeof(void*),&bogus,NULL,NULL);
	}

	return ret;
}

int
win32_socket_close(uintptr_t s){
	return closesocket((SOCKET)s);
}

// mode = 0 .. default
//        4 .. IPv4
//        6 .. IPv6
// proto = 0 .. default
//         1 .. TCP
//         2 .. UDP
int
win32_getaddrinfo(wchar_t* name,wchar_t* servicename,uintptr_t ret_addrinfoex,int mode,int proto){
	int ret;
	ADDRINFOW aie;
	ZeroMemory(&aie,sizeof(aie));
	switch(mode){
	case 0:
		aie.ai_family = AF_UNSPEC;
		break;
	case 4:
		aie.ai_family = AF_INET;
		break;
	case 6:
		aie.ai_family = AF_INET6;
		break;
	}
	switch(proto){
	case 0:
		aie.ai_socktype = 0;
		aie.ai_protocol = 0;
		break;
	case 1:
		aie.ai_socktype = SOCK_STREAM;
		aie.ai_protocol = IPPROTO_TCP;
		break;
	case 2:
		aie.ai_socktype = SOCK_DGRAM;
		aie.ai_protocol = IPPROTO_UDP;
		break;
	}

	
	ret = GetAddrInfoW(name,servicename,&aie,(PADDRINFOW*)ret_addrinfoex);
	return ret;
}

void
win32_addrinfoex_free(uintptr_t aie){
	FreeAddrInfoW((ADDRINFOW *)aie);
}

void
win32_addrinfoex_read(uintptr_t aie,uintptr_t* ret_family,uintptr_t* ret_sockaddr,uintptr_t* ret_namelen,uintptr_t* ret_next){
	ADDRINFOW *aiep = (ADDRINFOW *)aie;
	switch(aiep->ai_family){
	case AF_INET:
		*ret_family = 4;
		break;
	case AF_INET6:
		*ret_family = 6;
		break;
	default:
		*ret_family = 0;
		break;
	}
	*ret_sockaddr = (uintptr_t)aiep->ai_addr;
	*ret_namelen = (uintptr_t)aiep->ai_addrlen;
	*ret_next = (uintptr_t)aiep->ai_next;
}

int
win32_socket_connect(uintptr_t func,uintptr_t s,uintptr_t saddr,int namelen,uintptr_t overlapped){
	LPFN_CONNECTEX con = (LPFN_CONNECTEX)func;
	BOOL b;
	int err;
	struct sockaddr_in sin;
	int ret;

	// FIXME: we need this API...
	ZeroMemory(&sin,sizeof(sin));
	sin.sin_family = AF_INET;
	ret = bind((SOCKET)s,(const struct sockaddr *)&sin,sizeof(sin));
	ZeroMemory((OVERLAPPED*)overlapped,sizeof(OVERLAPPED));
	b = con((SOCKET)s,(const struct sockaddr *)saddr,namelen,NULL,0,NULL,(OVERLAPPED *)overlapped);
	if(b){
		return 1;
	}else{
		err = WSAGetLastError();
		if(err == ERROR_IO_PENDING){
			return 1;
		}else{
			return 0;
		}
	}
}

int
win32_socket_getsockname(uintptr_t s, uintptr_t buf,int bufsize){
    int size = bufsize;
    return getsockname((SOCKET)s, (struct sockaddr*)buf,&size);
}

int
win32_socket_accept(uintptr_t func,uintptr_t slisten,uintptr_t saccept,uintptr_t buf,int bufsize,uintptr_t overlapped){
	BOOL b;
	LPFN_ACCEPTEX acc = (LPFN_ACCEPTEX)func;
	DWORD len;
	int err;
	int addrlen = sizeof(SOCKADDR_STORAGE)+16;
	int datasize = bufsize - addrlen - addrlen;
	ZeroMemory((OVERLAPPED*)overlapped,sizeof(OVERLAPPED));
	b = acc((SOCKET)slisten,(SOCKET)saccept,(void *)buf,datasize,addrlen,addrlen,&len,(OVERLAPPED *)overlapped);
	if(!b){
		err = WSAGetLastError();
		if(err == ERROR_IO_PENDING){
			return 1;
		}else{
			return 0;
		}
	}else{
		return 1;
	}
}

int
win32_socket_bind(uintptr_t s,uintptr_t name,int namelen){
	int ret;
	ret = bind((SOCKET)s,(const struct sockaddr *)name,namelen);
	if(ret == SOCKET_ERROR){
		return 0;
	}else{
		return 1;
	}
}

int
win32_socket_listen(uintptr_t s,int l){
	return listen((SOCKET)s,(l == 0)?SOMAXCONN:l);
}

/* simple GUI elements */

// DLGTYPE: 0:OK 1: YESNO 2: YESNOCANCEL
// ICONTYPE: 0:OK(INFO), 1:OK(WARN), 2:OK(ERR) 
int
win32_messagebox(wchar_t* caption,wchar_t* msg,int dlgtype,int icontype){
	unsigned int buttontype;
	unsigned int msgtype;

	switch(dlgtype){
	case 0:
	default:
		buttontype = MB_OK;
		break;
	case 1:
		buttontype = MB_YESNO;
		break;
	case 2:
		buttontype = MB_YESNOCANCEL;
		break;
	}
	switch(icontype){
	default:
	case 0:
		msgtype = MB_ICONINFORMATION;
		break;
	case 1:
		msgtype = MB_ICONWARNING;
		break;
	case 2:
		msgtype = MB_ICONHAND;
		break;
	}

	return MessageBoxW(NULL,msg,caption,buttontype|msgtype);
}

// initialize base window classes
#define BASECLASS L"nmosh win32"

typedef struct{
    // N.B. should synced with OVPAIR
    OVERLAPPED ovl;
    void* ptr;

    HANDLE iocp;
    HWND hWnd;
    HDC hBufferDataDC;
    HBITMAP buffer;
    CRITICAL_SECTION cs;
    int enable;
}window_handler_data;

static void
window_handler(void* p){
    MSG msg;
	HWND hWnd;
    window_handler_data* whd = (window_handler_data *)p;

	hWnd = CreateWindowExW(
        0, //exstyle
        BASECLASS,
        L"", // title
        WS_OVERLAPPEDWINDOW, // style
        0,
        0,
        0,
        0,
        NULL,
        NULL,
        GetModuleHandle(0),
        p);
    whd->hWnd = hWnd;
    whd->enable = 1;
    while(GetMessageW(&msg,0,0,0)){
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
        if(!whd->enable) return;
    }
}
static void
post_window_event(window_handler_data* whd,int id,uintptr_t param){
    emit_queue_event(whd->iocp,(uintptr_t)id,(intptr_t)param,(uintptr_t)&whd->ovl);
}

static void
clearbuffer(window_handler_data* whd){
	if(whd->buffer){
		DeleteObject(whd->buffer);
	}
	if(whd->hBufferDataDC){
		DeleteDC(whd->hBufferDataDC);
	}
	whd->buffer = 0;
    whd->hBufferDataDC = 0;
}

// events:
// 0 : create (HWND)
// 1 : destroy (0)
// 2 : close (0)
// 3 : char (c)
// 4 : MouseMove (XY)
// 5 : MouseEvent (XY)
// 6 : VScroll
// 7 : HScroll
// 8 : Key Modifier ON
// 9 : Key Modifier OFF
// 11 : Mouse Event (mouse)
// 20 : SIZE (WH)
// 30 : active
// 31 : inactive
// Gestures
// 40 : L down
// 41 : L up
// 42 : L double click
// 43 : R down
// 44 : R up
// 45 : R double click
// 46 : M down
// 47 : M up
// 48 : M double click
LRESULT CALLBACK
BaseWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam){
    window_handler_data* whd = (window_handler_data *)GetWindowLongPtr(hWnd,GWLP_USERDATA);
    PAINTSTRUCT ps;
    HDC hDC;
    RECT r;
    LPARAM l;
    int w,h;
    BOOL b;

    switch(msg){
        case WM_PAINT:
            hDC = BeginPaint(hWnd,&ps);
            if(whd->hBufferDataDC){
                GetClientRect(hWnd,&r);
                EnterCriticalSection(&whd->cs);
                b = BitBlt(hDC,0,0,r.right,r.bottom,whd->hBufferDataDC,0,0,SRCCOPY);
                if(!b){
                    printf("ERROR = %d\n",GetLastError());
                }
                LeaveCriticalSection(&whd->cs);
            }
            EndPaint(hWnd,&ps);
            return 0;
        case WM_ERASEBKGND:
            return 0;
        case WM_CREATE:
            whd = (window_handler_data *)(((CREATESTRUCT *)lParam)->lpCreateParams);
            SetWindowLongPtrW(hWnd,GWLP_USERDATA,(LONG_PTR)whd);
            post_window_event(whd,0,(uintptr_t)hWnd);
            return 0;
        case WM_DESTROY:
            clearbuffer(whd);
            post_window_event(whd,1,0);
            whd->enable=0;
            return 0;
        case WM_CLOSE:
            post_window_event(whd,2,0);
            return 0;
        case WM_CHAR:
            post_window_event(whd,3,wParam); // UTF-16 keycode
            return 0;
        case WM_MOUSEMOVE:
            post_window_event(whd,4,lParam);
            return 0;
        case WM_LBUTTONDOWN:
            post_window_event(whd,40,lParam);
            return 0;
        case WM_LBUTTONUP:
            post_window_event(whd,41,lParam);
            return 0;
        case WM_LBUTTONDBLCLK:
            post_window_event(whd,42,lParam);
            return 0;
        case WM_RBUTTONDOWN:
            post_window_event(whd,43,lParam);
            return 0;
        case WM_RBUTTONUP:
            post_window_event(whd,44,lParam);
            return 0;
        case WM_RBUTTONDBLCLK:
            post_window_event(whd,45,lParam);
            return 0;
        case WM_MBUTTONDOWN:
            post_window_event(whd,46,lParam);
            return 0;
        case WM_MBUTTONUP:
            post_window_event(whd,47,lParam);
            return 0;
        case WM_MBUTTONDBLCLK:
            post_window_event(whd,48,lParam);
            return 0;
        case WM_MOUSEWHEEL:
            post_window_event(whd,6,GET_WHEEL_DELTA_WPARAM(wParam));
            return 0;
//FIXME: MinGW fix..
#ifndef WM_MOUSEHWHEEL
#define WM_MOUSEHWHEEL 0x20e
#endif
        case WM_MOUSEHWHEEL:
            post_window_event(whd,7,GET_WHEEL_DELTA_WPARAM(wParam));
            return 0;
        case WM_SIZING:
            r = *(RECT *)lParam;
            w = r.right - r.left;
            h = r.bottom - r.top;
            l = (h << 16) + w;
            // Post Sizing event
            post_window_event(whd,20,l);

            // Restore previous state
            GetWindowRect(hWnd,(RECT *)lParam);
            return 1;
        case WM_USER:
            DestroyWindow(hWnd);
            return 0;
        case WM_USER+1:
            SetCursor(NULL);
            return 0;
        case WM_ACTIVATE:
            if(wParam == WA_INACTIVE){
                post_window_event(whd,31,0);
            }else{
                post_window_event(whd,30,0);
            }
            goto do_default;
        // mouse keys
        // keyboard keys
do_default:
        default:
            return DefWindowProcW(hWnd, msg, wParam, lParam);
    }
}

void
win32_window_move(void* hWnd,signed int x,signed int y,signed int w,signed int h){
    MoveWindow((HWND)hWnd,x,y,w,h,TRUE);
}


void
win32_window_fitbuffer(void* p){
    window_handler_data* whd = (window_handler_data *)p;
    RECT r;
    HDC hDC;
    HDC hBufferDataDC;
    HBITMAP buffer;
    HWND hWnd = whd->hWnd;
    GetClientRect(hWnd,&r);

    clearbuffer(whd);
    hBufferDataDC = CreateCompatibleDC(NULL);

    hDC = GetDC(hWnd);
    buffer = CreateCompatibleBitmap(hDC,r.right,r.bottom);
    ReleaseDC(hWnd,hDC);
    SelectObject(hBufferDataDC,buffer);

    whd->hBufferDataDC = hBufferDataDC;
    whd->buffer = buffer;
}

// 0 = not activate, 1 = activate
void
win32_window_show(void* hWnd,int cmd){
    int sw;
    switch(cmd){
    case 1:
        sw = SW_SHOW;
        break;
    case 0:
        sw = SW_SHOWNA;
        break;
    }
    ShowWindow((HWND)hWnd,sw);
	UpdateWindow((HWND)hWnd);
}

void
win32_window_hide(void* hWnd){
    ShowWindow((HWND)hWnd,SW_HIDE);
}

void
win32_window_settitle(void* hWnd,wchar_t* text){
    SetWindowTextW((HWND)hWnd,text);
}

void
win32_window_close(void* hWnd){
    CloseWindow((HWND)hWnd);
}

void
win32_window_destroy(void* hWnd){
    PostMessage((HWND)hWnd,WM_USER,0,0);
}

void
win32_registerwindowclass(void){
    WNDCLASSEXW cls;
    ZeroMemory(&cls,sizeof(cls));
    cls.hbrBackground = (HBRUSH)GetStockObject(NULL_BRUSH);
    cls.cbSize = sizeof(cls); 
    cls.lpfnWndProc = BaseWndProc;
    cls.hInstance = (HINSTANCE)GetModuleHandle(0);
    cls.lpszClassName = L"nmosh win32";
    cls.style = CS_HREDRAW|CS_VREDRAW;

    RegisterClassExW(&cls);
}

// freed by overlapped_free
void*
win32_window_alloc(void){
    return GC_MALLOC_UNCOLLECTABLE(sizeof(window_handler_data));
}

void
win32_window_create(void* iocp,void* overlapped){
    window_handler_data *whd = (window_handler_data *)overlapped;
    whd->iocp = iocp;
    whd->hBufferDataDC = 0;
    whd->buffer = 0;
    InitializeCriticalSection(&whd->cs);
    _beginthread(window_handler,0,overlapped);
}

typedef struct{
    int ignorecount;
    int cmd;
    int valid;
    int x0;
    int y0;
    int x1;
    int y1;
}monitor_enum_state;

static BOOL CALLBACK
monitorenum_handler(HMONITOR hMoni,HDC bogus0,LPRECT lprcMoni,monitor_enum_state* s){
    MONITORINFO mi;
    mi.cbSize = sizeof(mi);
    if(s->ignorecount){
        s->ignorecount--;
        return TRUE; // continue
    }else{
        s->valid = 1;
        if(s->cmd==0){
            s->x0 = lprcMoni->left;
            s->y0 = lprcMoni->top;
            s->x1 = lprcMoni->right;
            s->y1 = lprcMoni->bottom;
        }else{
            GetMonitorInfo(hMoni,&mi);
            s->x0 = mi.rcWork.left;
            s->y0 = mi.rcWork.top;
            s->x1 = mi.rcWork.right;
            s->y1 = mi.rcWork.bottom;
        }
        return FALSE; // stop
    }
}

// cmd 0 = phys area, 1 = work area (i.e. excludes taskbar)
void
win32_getmonitorinfo(int id,int cmd,signed int *valid,signed int *x0,signed int* y0,signed int *x1,signed int *y1){
    monitor_enum_state s;
    s.valid = 0;
    s.ignorecount = id;
    s.cmd = cmd;
    EnumDisplayMonitors(NULL,NULL,(MONITORENUMPROC)monitorenum_handler,(LPARAM)&s);
    *valid = s.valid;
    *x0 = s.x0;
    *y0 = s.y0;
    *x1 = s.x1;
    *y1 = s.y1;
}

/* bitmap drawing */

// FIXME: compose rgn?

static void
performupdate(HDC hDCBuf,HDC hDCSrc,int x0,int y0,int x1,int y1){
    BitBlt(hDCBuf,x0,y0,x1,y1,hDCSrc,x0,y0,SRCCOPY);
}

static void
sendupdate(HWND hWnd,int x0,int y0,int x1,int y1){
    RECT r;
    r.left = x0;
    r.top = y0;
    r.right = x1;
    r.bottom = y1;

    InvalidateRect(hWnd,&r,FALSE);
}

// N.B.: dc should select Buffer surface before call this
void
win32_window_updaterects(void* w,void* dc,int* rects, int count){
    window_handler_data* whd = (window_handler_data *)w;
    HDC hDC = (HDC)dc;
    int i,p;
    XFORM current;
    GetWorldTransform(hDC,&current);
    ModifyWorldTransform(hDC,NULL,MWT_IDENTITY);
    EnterCriticalSection(&whd->cs);
    for(i=0;i!=count;i++){
        p = i*4;
        performupdate(whd->hBufferDataDC,hDC,rects[p],rects[p+1],rects[p+2],rects[p+3]);
    }
    LeaveCriticalSection(&whd->cs);
    for(i=0;i!=count;i++){
        p = i*4;
        sendupdate(whd->hWnd,rects[p],rects[p+1],rects[p+2],rects[p+3]);
    }
    SetWorldTransform(hDC,&current);
}

void*
win32_window_createbitmap(void *w,int x,int y){
    HDC hDC;
    HBITMAP hBitmap;
    window_handler_data *whd = (window_handler_data *)w;
    hDC = GetDC(whd->hWnd);
    hBitmap = CreateCompatibleBitmap(hDC,x,y);
    ReleaseDC(whd->hWnd,hDC);
    return hBitmap;
}

void
win32_window_getwindowrect(void* h,int* x0,int* y0,int* x1,int* y1){
    HWND hWnd = (HWND)h;
    RECT r;
    GetWindowRect(hWnd,&r);
    *x0 = r.left;
    *y0 = r.top;
    *x1 = r.right;
    *y1 = r.bottom;
}

void
win32_window_getclientrect(void* h,int* x0,int* y0,int* x1,int* y1){
    HWND hWnd = (HWND)h;
    RECT r;
    GetClientRect(hWnd,&r);
    *x0 = r.left;
    *y0 = r.top;
    *x1 = r.right;
    *y1 = r.bottom;
}

void
win32_window_clienttoscreen(void* h,int x,int y,int* xr,int *yr){
    HWND hWnd = (HWND)h;
    POINT p;
    p.x = x;
    p.y = y;
    ClientToScreen(hWnd,&p);
    *xr = p.x;
    *yr = p.y;
}

int
win32_window_getclientrect_x(void* h){
    HWND hWnd = (HWND)h;
    RECT r;
    GetClientRect(hWnd,&r);
    return r.right;
}

int
win32_window_getclientrect_y(void* h){
    HWND hWnd = (HWND)h;
    RECT r;
    GetClientRect(hWnd,&r);
    return r.bottom;
}

// Device Context
void*
win32_dc_create(void){
    HDC hDC;
    hDC = CreateCompatibleDC(NULL);
    SetGraphicsMode(hDC,GM_ADVANCED); // enable matrix op
    return hDC;
}

void
win32_dc_dispose(void* d){
    HDC hDC = (HDC)d;
    DeleteDC(hDC);
}

void
win32_dc_selectobject(void* d,void* obj){
    HDC hDC = (HDC)d;
    HGDIOBJ hobj = (HGDIOBJ)obj;
    SelectObject(hDC,hobj);
}

void
win32_dc_transform(void* d,void* m){
    HDC hDC = (HDC)d;
    window_handler_data* whd = (window_handler_data *)d;
    XFORM* xf = (XFORM *)m;
    ModifyWorldTransform(hDC,xf,MWT_RIGHTMULTIPLY);
}

void
win32_dc_settransform(void* d,void* m){
    HDC hDC = (HDC)d;
    XFORM* xf = (XFORM *)m;
    SetWorldTransform(hDC,xf);
}


// Generic GDI Object management(bitmap brush pen font)
void
win32_gdi_deleteobject(void* obj){
    HGDIOBJ hobj = (HGDIOBJ)obj;
    DeleteObject(hobj);
}

void*
win32_pen_create(int w,int r,int g,int b){
    return CreatePen(PS_SOLID,w,RGB(r,g,b));
}

void*
win32_brush_create(int r,int g,int b){
    return CreateSolidBrush(RGB(r,g,b));
}

void*
win32_font_create(int h,int weight,int italicp,wchar_t* face){
    return CreateFontW(h,0,0,0,weight,italicp,0,0,DEFAULT_CHARSET,0,0,0,0,face);
}

// DRAW OPS
// PATH:
 // 90 BEGIN_PATH
 // 91 CLOSE_PATH
 // 1 MOVE [X Y]
 // 2 LINE [X Y]
 // 3 QCURVE [CPX CPY X Y]
 // 4 BCURVE [CP1X CP1Y CP2X CP2Y X Y]
 // 5 ARCTO [X1 Y1 X2 Y2 R]
 // 6 ARC [X Y R startA endA ACLW?]
 // 7 RECT [X Y W H]
// DRAW:
 // 8 FILL
 // 9 STROKE
 // 10 FILLSTROKE
 // -- CLIP
 // -- ISPOINTINPATH [X Y]

// bmpdc will only be used if BLT occur. bmpdc shouldn't be transformed. 
void
win32_dc_draw(void* dc,void* bmpdc,intptr_t* ops,int len){
    int p = 0;
    int* arg;
    HDC hDC = (HDC)dc;
    HDC hBmpDC = (HDC)bmpdc;
    POINT pbuf[3];

    while(p!=len){
        arg = &ops[p];
        switch(arg[0]){
// Path control
 // 90 BEGIN_PATH
 // 91 CLOSE_PATH
        case 90:
            BeginPath(hDC);
            p++;
            continue;
        case 91:
            EndPath(hDC);
            p++;
            continue;

// Primitives
        case 1: //MOVE [X Y]
            MoveToEx(hDC,arg[1],arg[2],NULL);
            p+=3;
            continue;
        case 2: //LINE [X Y]
            LineTo(hDC,arg[1],arg[2]);
            p+=3;
            continue;
        case 3: //QCURVE [CX CY X Y]
            // FIXME: WRONG
            pbuf[0].x = arg[1];
            pbuf[0].y = arg[2];
            pbuf[1].x = arg[1];
            pbuf[1].y = arg[2];
            pbuf[2].x = arg[3];
            pbuf[2].y = arg[4];
            PolyBezierTo(hDC,pbuf,3);
            p+=5;
            continue;
        case 4: //BCURVE [C0X C0Y C1X C1Y X Y]
            pbuf[0].x = arg[1];
            pbuf[0].y = arg[2];
            pbuf[1].x = arg[3];
            pbuf[1].y = arg[4];
            pbuf[2].x = arg[5];
            pbuf[2].y = arg[6];
            PolyBezierTo(hDC,pbuf,3);
            p+=7;
            continue;
 // 5 ARCTO [X1 Y1 X2 Y2 R]
 // 6 ARC [X Y R startA endA ACLW?]
 // 7 RECT [X Y W H]
// DRAW:
 // 8 FILL
        case 8:
            FillPath(hDC);
            p++;
            continue;
 // 9 STROKE
        case 9:
            StrokePath(hDC);
            p++;
            continue;
 // 10 STROKEFILL
        case 10:
            StrokeAndFillPath(hDC);
            p++;
            continue;
// Text:
 // 20 TEXT [X Y LEN TEXT_PTR] // N.B. Text won't use current point
        case 20:
            TextOutW(hDC,arg[2],arg[3],(LPCWSTR)arg[1],arg[4]);
            p+=5;
            continue;
// GDI Local
 // 30 SELECT_OBJECT [PTR]
        case 30:
            SelectObject(hDC,(HGDIOBJ)arg[1]);
            p+=2;
            continue;
// 31 BLT [XD YD X0 Y0 X1 Y1] // N.B. This cannot COPY on same bitmap
        case 31:
            BitBlt(hDC,arg[3],arg[4],arg[5],arg[6],hBmpDC,arg[1],arg[2],
                   SRCCOPY);
            p+=7;
            continue;
        }
    }
}

int // BOOL
win32_dc_measure_text(void* d,wchar_t* str,int len,int* x,int* y){
    BOOL b;
    HDC hDC = (HDC)d;
    SIZE s;
    b = GetTextExtentPoint32W(hDC,str,len,&s);
    *x=s.cx;
    *y=s.cy;
    return b;
}

void
win32_cursor_hide(void* h){
    HWND hWnd = (HWND)h;
    PostMessage(hWnd,WM_USER+1,0,0);
}

/* misc */


// from http://stackoverflow.com/questions/150355/programmatically-find-the-number-of-cores-on-a-machine
int
win32_get_processor_count(void){
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwNumberOfProcessors;
}

int
win32_get_ansi_codepage(void){
    return GetACP();
}

// 1: success, otherwise: error
// N.B.: Do not feed null strings
int
win32_multibyte_to_widechar(int cp, void* input, int input_count, void* output, int output_count, int* output_size){
    int ret;
    ret = MultiByteToWideChar(cp,0,(LPCSTR)input,input_count,(LPWSTR)output,output_count);
    if(!ret){ // error
        *output_size = 0;
        return -1;
    }else{
        *output_size = ret * 2;
        return 1;
    }
}

// 0<: success
// N.B.: Do not feed null strings
int
win32_measure_multibyte_to_widechar(int cp, void* input, int input_count){
    int ret;
    ret = MultiByteToWideChar(cp,0,(LPCSTR)input,input_count,(LPWSTR)NULL,0);
    return ret*2;
}

// 1 = success, otherwise = failure
int
win32_mypath(wchar_t* buf,int len){
    int ret;
    ret = GetModuleFileNameW(NULL,buf,len);
    if(ret==len||ret==0){
        return -1; // fail
    }else{
        return 1;
    }
}

int
win32_setenv(wchar_t* var,wchar_t* val){
    return SetEnvironmentVariableW(var,val);
}

int
win32_querydosdevice(wchar_t* drive,wchar_t* buf,int len){
    return QueryDosDeviceW(drive,buf,len);
}

int
win32_extent_size(int count){
    return sizeof(DWORD)+sizeof(DISK_EXTENT)*count+16/*padding*/;
}

int
win32_extent_get(wchar_t* partition, void* out,int len,int* out_len){
    HANDLE hPartition;
    BOOL b;
	DWORD err;
    hPartition = CreateFileW(partition,GENERIC_READ,FILE_SHARE_READ|FILE_SHARE_WRITE,NULL,OPEN_EXISTING,0,NULL);
	err = GetLastError();
    b = DeviceIoControl(hPartition,
            IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
            NULL,
            0,
            out,
            len,
            out_len,
            NULL);
	err = GetLastError();
    return b;
}

int
win32_extent_disknumber(void* p,int id){
    VOLUME_DISK_EXTENTS* vde = (VOLUME_DISK_EXTENTS*)p;
    return vde->Extents[id].DiskNumber;
}
void
win32_extent_offset(void* p,int id,int *upper,unsigned int *lower){
    VOLUME_DISK_EXTENTS* vde = (VOLUME_DISK_EXTENTS*)p;
    *upper = vde->Extents[id].StartingOffset.u.HighPart;
    *lower = vde->Extents[id].StartingOffset.u.LowPart;
}

void
win32_extent_length(void* p,int id,int *upper,unsigned int *lower){
    VOLUME_DISK_EXTENTS* vde = (VOLUME_DISK_EXTENTS*)p;
    *upper = vde->Extents[id].ExtentLength.u.HighPart;
    *lower = vde->Extents[id].ExtentLength.u.LowPart;
}


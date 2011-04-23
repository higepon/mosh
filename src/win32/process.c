#include <winsock2.h>
#include <mswsock.h>
#include <ws2tcpip.h>

#include <windows.h>
#include <config.h>

#include <process.h>
#include <stddef.h>

#define GC_NO_THREAD_REDIRECTS // we don't need override
#include <gc.h>


#include "aio_win32.h"

#ifndef WSAID_ACCEPTEX
#error Your MinGW/Win32 runtime is too old... Try Mingw-w64 with -m32 option.
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

	ret = CreateIoCompletionPort(h,(HANDLE)iocp,key,0);
	if(ret == NULL){
		// error
		return 0;
	}
	return 1;
}

int
win32_iocp_pop(uintptr_t iocp, intptr_t timeout_in,uintptr_t ret_bytestrans, uintptr_t ret_key, uintptr_t ret_overlapped){
	BOOL b;
	DWORD timeout;
	timeout = (timeout_in == -1)?INFINITE:timeout_in;
	
	
	// GetQueuedCompletionStatus will return status of de-queued I/O ops.
	// So we should handle the result even if we get FALSE here..
	b = GetQueuedCompletionStatus((HANDLE)iocp,(LPDWORD)ret_bytestrans,(PULONG_PTR)ret_key,(LPOVERLAPPED *)ret_overlapped,timeout);
	if(!b){
		return 0;
	}else{
		return 1;
	}
}

// allocate and free OVERLAPPED. OVERLAPPED may passed to non-GC-managed threads.

typedef struct{
    // N.B. should synced with finalization handler
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
win32_handle_read_async(uintptr_t h,uintptr_t offsetL,uintptr_t offsetH,uintptr_t length,uintptr_t buf,uintptr_t ol){
	BOOL b;
	OVERLAPPED* ovl = (OVERLAPPED *)ol;
	ovl->Offset = offsetL;
	ovl->OffsetHigh = offsetH;
	ovl->hEvent = NULL;
	b = ReadFile((HANDLE)h,(void*)buf,length,NULL,ovl);
	if(!b){
		return 0;
	}else{
		return 1;
	}
}

int
win32_handle_write_async(uintptr_t h,uintptr_t offsetL,uintptr_t offsetH,uintptr_t length,uintptr_t buf,uintptr_t ol){
	BOOL b;
	OVERLAPPED* ovl = (OVERLAPPED *)ol;
	ovl->Offset = offsetL;
	ovl->OffsetHigh = offsetH;
	ovl->hEvent = NULL;
	b = WriteFile((HANDLE)h,(void *)buf,length,NULL,ovl);
	if(!b){
		return 0;
	}else{
		return 1;
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
uintptr_t
win32_process_redirected_child2(wchar_t* spec,wchar_t* dir, wchar_t* std_in, wchar_t* std_out, wchar_t* std_err, int in_mode, int out_mode, int err_mode){
	PROCESS_INFORMATION pi;
	STARTUPINFOW si;
	BOOL r;
	HANDLE h;
	HANDLE ex_out, ex_err;
	ex_out = 0;
	ex_err = 0;

	ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
	ZeroMemory(&si, sizeof(STARTUPINFOW));

	si.cb = sizeof(STARTUPINFO);
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
		if(h == INVALID_HANDLE_VALUE) return 0;
		si.hStdOutput = h;
		ex_out = h;
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
	}

	//FIXME: should we use CREATE_NO_WINDOW ?
	r = CreateProcessW(NULL, spec, NULL, NULL, TRUE, 0, NULL, dir, &si, &pi);

	if(ex_out != 0){
		CloseHandle(ex_out);
	}

	if(ex_err != 0){
		CloseHandle(ex_err);
	}

	if (! r){
		OSERROR("CreateProcess");
		return 0;
	}
	CloseHandle(pi.hThread);
	return (uintptr_t)pi.hProcess;
}

uintptr_t
win32_create_named_pipe_async(wchar_t* name){
	HANDLE h;
	h = CreateNamedPipeW(name,PIPE_ACCESS_DUPLEX|FILE_FLAG_OVERLAPPED,PIPE_WAIT,PIPE_UNLIMITED_INSTANCES,4096,4096,0,NULL);
	if(h == INVALID_HANDLE_VALUE){
		OSERROR("CreateNamedPipeW");
		return -1;
	}
	return (uintptr_t)h;
}
int
win32_wait_named_pipe_async(uintptr_t h, uintptr_t ovl){
	BOOL b;
	b = ConnectNamedPipe((HANDLE)h,(OVERLAPPED *)ovl);
	if(!b){
		return 0;
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
	invoke_thread_waiter((HANDLE)h,(HANDLE)iocp,key,overlapped);
	return 1;
}


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

	if(atype == SOCK_STREAM){
		WSAIoctl(ret,SIO_GET_EXTENSION_FUNCTION_POINTER,&guidConnectEx,sizeof(GUID),(void *)ret_connectex,sizeof(void*),&bogus,NULL,NULL);
		WSAIoctl(ret,SIO_GET_EXTENSION_FUNCTION_POINTER,&guidAcceptEx,sizeof(GUID),(void *)ret_acceptex,sizeof(void*),&bogus,NULL,NULL);
	}

	return ret;
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
win32_socket_accept(uintptr_t func,uintptr_t slisten,uintptr_t saccept,uintptr_t buf,int bufsize,uintptr_t overlapped){
	BOOL b;
	LPFN_ACCEPTEX acc = (LPFN_ACCEPTEX)func;
	DWORD len;
	int err;
	int addrlen = sizeof(SOCKADDR_STORAGE)+16;
	int datasize = bufsize - addrlen - addrlen;
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
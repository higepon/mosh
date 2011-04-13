#include <windows.h>
#include <config.h>

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#include <process.h>
#include <stddef.h>

#include "aio_win32.h"

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
void*
win32_overlapped_alloc(void){
	return malloc(sizeof(OVERLAPPED)); // FIXME: should be aligned ??
}

void
win32_overlapped_free(void* p){
	free(p);
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
	h = CreateNamedPipeW(name,PIPE_ACCESS_DUPLEX|FILE_FLAG_OVERLAPPED,PIPE_WAIT,PIPE_UNLIMITED_INSTANCES|PIPE_REJECT_REMOTE_CLIENTS,4096,4096,0,NULL);
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
} thread_waiter_param;


static void
emit_queue_event(HANDLE iocp,uintptr_t key,intptr_t res){
	PostQueuedCompletionStatus(iocp,res,key,NULL);
}

static void
thread_waiter(void* p){
	thread_waiter_param* param = (thread_waiter_param *)p;
	BOOL r;
	DWORD res;
	HANDLE h;
	HANDLE iocp;
	uintptr_t key;
	h = param->h;
	iocp = param->iocp;
	key = param->key;
	free(param);

	r = WaitForSingleObject(h,INFINITE);
	if(WAIT_FAILED == r){
		OSERROR("WaitForSingleObject");
		emit_queue_event(iocp,key,-1);
	}else{
		GetExitCodeProcess(h,&res);
		CloseHandle(h);
		emit_queue_event(iocp,key,res);
	}
}

static void
invoke_thread_waiter(HANDLE h, HANDLE iocp, uintptr_t key){
	thread_waiter_param* param;
	param = (thread_waiter_param *)malloc(sizeof(thread_waiter_param));
	param->h = h;
	param->iocp = iocp;
	param->key = key;
	_beginthread(thread_waiter,0,param);
}

int
win32_process_wait_async(uintptr_t h,uintptr_t iocp,uintptr_t key){
	invoke_thread_waiter((HANDLE)h,(HANDLE)iocp,key);
	return 1;
}

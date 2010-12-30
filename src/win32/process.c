#include <windows.h>

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
	if(!r){
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



// process stub

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

// predef
extern "C" int win32_process_pipe(uintptr_t* p);
extern "C" uintptr_t win32_process_redirected_child(uint8_t* p0,uint8_t* p1, uintptr_t p2, uintptr_t p3, uintptr_t p4);
extern "C" int win32_handle_read(uintptr_t p0,void* p1,unsigned int len, unsigned int *res);
extern "C" int win32_handle_write(uintptr_t p0,void* p1,unsigned int len, unsigned int *res);
extern "C" int win32_handle_close(uintptr_t p0);
extern "C" int win32_handle_wait(uintptr_t p0);
extern "C" int win32_process_wait(uintptr_t p);
extern "C" uintptr_t win32_create_named_pipe(uint8_t* name);
extern "C" int win32_wait_named_pipe(uintptr_t p);

Object
stub_win32_process_pipe(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("%win32_process_pipe");
    uintptr_t inout0[2];
	checkArgumentLength(0);

	int r = win32_process_pipe(inout0);

	if(r){
		return theVM->values2(Object::makePointer((void*)inout0[0]),Object::makePointer((void*)inout0[1]));
	}else{
		return theVM->values2(Object::False,Object::False);
	}
}

Object
stub_win32_process_redirected_child(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("%win32_process_redirected_child");
	checkArgumentLength(5);
	argumentAsByteVector(0,bv0);
	argumentAsByteVector(1,bv1);
	argumentAsPointer(2,p1);
	argumentAsPointer(3,p2);
	argumentAsPointer(4,p3);
	uint8_t* pos;

	if(bv1->length() == 0){
		pos = 0;
	}else{
		pos = bv1->data();
	}

	void* p = (void *)win32_process_redirected_child(bv0->data(),pos,p1->pointer(),p2->pointer(),p3->pointer());

	if(p){
		return Object::makePointer(p);
	}else{
		return Object::False;
	}
}

Object
stub_win32_handle_read(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("win32_handle_read");
	unsigned int res;
	checkArgumentLength(3);
	argumentAsPointer(0,p0);
	argumentAsByteVector(1,bv);
	argumentAsFixnum(2,len); // FIXME!!
	int r = win32_handle_read(p0->pointer(),bv->data(),len,&res);
	if(r){
		return Object::makeFixnum(res);
	}else{
		return Object::False;
	}
}

Object
stub_win32_handle_write(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("win32_handle_write");
	unsigned int res;
	checkArgumentLength(3);
	argumentAsPointer(0,p0);
	argumentAsByteVector(1,bv);
	argumentAsFixnum(2,len); // FIXME!!
	int r = win32_handle_write(p0->pointer(),bv->data(),len,&res);
	if(r){
		return Object::makeFixnum(res);
	}else{
		return Object::False;
	}
}

Object
stub_win32_handle_close(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("win32_handle_close");
	checkArgumentLength(1);
	argumentAsPointer(0,p0);
	int r = win32_handle_close(p0->pointer());
	if(r){
		return Object::True;
	}else{
		return Object::False;
	}
}

Object
stub_win32_process_wait(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("win32_process_wait");
	checkArgumentLength(1);
	argumentAsPointer(0,p);
	int r = win32_process_wait(p->pointer());
        return Object::makeFixnum(r);
}

Object
stub_win32_handle_wait(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("win32_handle_wait");
	checkArgumentLength(1);
	argumentAsPointer(0,p0);
	int r = win32_handle_wait(p0->pointer());
	if(r){
		return Object::True;
	}else{
		return Object::False;
	}
}

Object
stub_win32_named_pipe_create(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("%win32_named_pipe_create");
	checkArgumentLength(1);
	argumentAsByteVector(0,bv);

	void* p = (void *)win32_create_named_pipe(bv->data());

	if(p){
		return Object::makePointer(p);
	}else{
		return Object::False;
	}
}

Object
stub_win32_named_pipe_wait(VM* theVM, int argc, const Object* argv){
	DeclareProcedureName("win32_named_pipe_wait");
	checkArgumentLength(1);
	argumentAsPointer(0,p0);
	int r = win32_wait_named_pipe(p0->pointer());
	if(r){
		return Object::True;
	}else{
		return Object::False;
	}
}


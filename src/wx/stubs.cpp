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

// resources
Object stub_wx_init(VM* theVM, int argc, const Object* argv);
Object stub_wx_loadresource(VM* theVM, int argc, const Object* argv);
Object stub_wx_freeresource(VM* theVM, int argc, const Object* argv);

// frames
Object stub_wx_frame_new(VM* theVM, int argc, const Object* argv);
Object stub_wx_frame_delete(VM* theVM, int argc, const Object* argv);
Object stub_wx_frame_show(VM* theVM, int argc, const Object* argv);
Object stub_wx_frame_raise(VM* theVM, int argc, const Object* argv);

void
wx_register_stubs(VM* theVM){
    // resources
	theVM->setValueString(UC("%wx_init"),Object::makeCProcedure(stub_wx_init));
    theVM->setValueString(UC("%wx_loadresource"),Object::makeCProcedure(stub_wx_loadresource));
    theVM->setValueString(UC("%wx_freeresource"),Object::makeCProcedure(stub_wx_freeresource));

	// frames
	theVM->setValueString(UC("%wx_frame_new"),Object::makeCProcedure(stub_wx_frame_new));
	theVM->setValueString(UC("%wx_frame_delete"),Object::makeCProcedure(stub_wx_frame_delete));
	theVM->setValueString(UC("%wx_frame_show"),Object::makeCProcedure(stub_wx_frame_show));
	theVM->setValueString(UC("%wx_frame_raise"),Object::makeCProcedure(stub_wx_frame_raise));
}

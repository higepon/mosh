// wxWidgets XML Resources handlers
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#   pragma hdrstop
#endif

#ifndef WX_PRECOMP
    // Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif
#include <wx/xrc/xmlres.h>

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

Object
stub_wx_frame_new(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_new");
    checkArgumentLength(0);
    wxFrame* wf;
	wf = new wxFrame(NULL,-1,"skyMosh",wxDefaultPosition,wxDefaultSize,wxDEFAULT_FRAME_STYLE,"skyframe");
    return Object::makePointer(wf);
}

Object
stub_wx_frame_delete(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_delete");
    checkArgumentLength(1);
    argumentAsPointer(0,p);
    delete (wxFrame *)p->pointer();
    return Object::True;
}

Object
stub_wx_frame_show(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_show");
    checkArgumentLength(1);
    argumentAsPointer(0,p);
    wxFrame* wf;
	wf = (wxFrame *)p->pointer();
	wf->Show();
    return Object::True;
}

Object
stub_wx_frame_raise(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_raise");
    checkArgumentLength(1);
    argumentAsPointer(0,p);
    wxFrame* wf;
	wf = (wxFrame *)p->pointer();
	wf->Raise();
    return Object::True;
}

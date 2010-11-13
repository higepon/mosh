// wxWidgets XML Resources handlers
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#   pragma hdrstop
#endif

#ifndef WX_PRECOMP
    // Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif

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

class skyFrame : public wxFrame {
public:
	skyFrame(VM* creatorVM):wxFrame(NULL,-1,"skyMosh",wxDefaultPosition,wxDefaultSize,wxDEFAULT_FRAME_STYLE,"skyframe"){
		// TODO: Default handlers??
		theVM = creatorVM;
		sizeEventListener = Object::False;
	};

	// Listeners
	Object sizeEventListener;


	void OnSize(wxSizeEvent& e){
		if(sizeEventListener != Object::False){
			wxSize s = e.GetSize();
			int x = s.GetWidth();
			int y = s.GetHeight();
			theVM->apply(sizeEventListener,Pair::list2(Object::makeFixnum(x),Object::makeFixnum(y)));
		}
	};

	DECLARE_EVENT_TABLE();

private:
	VM* theVM;
};

BEGIN_EVENT_TABLE(skyFrame, wxFrame)
	EVT_SIZE(skyFrame::OnSize)
END_EVENT_TABLE()

Object
stub_wx_frame_setlistener_size(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_setlistener_size");
    checkArgumentLength(2);
	argumentAsPointer(0,p);
	argumentCheckProcedure(1,cls);
	skyFrame* sf = (skyFrame *)p->pointer();
	sf->sizeEventListener = cls;
	return Object::True;
}

Object
stub_wx_frame_new(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_new");
    checkArgumentLength(0);
    skyFrame* sf;
	sf = new skyFrame(theVM);
    return Object::makePointer(sf);
}

Object
stub_wx_frame_delete(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_delete");
    checkArgumentLength(1);
    argumentAsPointer(0,p);
    delete (skyFrame *)p->pointer();
    return Object::True;
}

Object
stub_wx_frame_show(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_show");
    checkArgumentLength(1);
    argumentAsPointer(0,p);
    skyFrame* sf;
	sf = (skyFrame *)p->pointer();
	sf->Show();
    return Object::True;
}

Object
stub_wx_frame_raise(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_frame_raise");
    checkArgumentLength(1);
    argumentAsPointer(0,p);
    skyFrame* sf;
	sf = (skyFrame *)p->pointer();
	sf->Raise();
    return Object::True;
}


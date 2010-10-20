// wxWidgets XML Resources handlers
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#   pragma hdrstop
#endif

#ifndef WX_PRECOMP
    // Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#include <wx/xrc/xmlres.h>
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

Object
stub_wx_init(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_init");
    checkArgumentLength(0);
    wxXmlResource::Get()->InitAllHandlers();
    return Object::True;
}

Object
stub_wx_loadresource(VM* theVM, int argc, const Object* argv){
    void* p;
    DeclareProcedureName("%wx_loadresource");
    checkArgumentLength(1);
    argumentAsString(0,name);
    p = (void *)new wxXmlResource((const wxString)name->data().ascii_c_str(),wxXRC_USE_LOCALE,"");
    return Object::makePointer(p);
}

Object
stub_wx_freeresource(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%wx_freeresource");
    checkArgumentLength(1);
    argumentAsPointer(0,p);
    delete (wxXmlResource *)p->pointer();
    return Object::True;
}

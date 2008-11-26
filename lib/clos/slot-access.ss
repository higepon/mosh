; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************
;
; port to R6RS -- 2007 Christian Sloma
; 

(library (clos slot-access)
  
  (export slot-ref
          slot-set!
          register-class-of-classes!)
  
  (import (rnrs)
          (clos private allocation)
          (clos private core-class-layout)
          (clos private compat))

  (define <class> #f)

  (define (register-class-of-classes! class)
    (set! <class> class))
  
  (define (slot-ref inst slot-name)
    (let ((class (instance-class inst)))
      (if (eq? class <class>) ;; break loop -- assumes <class> does not change
          (instance-ref inst (position slot-name core-class-slot-names))
          (let ((slot-info (get-slot-info class slot-name)))
            ((cadr slot-info) inst)))))
  
  (define (slot-set! inst slot-name val)
    (let ((class (instance-class inst)))
      (if (eq? class <class>) ;; break loop -- assumes <class> does not change
          (instance-set! inst (position slot-name core-class-slot-names) val)
          (let ((slot-info (get-slot-info class slot-name)))
            ((caddr slot-info) inst val)))))
  
  (define (get-slot-info class slot-name)
    (let ((getters-and-setters (slot-ref class 'getters-and-setters)))
      (assq slot-name getters-and-setters)))
  
  ) ;; library (clos slot-access)

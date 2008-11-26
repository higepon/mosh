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

(library (clos std-protocols add-method)
  
  (export generic-add-method)
  
  (import (rnrs)
          (clos private allocation)
          (clos private method-cache)
          (clos std-protocols generic-invocation)
          (clos slot-access)
          (clos introspection)
          (clos private compat))
  
  (define (generic-add-method generic method
                              compute-apply-generic)
    (slot-set! generic
               'methods
               (merge-new-method method (slot-ref generic 'methods)))
    (if (generic-invocation-generic? generic)
        (invalidate-method-caches!))
    (set-instance-proc! generic (compute-apply-generic generic)))
  
  (define (merge-new-method new-method methods)
    (cons new-method
          (filter (lambda (method)
                    (not (and (every eq?
                                     (method-specializers new-method)
                                     (method-specializers method))
                              (eq? (method-qualifier new-method)
                                   (method-qualifier method)))))
                  methods)))
 
  ) ;; library (clos std-protocols add-method)

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

(library (clos introspection)

  (export set-primitive-class-of!
          class-of
          class-direct-supers
          class-direct-slots
          class-precedence-list
          class-slots
          class-definition-name
          generic-methods
          method-specializers
          method-qualifier
          method-procedure)
  
  (import (rnrs)
          (clos private allocation)
          (clos slot-access))

  (define primitive-class-of #f)
  
  (define (set-primitive-class-of! proc)
    (set! primitive-class-of proc))
  
  (define (class-of obj)
    (if (instance? obj)    
        (instance-class obj)
        (primitive-class-of obj)))
  
  (define (class-direct-supers class)
    (slot-ref class 'direct-supers))
  
  (define (class-direct-slots class)
    (slot-ref class 'direct-slots))
  
  (define (class-precedence-list class)
    (slot-ref class 'precedence-list))
  
  (define (class-slots class)
    (slot-ref class 'slots))

  (define (class-definition-name class)
    (slot-ref class 'definition-name))
  
  (define (generic-methods generic)
    (slot-ref generic 'methods))
  
  (define (method-specializers method)
    (slot-ref method 'specializers))
  
  (define (method-qualifier method)
    (slot-ref method 'qualifier))

  (define (method-procedure method)
    (slot-ref method 'procedure))
  
  ) ;; library (clos introspection)

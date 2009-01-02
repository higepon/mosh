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

(library (clos helpers)

  (export unmangle-class-name
          print-unreadable-object
          print-object-with-slots
          initialize-direct-slots)
  
  (import (only (rnrs) define define-syntax syntax-rules let if and >= string-length char=? string-ref - ... lambda
                string->symbol substring symbol->string display when write or quote null? not caar cdr
                )
          (clos introspection)
          (clos slot-access)
          (clos private compat))

  (define (unmangle-class-name class-name)
    (let ((str (symbol->string class-name)))
      (if (and (>= (string-length str) 3)
               (char=? (string-ref str 0) #\<)
               (char=? (string-ref str (- (string-length str) 1)) #\>))
          (string->symbol (substring str 1  (- (string-length str) 1)))
          class-name)))
  
  (define (print-unreadable-object* port type? addr? obj thunk)
    (display "#<" port)
    (when type? 
      (write (unmangle-class-name 
                 (or (class-definition-name (class-of obj)) 'unknown))
               port)
      (display " " port))
    (thunk)
    (when addr?
      (display "{" port)
      (write (pointer-value obj) port)
      (display "}" port))
    (display ">" port))

  (define-syntax print-unreadable-object
    (syntax-rules ()
      ((print-unreadable-object (?port ?type? ?addr? ?obj) ?body ...)
       (print-unreadable-object* ?port 
                                 ?type? 
                                 ?addr? 
                                 ?obj 
                                 (lambda () ?body ... 'ignored)))))

  (define (print-object-with-slots obj port)
    (print-unreadable-object (port #t #t obj)
      (let loop ((slots (class-slots (class-of obj))))
        (when (not (null? slots))
          (write (caar slots) port)
          (display ": " port)
          (write (slot-ref obj (caar slots)) port)
          (display " " port)
          (loop (cdr slots))))))

  (define (initialize-direct-slots obj cls init-args)
    (let loop ((slots (class-direct-slots cls)))
      (when (not (null? slots))
        (slot-set! obj (caar slots) (get-arg (caar slots) init-args))
        (loop (cdr slots)))))
  
  ) ;; library (clos helpers)

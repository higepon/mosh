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

(library (clos private allocation)
  
  (export really-allocate-instance
          really-allocate-entity-instance
          instance?
          instance-class
          set-instance-class-to-self!
          set-instance-proc!
          instance-ref
          instance-set!
          set-entity-print-name!
          set-instance-printer!)
  
  (import (only (rnrs) define-record-type define lambda opaque sealed fields mutable case-lambda apply if
                make-eq-hashtable make-vector let* hashtable-set! values and procedure? hashtable-ref vector-ref
                vector-set! quote))
  
  (define-record-type instance-record
    (opaque #t)
    (sealed #t)
    (fields (mutable class) slots (mutable iproc)))
  
  (define *entity-table* (make-eq-hashtable))
  
  (define (really-allocate-instance class field-count)
    (make-instance-record class (make-vector field-count) #f))

  (define (really-allocate-entity-instance class field-count)
    (let* ((inst (really-allocate-instance class field-count))
           (proc (case-lambda 
                   (()
                    ((instance-record-iproc inst)))
                   ((a)
                    ((instance-record-iproc inst) a))
                   ((a b)
                    ((instance-record-iproc inst) a b))
                   ((a b c)
                    ((instance-record-iproc inst) a b c))
                   ((a b c d)
                    ((instance-record-iproc inst) a b c d))
                   (args
                    (apply (instance-record-iproc inst) args)))))
      (hashtable-set! *entity-table* proc inst)
      (values proc)))
  
  (define (get-instance-record obj)
    (if (instance-record? obj)
        obj
        (and (procedure? obj)
             (hashtable-ref *entity-table* obj #f))))
  
  (define (instance? obj)
    (if (get-instance-record obj) #t #f))
  
  (define (instance-class inst)
    (instance-record-class (get-instance-record inst)))
  
  (define (set-instance-class-to-self! inst)
    (instance-record-class-set! (get-instance-record inst) inst))
  
  (define (set-instance-proc! inst proc)
    (instance-record-iproc-set! (get-instance-record inst) proc))
  
  (define (instance-ref inst idx)
    (vector-ref (instance-record-slots (get-instance-record inst)) idx))
  
  (define (instance-set! inst idx val)
    (vector-set! (instance-record-slots (get-instance-record inst)) idx val))

  (define (set-entity-print-name! proc symbol)
    'ignore)

  (define (set-instance-printer! proc)
    'ignore)
  
  ) ;; library (clos private allocation)

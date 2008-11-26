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

(library (clos std-protocols initialize)
  
  (export class-initialize
          generic-initialize
          method-initialize)
  
  (import (rnrs)
          (clos private allocation)
          (clos private compat)
          (clos slot-access))
  
  (define (class-initialize class-inst init-args
                            compute-precedence-list
                            compute-slots
                            compute-getter-and-setter)
    (slot-set! class-inst
               'direct-supers
               (get-arg 'direct-supers init-args))
    (slot-set! class-inst
               'direct-slots 
               (map (lambda (slot)
                      (if (list? slot) 
                          slot
                          (list slot)))
                    (get-arg 'direct-slots init-args)))
    (slot-set! class-inst
               'definition-name
               (get-arg 'definition-name init-args #f))
    (slot-set! class-inst
               'precedence-list
               (compute-precedence-list class-inst))
    (slot-set! class-inst
               'slots
               (compute-slots class-inst))
    
    (let* ((field-count 0)
           (field-inits '())
           (allocator
            (lambda (init-fn)
              (let ((idx field-count))
                (set! field-count (+ field-count 1))
                (set! field-inits (cons init-fn field-inits))
                (list (lambda (inst)
                        (instance-ref inst idx))
                      (lambda (inst val)
                        (instance-set! inst idx val))))))
           (getters-and-setters
            (map (lambda (slot)
                   (cons (car slot)
                         (compute-getter-and-setter class-inst
                                                    slot
                                                    allocator)))
                 (slot-ref class-inst 'slots))))
      
      (slot-set! class-inst
                 'number-of-fields
                 field-count)
      (slot-set! class-inst
                 'field-initializers
                 (reverse field-inits))
      (slot-set! class-inst
                 'getters-and-setters
                 getters-and-setters)))
  
  (define (generic-initialize generic-inst init-args)
    (slot-set! generic-inst 
               'methods
               '())
    (set-entity-print-name! generic-inst
                            (get-arg 'definition-name init-args #f))
    (set-instance-proc! generic-inst
                        (lambda args
                          (error 'generic
                                 "generic called without method installed"))))
  
  (define (method-initialize meth-inst init-args)
    (slot-set! meth-inst
               'specializers
               (get-arg 'specializers init-args))
    (slot-set! meth-inst
               'qualifier
               (get-arg 'qualifier init-args 'primary))
    (slot-set! meth-inst
               'procedure
               (get-arg 'procedure init-args)))
  
  ) ;; library (clos std-protocols initialize)

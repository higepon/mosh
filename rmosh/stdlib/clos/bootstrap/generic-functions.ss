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

(library (clos bootstrap generic-functions)

  (export make
          initialize
          allocate-instance
          compute-getter-and-setter
          compute-precedence-list
          compute-slots
          add-method
          compute-apply-generic
          compute-methods
          compute-method-more-specific?
          compute-apply-methods
          print-object)

  (import (only (rnrs) define quote list lambda car cdr begin let cond eq? else error)
          (only (clos bootstrap standard-classes) bootstrap-make <method> <class> <entity-class> <object>
                <generic>)
          (only (clos private allocation) set-instance-printer!)
          (only (clos introspection) class-of)
          (only (clos std-protocols make) class-make)
          (only (clos std-protocols allocate-instance) class-allocate-instance entity-class-allocate-instance)
          (only (clos std-protocols initialize) class-initialize generic-initialize method-initialize)
          (only (clos std-protocols class-initialization) class-compute-precedence-list class-compute-slots class-compute-getter-and-setter)
          (only (clos std-protocols add-method) generic-add-method)
          (only (clos std-protocols generic-invocation) generic-compute-methods generic-compute-apply-generic generic-compute-method-more-specific?
                generic-compute-apply-methods register-generic-invocation-generics!)
          (only (clos std-protocols print-object) object-print-object))

  (define make
    (bootstrap-make <generic>
      'definition-name 'make))
  
  (define initialize                    
    (bootstrap-make <generic>
      'definition-name 'initialize))
  
  (define allocate-instance             
    (bootstrap-make <generic>
      'definition-name 'allocate-instance))
  
  (define compute-getter-and-setter     
    (bootstrap-make <generic>
      'definition-name 'compute-getter-and-setter))
  
  (define compute-precedence-list       
    (bootstrap-make <generic>
      'definition-name 'compute-precedence-list))
  
  (define compute-slots                 
    (bootstrap-make <generic>
      'definition-name 'compute-slots))
  
  (define add-method
    (bootstrap-make <generic>
      'definition-name 'add-method))
  
  (define compute-apply-generic         
    (bootstrap-make <generic>
      'definition-name 'compute-apply-generic))
  
  (define compute-methods               
    (bootstrap-make <generic>
      'definition-name 'compute-methods))
  
  (define compute-method-more-specific? 
    (bootstrap-make <generic>
      'definition-name 'compute-method-more-specific?))
  
  (define compute-apply-methods         
    (bootstrap-make <generic>
      'definition-name 'compute-apply-methods))

  (define print-object 
    (bootstrap-make <generic>
      'definition-name 'print-object))

  (define bootstrap-add-method 
    (begin

      (register-generic-invocation-generics! 
       compute-apply-generic
       compute-apply-methods
       compute-methods
       compute-method-more-specific?)

      (lambda (entity method)
        (let ((class (class-of entity)))
          (cond 
            ((eq? class <generic>)
             (generic-add-method entity method generic-compute-apply-generic))
            (else
             (error 'bootstrap-add-method
                    "cannot add method to instance of class ~a" class)))))))

  (bootstrap-add-method make
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (%generic %next-methods class . init-args)
                      (class-make class init-args 
                                  allocate-instance initialize))))

  (bootstrap-add-method allocate-instance
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (%generic %next-methods class)
                      (class-allocate-instance class))))

  (bootstrap-add-method allocate-instance 
    (bootstrap-make <method>
      'specializers (list <entity-class>)
      'procedure    (lambda (%generic %next-methods entity-class)
                      (entity-class-allocate-instance entity-class))))

  (bootstrap-add-method initialize 
    (bootstrap-make <method>
      'specializers (list <object>)
      'procedure    (lambda (%generic %next-methods object init-args) object)))

  (bootstrap-add-method initialize
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (%generic %next-methods class-inst init-args)
                      ;; call-next-method, the hard way ...
                      ((car %next-methods) %generic (cdr %next-methods) class-inst init-args)
                      (class-initialize class-inst init-args
                                        compute-precedence-list
                                        compute-slots
                                        compute-getter-and-setter))))

  (bootstrap-add-method initialize
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (%generic %next-methods generic-inst init-args)
                      ;; call-next-method, the hard way ...
                      ((car %next-methods) %generic (cdr %next-methods) generic-inst init-args)
                      (generic-initialize generic-inst init-args))))

  (bootstrap-add-method initialize 
    (bootstrap-make <method>
      'specializers (list <method>)
      'procedure    (lambda (%generic %next-methods method-inst init-args)
                      ;; call-next-method, the hard way ...
                      ((car %next-methods) %generic (cdr %next-methods) method-inst init-args)
                      (method-initialize method-inst init-args))))

  (bootstrap-add-method compute-precedence-list
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (%generic %next-methods class)
                      (class-compute-precedence-list class))))

  (bootstrap-add-method compute-slots 
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (%generic %next-methods class)
                      (class-compute-slots class))))

  (bootstrap-add-method compute-getter-and-setter
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (%generic %next-methods class slot allocator)
                      (class-compute-getter-and-setter class slot allocator))))

  (bootstrap-add-method add-method
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (%generic %next-methods entity method)
                      (generic-add-method entity method compute-apply-generic))))

  (bootstrap-add-method compute-apply-generic
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (%generic %next-methods generic)
                      (generic-compute-apply-generic generic))))

  (bootstrap-add-method compute-methods
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (%generic %next-methods generic args)
                      (generic-compute-methods generic args))))

  (bootstrap-add-method compute-method-more-specific?
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (%generic %next-methods generic args)
                      (generic-compute-method-more-specific? generic args))))

  (bootstrap-add-method compute-apply-methods
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (%generic %next-methods generic methods)
                      (generic-compute-apply-methods generic methods))))

  (bootstrap-add-method print-object
    (bootstrap-make <method>
      'specializers (list <object>)
      'procedure    (lambda (%generic %next-methods object port)
                      (object-print-object object port))))

  (set-instance-printer! print-object)

  ) ;; library (clos bootstrap generic-functions)

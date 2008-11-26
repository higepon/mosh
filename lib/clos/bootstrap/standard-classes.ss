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

(library (clos bootstrap standard-classes)

  (export <class>
          <top>
          <object>
          <procedure-class>
          <entity-class>
          <generic>
          <method>
          <pair>
          <null>
          <symbol>
          <boolean>
          <procedure>
          <number>
          <vector>
          <char>
          <string>
          <input-port>
          <output-port>
          bootstrap-make)

  (import (rnrs)
          (clos private allocation)
          (clos private core-class-layout)
          (clos slot-access)
          (clos introspection)
          (clos std-protocols make)
          (clos std-protocols allocate-instance)
          (clos std-protocols initialize)
          (clos std-protocols class-initialization))

  (define <class>  
    (really-allocate-instance 'ignore core-class-slot-count))

  (define <top>    
    (really-allocate-instance <class> core-class-slot-count))

  (define <object> 
    (really-allocate-instance <class> core-class-slot-count))

  (define bootstrap-initialize
    (begin 

      (set-instance-class-to-self! <class>)
      (register-class-of-classes!  <class>)

      (lambda (inst init-args)
        (let ((class (class-of inst)))
          (cond
            ((or (eq? class <class>)
                 (eq? class <procedure-class>)
                 (eq? class <entity-class>)
                 (eq? class <primitive-class>))
             (class-initialize inst init-args
                               class-compute-precedence-list
                               class-compute-slots
                               class-compute-getter-and-setter))
            ((eq? class <generic>)
             (generic-initialize inst init-args))
            ((eq? class <method>)
             (method-initialize inst init-args))
            (else
             (error 'bootstrap-initialize 
                    "cannot initialize instance of class ~a" class)))))))

  (define bootstrap-allocate-instance 
    (begin 

      (bootstrap-initialize <top>
        (list 'definition-name '<top>
              'direct-supers   (list)
              'direct-slots    (list)))

      (bootstrap-initialize <object>
        (list 'definition-name '<object>
              'direct-supers   (list <top>)
              'direct-slots    (list)))

      (bootstrap-initialize <class>
        (list 'definition-name '<class>
              'direct-supers   (list <object>)
              'direct-slots    core-class-slot-names))

      (lambda (class)
        (let ((class-of-class (class-of class)))
          (cond
            ((eq? class-of-class <class>)
             (class-allocate-instance class))
            ((eq? class-of-class <entity-class>)
             (entity-class-allocate-instance class))
            (else
             (error 'bootstrap-allocate-instance
                    "cannot allocate instance for class ~a" class)))))))

  (define (bootstrap-make class . init-args)
    (class-make class init-args
                bootstrap-allocate-instance
                bootstrap-initialize))

  (define <procedure-class>
    (bootstrap-make <class>
      'definition-name '<procedure-class>
      'direct-supers   (list <class>)
      'direct-slots    (list)))

  (define <entity-class>
    (bootstrap-make <class>
      'definition-name '<entity-class>
      'direct-supers   (list <procedure-class>)
      'direct-slots    (list)))

  (define <generic>
    (bootstrap-make <entity-class>
      'definition-name '<generic>
      'direct-supers   (list <object>)
      'direct-slots    (list 'methods)))

  (define <method>
    (bootstrap-make <class>
      'definition-name '<method>
      'direct-supers   (list <object>)
      'direct-slots    (list 'specializers
                             'qualifier
                             'procedure)))

  (define <primitive-class>
    (bootstrap-make <class>
      'definition-name '<primitive-class>
      'direct-supers   (list <class>)
      'direct-slots    (list)))

  (define (make-primitive-class name . class)
    (bootstrap-make (if (null? class) <primitive-class> (car class))
      'definition-name name
      'direct-supers   (list <top>)
      'direct-slots    (list)))

  (define <pair>        (make-primitive-class '<pair>))
  (define <null>        (make-primitive-class '<null>))
  (define <symbol>      (make-primitive-class '<symbol>))
  (define <boolean>     (make-primitive-class '<boolean>))
  (define <procedure>   (make-primitive-class '<procedure> <procedure-class>))
  (define <number>      (make-primitive-class '<number>))
  (define <vector>      (make-primitive-class '<vector>))
  (define <char>        (make-primitive-class '<char>))
  (define <string>      (make-primitive-class '<string>))
  (define <input-port>  (make-primitive-class '<input-port>))
  (define <output-port> (make-primitive-class '<output-port>))
  
  (define (primitive-class-of x)
    (cond 
      ((pair? x)        <pair>)         ;If all Schemes were IEEE 
      ((null? x)        <null>)         ;compliant, the order of
      ((boolean? x)     <boolean>)      ;these wouldn't matter?
      ((symbol? x)      <symbol>)
      ((procedure? x)   <procedure>)
      ((number? x)      <number>)
      ((vector? x)      <vector>)
      ((char? x)        <char>)
      ((string? x)      <string>)
      ((input-port? x)  <input-port>)
      ((output-port? x) <output-port>)
      (else             <top>)))

  (set-primitive-class-of! primitive-class-of)

  ) ;; library (clos bootstrap standard-classes)

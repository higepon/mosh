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

(library (clos std-protocols generic-invocation)
  
  (export register-generic-invocation-generics!
          generic-invocation-generic?
          generic-compute-apply-generic
          generic-compute-apply-methods
          generic-compute-methods
          generic-compute-method-more-specific?)
           
  (import (rnrs)
          (clos private method-cache)
          (clos introspection)
          (clos private compat))

  (define compute-apply-generic #f)
  (define compute-apply-methods #f)
  (define compute-methods #f)      
  (define compute-method-more-specific? #f)

  (define (register-generic-invocation-generics! 
           gf-compute-apply-generic
           gf-compute-apply-methods
           gf-compute-methods
           gf-compute-method-more-specific?)

    (set! compute-apply-generic
          gf-compute-apply-generic)
    (set! compute-apply-methods
          gf-compute-apply-methods)
    (set! compute-methods
          gf-compute-methods)
    (set! compute-method-more-specific?
          gf-compute-method-more-specific?))
  
  (define (generic-invocation-generic? obj)
    (or (eq? obj compute-apply-generic)
        (eq? obj compute-apply-methods)
        (eq? obj compute-methods)
        (eq? obj compute-method-more-specific?)))
  
  (define (generic-compute-apply-generic generic)
    (let ((fallback (and (generic-invocation-generic? generic)
                         (method-procedure (last (generic-methods generic)))))
          (dispatch (make-cached-dispatch generic
                     (lambda (args)
                      (let ((methods (compute-methods generic args)))
                        (compute-apply-methods generic methods))))))
      (case-lambda
  	((a)
         (if (and fallback (generic-invocation-generic? a))
             (fallback generic '() a)
             (let ((args (list a)))
               ((dispatch args) args))))
        ((a b)
         (if (and fallback (generic-invocation-generic? a))
             (fallback generic '() a b)
             (let ((args (list a b)))
               ((dispatch args) args))))
        ((a b c)
         (if (and fallback (generic-invocation-generic? a))
             (fallback generic '() a b c)
             (let ((args (list a b c)))
               ((dispatch args) args))))
        ((a b c d)
         (if (and fallback (generic-invocation-generic? a))
             (fallback generic '() a b c d)
             (let ((args (list a b c d)))
               ((dispatch args) args))))
      	(args
         (if (and fallback (generic-invocation-generic? (car args)))
             (apply fallback generic '() args)
             ((dispatch args) args))))))
  
  (define (generic-compute-methods generic args)
    (let ((applicable
           (filter (lambda (method)
                     (every-2 applicable? 
                              (method-specializers method) 
                              args))
                   (generic-methods generic)))
          (method-more-specific?
           (compute-method-more-specific? generic args)))
      (list-sort method-more-specific? applicable)))
  
  (define (generic-compute-method-more-specific? generic args)
    (lambda (m1 m2) 
      (let loop ((specls1 (method-specializers m1))
                 (specls2 (method-specializers m2))
                 (args args))
        (cond ((and (null? specls1) (null? specls2))
               (if (not (eq? (method-qualifier m1)
                             (method-qualifier m2)))
                   #f 
                   (error 'compute-method-more-specific?
                          "Two methods are equally specific.")))
              ((or  (null? specls1) (null? specls2))
               (error 'compute-method-more-specific?
                      "Two methods have a different number of specializers."))
              ((null? args)
               (error 'compute-method-more-specific?
                      "Fewer arguments than specializers."))
              (else
               (let ((c1  (car specls1))
                     (c2  (car specls2))
                     (arg (car args)))
                 (if (eq? c1 c2)
                     (loop (cdr specls1)
                           (cdr specls2)
                           (cdr args))
                     (more-specific? c1 c2 arg))))))))
  
  (define (generic-compute-apply-methods generic methods)
    (let-values (((arround-methods
                   before-methods
                   primary-methods
                   after-methods)
                  (sort-methods-by-qualifier methods)))
      (cond
        ((null? primary-methods)
         (compute-apply-no-primary-methods generic))
        ((and (null? arround-methods)
              (null? before-methods)
              (null? after-methods))
         (compute-apply-primary-methods generic
                                        primary-methods))
        (else
         (compute-apply-arround-methods generic
                                        arround-methods
                                        before-methods
                                        primary-methods
                                        after-methods)))))
  
  (define (sort-methods-by-qualifier methods)
    (let loop ((methods         methods)
               (arround-methods '())
               (before-methods  '())
               (primary-methods '())
               (after-methods   '()))
      (if (null? methods)
          (values (reverse arround-methods)
                  (reverse before-methods)
                  (reverse primary-methods)
                  ;; after-methods are applied 
                  ;; in reverse order
                  after-methods)
          (case (method-qualifier (car methods))
            ((arround)
             (loop (cdr methods)
                   (cons (car methods) arround-methods)
                   before-methods
                   primary-methods
                   after-methods))
            ((before)
             (loop (cdr methods)
                   arround-methods
                   (cons (car methods) before-methods)
                   primary-methods
                   after-methods))
            ((primary)
             (loop (cdr methods)
                   arround-methods
                   before-methods
                   (cons (car methods) primary-methods)
                   after-methods))
            ((after)
             (loop (cdr methods)
                   arround-methods
                   before-methods
                   primary-methods
                   (cons (car methods) after-methods)))
            (else
             (error 'apply
                    "wrong method-qualifier"
                    (car methods)))))))
  
  (define (compute-apply-no-primary-methods generic)
    (lambda (args)
      (error 'apply "No applicable methods." generic args)))
  
  (define (compute-apply-primary-methods generic 
                                         primary-methods)
    (let ((procs (map method-procedure primary-methods)))
      (lambda (args)
        (apply-nested-procs generic procs args))))
  
  (define (compute-apply-arround-methods generic
                                         arround-methods
                                         before-methods
                                         primary-methods
                                         after-methods)
    (let* ((arround-procs (map method-procedure arround-methods))
           (before-procs  (map method-procedure before-methods))
           (primary-procs (map method-procedure primary-methods))
           (after-procs   (map method-procedure after-methods))
           (inner-proc    (lambda (generic empty-list . args)
                            (apply-before/after-procs generic 
                                                      before-procs 
                                                      args)
                            (let ((result (apply-nested-procs generic 
                                                              primary-procs 
                                                              args)))
                              (apply-before/after-procs generic 
                                                        after-procs 
                                                        args)
                              result)))
           (procs         (append arround-procs (list inner-proc))))
      (lambda (args)
        (apply-nested-procs generic procs args))))
  
  (define (apply-nested-procs generic procs args)
    (apply (car procs) generic (cdr procs) args))
  
  (define (apply-before/after-procs generic procs args)
    (if (not (null? procs))
        (begin
          (apply (car procs) generic '() args)
          (apply-before/after-procs generic (cdr procs) args))))
 
  (define (applicable? c arg)
    (memq c (class-precedence-list (class-of arg))))
  
  (define (more-specific? c1 c2 arg)
    (memq c2 (memq c1 (class-precedence-list (class-of arg)))))
  
  ) ;; library (clos std-protocols generic-invocation)

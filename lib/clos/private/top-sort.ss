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

;
; A simple topological sort.
;
; This is a fairly modified version of code I originally got from Anurag
; Mendhekar <anurag@moose.cs.indiana.edu>.
;
;

(library (clos private top-sort)
  
  (export top-sort
          standard-tie-breaker
          build-transitive-closure
          build-constraints)

  (import (rnrs)
          (clos private compat))
  
  (define top-sort
    (lambda (elements constraints tie-breaker)
      (let loop ((elements    elements)
                 (constraints constraints)
                 (result      '()))
        (if (null? elements)
            result
            (let ((can-go-in-now
                   (filter
                    (lambda (x)
                      (every (lambda (constraint)
                               (or (not (eq? (cadr constraint) x))
                                   (memq (car constraint) result)))
                             constraints))
                    elements)))
              (if (null? can-go-in-now)
                  (error 'top-sort "Invalid constraints")
                  (let ((choice (if (null? (cdr can-go-in-now))
                                    (car can-go-in-now)
                                    (tie-breaker result
                                                 can-go-in-now))))
                    (loop
                     (filter (lambda (x) (not (eq? x choice)))
                             elements)
                     constraints
                     (append result (list choice))))))))))
  
  (define standard-tie-breaker
    (lambda (get-supers)
      (lambda (partial-cpl min-elts)
        (let loop ((pcpl (reverse partial-cpl)))
          (let ((current-elt (car pcpl)))
            (let ((ds-of-ce (get-supers current-elt)))
              (let ((common (filter (lambda (x)
                                      (memq x ds-of-ce))
                                    min-elts)))
                (if (null? common)
                    (if (null? (cdr pcpl))
                        (error 'std-tie-breaker "Nothing valid")
                        (loop (cdr pcpl)))
                    (car common)))))))))
  
  (define build-transitive-closure
    (lambda (get-follow-ons)
      (lambda (x)
        (let track ((result '())
                    (pending (list x)))
          (if (null? pending)
              result
              (let ((next (car pending)))
                (if (memq next result)
                    (track result (cdr pending))
                    (track (cons next result)
                           (append (get-follow-ons next)
                                   (cdr pending))))))))))
  
  (define build-constraints
    (lambda (get-follow-ons)
      (lambda (x)
        (let loop ((elements ((build-transitive-closure get-follow-ons) x))
                   (this-one '())
                   (result '()))
          (if (or (null? this-one) (null? (cdr this-one)))
              (if (null? elements)
                  result
                  (loop (cdr elements)
                        (cons (car elements)
                              (get-follow-ons (car elements)))
                        result))
              (loop elements
                    (cdr this-one)
                    (cons (list (car this-one) (cadr this-one))
                          result)))))))
  
  ) ;; library (clos private top-sort)

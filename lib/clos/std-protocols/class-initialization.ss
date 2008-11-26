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

(library (clos std-protocols class-initialization)
  
  (export class-compute-precedence-list
          class-compute-slots
          class-compute-getter-and-setter)
  
  (import (rnrs)
          (clos introspection)
          (clos private top-sort)
          (clos private compat))
  
  (define (class-compute-precedence-list class)
    (compute-standard-precedence-list class class-direct-supers))
  
  (define (compute-standard-precedence-list class get-direct-supers)
    (top-sort ((build-transitive-closure get-direct-supers) class)
              ((build-constraints get-direct-supers) class)
              (standard-tie-breaker get-direct-supers)))
  
  (define (class-compute-slots class)
    (let loop ((todo (append-map class-direct-slots 
                                 (class-precedence-list class)))
               (done '()))
      (if (null? todo)
          (reverse done)
          (let* ((curr (car todo))
                 (name (car curr))
                 (same '())
                 (rest (filter (lambda (slot)
                                 (if (eq? (car slot) name)
                                     (begin (set! same (cons slot same)) #f)
                                     #t))
                               (cdr todo))))
            (loop rest
                  (cons (append curr (append-map cdr same)) done))))))
  
  
  
  (define (class-compute-getter-and-setter class slot allocator)
    (allocator (lambda () '())))
  
  ) ;; library (clos std-protocols class-initialization)

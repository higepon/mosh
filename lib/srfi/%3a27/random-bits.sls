;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

#!r6rs
(library (srfi :27 random-bits)
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals)
  
  (import 
   (only (except (rnrs) error) define begin define-record-type fields apply let vector-set! - * vector-ref let* do < + cons quote vector->list if and list? = length eq? car list-ref or zero? list->vector cdr integer? exact? <= not set! list vector expt cond even? else >= ... _ / lambda positive? null? real?)
   (only (rnrs r5rs) modulo quotient exact->inexact)
   (only (srfi :39 parameters) parameterize)
   (only (srfi :19 time) time-nanosecond current-time)
   (only (prefix (srfi :23 error) ER:) ER:error ER:error-who)
   (only (srfi private include) include/resolve)
          )
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi :27 random-bits))"])
      (apply ER:error args)))
  
   (include/resolve ("srfi" "27") "random.ss")
  )
 )


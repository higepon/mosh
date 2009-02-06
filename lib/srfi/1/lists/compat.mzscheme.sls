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
(library (srfi :1 lists compat)
  (export 
    last-pair make-list)
  (import 
    (rnrs)
    (only (scheme base) void sub1))  
  
  (define (last-pair x)
    (unless (pair? x)
      (assertion-violation 'last-pair "not a pair" x))
    (let loop ([x x])
      (if (pair? (cdr x))
        (loop (cdr x))
        x)))
  
  (define make-list
    (case-lambda 
      [(n) (make-list n (void))]
      [(n v)
       (unless (and (integer? n) (exact? n) (not (negative? n)))
         (assertion-violation 'make-list "not a valid length" n))
       (let loop ([n n] [r '()])
         (if (= 0 n)
           r
           (loop (sub1 n) (cons v r))))]))
  
)

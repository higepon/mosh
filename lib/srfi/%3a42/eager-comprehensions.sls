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
(library (srfi :42 eager-comprehensions)
  (export
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
    dispatch-union :generator-proc)
  (import
   (only (rnrs) _ ... define begin define-syntax syntax-rules apply if null? quote zero? vector-length car cdr cons lambda let append list? list case else cond and not set! string? string-length < string-ref + vector? vector-ref integer? exact? real? or / - * input-port? eof-object? read string-append char? <= integer->char char->integer procedure? cadr max ceiling = caddr letrec map vector->list length reverse eq? list->vector)
   (only (rnrs r5rs) exact->inexact)
   (only (srfi :39 parameters) parameterize make-parameter)
   (only (prefix (srfi :23 error) ER:) ER:error ER:error-who)
   (only (srfi private include) include/resolve)
)
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi :42 eager-comprehensions))"])
      (apply ER:error args)))
  
  (include/resolve ("srfi" "42") "ec.scm")  
)

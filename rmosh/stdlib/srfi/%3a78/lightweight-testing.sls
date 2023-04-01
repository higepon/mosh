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
(library (srfi :78 lightweight-testing)
  (export
    check
    check-ec
    check-report
    check-set-mode!
    check-reset!
    check-passed?
    ;; All of (srfi :42 eager-comprehensions):
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
    dispatch-union :generator-proc)
  (import 
(only (rnrs) define begin define-syntax syntax-rules ... _ quote set! apply write lambda case else + cons list newline display if not = >= length or null? <= let* caddr cadr car reverse and let cadddr cddddr => equal?)
(only (srfi :78 lightweight-testing compat) pretty-print/no-trailing-newline)
    (only (srfi :39 parameters) parameterize make-parameter)
(only (srfi private include) include/resolve)
(only (prefix (srfi :23 error) ER:) ER:error ER:error-who)
(only (srfi :42 eager-comprehensions) :generator-proc dispatch-union make-initial-:-dispatch :-dispatch-set! :-dispatch-ref :until :while :parallel :let :do :dispatched :port :char-range :real-range :range :integers :vector :string :list : fold3-ec fold-ec last-ec first-ec every?-ec any?-ec max-ec min-ec product-ec sum-ec vector-of-length-ec vector-ec string-append-ec string-ec append-ec list-ec do-ec)
)
  
  (define (error . args)
    (parameterize ([ER:error-who
                    "(library (srfi :78 lightweight-testing))"])
      (apply ER:error args)))
  
  (include/resolve ("srfi" "78") "check.scm")
  
  (set! check:write pretty-print/no-trailing-newline)  
)

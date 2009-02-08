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
(library (srfi :67 compare-procedures)
  (export  </<=? </<? <=/<=? <=/<? <=? <? =?
           >/>=? >/>? >=/>=? >=/>? >=? >?
           boolean-compare chain<=? chain<? chain=? chain>=? chain>?
           char-compare char-compare-ci
           compare-by< compare-by<= compare-by=/< compare-by=/> compare-by> 
           compare-by>= complex-compare cond-compare
           debug-compare default-compare
           if-not=? if3 if<=? if<? if=? if>=? if>? integer-compare
           kth-largest list-compare list-compare-as-vector
           max-compare min-compare not=? number-compare
           pair-compare pair-compare-car pair-compare-cdr
           pairwise-not=? rational-compare real-compare
           refine-compare select-compare string-compare string-compare-ci 
           symbol-compare vector-compare vector-compare-as-list)
  
  (import
(only (rnrs) ... _ define begin define-syntax syntax-rules apply for-each lambda case-lambda case else if procedure? let null? car cdr < and let* length + cons quote list-ref = <= not integer? exact? cond - boolean? string-append char? eq? char=? char<? char-ci=? char-ci<? string? string=? string<? string-ci=? string-ci<? symbol? symbol->string rational? real? complex? imag-part real-part number? pair? vector-length vector-ref min vector? zero? list? map * set! or eqv? list eq?)
(only (rnrs r5rs) modulo)
(only (srfi :27 random-bits) random-integer)
(only (srfi :39 parameters) parameterize)
(only (prefix (srfi :23 error) ER:) ER:error ER:error-who)
(only (srfi private include) include/resolve)
)
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi :67 compare-procedures))"])
      (apply ER:error args)))
  
  (include/resolve ("srfi" "67") "compare.ss")  
  )
compare.ss")  
  )
mpare.ss")  
  )


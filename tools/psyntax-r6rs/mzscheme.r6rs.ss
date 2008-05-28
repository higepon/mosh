;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 


(define for-all andmap) ; almost
(define exists ormap) ; almost
(define symbol-value 
  (lambda (x) (namespace-variable-value x)))
(define set-symbol-value!
  (lambda (x v) (namespace-set-variable-value! x v)))
(define eval-core 
  (lambda (x) (eval x)))
(define cons* list*)

(define (open-string-output-port)
  (let ([p (open-output-string)])
    (values p (lambda () (get-output-string p)))))

(define make-eq-hashtable make-hash-table)
(define hashtable-ref 
  (lambda (h x v) 
    (hash-table-get h x (lambda () v))))
(define hashtable-set! hash-table-put!)
(define hashtable? hash-table?)

(define command-line 
  (lambda () 
    (cons "mzscheme" 
      (vector->list (current-command-line-arguments)))))

(define (pretty-print x p)
  (write x p)
  (newline p))

(define gensym-count 0)
(define session-id 0)
(define strip
  (lambda (str)
    (list->string
      (let f ([ls (string->list str)])
        (cond
          [(null? ls) '()]
          [(char=? (car ls) #\$) '()]
          [else (cons (car ls) (f (cdr ls)))])))))

(define gensym
  (case-lambda
    [() (gensym "g")]
    [(x) 
     (let ([i gensym-count]
           [str
            (cond
              [(symbol? x) (symbol->string x)]
              [(string? x) x]
              [else (error 'gensym "invalid")])])
       (set! gensym-count (+ 1 i))
       (string->symbol 
         (string-append (strip str) "$" 
                        (number->string session-id)
                        "$"
                        (number->string i))))]))

(when (file-exists? "session-id")
  (set! session-id 
    (with-input-from-file "session-id" read))
  (delete-file "session-id"))

(with-output-to-file "session-id" 
  (lambda () 
    (write (+ 1 session-id))))

(load "psyntax.pp")

; shorten.ss
;
;   Copyright (c) 2010 OKUMURA Yuki and Higepon(Taro Minowa)
;
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
#|
    Title: Shorten

    This library provides a short alias for lambda expression.
    The idea come from "Gauche Devlog - Shorter names http://blog.practical-scheme.net/gauche/20100428-shorter-names" by Shiro.

    Example:
    (start code)
    (map (^(x y) (+ x y)) '(1 2 3 4) '(1 2 3 4)) => (map (lambda (x y) (+ x y)) '(1 2 3 4) '(1 2 3 4))

    (^a body ...) => (lambda (a) body ...)
    (^b body ...) => (lambda (b) body ...)
    ...
    (^z body ...) => (lambda (z) body ...)
    (^_ body ...) => (lambda (_) body ...)

    (^a* body ...) => (lambda a* body ...)
    (^b* body ...) => (lambda b* body ...)
    ...
    (^z* body ...) => (lambda z* body ...)
    (^_* body ...) => (lambda _* body ...)

    (end code)

    library: (shorten)
|#

(library (shorten)
         (export ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z ^_ ^
                 ^a* ^b* ^c* ^d* ^e* ^f* ^g* ^h* ^i* ^j* ^k* ^l* ^m* ^n* ^o* ^p* ^q* ^r* ^s* ^t* ^u* ^v* ^w* ^x* ^y* ^z* ^_*)
         (import 
           (only (rnrs) define-syntax lambda syntax-case define let begin with-syntax
                 ...
                 symbol->string string->symbol string-append cons map cdr syntax
                 quote quasiquote unquote
                 datum->syntax)
           (for (shorten helper) expand))

(define-syntax ^
  (lambda (x)
    (syntax-case x ()
      [(_ args ...)
       #'(lambda args ...)])))

(define-syntax define-^
  (lisp-transformer
    (lambda (f)
      (define (entry name)
        (let ((namestr (symbol->string name)))
          `(define-syntax ,(string->symbol (string-append "^" namestr))
             (lambda (x)
               (syntax-case x ()
                 ((k args ...)
                  (with-syntax ((larg (datum->syntax #'k (quote ,name))))
                    #'(lambda (larg) args ...))))))))
      (let ((l (cdr f)))
        (cons 'begin (map entry l))))))

(define-syntax define-^*
  (lisp-transformer
    (lambda (f)
      (define (entry name)
        (let ((namestr (symbol->string name)))
          `(define-syntax ,(string->symbol (string-append "^" namestr "*"))
             (lambda (x)
               (syntax-case x ()
                 ((k args ...)
                  (with-syntax ((larg (datum->syntax #'k (quote ,(string->symbol (string-append (symbol->string name) "*"))))))
                    #'(lambda larg args ...))))))))
      (let ((l (cdr f)))
        (cons 'begin (map entry l))))))

(define-^
  _ a b c d e f g h i j k l m n o p q r s t u v w x y z)

(define-^*
  _ a b c d e f g h i j k l m n o p q r s t u v w x y z)

)

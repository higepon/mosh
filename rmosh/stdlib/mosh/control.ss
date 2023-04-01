; control.ss - Control syntax
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
;  $Id: control.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: Control

    Useful control structures.

    library: (mosh control)

    Control structure library
|#
(library (mosh control)
  (export aif begin0 let1 let-optionals* :optional)
  (import (only (rnrs) define-syntax syntax-case ... syntax let syntax-rules _ null? car cdr
                       if lambda with-syntax datum->syntax quote begin error
                       call-with-values apply values))

  #|
      Function: aif

      anaphoric if

      Prototype:
      > (aif test consequent alternate)

      Example:
      >(aif 3 it 4) => 3

      Parameters:

        test - test condition
        consequent - consequent can contain it.
        alternate - alternate can contain it.

  |#
  (define-syntax aif
    (lambda (x)
      (syntax-case x ()
        [(ctx expr then else ...)
         (with-syntax ((it (datum->syntax #'ctx 'it)))
                      #'(let ((it expr))
                          (if it then else ...)))])))


  #|
      Function: begin0

      Evaluates exp0, exp1, …, then returns the result(s) of exp0.

      Prototype:
      > (begin0 exp0 exp1 ...)

  |#
  (define-syntax begin0
    (lambda (x)
      (syntax-case x ()
        [(_ exp0 exp* ...)
         #'(call-with-values (lambda () exp0) (lambda args exp* ... (apply values args)))])))

  #|
      Function: let1

      A convenient macro when you have only one variable. Expanded as follows.
      > (let ((var expr)) body ...)

      Prototype:
      > (let1 var var body ...)

  |#
  (define-syntax let1
    (lambda (x)
      (syntax-case x ()
        [(_ var val body body* ...)
         #'(let ([var val]) body body* ...)])))

; LET-OPTIONALS macros
; Copyright (c) 2001 by Olin Shivers.

; Copyright (c) 1993-2003 Richard Kelsey and Jonathan Rees
; Copyright (c) 1994-2003 by Olin Shivers and Brian D. Carlstrom.
; Copyright (c) 1999-2003 by Martin Gasbichler.
; Copyright (c) 2001-2003 by Michael Sperber.

  #|
      Function: :optional

  |#
  (define-syntax :optional
    (syntax-rules ()
      ([_ rest default-exp]
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg)) (car maybe-arg)
                 (error ':optional "too many optional arguments" maybe-arg))
             default-exp)))
      ([_ rest default-exp arg-test]
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg))
                 (let ((val (car maybe-arg)))
                   (if (arg-test val) val
                       (error ':optional "optional argument failed test" val)))
                 (error ':optional "too many optional arguments" maybe-arg))
             default-exp)))))

  #|
      Function: let-optionals*

  |#
  (define-syntax let-optionals*
    (syntax-rules ()
      ((let-optionals* arg (opt-clause ...) body ...)
       (let ((rest arg))
         (%let-optionals* rest (opt-clause ...) body ...)))))

  (define-syntax %let-optionals*
    (syntax-rules ()
      ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
       (call-with-values (lambda () (xparser arg))
         (lambda (rest var ...)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg ((var default) opt-clause ...) body ...)
       (call-with-values (lambda () (if (null? arg) (values default '())
                                        (values (car arg) (cdr arg))))
         (lambda (var rest)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
       (call-with-values (lambda ()
                           (if (null? arg) (values default '())
                               (let ((var (car arg)))
                                 (if test (values var (cdr arg))
                                     (error 'let-optionals* "arg failed LET-OPT test" var)))))
         (lambda (var rest)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
       (call-with-values (lambda ()
                           (if (null? arg) (values default #f '())
                               (let ((var (car arg)))
                                 (if test (values var #t (cdr arg))
                                     (error 'let-optionals* "arg failed LET-OPT test" var)))))
         (lambda (var supplied? rest)
           (%let-optionals* rest (opt-clause ...) body ...))))

      ((%let-optionals* arg (rest) body ...)
       (let ((rest arg)) body ...))

      ((%let-optionals* arg () body ...)
       (if (null? arg) (begin body ...)
           (error 'let-optionals* "too many arguments in let-opt" arg)))))

) ;; library (mosh control)

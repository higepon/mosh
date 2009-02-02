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
  (export aif begin0 let1)
  (import (only (rnrs) define-syntax syntax-case ... syntax let
                       if lambda with-syntax datum->syntax quote
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

) ;; library (mosh control)

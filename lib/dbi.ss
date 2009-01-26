; dbi.ss - DBI(Database Interface)
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
;  $Id: dbi.ss 621 2008-11-09 06:22:47Z higepon $

(library (dbi)
  (export dbi-connect <connection>
          dbd-connect <dbd>)
  (import
     (only (rnrs) define quote cond => lambda assertion-violation values
                  let-values else and quasiquote unquote string->symbol let)
     (only (mosh) symbol-value format)
     (clos core)
     (clos user))

(define-class <connection> ())
(define-class <dbd> ())

(define-generic dbd-connect)

(define (parse-dsn dsn)
  (cond
   [(#/dbi:([^:]+):(.+)/ dsn) =>
    (lambda (m)
      (values (m 1) (m 2)))]
   [else
    (values #f #f)]))

(define (make-driver name)
  (let ([eval-r6rs (symbol-value 'eval-r6rs)])
    (eval-r6rs `(import (clos core)))
    (eval-r6rs `(import (dbd ,(string->symbol name))))
    (eval-r6rs `(make ,(string->symbol (format "<dbd-~a>" name))))))

(define (dbi-connect dsn user password)
  (let-values ([(name options) (parse-dsn dsn)])
    (cond
     [(and name options)
      (let ([driver (make-driver name)])
        (dbd-connect driver user password options))]
     [else
      (assertion-violation 'dbi-connect "invalid dsn. dbi:drivername:options required" dsn)])))
)

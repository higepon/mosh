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
  (export dbi-connect dbi-prepare dbi-execute dbi-getter dbi-result->list
          <connection> <query> <result>
          dbd-connect dbd-execute <dbd>)
  (import
     (only (rnrs) define quote cond => lambda assertion-violation values
                  let-values else and quasiquote unquote string->symbol let
                  let* null? apply string-append reverse if string=? car
                  cdr cons string? number? char? when display)
     (only (mosh) symbol-value format string-split)
     (only (clos user) define-class define-generic define-method initialize initialize-direct-slots
                       make slot-ref))

(define-class <connection> ())
(define-class <dbd> ())
(define-class <result> ())
(define-class <query> () prepared connection)

(define-generic dbd-connect)
(define-generic dbd-execute)
(define-generic dbi-prepare)
(define-generic dbi-result->list)
(define-generic dbi-getter)

(define (make-driver name)
  (let ([eval-r6rs (symbol-value 'eval-r6rs)])
    (eval-r6rs `(import (clos core)))
    (eval-r6rs `(import (dbd ,(string->symbol name))))
    (eval-r6rs `(make ,(string->symbol (format "<dbd-~a>" name))))))

(define (dbi-connect dsn user password)
  (define (parse-dsn dsn)
    (cond
     [(#/dbi:([^:]+):(.+)/ dsn) =>
      (lambda (m)
        (values (m 1) (m 2)))]
     [else
      (values #f #f)]))
  (let-values ([(name options) (parse-dsn dsn)])
    (cond
     [(and name options)
      (let ([driver (make-driver name)])
        (dbd-connect driver user password options))]
     [else
      (assertion-violation 'dbi-connect "invalid dsn. dbi:drivername:options required" dsn)])))

(define (prepare-helper obj)
  (cond
   [(string? obj)
    (format "~s" obj)]
   [(number? obj)
    (format "~a" obj)]
   [(char? obj)
    (format "~a" obj)]
   [else
    (assertion-violation 'dbi-execute "not supported argument for prepared sql" obj)]))

(define (dbi-set-prepared prepared args)
  (let* ([tokens (string-split prepared #\space)])
    (let loop ([tokens tokens]
               [ret '()])
      (cond
       [(null? tokens)
        (apply string-append (reverse ret))]
       [else
        (cond
         [(string=? "?" (car tokens))
          (when (null? args)
            (assertion-violation 'dbi-execute "args to short for prepared sql" prepared args))
          (loop (cdr tokens) (cons (prepare-helper (car args)) (cons " "ret)))]
         [else
          (loop (cdr tokens) (cons (car tokens) (cons " " ret)))])]))))

(define (dbi-execute query . args)
  (dbd-execute (slot-ref query 'connection) (dbi-set-prepared (slot-ref query 'prepared) args)))

(define-method initialize ((q <query>) init-args)
  (initialize-direct-slots q <query> init-args))

; default implementaion of dbi-prepare
; This may be overwritten in dbd.
(define-method dbi-prepare ((conn <connection>) sql)
  (make <query> 'prepared sql 'connection conn))


)

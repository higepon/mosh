; c-type.ss - C-Type interface
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
;  $Id: c-type.ss 621 2008-11-09 06:22:47Z higepon $


;;;; DONT USE THIS library.

(library (mosh c-type)
  (export
    define-c-struct
    define-c-struct-accessors
    c-type-test)
  (import (mosh ffi)
          (mosh)
          (mosh c-type helper)
          (mosh test)
          (rnrs))

;; (define-c-struct-accessors struct-name (int a) offset) => (bytevector-int-ref bv 0 (native-endianness))
(define-syntax define-c-struct-accessors
  (lambda (x)
    (syntax-case x ()
      [(_ struct-name (type var) offset)
       (with-syntax ([getter (gen-id #'struct-name #'var)])
          #'(define getter (lambda (bv) (bytevector-s32-ref bv offset (native-endianness)))))])))

#;(define-syntax define-c-struct
  (lambda (x)
    (define (size-of type)
      (let ([size (os-constant (string->symbol (format "size-of-~a" type)))])
        (assert size)
        size))
    (define (calc-struct-size field-spec*)
      (let loop ([size 0]
                 [spec* field-spec*])
        (cond
         [(null? spec*) size]
         [else
          (loop (+ size (size-of (caar spec*)))
                (cdr spec*))])))
    (syntax-case x (struct)
      [(_ name (struct field-spec* ...))
         (with-syntax ([size-getter (gen-id 'size-of #'name)]
                       [struct-size (calc-struct-size (syntax->datum #'(field-spec* ...)))]
                       [constructor (datum->syntax #'name (string->symbol (string-append "make-" (symbol->string (syntax->datum #'name)))))])
           #'(begin
               (define size-getter (lambda () struct-size))
               (define constructor (lambda () (make-bytevector struct-size))))]))))

(define-syntax define-c-struct-accessor*
  (lambda (x)
    (syntax-case x ()
      [(_ (type var offset) ...)
       #'(begin
           (define-c-struct-accessor type var offset)
           (define-c-struct-accessor* ...))])))

(define-syntax define-c-struct
  (lambda (x)
    (syntax-case x (struct)
      [(_ name (struct field-spec* ...))
         (let-values ([(struct-size field*) (process-c-struct-field* (syntax->datum #'(field-spec* ...)))])
           (with-syntax ([struct-size struct-size]
                         [size-getter (gen-id 'size-of #'name)]
                         [constructor (gen-id 'make #'name)]
                         [(f* ...) (datum->syntax #'name field*)])
           #`(begin
               (define-c-struct-accessor* #,@#'f*)
               (define size-getter (lambda () struct-size))
               (define constructor (lambda () (make-bytevector struct-size))))])))))

(define (c-type-test)
  (test-begin "c-type tests")
  (let ()
    (define-c-struct-accessors struct1 (int a) 0)
   (test-assert struct1-a)
   (test-true (procedure? struct1-a))
    #t)
  (test-end))
)

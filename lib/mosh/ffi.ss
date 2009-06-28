; ffi.ss - Foreign Function Interface.
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
;  $Id: ffi.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: Foreign Function Interface

    What is FFI?:
    Mosh provides a general Foreign Function Interface (FFI) methods.

    With these methods, you can load shared library, call C-function in it and get a result of function call.


    FFI is not supported on Windows.

    Example:
    (start code)
    ;; use mysql client library
    (let* ([libmysqlclient (open-shared-library "libmysqlclient.so.15")]
           [mysql-init     (c-function libmysqlclient void* mysql_init void*)])
      (display (mysql-init 0)))
    (end code)

    (start code)
    ;; generate png image with Cairo library.
    (import (rnrs)
            (mosh ffi))

    (let* ((libcairo (open-shared-library "libcairo.so"))
             (cairo-image-surface-create (c-function libcairo void* cairo_image_surface_create int int int))
             (cairo-surface-write-to-png (c-function libcairo int cairo_surface_write_to_png void* char*))
             (cairo-create (c-function libcairo void* cairo_create void*))
             (set-line-width (c-function libcairo void cairo_set_line_width void* double))
             (rgba (c-function libcairo void cairo_set_source_rgba void* double double double double))
             (move-to (c-function libcairo void cairo_move_to void* double double))
             (line-to (c-function libcairo void cairo_line_to void* double double))
             (TOY-show-text (c-function libcairo void cairo_show_text void* char*))
             (stroke (c-function libcairo void cairo_stroke void*)))

           (let* ((surface (cairo-image-surface-create 1 300 300))
                  (ctx (cairo-create surface)))
             (rgba ctx 1.0 1.0 1.0 1.0)
             (set-line-width ctx 8.0)
             (move-to ctx 10.0 10.0)
             (line-to ctx 10.0 290.0)
             (line-to ctx 290.0 290.0)
             (line-to ctx 290.0 10.0)
             (line-to ctx 10.0 10.0)
             (move-to ctx 100.0 150.0)
             (TOY-show-text ctx "mosh")
             (stroke ctx)
             (display (cairo-surface-write-to-png surface "test.png"))))
    (end code)


    library: (mosh ffi)

    Foreign Function Interface Library
|#
(library (mosh ffi)
  (export make-c-function c-function open-shared-library find-shared-library
          (rename (%ffi-pointer->string pointer->string)
                  (%ffi-supported? ffi-supported?))
          size-of-bool size-of-short size-of-int size-of-long size-of-void* size-of-size_t
          align-of-bool align-of-short align-of-int align-of-long align-of-void* align-of-size_t align-of-float
          align-of-double align-of-int8_t align-of-int16_t align-of-int32_t align-of-int64_t
          on-darwin on-linux on-freebsd on-openbsd on-windows
          pointer?
          pointer->integer
          integer->pointer ;; temp
          pointer-set-c-int8!
          pointer-set-c-int16!
          pointer-set-c-int32!
          pointer-set-c-int64!
          pointer-ref-c-uint8
          pointer-ref-c-uint16
          pointer-ref-c-uint32
          pointer-ref-c-uint64
          pointer-ref-c-int8
          pointer-ref-c-int16
          pointer-ref-c-int32
          pointer-ref-c-int64
          pointer-ref-c-signed-char
          pointer-ref-c-unsigned-char
          pointer-ref-c-signed-short
          pointer-ref-c-unsigned-short
          pointer-ref-c-signed-int
          pointer-ref-c-unsigned-int
          pointer-ref-c-signed-long
          pointer-ref-c-unsigned-long
          pointer-ref-c-signed-long-long
          pointer-ref-c-unsigned-long-long
          pointer-ref-c-float
          pointer-ref-c-double
          pointer-ref-c-pointer
          pointer-set-c-char!
          pointer-set-c-short!
          pointer-set-c-int!
          pointer-set-c-long!
          pointer-set-c-long-long!
          pointer-set-c-float!
          pointer-set-c-double!
          pointer-set-c-pointer!
          pointer-null
          pointer-null?
          pointer-diff
          pointer-add
          pointer=?
          pointer<?
          pointer>?
          pointer<=?
          pointer>=?
          pointer<>?)
  (import (only (rnrs) define define-syntax syntax-case lambda map let syntax exists string=?
                       quasiquote unless assertion-violation quote = length and number?
                       for-each apply hashtable-ref unquote integer? string? ... or zero? filter
                       for-all procedure? flonum? fixnum? cond else inexact guard file-exists? find > < >= <= not syntax-rules -
                       + case-lambda cons)
          (only (mosh) alist->eq-hash-table format os-constant host-os)
          (rename (system) (%ffi-open open-shared-library))
          (only (system) directory-list %ffi-lookup %ffi-call->void %ffi-call->void* %ffi-call->int %ffi-call->double %ffi-call->string-or-zero
                pointer?
                pointer->integer
                integer->pointer ;; temp
                pointer-ref-c-signed-char
                pointer-ref-c-unsigned-char
                pointer-ref-c-signed-short
                pointer-ref-c-unsigned-short
                pointer-ref-c-signed-int
                pointer-ref-c-unsigned-int
                pointer-ref-c-signed-long
                pointer-ref-c-unsigned-long
                pointer-ref-c-signed-long-long
                pointer-ref-c-unsigned-long-long
                pointer-ref-c-float
                pointer-ref-c-double
                pointer-ref-c-pointer
                pointer-set-c-char!
                pointer-set-c-short!
                pointer-set-c-int!
                pointer-set-c-long!
                pointer-set-c-long-long!
                pointer-set-c-float!
                pointer-set-c-double!
                pointer-set-c-pointer!
                pointer-set-c-int8!
                pointer-set-c-int16!
                pointer-set-c-int32!
                pointer-set-c-int64!
                pointer-ref-c-uint8
                pointer-ref-c-uint16
                pointer-ref-c-uint32
                pointer-ref-c-uint64
                pointer-ref-c-int8
                pointer-ref-c-int16
                pointer-ref-c-int32
                pointer-ref-c-int64
                ))
#|
    Function: ffi-supported?

    Returns #t when ffi is supported, otherwise #f.

    Prototype:
    > (ffi-supported?)

    Returns:

      #t when ffi is supported, otherwise #f.
|#

#|
    Function: pointer?

    Returns #t if obj is pointer, otherwise #f

    Prototype:
    > (pointer? obj)

    Returns:

      #t if obj is pointer, otherwise #f
|#

#|
    Function: pointer->integer

    convert pointer to integer

    Prototype:
    > (pointer->integer pointer)

    Returns:

      integer represention of pointer.
|#

#|
    Function: pointer->string

    Returns string value at which pointer points.

    Prototype:
    > (pointer->string pointer)

    Parameters:

      pointer - integer valued pointer. When not-pointer integer is passed, it may cause crash.

    Returns:

      string value at which pointer points.
|#


#|
    Function: open-shared-library

    Open shared library.

    Prototype:
    > (open-shared-library library)

    Parameters:

      library - Path to library.

    Returns:

      Loaded shared library object.

    Errors:
      Raise error when can't load library
|#


#|
    Function: c-function

    Make foreign c-function closure.

    Prototype:
    > (c-function lib ret func . arg)

    Parameters:

      lib - library object returned by <open-shared-library>
      ret - return type of c-function. void*, char*, void, double and int are supported.
      func - name of c-function as symbol
      arg - list of argument types. void*, int, double and char* are supported.

    Returns:

      Foreign function closure
|#
(define-syntax c-function
  (lambda (x)
    (syntax-case x ()
      [(_ lib ret func arg ...)
       #'(make-c-function lib 'ret 'func '(arg ...)))]))

(define stub-ht (alist->eq-hash-table
                 `((void*  . ,%ffi-call->void*)
                   (char*  . ,%ffi-call->string-or-zero) ;; char* may be NULL,
                   (void   . ,%ffi-call->void)
                   (double . ,%ffi-call->double)
                   (int    . ,%ffi-call->int))))

(define checker-ht (alist->eq-hash-table
                    `((void*  . ,(lambda (x) (and (integer? x) x)))
                      (int    . ,(lambda (x) (and (integer? x) x)))
                      (double . ,(lambda (x) (cond
                                              [(flonum? x) x]
                                              [(fixnum? x) (inexact x)]
                                              [else #f])))
                      (char*  . ,(lambda (x) (and (or (and (number? x) (zero? x)) string?) x))))))

(define (find-shared-library regex)
  (exists
   (lambda (path)
     (find regex (guard [c (#t '())] (directory-list path))))
   (filter file-exists? '("/lib" "/usr/lib/" "/usr/local/lib"))))

(define (make-c-function lib ret-type name arg-types)
  (let ([func (%ffi-lookup lib name)]
        [stub (hashtable-ref stub-ht ret-type #f)]
        [checkers (map (lambda (type) (hashtable-ref checker-ht type #f)) arg-types)])
    (unless (for-all procedure? checkers)
      (assertion-violation 'c-function "invalid argument type for c-function"))
    (unless stub
      (assertion-violation 'c-function "wrong return type" ret-type))
    (unless func
      (assertion-violation 'c-function "c-function not found" name))
    (lambda args
      (unless (= (length arg-types) (length args))
        (assertion-violation name (format "wrong arguments number ~d required, but got ~d"
                                          (length arg-types)
                                          (length args)) args))
      (apply stub func (map
                        (lambda (checker arg)
                          (let ([valid-arg (checker arg)])
                            (unless valid-arg
                              (assertion-violation name "wrong argument " arg))
                            valid-arg))
                        checkers
                        args)))))

#|
    Constant: size-of-bool

    size-of(bool)
|#
(define size-of-bool (os-constant 'size-of-bool))

#|
    Constant: size-of-short

    size-of(short)
|#
(define size-of-short (os-constant 'size-of-short))

#|
    Constant: size-of-int

    size-of(int)
|#
(define size-of-int (os-constant 'size-of-int))

#|
    Constant: size-of-long

    size-of(long)
|#
(define size-of-long (os-constant 'size-of-long))

#|
    Constant: size-of-void*

    size-of(void*)
|#
(define size-of-void* (os-constant 'size-of-void*))

#|
    Constant: size-of-size_t

    size-of(size_t)
|#
(define size-of-size_t (os-constant 'size-of-size_t))

#|
    Constant: align-of-bool

    struct x { char y; bool z; };

    -offset-of(x, z)
|#
(define align-of-bool (os-constant 'align-of-bool))

#|
    Constant: align-of-short

    struct x { char y; short z; };

    -offset-of(x, z)
|#
(define align-of-short (os-constant 'align-of-short))

#|
    Constant: align-of-int

    struct x { char y; int z; };

    -offset-of(x, z)
|#
(define align-of-int (os-constant 'align-of-int))

#|
    Constant: align-of-long

    struct x { char y; long z; };

    -offset-of(x, z)
|#
(define align-of-long (os-constant 'align-of-long))

#|
    Constant: align-of-void*

    struct x { char y; void* z; };

    -offset-of(x, z)
|#
(define align-of-void* (os-constant 'align-of-void*))

#|
    Constant: align-of-size_t

    struct x { char y; size_t z; };

    -offset-of(x, z)
|#
(define align-of-size_t (os-constant 'align-of-size_t))

#|
    Constant: align-of-float

    struct x { char y; float z; };

    -offset-of(x, z)
|#
(define align-of-float (os-constant 'align-of-float))

#|
    Constant: align-of-double

    struct x { char y; double z; };

    -offset-of(x, z)
|#
(define align-of-double (os-constant 'align-of-double))

#|
    Constant: align-of-int8_t

    struct x { char y; int8_t z; };

    -offset-of(x, z)
|#
(define align-of-int8_t (os-constant 'align-of-int8_t))

#|
    Constant: align-of-int16_t

    struct x { char y; int16_t z; };

    -offset-of(x, z)
|#
(define align-of-int16_t (os-constant 'align-of-int16_t))

#|
    Constant: align-of-int32_t

    struct x { char y; int32_t z; };

    -offset-of(x, z)
|#
(define align-of-int32_t (os-constant 'align-of-int32_t))

#|
    Constant: align-of-int64_t

    struct x { char y; int64_t z; };

    -offset-of(x, z)
|#
(define align-of-int64_t (os-constant 'align-of-int64_t))

#|
    Constant: on-darwin
|#
(define on-darwin        (string=? (host-os) "darwin"))

#|
    Constant: on-linux
|#
(define on-linux         (string=? (host-os) "linux"))

#|
    Constant: on-freebsd
|#
(define on-freebsd       (string=? (host-os) "freebsd"))

#|
    Constant: on-openbsd
|#
(define on-openbsd       (string=? (host-os) "openbsd"))

#|
    Constant: on-windows
|#
(define on-windows       (string=? (host-os) "windows"))

(define pointer-null
  (integer->pointer 0))

(define (pointer-null? pointer)
  (= 0 (pointer->integer pointer)))

(define (pointer-diff pointer-1 pointer-2)
  (- (pointer->integer pointer-1)
     (pointer->integer pointer-2)))

(define (pointer-add pointer offset)
  (integer->pointer (+ (pointer->integer pointer)
                       offset)))

(define-syntax define-pointer-comparison
  (syntax-rules ()
    ((_ ?name ?func)
     (define ?name
       (case-lambda
        (()
         #f)
        ((pointer)
         #t)
        ((pointer-a pointer-b)
         (?func (pointer->integer pointer-a)
                (pointer->integer pointer-b)))
        ((pointer-a pointer-b . pointers)
         (apply ?func (map pointer->integer
                           (cons pointer-a (cons pointer-b pointers))))))))))

(define-pointer-comparison pointer=? =)
(define-pointer-comparison pointer<? <)
(define-pointer-comparison pointer>? >)
(define-pointer-comparison pointer<=? <=)
(define-pointer-comparison pointer>=? >=)

(define pointer<>?
  (case-lambda
   (()
    #f)
   ((pointer . pointers)
    (not (apply pointer=? pointer pointers)))))


)

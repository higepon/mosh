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

    Example:
    (start code)
    (let* ([libmysqlclient (open-shared-library "libmysqlclient.so.15")]
           [mysql-init     (c-function libmysqlclient void* mysql_init void*)])
      (display (mysql-init 0)))
    (end code)

    library: (mosh ffi)

    Foreign Function Interface Library
|#
(library (mosh ffi)
  (export c-function open-shared-library
          (rename (%ffi-pointer->string pointer->string) (%ffi-pointer-ref pointer-ref)
                  (%ffi-supported? ffi-supported?)))
  (import (only (rnrs) define define-syntax syntax-case lambda map let syntax
                       quasiquote unless assertion-violation quote = length and number?
                       for-each apply hashtable-ref unquote integer? string? ... or zero?
                       for-all procedure?)
          (only (mosh) alist->eq-hash-table)
          (rename (system) (%ffi-open open-shared-library))
          (only (system) %ffi-lookup %ffi-call->void %ffi-call->void* %ffi-call->int))

#|
    Function: ffi-supported?

    Returns #t when ffi is supported, otherwise #f.

    Prototype:
    > (ffi-supported?)

    Returns:

      #t when ffi is supported, otherwise #f.
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
    Function: pointer-ref

    Refer the value of pointer[index].

    Prototype:
    > (pointer-ref pointer . index)

    Parameters:

      pointer - integer valued pointer. When not-pointer integer is passed, it may cause crash.
      index   - index. default values is 0.

    Returns:

      pointer value of pointer[index]
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
    > (c-function lib ret func arg ...)

    Parameters:

      lib - library object returned by <open-shared-library>
      ret - return type of c-function. void*, char*, void and int are supported.
      func - name of c-function as symbol
      arg ... - list of argument types. void*, int and char* are supported.

    Returns:

      Foreign function closure
|#
(define-syntax c-function
  (lambda (x)
    (syntax-case x ()
      [(_ lib ret func arg ...)
       #'(make-c-function lib 'ret 'func '(arg ...)))]))

(define stub-ht (alist->eq-hash-table
                 `((void* . ,%ffi-call->void*)
                   (char* . ,%ffi-call->string-or-zero) ;; char* may be NULL,
                   (void  . ,%ffi-call->void)
                   (int   . ,%ffi-call->int))))

(define checker-ht (alist->eq-hash-table
                    `((void* . ,integer?)
                      (int   . ,integer?)
                      (char* . ,(lambda (x) (or (and (number? x) (zero? x)) string?))))))

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
        (assertion-violation name "wrong arguments number" args))
      (for-each
       (lambda (checker arg)
         (unless (checker arg)
           (assertion-violation name "wrong argument " arg)))
       checkers
       args)
      (apply stub func args))))


)

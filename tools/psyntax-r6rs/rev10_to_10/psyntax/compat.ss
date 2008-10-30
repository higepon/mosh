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

(library (psyntax compat)
  (export make-parameter parameterize define-record compile-core
          gensym void eval-core symbol-value set-symbol-value! file-options-spec
          read-annotated annotation? annotation-expression annotation-source
          load-serialized-library serialize-library
          annotation-stripped make-record-printer read-library-source-file)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (mosh)
    (mosh file)
    (mosh string)
    (only (psyntax system $bootstrap)
          void gensym eval-core set-symbol-value! symbol-value)) ;; removed compile-core for mosh



  ;; defined for mosh
  (define read-annotated read)
;  (define (annotation-stripped x) (set-source-info! x #f))
  (define (annotation-stripped x) (annotation-expression x)) ;; what is difference between annotation-stripped and annotation-expression?
  (define (annotation? x) (source-info x))
;  (define (annotation? x) #f)
  (define (annotation-source x) (source-info x))
;  (define (annotation-source x) x)
  (define (annotation-expression x)
    (if (pair? x)
        (cons (car x) (cdr x))
        (display "line:46\n")))

(define (scm->fasl filename)
  (string-append filename ".fasl"))

(define (serialize-library filename obj)
  (format #t "serialize-library ~a\n..." filename)
  (let ([fasl-file (scm->fasl filename)])
    (when (file-exists? fasl-file)
      (delete-file fasl-file))
    (guard [c (#t (format #t "Warning:serialize-library failed " filename)
                  (when (file-exists? fasl-file)
                    (delete-file fasl-file))
                  #f)]
           (call-with-port (open-file-output-port fasl-file) (lambda (port) ((symbol-value 'fasl-write!) obj port)))
           (display "OK\n"))))

(define (load-serialized-library filename obj)
  (let ([fasl-file (scm->fasl filename)])
    (if (and (file-exists? fasl-file) ((symbol-value 'file-newer?) fasl-file filename)) ;; todo we may use file-newer? directory.
        (let* ([expanded2core (symbol-value 'expanded2core)]
               [code (call-with-port (open-file-input-port fasl-file) (symbol-value 'fasl-read!))]
               [pivot (cddddr (cddddr code))]
                          [visit (expanded2core (car pivot))]
                          [visit-proc (lambda () (eval-core visit))])
                     (set-car! pivot visit-proc)
                     (let* ([pivot (cdr pivot)]
                            [invoke (expanded2core (car pivot))])
                       (set-car! pivot (lambda () (eval-core invoke)))
                       (apply obj code))
                     #t)
        #f)))

  (define (make-record-printer name printer)
    (lambda x
      (display "record printer")
      (for-each display x)))
  ;; defined for mosh end

  (define (compile-core . x)
    (apply error 'comile-core "not implementated" x))

  (define (read-library-source-file file-name)
        (with-input-from-file file-name read-annotated))

  (define make-parameter
    (case-lambda
      ((x) (make-parameter x (lambda (x) x)))
      ((x fender)
;       (assert (procedure? fender))
       (let ((x (fender x)))
         (case-lambda
           (() x)
           ((v) (set! x (fender v))))))))

  (define-syntax parameterize
    (lambda (x)
      (syntax-case x ()
        ((_ () b b* ...) (syntax (let () b b* ...)))
        ((_ ((olhs* orhs*) ...) b b* ...)
         (with-syntax (((lhs* ...) (generate-temporaries (syntax (olhs* ...))))
                       ((rhs* ...) (generate-temporaries (syntax (olhs* ...)))))
           (syntax (let ((lhs* olhs*) ...
                   (rhs* orhs*) ...)
               (let ((swap
                      (lambda ()
                        (let ((t (lhs*)))
                          (lhs* rhs*)
                          (set! rhs* t))
                        ...)))
                 (dynamic-wind
                   swap
                   (lambda () b b* ...)
                   swap)))))))))

(define-syntax define-record
  (lambda (x)
      (define (syn->str s)
          (symbol->string
              (syntax->datum s)))
    (define (gen-getter id)
      (lambda (fld)
        (datum->syntax id
          (string->symbol
            (string-append (syn->str id) "-" (syn->str fld))))))
    (define (gen-setter id)
      (lambda (fld)
        (datum->syntax id
          (string->symbol
            (string-append "set-" (syn->str id) "-" (syn->str fld) "!")))))
    (syntax-case x ()
      [(_ name (field* ...) printer)
       #`(begin 
           (define-record name (field* ...)) 
           (define rp (make-record-printer 'name printer)))]
      [(_ name (field* ...))
       (with-syntax ([(getter* ...)
                      (map (gen-getter #'name) #'(field* ...))]
                     [(setter* ...)
                      (map (gen-setter #'name) #'(field* ...))])
         #`(define-record-type name
             (sealed #t) ; for better performance
             (opaque #t) ; for security
             (nongenerative) ; for sanity
             (fields (mutable field* getter* setter*) ...)))])))

  (define (file-options-spec x) x)
)

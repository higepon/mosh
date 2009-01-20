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

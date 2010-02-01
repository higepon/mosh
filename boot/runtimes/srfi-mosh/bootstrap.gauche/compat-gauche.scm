
;; A numeric string that uniquely identifies this run in the universe

(define now-time (sys-time))

(define (ex:unique-token) 
  (number->string now-time))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

;; Just give this damn thing a binding

(define assertion-violation 
  (lambda args 
    (display 'assertion-violation)
    (newline)
    (display args)
    (newline)
    (car #f)))

(define pretty-print write)

;; These are only partial implementations for specific use cases needed.
;; Full implementations should be provided by host implementation.

(define (memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (memp proc (cdr ls))))
        (else (assertion-violation 'memp "Invalid argument" ls))))

(define (filter p? lst)
  (if (null? lst)
      '()
      (if (p? (car lst))
          (cons (car lst)
                (filter p? (cdr lst)))
          (filter p? (cdr lst)))))

(define (for-all proc l . ls)
  (or (null? l)
      (and (apply proc (car l) (map car ls))
           (apply for-all proc (cdr l) (map cdr ls)))))

;; The best we can do in r5rs is make these no-ops

(define (file-exists? fn) 
  #f)

(define (delete-file fn)
  (values))

; FIXME FIXME FIXME! : we use vectors for records..

(define id-symbol (string->symbol (string-append "*IDENTIFIER*" (ex:unique-token))))

(define true-vector? vector?)
(define (hooked-vector? x)
  (and
    (vector? x)
    (not (= (vector-length x) 0))
    (not (eq? id-symbol (vector-ref x 0)))))


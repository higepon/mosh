
;; 
;(define (interaction-environment) #f)

;; A numeric string that uniquely identifies this run in the universe

(define BOGUS (lambda e #f))

(define (ex:unique-token) 
  (number->string (car (get-timeofday))))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

(define true-vector? vector?)
(define (hooked-vector? x)
  (and
    (vector? x)
    (if (= (vector-length x) 0)
      #t
      (not (eq? '*IDENTIFIER* (vector-ref x 0))))))
(define (debug-source-info x)
  (source-info x))

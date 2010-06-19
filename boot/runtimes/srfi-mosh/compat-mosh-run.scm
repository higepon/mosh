;; 
(define BOGUS (lambda e #f))

;; A numeric string that uniquely identifies this run in the universe
;NMOSH: it MUST be filesystem-safe (used by cache-system)
(define (ex:unique-token) 
  (number->compact-form (car (get-timeofday))))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

(define id-symbol '*ID*)

(define (debug-source-info x)
  (source-info x))

; Laceny's idea
(define (number->compact-form n)
  (define digits "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!-")
  (define (itr n cur)
    (if (= n 0)
      (if (null? cur)
	"0"
	(list->string cur))
      (itr (div n 64)
	   (cons
	     (string-ref digits (bitwise-and n 63))
	     cur))))
  (itr n '()))

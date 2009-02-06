(import (except (rnrs) let-values error define-record-type)
        (srfi :0)
        (srfi :1)
        (srfi :6)
        (srfi :8)
        (srfi :9)
        (srfi :11)
        (only (srfi :13) string-reverse string-concatenate)
        (srfi :14)
        (srfi :16)
        (srfi :19)
        (srfi :23)
        (srfi :39)
        (mosh test))

;;;;;  SRFI-0   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond-expand
 (mosh
   (define name 'mosh))
 (else
  (define name 'other)))

(test* name 'mosh)

;;;;;  SRFI-1   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (first '(1 2)) 1)
(test* (second '(1 2)) 2)

;;;;;  SRFI-6   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (let ((q (open-output-string))
             (x '(a b c)))
         (write (car x) q)
         (write (cdr x) q)
         (get-output-string q)) "a(b c)")

;;;;;  SRFI-8   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (receive (x y) (values 1 2)
         (- x y)) -1)

;;;;;  SRFI-9   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ()
  (define-record-type pare
    (kons x y) pare?
    (x kar set-kar!)
    (y kdr))
  (test/t (pare? (kons 2 3)))
  (test/f (pare? (cons 2 3))))

;;;;;  SRFI-11   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (let-values (([x y] (values 1 2)))
         (- x y)) -1)

;;;;;  SRFI-13   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (string-reverse "あいうえお") "おえういあ")
(test* (string-concatenate '("ab" "cd")) "abcd")

;;;;;  SRFI-14   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t (char-set #\a #\b #\c))

;;;;;  SRFI-16   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t (let ()
          (define plus
            (case-lambda 
             (() 0)
             ((x) x)
             ((x y) (+ x y))
             ((x y z) (+ (+ x y) z))
             (args (apply + args))))
          (test* (plus) 0)
          (test*  (plus 1) 1)
          (test*  (plus 1 2 3) 6)))

;;;;;  SRFI-19   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tested on another place

;;;;;  SRFI-23   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/exception error? (error "hoge" 1 2 3))

;;;;;  SRFI-39   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* ((make-parameter 1234)) 1234)

(test-end)



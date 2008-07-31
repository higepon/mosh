(import (rnrs))

(define-record-type (point make-point point?)
  (fields (immutable x point-x)
          (mutable y point-y set-point-y!))
  (nongenerative
    point-4893d957-e00b-11d9-817f-00111175eb9e))

(define-record-type (cpoint make-cpoint cpoint?)
  (parent point)
  (protocol
   (lambda (n)
     (lambda (x y c) 
       ((n x y) (color->rgb c)))))
  (fields
    (mutable rgb cpoint-rgb cpoint-rgb-set!)))

(define (color->rgb c)
  (cons 'rgb c))

(define p1 (make-point 1 2))
(define p2 (make-cpoint 3 4 red))

;; (point? p1)         ⇒ #t
;; (point? p2)         ⇒ #t
;; (point? (vector))         ⇒ #f
;; (point? (cons ’a ’b))         ⇒ #f
;; (cpoint? p1)         ⇒ #f
;; (cpoint? p2)         ⇒ #t
;; (point-x p1)         ⇒ 1
;; (point-y p1)         ⇒ 2
;; (point-x p2)         ⇒ 3
;; (point-y p2)         ⇒ 4
;; (cpoint-rgb p2)         ⇒ (rgb . red)

;; (set-point-y! p1 17)         ⇒ unspecified
;; (point-y p1)         ⇒ 17)

;; (record-rtd p1) 
;;                 ⇒ (record-type-desc

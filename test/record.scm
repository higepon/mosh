(import (rnrs)
        (mosh test))

(define (print str) (display str) (newline))

;; (define-syntax or
;;   (lambda (x)
;;     (syntax-case x ()
;;       [(_) (syntax #f)]
;;       [(_ e) (syntax e)]
;;       [(_ e1 e2 e3 ...)
;;        (syntax (let ([t e1])
;;                  (if t t (or e2 e3 ...))))])))

;; (define-syntax test-and-print
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ test expected)
;;        (syntax
;;         (let* ([result test]
;;                [test-ok? (equal? result expected)])
;;           (if test-ok?
;;               (format #t "OK: ~a => ~a\n" (syntax->datum (syntax test)) result)
;;               (format #t "NG: ~a => ~a expected ~a\n"
;;                       (syntax->datum (syntax test))
;;                       result
;;                       expected))))))))

(let ()
  (define :point
    (make-record-type-descriptor
     'point #f
     #f #f #f
     '#((mutable x) (mutable y))))
  (define :point-cd
    (make-record-constructor-descriptor :point #f #f))
  (define make-point (record-constructor :point-cd))
  (define point? (record-predicate :point))
  (define point-x (record-accessor :point 0))
  (define point-y (record-accessor :point 1))
  (define point-x-set! (record-mutator :point 0))
  (define point-y-set! (record-mutator :point 1))
  (define p1 (make-point 1 2))
  (test* (point? p1) #t)
  (test* (point? p1) #t)
  (test* (point-x p1) 1)
  (test* (point-y p1) 2)
  (point-y-set! p1 3)
  (test* (point-y p1) 3))

(let ()
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
  (define p2 (make-cpoint 3 4 'red))
  (test* (point? p1) #t)
  (test* (point? p2) #t)
  (test* (point? (vector)) #f)
  (test* (point? (cons 'a 'b)) #f)
  (test* (cpoint? p1) #f)
  (test* (cpoint? p2) #t)
  (test* (point-x p1) 1)
  (test* (point-y p1) 2)
  (test* (point-x p2) 3)
  (test* (point-y p2) 4)
  (test* (cpoint-rgb p2) '(rgb . red))
  (set-point-y! p1 17)
  (test* (point-y p1) 17)
  (print (record-rtd p1)))

(let ()
  (define-record-type (ex1 make-ex1 ex1?)
    (protocol (lambda (p) (lambda a (p a))))
    (fields (immutable f ex1-f)))
  (define ex1-i1 (make-ex1 1 2 3))
  (test* (ex1-f ex1-i1) '(1 2 3)))

(let ()
  (define-record-type (ex2 make-ex2 ex2?)
    (protocol
     (lambda (p) (lambda (a . b) (p a b))))
    (fields (immutable a ex2-a)
            (immutable b ex2-b)))
  (define ex2-i1 (make-ex2 1 2 3))
  (test* (ex2-a ex2-i1) 1)
  (test* (ex2-b ex2-i1) '(2 3)))

(let ()
  (define (color->rgb c)
    (cons 'rgb c))
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
  (define-record-type (unit-vector
                       make-unit-vector
                       unit-vector?)
    (protocol
     (lambda (p)
       (lambda (x y z)
         (let ((length
                (sqrt (+ (* x x)
                         (* y y)
                         (* z z)))))
           (p (/ x length)
              (/ y length)
              (/ z length))))))
    (fields (immutable x unit-vector-x)
            (immutable y unit-vector-y)
            (immutable z unit-vector-z)))
  (define *ex3-instance* #f)
  (define-record-type ex3
    (parent cpoint)
    (protocol
     (lambda (n)
       (lambda (x y t)
         (let ((r ((n x y 'red) t)))
           (set! *ex3-instance* r)
           r))))
    (fields
     (mutable thickness))
    (sealed #t) (opaque #t)) ; this is bug of psyntax (sealed? #t) (opaque? #t))
  (define ex3-i1 (make-ex3 1 2 17))
  (test* (ex3? ex3-i1) #t)
  (test* (cpoint-rgb ex3-i1) '(rgb . red))
;  (test* (ex3-thickness ex3-i1) 17)
  (ex3-thickness-set! ex3-i1 18);   ⇒ unspecified
  (test* (ex3-thickness ex3-i1) 18)
  (print *ex3-instance*)
  (test* (record? ex3-i1) #f)
)

 (import (rnrs)
        (srfi :99))

(define rtd1
  (make-rtd 'rtd1 '#((immutable x1) (immutable x2))))

(define rtd2
  (make-rtd 'rtd2 '#((immutable x3) (immutable x4)) rtd1))

(define rtd3
  (make-rtd 'rtd3 '#((immutable x5) (immutable x6)) rtd2))

(define protocol1
  (lambda (p)
    (lambda (a b c)
      (p (+ a b) (+ b c)))))

(define protocol2
  (lambda (n)
    (lambda (a b c d e f)
      (let ((p (n a b c)))
        (p (+ d e) (+ e f))))))

(define protocol3
  (lambda (n)
    (lambda (a b c d e f g h i)
      (let ((p (n a b c d e f)))
        (p (+ g h) (+ h i))))))

(define make-rtd1
  (protocol1 (rtd-constructor rtd1)))

(define make-rtd2
  (let ((maker2 (rtd-constructor rtd2)))
    (protocol2
     (protocol1
      (lambda (x1 x2)
        (lambda (x3 x4)
          (maker2 x1 x2 x3 x4)))))))

(define make-rtd3
  (let ((maker3 (rtd-constructor rtd3)))
    (protocol3
     (protocol2
      (protocol1
       (lambda (x1 x2)
         (lambda (x3 x4)
           (lambda (x5 x6)
             (maker3 x1 x2 x3 x4 x5 x6)))))))))

(display (make-rtd3 1 2 3 4 5 6 7 8 9))

(define (list-transpose+ . ll)
  (define (map1 f l)
    (if (null? l)
        l
        (cons (f (car l)) (map1 f (cdr l)))))
  (define (map proc . ll)
    (if (null? (car ll))
        '()
        (if (null? (cdr ll))
            (map1 proc (car ll))
            (let ((tetes (map1 car ll))
                  (queues (map1 cdr ll)))
              (cons (apply proc tetes)
                    (apply map (cons proc queues)))))))
  (let loop ([lst ll]
             [ret '()])
    (format #t "lst=~a\n" lst)
    (cond
     [(null? lst) (reverse ret)]
     [else
      (loop (cdr lst) (cons (map car (car lst)) ret))])))

(display (list-transpose+ '(a b c) '(A B C)))


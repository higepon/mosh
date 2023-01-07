(define (map1 f l)
    (if (null? l)
        l
        (cons (f (car l)) (map1 f (cdr l)))))

(define for-all
  (lambda (pred lst1 . lst2)
    (cond ((null? lst2)
           (for-all-1 pred lst1))
          ((apply list-transpose+ lst1 lst2)
           => (lambda (lst) (for-all-n-quick pred lst)))
          (else
           (assertion-violation 'for-all "expected same length proper lists" (cons* lst1 lst2))))))

(define for-all-1
  (lambda (pred lst)
    (cond ((null? lst) #t)
          ((pair? lst)
           (let loop ((head (car lst)) (rest (cdr lst)))
             (cond ((null? rest) (pred head))
                   ((pair? rest)
                    (and (pred head)
                         (loop (car rest) (cdr rest))))
                   (else
                    (and (pred head)
                         (assertion-violation 'for-all (format "traversal reached to non-pair element ~s" rest) (list pred lst)))))))
          (else
           (assertion-violation 'for-all (format "expected chain of pairs, but got ~r, as argument 2" lst) (list pred lst))))))

(define for-all-n-quick
  (lambda (pred lst)
    (or (null? lst)
        (let loop ((head (car lst)) (rest (cdr lst)))
          (if (null? rest)
              (apply pred head)
              (and (apply pred head)
                   (loop (car rest) (cdr rest))))))))

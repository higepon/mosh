(import (rnrs)
        (mosh)
        (match)
        (only (srfi :1) split-at)
        (system))

(define lst '((CONST) (REFER) (BRANCH 2) (X) (BT -1) (Y) (Z)))

(define (insert lst i obj)
  (let-values (([x y] (split-at lst i)))
    (append x (lst obj) y)))

(define (collect-labels lst)
  (let loop ([i 0]
             [lst lst]
             [ret '()])
    (cond
     [(null? lst) (list-sort (lambda (x y) (> (cdr x) (cdr y))) ret)]
     [else
      (match (car lst)
        [((or 'BRANCH 'BT) x)
         (loop (+ i 1) (cdr lst) (cons (cons (gensym) (+ x i)) ret))]
        [else
         (loop (+ i 1) (cdr lst) ret)])])))

(define (insert-labels lst labels)
  (let loop ([labels labels]
             [lst lst])
    (cond
     [(null? labels) lst]
     [else
      (format #t "labels=~a\n" labels)
      (loop (cdr labels) (insert lst (cdar labels) (caar labels)))])))


(display (insert-labels lst (collect-labels lst)))



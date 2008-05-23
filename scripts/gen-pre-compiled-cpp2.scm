#!/usr/bin/env gosh
(use srfi-1)
(use srfi-43)
(use gauche.sequence)



(define (collect-list lst)
  (define (rec lst)
    (cond
     [(null? lst) '()]
     [(number? (car lst))
      (rec (cdr lst))]
     [(list? (car lst))
      (append (list (car lst)) (rec (car lst)) (rec (cdr lst)))]
     [else
      (error "hige")]))
  (reverse (rec lst)))

(define ht (make-hash-table))
(define inc 0)

(define (scm->cpp src lst)
  (define (rec obj)
    (cond
     [(number? obj)
      (format #t "Object::makeInt(~d)" obj)]
     [(list? obj)
      (if (hash-table-get ht obj #f)
          (format #t "<found ~d>" (hash-table-get ht obj #f))
          (begin
            (hash-table-put! ht obj inc)
            (set! inc (+ inc 1))
            (format #t "Object* array = {")
            (for-each (lambda (x) (format #t "~a, " (rec x))) obj)
            (format #t "};\n")
            ))]))
  (rec src))

(let* ([source '(1 (2 3 (4 5 (6 7))))]
       [collected (collect-list source)])
  (print collected)
  (print (map (lambda (x) (scm->cpp x collected)) collected)))

(import (rnrs)
        (mosh)
        (mosh test)
        (mosh control)
        (match)
        (only (srfi :1) split-at)
        (system))

(define lst '((CLOSURE 36 1 #f 0 11 (#f bC1@fib bC3@n))
              (REFER_LOCAL_PUSH_CONSTANT 0 2)
              (BRANCH_NOT_LT 5)
              (CONSTANT 1)
              (RETURN 1)
              (FRAME 8)
              (REFER_LOCAL_PUSH_CONSTANT 0 2)))

(define (map-accum proc seed lst)
  (let loop ([lst lst]
             [accum '()]
             [seed seed])
    (cond
     [(null? lst) (values (reverse accum) seed)]
     [else
      (let-values (([a s] (proc (car lst) seed)))
        (loop (cdr lst) (cons a accum) s))])))

;; Instruction is just a serialized list
;; We convert the list to a list of instruction packet.
;; (CONST 3 PUSH FRAME 3) => (((CONST 3) . 0) ((PUSH) . 2) ((FRAME 3). 3))
(define (pack-instruction lst)
  (let loop ([lst lst]
             [accum '()]
             [packed '()])
    (cond
     [(null? lst)
      (let1 packed (reverse (cons (reverse accum) packed))
        (map-accum
         (lambda (x seed) (values (cons x seed) (+ seed (length x))))
         0
         packed))]
     [(symbol? (car lst)) ;; this should be instrucion
      (loop (cdr lst)
            (cons (car lst) '())
            (if (null? accum) packed (cons (reverse accum) packed)))]
     [else
      (loop (cdr lst)
            (cons (car lst) accum)
            packed)])))


(define (insert lst i obj)
  (format #t "lst=~a i =~a obj=~a\n" lst i obj)
  (let-values (([x y] (split-at lst i)))
    (append x (list obj) y)))

(define (collect-labels lst)
  (let loop ([i 0]
             [lst lst]
             [ret '()])
    (cond
     [(null? lst) (list-sort (lambda (x y) (> (cdr x) (cdr y))) ret)]
     [else
      (match (caar lst)
        [((or 'BRANCH_NOT_LT 'BT) x)
         (loop (+ i (length (caar lst))) (cdr lst) (cons `((label ,(gensym)) . ,(+ x i 1)) ret))]
        [else
         (loop (+ i (length (caar lst))) (cdr lst) ret)])])))

(define (insert-labels lst labels)
  (let loop ([labels labels]
             [lst lst]
             [ret '()])
    (cond
     [(null? lst)
      (unless (null? labels)
        (error 'insert-labels "hoge"))
        (reverse ret)]
     [(and (not (null? labels)) (= (cdar labels) (cdar lst)))
      (loop (cdr labels) (cdr lst) (append (list (car lst) (car labels)) ret))]
     [else
      (loop labels (cdr lst) (cons (car lst) ret))])))



(test-equal
'(((CLOSURE 36 1 #f 0 11 (#f bC1@fib bC3@n)) . 0)
                  ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 7)
                  ((BRANCH_NOT_LT 5) . 10)
                  ((CONSTANT 1) . 12)
                  ((RETURN 1) . 14)
                  ((label labela) . 16)
                  ((FRAME 8) . 16)
                  ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 18))
 (insert-labels '(((CLOSURE 36 1 #f 0 11 (#f bC1@fib bC3@n)) . 0)
                  ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 7)
                  ((BRANCH_NOT_LT 5) . 10)
                  ((CONSTANT 1) . 12)
                  ((RETURN 1) . 14)
                  ((FRAME 8) . 16)
                  ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 18))
                '(((label labela) . 16))))

(test-true (match (collect-labels
                   '(((CLOSURE 36 1 #f 0 11 (#f bC1@fib bC3@n)) . 0)
                     ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 7)
                     ((BRANCH_NOT_LT 5) . 10)
                     ((CONSTANT 1) . 12)
                     ((RETURN 1) . 14)
                     ((FRAME 8) . 16)
                     ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 18)))
             [((('label _) . 16)) #t]
             [else #f]))

(test-true (match (collect-labels
                   '(((CLOSURE 36 1 #f 0 11 (#f bC1@fib bC3@n)) . 0)
                     ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 7)
                     ((BRANCH_NOT_LT 5) . 10)
                     ((CONSTANT 1) . 12)
                     ((RETURN 1) . 14)
                     ((FRAME 8) . 16)
                     ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 18)
                     ((BRANCH_NOT_LT 1) . 21)
                     ((CONSTANT 1) . 23)))
             [((('label _1) . 23) (('label _2) . 16)) #t]
             [else #f]))



(test-equal '(
              ((CLOSURE 36 1 #f 0 11 (#f bC1@fib bC3@n)) . 0)
              ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 7)
              ((BRANCH_NOT_LT 5) . 10)
              ((CONSTANT 1) . 12)
              ((RETURN 1) . 14)
              ((FRAME 8) . 16)
              ((REFER_LOCAL_PUSH_CONSTANT 0 2) . 18))
            (pack-instruction
             '(CLOSURE 36 1 #f 0 11 (#f bC1@fib bC3@n)
              REFER_LOCAL_PUSH_CONSTANT 0 2
              BRANCH_NOT_LT 5
              CONSTANT 1
              RETURN 1
              FRAME 8
              REFER_LOCAL_PUSH_CONSTANT 0 2)))

(test-equal '((((a) . 0) ((b c) . 1) ((d e f) . 3)) 6)
            (let-values (([accum seed] (map-accum
                                        (lambda (x seed) (values (cons x seed) (+ seed (length x))))
                                        0
                                        '((a) (b c) (d e f)))))
              (list accum seed)))

(test-results)

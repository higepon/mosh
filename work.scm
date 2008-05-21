
(define result '(69 ((generic-assoc . 10000) (hoge . 3) (hige . 13) (append . 100000000) ($append-map1-sum . 2)) generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc generic-assoc append $append-map1-sum))


;;; borrowed from elk
(define (sort obj pred)
  (vector->list (sort! (list->vector obj) pred)))

(define (sort! v pred)
    (define (internal-sort l r)
      (let ((i l) (j r) (x (vector-ref v (/ (- (+ l r) 1) 2))))
    (let loop ()
         (do () ((not (pred (vector-ref v i) x))) (set! i (+ 1 i)))
         (do () ((not (pred x (vector-ref v j)))) (set! j (- j 1)))
         (if (<= i j)
         (let ((temp (vector-ref v i)))
           (vector-set! v i (vector-ref v j))
           (vector-set! v j temp)
           (set! i (+ 1 i))
           (set! j (- j 1))))
         (if (<= i j)
         (loop)))
    (if (< l j)
        (internal-sort l j))
    (if (< i r)
        (internal-sort i r))))
  (let ((len (vector-length v)))

    (if (> len 1)
    (internal-sort 0 (- len 1)))
    v))


(define (lpad str pad n)
  (let1 rest (- n (string-length (format "~a" str)))
    (let loop ([rest rest]
               [ret (format "~a" str)])
      (if (<= rest 0)
          ret
          (loop (- rest 1) (string-append pad ret))))))

(define (rpad str pad n)
  (let1 rest (- n (string-length (format "~a" str)))
    (let loop ([rest rest]
               [ret (format "~a" str)])
      (if (<= rest 0)
          ret
          (loop (- rest 1) (string-append ret pad))))))


;; result format is '(total-sample-count sym1 sym2 ... symn)
(define (show-profile result)
  (let ([total (first result)]
        [calls (second result)]
        [syms  (cddr result)]
        [table (make-eq-hashtable)])
    (print "time%       msec      calls   name")
    (let loop ([syms syms])
      (cond
       [(null? syms)
          '()]
       [else
        (aif (hash-table-ref table (car syms) #f)
            (hash-table-set! table (car syms) (+ it 1))
            (hash-table-set! table (car syms) 1))
        (loop (cdr syms))]))
    (for-each
     (lambda (x)
       (let1 call-info (assoc (first x) calls)
         (format #t " ~a   ~a ~a   ~a    ~a\n" (lpad (third x) " " 2) (lpad (* (second x) 10) " " 10) (lpad (cdr call-info) " " 10) (rpad (first x) " " 30))))
     (sort
      (hash-table-map
       (lambda (key value)
         (list key value (/ (* 100 value) total)))
       table)
      (lambda (x y) (> (third x) (third y)))))
    (let1 seen-syms (vector->list (hash-table-keys table))
    (for-each
     (lambda (p)
         (format #t "  0            0 ~a   ~a\n" (lpad (cdr p) " " 10) (rpad (car p) " " 30)))
     (sort (filter (lambda (x) (not (memq (car x) seen-syms))) calls) (lambda (a b) (< a b)))))
    (format #t "  **  ~d         **   total\n" (lpad (* (* total 10)) " " 10))))


(define (show-ht ht)
  (for-each
   (lambda (x)
     (format #t "~a ~a\n" (first x) (second x)))
   ht))

(show-profile result)

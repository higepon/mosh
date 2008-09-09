(#\space)





























(import (rnrs))
=======
;; quick hack
;; (define-macro (let-values clause . body)
;; `(receive ,(car clause
;; )
>>>>>>> .r387

;; quick hack
(define-macro (let-values clause . body)
  `(receive ,(caar clause) ,(cadar clause)
       ,@body))
(define list-head take)


<<<<<<< .mine

;; (let ([hashtable (make-eq-hashtable)])
;;   (hashtable-set! hashtable 'red 'apple)
;;   (display (hashtable-ref hashtable 'red)))
=======
  (define list-sort
    (lambda (proc lst)
>>>>>>> .r387

      (define merge
        (lambda (lst1 lst2)
          (cond
           ((null? lst1) lst2)
           ((null? lst2) lst1)
           (else
            (if (proc (car lst2) (car lst1))
                (cons (car lst2) (merge lst1 (cdr lst2)))
                (cons (car lst1) (merge (cdr lst1) lst2)))))))

      (define sort
        (lambda (lst n)
          (cond ((= n 1)
                 (list (car lst)))
                ((= n 2)
                 (if (proc (cadr lst) (car lst))
                     (list (cadr lst) (car lst))
                     (list (car lst) (cadr lst))))
                (else
                 (let ((n/2 (div n 2)))
                   (merge (sort lst n/2)
                          (sort (list-tail lst n/2) (- n n/2))))))))

      (define divide
        (lambda (lst)
          (let loop ((acc 1) (lst lst))
            (cond ((null? (cdr lst)) (values acc '()))
                  (else
                   (if (proc (car lst) (cadr lst))
                       (loop (+ acc 1) (cdr lst))
                       (values acc (cdr lst))))))))

      (cond ((null? lst) '())
            (else
             (let ((len (length lst)))
               (let-values (((n rest) (divide lst)))
                 (cond ((null? rest) lst)
                       (else
                        (merge (list-head lst n)
                               (sort rest (- len n)))))))))))

  (define vector-sort
    (lambda (proc vect)
      (let ((lst (vector->list vect)))
        (let ((lst2 (list-sort proc lst)))
          (cond ((eq? lst lst2) vect)
                (else
                 (list->vector lst2)))))))

  (define vector-sort!
    (lambda (proc vect)
      (let* ((n (vector-length vect)) (work (make-vector (+ (div n 2) 1))))

        (define simple-sort!
          (lambda (first last)
            (let loop1 ((i first))
              (cond ((< i last)
                     (let ((m (vector-ref vect i)) (k i))
                       (let loop2 ((j (+ i 1)))
                         (cond ((<= j last)
                                (if (proc (vector-ref vect j) m)
                                    (begin
                                      (set! m (vector-ref vect j))
                                      (set! k j)))
                                (loop2 (+ j 1)))
                               (else
                                (vector-set! vect k (vector-ref vect i))
                                (vector-set! vect i m)
                                (loop1 (+ i 1)))))))))))

        (define sort!
          (lambda (first last)
            (cond ((> (- last first) 10)
                   (let ((middle (div (+ first last) 2)))
                     (sort! first middle)
                     (sort! (+ middle 1) last)
                     (let loop ((i first) (p2size 0))
                       (cond ((> i middle)
                              (let loop ((p1 (+ middle 1)) (p2 0) (p3 first))
                                (cond ((and (<= p1 last) (< p2 p2size))
                                       (cond ((proc (vector-ref work p2) (vector-ref vect p1))
                                              (vector-set! vect p3 (vector-ref work p2))
                                              (loop p1 (+ p2 1) (+ p3 1)))
                                             (else
                                              (vector-set! vect p3 (vector-ref vect p1))
                                              (loop (+ p1 1) p2 (+ p3 1)))))
                                      (else
                                       (let loop ((s2 p2)(d3 p3))
                                         (cond ((< s2 p2size)
                                                (vector-set! vect d3 (vector-ref work s2))
                                                (loop (+ s2 1) (+ d3 1)))))))))
                             (else
                              (vector-set! work p2size (vector-ref vect i))
                              (loop (+ i 1) (+ p2size 1)))))))
                  (else
                   (simple-sort! first last)))))

        (sort! 0 (- n 1)))))

(display (list-sort > '(1 5 3)))

;; (import (rnrs))


























;; (import (rnrs))

;; (define-record-type (point make-point point?)
;;   (fields (mutable x)
;;           (immutable y)))

;; (let ([p (make-point 3 4)])
;;   (display (point-x p))) ; => 3

;; ;; (let ([hashtable (make-eq-hashtable)])
;; ;;   (hashtable-set! hashtable 'red 'apple)
;; ;;   (display (hashtable-ref hashtable 'red)))

;; ;; (import (rnrs))
;; ;; (guard
;; ;;  [con
;; ;;   [(message-condition? con)
;; ;;    (display (condition-message con))]]
;; ;;  (car 3))

;; (exit)
;; ;; ;(display (string? #vu8(1 2)))
;; ;; (exit)
;; ;; ;(display #e1e100)
;; ;; ;#e10e
;; ;; (exit)
;; ;; '#vu8(1 2)
;; ;; (exit)
;; ;; (call-with-string-input-port "("
;; ;;      (lambda (in)
;; ;;        (read in)))
;; ;; (call-with-input-file "./hige.scm"
;; ;;   (lambda (in)
;; ;;     (read in)))

;; ;; (exit)

;; ;; (define (hige) 3)


;; ;; (hige)
;; ;; (exit)
;; ;; (define (symbol-append . symbols)
;; ;;   (string->symbol (apply string-append (map symbol->string symbols))))

;; ;; (define-macro (define-simple-struct name fields)
;; ;;   (let ([rtd (symbol-append name '-rtd)]
;; ;;         [rcd (symbol-append name '-rcd)]
;; ;;         [accessors (map (lambda (field) (symbol-append name '- field)) fields)]
;; ;;         [field-set (list->vector (map (lambda (field) (list 'mutable field)) fields))]
;; ;;         [constructor (symbol-append 'make- name)])
;; ;;      `(begin
;; ;;         (define ,rtd
;; ;;           (make-record-type-descriptor
;; ;;            ',name #f #f #f #f
;; ;;            ',field-set))
;; ;;         (define ,rcd
;; ;;           (make-record-constructor-descriptor
;; ;;            ,rtd #f #f))
;; ;;         (define ,constructor
;; ;;           (record-constructor ,rcd))
;; ;;         ,@(let loop ([i 0]
;; ;;                      [accessors accessors]
;; ;;                      [ret '()])
;; ;;            (if (null? accessors)
;; ;;                ret
;; ;;                (loop (+ i 1) (cdr accessors) (cons (list 'define (car accessors) (list 'record-accessor rtd i) ) ret))))
;; ;;         )))

;; ;; (define-macro (t exp equal expected )
;; ;;   (let ([val (gensym)])
;; ;;     `(let ([,val ,exp])
;; ;;        (unless (equal? ,expected ,val)
;; ;;          (error 'test (format "~a failed" ',exp) (list ,expected '=> ,val))))))

;; ;; (define-simple-struct enum-set (type members))
;; ;; (define-simple-struct enum-type (universe indexer))

;; ;; (define (make-enumeration-type symbol-list)
;; ;;   (let ([ht (make-eq-hashtable)])
;; ;;     (let loop ([symbol-list symbol-list]
;; ;;                [i 0])
;; ;;       (if (null? symbol-list)
;; ;;           '()
;; ;;           (begin (hashtable-set! ht (car symbol-list) i)
;; ;;                  (loop (cdr symbol-list) (+ i 1)))))
;; ;;     (make-enum-type symbol-list
;; ;;                     (lambda (symbol)
;; ;;                       (hashtable-ref ht symbol #f)))))


;; ;; (define (make-enumeration symbol-list)
;; ;;   (make-enum-set (make-enumeration-type symbol-list) symbol-list))

;; ;; (define (enum-set-universe enum-set)
;; ;;   (make-enum-set (enum-set-type enum-set)
;; ;;                  (enum-type-universe (enum-set-type enum-set))))

;; ;; (define (enum-set-indexer enum-set)
;; ;;   (enum-type-indexer (enum-set-type enum-set)))

;; ;; (define (enum-set-constructor enum-set)
;; ;;   (lambda (symbol-list)
;; ;;     (let ([universe (enum-type-universe (enum-set-type enum-set))])
;; ;;       (if (for-all (lambda (x) (memq x universe)) symbol-list)
;; ;;           (make-enum-set (enum-set-type enum-set) symbol-list)
;; ;;           (assertion-violation 'enum-set-constructor "the symbol list must all belong to the universe." (list universe symbol-list))))))

;; ;; (define (enum-set->list enum-set)
;; ;;   (let ([universe (enum-type-universe (enum-set-type enum-set))]
;; ;;         [members (enum-set-members enum-set)])
;; ;;     (let loop ([universe universe])
;; ;;       (cond
;; ;;        [(null? universe) '()]
;; ;;        [(memq (car universe) members)
;; ;;         (cons (car universe) (loop (cdr universe)))]
;; ;;        [else
;; ;;         (loop (cdr universe))]))))

;; ;; (define (enum-set-member? symbol enum-set)
;; ;;   (and (memq symbol (enum-set-members enum-set)) #t))

;; ;; (define (enum-set-subset? enum-set1 enum-set2)
;; ;;   (and
;; ;;    (let ([enum-set2-univese (enum-set->list (enum-set-universe enum-set2))])
;; ;;      (for-all
;; ;;       (lambda (symbol) (memq symbol enum-set2-univese))
;; ;;       (enum-set->list (enum-set-universe enum-set1))))
;; ;;    (for-all
;; ;;     (lambda (symbol) (enum-set-member? symbol enum-set2))
;; ;;     (enum-set-members enum-set1))))

;; ;; (define (enum-set=? enum-set1 enum-set2)
;; ;;   (and (enum-set-subset? enum-set1 enum-set2)
;; ;;        (enum-set-subset? enum-set2 enum-set1)))

;; ;; (define (enum-set-union enum-set1 enum-set2)
;; ;;   (define (union lst1 lst2)
;; ;;     (let loop ([ret lst1]
;; ;;                [lst lst2])
;; ;;       (cond
;; ;;        [(null? lst) ret]
;; ;;        [(memq (car lst) ret)
;; ;;         (loop ret (cdr lst))]
;; ;;        [else
;; ;;         (loop (cons (car lst) ret) (cdr lst))])))
;; ;;   (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
;; ;;       (make-enum-set (enum-set-type enum-set1)
;; ;;                      (union (enum-set-members enum-set1) (enum-set-members enum-set2)))
;; ;;       (assertion-violation 'enum-set-union "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

;; ;; (define (enum-set-intersection enum-set1 enum-set2)
;; ;;   (define (intersection lst1 lst2)
;; ;;     (let loop ([ret '()]
;; ;;                [lst lst1])
;; ;;       (if (null? lst)
;; ;;           ret
;; ;;           (cond
;; ;;            [(memq (car lst) lst2)
;; ;;              (loop (cons (car lst) ret) (cdr lst))]
;; ;;            [else
;; ;;             (loop ret (cdr lst))]))))
;; ;;   (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
;; ;;       (make-enum-set (enum-set-type enum-set1)
;; ;;                      (intersection (enum-set-members enum-set1) (enum-set-members enum-set2)))
;; ;;       (assertion-violation 'enum-set-intersection "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

;; ;; (define (enum-set-difference enum-set1 enum-set2)
;; ;;   (define (difference lst1 lst2)
;; ;;     (let loop ([ret '()]
;; ;;                [lst lst1])
;; ;;       (if (null? lst)
;; ;;           ret
;; ;;           (cond
;; ;;            [(memq (car lst) lst2)
;; ;;             (loop ret (cdr lst))]
;; ;;            [else
;; ;;             (loop (cons (car lst) ret) (cdr lst))]))))
;; ;;   (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
;; ;;       (make-enum-set (enum-set-type enum-set1)
;; ;;                      (difference (enum-set-members enum-set1) (enum-set-members enum-set2)))
;; ;;       (assertion-violation 'enum-set-difference "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

;; ;; (define (enum-set-complement enum-set)
;; ;;   (let ([members (enum-set-members enum-set)])
;; ;;     (make-enum-set (enum-set-type enum-set)
;; ;;                    (filter (lambda (symbol) (not (memq symbol members))) (enum-type-universe (enum-set-type enum-set))))))

;; ;; (define (enum-set-projection enum-set1 enum-set2)
;; ;;   (if (enum-set-subset? enum-set1 enum-set2)
;; ;;       enum-set1
;; ;;       (let ([universe2 (enum-type-universe (enum-set-type enum-set2))]
;; ;;             [members1 (enum-set-members enum-set1)])
;; ;;         (make-enum-set (enum-set-type enum-set2)
;; ;;                        (filter (lambda (symbol) (memq symbol universe2)) members1)))))

;; ;; (t (let* ((e (make-enumeration '(red green blue)))
;; ;;           (i (enum-set-indexer e)))
;; ;;      (list (i 'red) (i 'green) (i 'blue) (i 'yellow)))
;; ;;    =>
;; ;;    '(0 1 2 #f))

;; ;; (t (enum-set->list (make-enumeration '(red green blue)))
;; ;;    =>
;; ;;    '(red green blue))

;; ;; (t (let* ((e (make-enumeration '(red green blue)))
;; ;;           (c (enum-set-constructor e)))
;; ;;      (enum-set->list (c '(blue red))))
;; ;;    =>
;; ;;    '(red blue))

;; ;; (t (let* ((e (make-enumeration '(red green blue)))
;; ;;           (c (enum-set-constructor e)))
;; ;;      (list
;; ;;       (enum-set-member? 'blue (c '(red blue)))
;; ;;       (enum-set-member? 'green (c '(red blue)))
;; ;;       (enum-set-subset? (c '(red blue)) e)
;; ;;       (enum-set-subset? (c '(red blue)) (c '(blue red)))
;; ;;       (enum-set-subset? (c '(red blue)) (c '(red)))
;; ;;       (enum-set=? (c '(red blue)) (c '(blue red)))))
;; ;;    =>
;; ;;    '(#t #f #t #t #f #t))

;; ;; (t (guard [c (#t 'error)]
;; ;;           (let* ((e (make-enumeration '(red green blue)))
;; ;;                  (c (enum-set-constructor e)))
;; ;;             (c '(pink))))
;; ;;    =>
;; ;;    'error)

;; ;; (let* ((e (make-enumeration '(red green blue)))
;; ;;        (r ((enum-set-constructor e) '(red))))
;; ;;   (t (enum-set->list (enum-set-universe e))
;; ;;      => '(red green blue))

;; ;;   (t (enum-set->list (enum-set-universe r))
;; ;;      => '(red green blue))

;; ;;   (t ((enum-set-indexer
;; ;;           ((enum-set-constructor e) '(red)))
;; ;;          'green)
;; ;;      => 1)

;; ;;   (t (enum-set-member? 'red e)
;; ;;      => #t)

;; ;;   (t (enum-set-member? 'black e)
;; ;;      => #f)

;; ;;   (t (enum-set-subset? e e)
;; ;;      => #t)

;; ;;   (t (enum-set-subset? r e)
;; ;;      => #t)

;; ;;   (t (enum-set-subset? e r)
;; ;;      => #f)

;; ;;   (t (enum-set-subset? e (make-enumeration '(blue green red)))
;; ;;      => #t)
;; ;;   (t (enum-set-subset? e (make-enumeration '(blue green red black)))
;; ;;      => #t)
;; ;;   (t (enum-set-subset? (make-enumeration '(blue green red black)) e)
;; ;;      => #f)
;; ;;   (t (enum-set-subset? ((enum-set-constructor
;; ;;                          (make-enumeration '(blue green red black)))
;; ;;                         '(red))
;; ;;                        e)
;; ;;      => #f)
;; ;;   (t (enum-set-subset? ((enum-set-constructor
;; ;;                          (make-enumeration '(green red)))
;; ;;                         '(red))
;; ;;                        e)
;; ;;      => #t)
;; ;;   (t (enum-set=? e e)
;; ;;      => #t)
;; ;;   (t (enum-set=? r e)
;; ;;      => #f)
;; ;;   (t (enum-set=? e r)
;; ;;      => #f)
;; ;;   (t (enum-set=? e (make-enumeration '(blue green red)))
;; ;;      => #t)
;; ;; )

;; ;; (t (let* ((e (make-enumeration '(red green blue)))
;; ;;           (c (enum-set-constructor e)))
;; ;;      (list
;; ;;       (enum-set-member? 'blue (c '(red blue)))
;; ;;       (enum-set-member? 'green (c '(red blue)))
;; ;;       (enum-set-subset? (c '(red blue)) e)
;; ;;       (enum-set-subset? (c '(red blue)) (c '(blue red)))
;; ;;       (enum-set-subset? (c '(red blue)) (c '(red)))
;; ;;       (enum-set=? (c '(red blue)) (c '(blue red)))))
;; ;;    => (list #t #f #t #t #f #t))

;; ;; (t (let* ((e (make-enumeration '(red green blue)))
;; ;;           (c (enum-set-constructor e)))
;; ;;      (enum-set->list (c '(blue red))))
;; ;;    => '(red blue))

;; ;; (t (let* ((e (make-enumeration '(red green blue)))
;; ;;           (c (enum-set-constructor e)))
;; ;;      (list
;; ;;       (enum-set->list
;; ;;        (enum-set-union (c '(blue)) (c '(red))))
;; ;;       (enum-set->list
;; ;;        (enum-set-intersection (c '(red green))
;; ;;                               (c '(red blue))))
;; ;;       (enum-set->list
;; ;;        (enum-set-difference (c '(red green))
;; ;;                             (c '(red blue))))))
;; ;;      => '((red blue) (red) (green)))

;; ;; (t (let* ((e (make-enumeration '(red green blue)))
;; ;;           (c (enum-set-constructor e)))
;; ;;      (enum-set->list
;; ;;       (enum-set-complement (c '(red)))))
;; ;;    =>
;; ;;    '(green blue))

;; ;; (t (let ((e1 (make-enumeration
;; ;;               '(red green blue black)))
;; ;;          (e2 (make-enumeration
;; ;;               '(red black white))))
;; ;;      (enum-set->list
;; ;;       (enum-set-projection e1 e2)))
;; ;;    =>
;; ;;    '(red black))


;; ;; ;; (define enum-set-rtd

;; ;; ;;   (make-record-type-descriptor
;; ;; ;;    'enum-set #f #f #f #f
;; ;; ;;    '#((mutable type) (mutable member))))

;; ;; ;; (define enum-set-rcd
;; ;; ;;   (make-record-constructor-descriptor
;; ;; ;;    enum-set-rtd #f #f))

;; ;; ;; (define make-enum-set (record-constructor enum-set-rcd))

;; ;; ;; (define enum-set-type (record-accessor enum-set-rtd 0))


;; ;; ;; (define enum-set-rtd
;; ;; ;;   (make-record-type-descriptor
;; ;; ;;    'enum-set #f #f #f #f
;; ;; ;;    '#((mutable type) (mutable member))))

;; ;; ;; (define enum-set-rcd
;; ;; ;;   (make-record-constructor-descriptor
;; ;; ;;    enum-set-rtd #f #f))

;; ;; ;; (define make-enum-set (record-constructor enum-set-rcd))

;; ;; ;; (define enum-set-type (record-accessor enum-set-rtd 0))

;; ;; ;; (print (make-enum-set 'color '(hoge hige)))
;; ;; ;; (print (enum-set-type (make-enum-set 'color '(hoge hige))))



;; ;; (exit)
;; ;; (fold-right + 0 '(1 2 3) '(4 5 6))
;; ;; (exit)
;; ;; (define (%cars+cdrs+ lists cars-final)
;; ;;   (call-with-current-continuation
;; ;;     (lambda (abort)
;; ;;       (let recur ((lists lists))
;; ;;         (if (pair? lists)
;; ;;         (receive (list other-lists) (car+cdr lists)
;; ;;           (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
;; ;;           (receive (a d) (car+cdr list)
;; ;;             (receive (cars cdrs) (recur other-lists)
;; ;;               (values (cons a cars) (cons d cdrs))))))
;; ;;         (values (list cars-final) '()))))))


;; ;; (define (fold2 kons knil lis1 . lists)
;; ;;   (check-arg procedure? kons fold)
;; ;;   (if (pair? lists)
;; ;;       (let lp ((lists (cons lis1 lists)) (ans knil))    ; N-ary case
;; ;;     (receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
;; ;;       (if (null? cars+ans) ans ; Done.
;; ;;           (lp cdrs (apply kons cars+ans)))))

;; ;;       (let lp ((lis lis1) (ans knil))           ; Fast path
;; ;;     (if (null-list? lis) ans
;; ;;         (lp (cdr lis) (kons (car lis) ans))))))

;; ;; (display  (fold2 + 0 '(1 2 3) '(4 5 6)))

;; ;; (receive a (call/cc (lambda (c) (c 1 2 3))) (display a))
;; ;; (exit)


;; ;; (exit)
;; ;; (display (fold-left (lambda (max-len s) (max max-len (string-length s))) 0 '("longest" "long" "longer")))



;; ;; (exit)
;; ;;  (fold-left (lambda (count x) (if (odd? x) (+ count 1) count)) 0 '(3 1 4 1 5 9 2 6 5 3))

;; ;; (display  (fold-left + 0 '(1 2 3) '(4 5 6)))
;; ;; (exit)
;; ;; (define (partition proc lst)
;; ;;   (let loop ([lst lst]
;; ;;              [lst1 '()]
;; ;;              [lst2 '()])
;; ;;     (if (null? lst)
;; ;;         (values (reverse lst1) (reverse lst2))
;; ;;         (let ([var (car lst)])
;; ;;           (if (proc var)
;; ;;               (loop (cdr lst) (cons var lst1) lst2)
;; ;;               (loop (cdr lst) lst1 (cons var lst2)))))))

;; ;; (receive (x y) (partition even? '(3 1 4 1 5 9 2 6))
;; ;;   (display x)
;; ;;   (display y))

;; ;; (exit)

;; ;;   (define (exists-1 proc lst1)
;; ;;     (if (null? lst1)
;; ;;         #f
;; ;;         (let loop ([lst lst1])
;; ;;           (cond
;; ;;            [(not (pair? lst))
;; ;;             (assertion-violation 'exists "proper list required" (list lst1))]
;; ;;            [(proc (car lst))
;; ;;             => (lambda (ret)
;; ;;                  ret)]
;; ;;            [else
;; ;;             (if (null? (cdr lst))
;; ;;                 #f
;; ;;                 (loop (cdr lst)))]))))

;; ;;   (define (exists-n proc list-n)
;; ;;     (define (map-car l)
;; ;;       (let loop ([l l])
;; ;;         (cond
;; ;;          [(null? l)
;; ;;           '()]
;; ;;          [(pair? (car l))
;; ;;           (cons (caar l) (loop (cdr l)))]
;; ;;          [else
;; ;;           (assertion-violation 'exists "the lists all should have the same length" (list list-n))])))
;; ;;     (if (null*? list-n)
;; ;;         #f
;; ;;         (let loop ([head (map-car list-n)]
;; ;;                    [rest (map cdr list-n)])
;; ;;           (if (null? (car rest))
;; ;;               (apply proc head)
;; ;;               (or (apply proc head)
;; ;;                    (loop (map-car rest) (map cdr rest)))))))

;; ;; (print (exists-1 even? '(3 1 4 1 5 9)) )
;; ;;                ; ⇒ #t
;; ;; (print (exists-1 even? '(3 1 1 5 9)));         ⇒ #f
;; ;; ;(print ((exists-1 even? '(3 1 1 5 9 . 2)) ))
;; ;;                ; ⇒  &assertion exception
;; ;; (print (exists-1 (lambda (n) (and (even? n) n)) '(2 1 4 14)))

;; ;; (print (exists-n < '((1 2 4) (2 3 4))))
;; ;; (print (exists-n > '((1 2 3) (2 3 4))))
;; ;; (exit)

;; ;; (= 3 'a)
;; ;; (exit)
;; ;; (define ($for-all proc lst . lists)
;; ;;   (define (for-all-1 proc lst1)
;; ;;     (if (null? lst1)
;; ;;         #t
;; ;;         (let loop ([lst lst1])
;; ;;           (cond
;; ;;            [(not (pair? lst))
;; ;;             (assertion-violation 'for-all "proper list required" (list lst1))]
;; ;;            [(proc (car lst))
;; ;;             => (lambda (ret)
;; ;;                  (if (null? (cdr lst))
;; ;;                      ret
;; ;;                      (loop (cdr lst))))]
;; ;;            [else #f]))))
;; ;;   (define (for-all-n proc list-n)
;; ;;     (define (map-car l)
;; ;;       (let loop ([l l])
;; ;;         (cond
;; ;;          [(null? l)
;; ;;           '()]
;; ;;          [(pair? (car l))
;; ;;           (cons (caar l) (loop (cdr l)))]
;; ;;          [else
;; ;;           (assertion-violation 'for-all "the lists all should have the same length" (list list-n))])))
;; ;;     (if (null*? list-n)
;; ;;         #t
;; ;;         (let loop ([head (map-car list-n)]
;; ;;                    [rest (map cdr list-n)])
;; ;;           (if (null? (car rest))
;; ;;               (apply proc head)
;; ;;               (and (apply proc head)
;; ;;                    (loop (map-car rest) (map cdr rest)))))))
;; ;;     (if (null? lists)
;; ;;         (for-all-1 proc lst)
;; ;;         (for-all-n proc (cons lst lists))))

;; ;; (define (null*? lst)
;; ;;   (let loop ([lst lst])
;; ;;     (if (null? lst)
;; ;;         #t
;; ;;         (and (null? (car lst))
;; ;;              (loop (cdr lst))))))

;; ;; (print (null*? '(() ())))

;; ;; (print ($for-all even? '(3 1 4 1 5 9)))
;; ;; (print ($for-all even? '(3 1 4 1 5 9 . 2)))
;; ;; (print ($for-all even? '(2 4 14)))
;; ;; (exit)
;; ;; ;(print ($for-all even? '(2 4 14 . 9)))
;; ;; ;; ;                ⇒  &assertion exception
;; ;; (print ($for-all (lambda (n) (and (even? n) n))
;; ;;           '(2 4 14)))

;; ;; (print ($for-all even? '()))
;; ;; (print ($for-all even? '() '()))
;; ;; ;; ;                ⇒ 14
;; ;; ;(print (for-all < '(1 2 3) '(2 3 4)))
;; ;; ;; ;                ⇒ #t
;; ;; ;; (for-all < '(1 2 4) '(2 3 4))

;; ;; (exit)

;; ;; (display '#(moge))
;; ;; (exit)

;; ;; (display '(3 #,@3))
;; ;; (exit)

;; ;; (delete-file "not-cgadad///")
;; ;; (exit)

;; ;; (define a 3)
;; ;; (define a 3)
;; ;; (exit)

;; ;; yyy
;; ;; (exit)
;; ;; (define (a y . x)
;; ;;   x)
;; ;; (display (a))
;; ;; (exit)
;; ;; (define (a x)
;; ;;   (set! x 1)
;; ;;   (a x)
;; ;;   3)
;; ;; (display (guard [c (#t (display c))]
;; ;;        (a 3)))
;; ;; (exit)

;; ;; (#/.*/ "あいうえ")
;; ;; (exit)
;; ;; (let1 x '(2)
;; ;;   (apply set-cdr! (list x 3))
;; ;;   (display x))
;; ;; (exit)
;; ;; (display (apply read-char (current-input-port)))

;; ;; (exit)
;; ;; (reverse 3)
;; ;; (exit)
;; ;; (display 'h (current-error-port))
;; ;; (apply cdr '(2))
;; ;; (exit)
;; ;; (make-record-constructor-descriptor)
;; ;; (number? 2 2)
;; ;; (exit)
;; ;; (apply (lambda (s) (display s)) '(1 2))
;; ;; (exit)
;; ;; (car 3)
;; ;; (display (guard (con [#t 'error])
;; ;;               (car 3)))
;; ;; (exit)

;; ;; (call/cc (lambda (cont)
;; ;;                (with-exception-handler
;; ;;                 (lambda (c)
;; ;;                   (cont c))
;; ;;                 (lambda () (car 3)))))
;; ;; (exit)

;; ;; (define (a) ((lambda ()
;; ;;   (throw 3))))
;; ;; (a)
;; ;; (exit)
;; ;; (call/cc
;; ;;  (lambda (cont)
;; ;;    (with-exception-handler
;; ;;     (lambda (c)
;; ;;       (display c)
;; ;;       (cont 3))
;; ;;     (lambda () (hashtable-clear! (hashtable-copy (make-eq-hashtable) #f))))))
;; ;; (exit)


;; ;; (display (equal? '(#(1 2 3)  . #(one two three)) '(#(1 2 3)  . #(one two three))))
;; ;; (exit)
;; ;; (display (test-temp 1  2 3))
;; ;; (display (test-temp 4 5 6))

;; ;; (exit)

;; ;; ;(eval (list 'quote  (cons '1 '2)) '())
;; ;; ;(eval '(quote  (cons '1 '2)) '())
;; ;; (eval 3 '())
;; ;; (display (apply (lambda (x y) (apply y '((3 2)))) `(,car ,cdr)))
;; ;; (display (apply (lambda (x) (car x)) '((3 2))))

;; ;; (display (test-temp 1  2 3))
;; ;; (display (test-temp 4 5 6))

;; ;;    (with-exception-handler
;; ;;     (lambda (e)
;; ;;       (display e))
;; ;;     (lambda ()
;; ;;       (raise-continuable "bokeeeee")))


;; ;; ;; (parameterize ((x y))
;; ;; ;;   body ...)

;; ;; ;; (define-macro (parameterize . sexp)
;; ;; ;;   (let ([var (caaar sexp)]
;; ;; ;;         [val (cadaar sexp)]
;; ;; ;;         [body (cdr sexp)]
;; ;; ;;         [save (gensym)]
;; ;; ;;         [new (gensym)])
;; ;; ;;     `(let ([,save #f]
;; ;; ;;            [,new ,val])
;; ;; ;;        (dynamic-wind
;; ;; ;;            (lambda () (set! ,save ,var) (set! ,var ,new))
;; ;; ;;            (lambda () ,@body)
;; ;; ;;            (lambda () (set! ,var ,save))))))

;; ;; (let1 x (make-parameter 3)
;; ;;   (parameterize ((x 4))
;; ;;     (display (x)))
;; ;;     (display (x)))


;; ;; (receive x (values)
;; ;;   (display x))

;; ;; ;(exit)


;; ;; ;; (let ([captured '()])
;; ;; ;;   (dynamic-wind
;; ;; ;;       (lambda () (print "before"))
;; ;; ;;       (lambda ()
;; ;; ;;         (if (call/cc
;; ;; ;;              (lambda (cont)
;; ;; ;;                (set! captured cont)))
;; ;; ;;             '()
;; ;; ;;             (set! captured #f)))
;; ;; ;;       (lambda () (print "after")))
;; ;; ;;   (if captured
;; ;; ;;       (captured #f)
;; ;; ;;       (print "done")))

;; ;; ;; (let ([captured '()])
;; ;; ;;   (dynamic-wind
;; ;; ;;       (lambda () (print "before1"))
;; ;; ;;       (lambda ()
;; ;; ;;         (print "thunk1")
;; ;; ;;         (if (call/cc
;; ;; ;;              (lambda (cont)
;; ;; ;;                (set! captured cont)))
;; ;; ;;             '()
;; ;; ;;             (set! captured #f)))
;; ;; ;;       (lambda () (print "after1")))
;; ;; ;;   (dynamic-wind
;; ;; ;;       (lambda () (print "before2"))
;; ;; ;;       (lambda ()
;; ;; ;;         (print "thunk2")
;; ;; ;;         (if captured
;; ;; ;;             (captured #f)
;; ;; ;;             (print "done")))
;; ;; ;;       (lambda () (print "after2"))))

;; ;; ;; (let ([captured '()])
;; ;; ;;   (dynamic-wind
;; ;; ;;       (lambda () (print "before1"))
;; ;; ;;       (lambda ()
;; ;; ;;         (print "thunk1")
;; ;; ;;         (dynamic-wind
;; ;; ;;             (lambda () (print "before1-1"))
;; ;; ;;             (lambda ()
;; ;; ;;               (print "thunk1-1")
;; ;; ;;               (if (call/cc
;; ;; ;;                    (lambda (cont)
;; ;; ;;                      (set! captured cont)))
;; ;; ;;                   '()
;; ;; ;;                   (set! captured #f)))
;; ;; ;;             (lambda () (print "after1-1"))))
;; ;; ;;       (lambda () (print "after1")))
;; ;; ;;   (dynamic-wind
;; ;; ;;       (lambda () (print "before2"))
;; ;; ;;       (lambda ()
;; ;; ;;         (print "thunk2")
;; ;; ;;         (if captured
;; ;; ;;             (captured #f)
;; ;; ;;             (print "done")))
;; ;; ;;       (lambda () (print "after2"))))




;; ;; ;; (call-with-values (lambda ()
;; ;; ;;                     ((call/cc
;; ;; ;;                       (lambda (outerk)
;; ;; ;;                         (lambda ()
;; ;; ;;                           (with-exception-handler
;; ;; ;;                            (lambda (c)
;; ;; ;;                              ((call/cc
;; ;; ;;                                (lambda (gen)
;; ;; ;;                                  (outerk
;; ;; ;;                                   (lambda () (if #t (begin #t) (gen (lambda () (raise c))))))))))
;; ;; ;;                            (lambda () #f (raise #f))))))))
;; ;; ;;   (lambda x
;; ;; ;;     x))

;; ;; ;; (call-with-values (lambda ()
;; ;; ;;                     ((call/cc
;; ;; ;;                       (lambda (outerk)
;; ;; ;;                         (lambda ()
;; ;; ;;                           (with-exception-handler
;; ;; ;;                            (lambda (c)
;; ;; ;;                              ((call/cc
;; ;; ;;                                (lambda (gen)
;; ;; ;;                                  (outerk
;; ;; ;;                                   (lambda () #t))))))
;; ;; ;;                            (lambda () #f (raise #f))))))))
;; ;; ;;   (lambda x
;; ;; ;;     x))

;; ;; ;; (call-with-values (lambda ()
;; ;; ;;                     ((call/cc
;; ;; ;;                       (lambda (outerk)
;; ;; ;;                         (lambda ()
;; ;; ;;                           (with-exception-handler
;; ;; ;;                            (lambda (c)
;; ;; ;;                              ((call/cc
;; ;; ;;                                (lambda (gen)
;; ;; ;;                                  (outerk
;; ;; ;;                                   (lambda () #t))))))
;; ;; ;;                            (lambda () (raise #f))))))))
;; ;; ;;   (lambda x x))

;; ;; ;; (call-with-values (lambda ()
;; ;; ;;                     ((call/cc
;; ;; ;;                       (lambda (outerk)
;; ;; ;;                         (lambda ()
;; ;; ;;                           (with-exception-handler
;; ;; ;;                            (lambda (c)
;; ;; ;;                              (
;; ;; ;;                                  (outerk
;; ;; ;;                                   (lambda () #t))))
;; ;; ;;                            (lambda () (raise #f))))))))
;; ;; ;;   (lambda x x))

;; ;; ;; (call-with-values
;; ;; ;;     (lambda ()
;; ;; ;;       ((call/cc
;; ;; ;;         (lambda (outerk)
;; ;; ;;           (lambda ()
;; ;; ;;             (with-exception-handler
;; ;; ;;              (lambda (c)
;; ;; ;;                (
;; ;; ;;                 (outerk
;; ;; ;;                  (lambda () #t))))
;; ;; ;;              (lambda () (raise #f)))
;; ;; ;;             )))))
;; ;; ;;   (lambda x x))

;; ;; ;; (call-with-values
;; ;; ;;     (lambda ()
;; ;; ;;       ((call/cc
;; ;; ;;         (lambda (outerk)
;; ;; ;;           (lambda ()
;; ;; ;;             (with-exception-handler
;; ;; ;;              (lambda (c)
;; ;; ;;                (
;; ;; ;;                 (outerk
;; ;; ;;                  (lambda () #t))))
;; ;; ;;              (lambda () (raise #f)))
;; ;; ;;             )))))
;; ;; ;;   (lambda x x))

;; ;; ;; (display
;; ;; ;;  ((lambda ()
;; ;; ;;     ((call/cc
;; ;; ;;       (lambda (outerk)
;; ;; ;;         (lambda ()
;; ;; ;;           (with-exception-handler
;; ;; ;;            (lambda (c)
;; ;; ;;              (
;; ;; ;;               (outerk
;; ;; ;;                (lambda () #t))))
;; ;; ;;            (lambda () (raise #f)))
;; ;; ;;           )))))))

;; ;; ;; (display
;; ;; ;;  ((call/cc
;; ;; ;;    (lambda (outerk)
;; ;; ;;      (lambda ()
;; ;; ;;        (with-exception-handler
;; ;; ;;         (lambda (c)
;; ;; ;;           (
;; ;; ;;            (outerk
;; ;; ;;             (lambda () #t))))
;; ;; ;;         (lambda () (raise #f)))
;; ;; ;;           )))))

;; ;; ;; (display
;; ;; ;;  ((call/cc
;; ;; ;;    (lambda (outerk)
;; ;; ;;      (lambda ()
;; ;; ;;        (with-exception-handler
;; ;; ;;         (lambda (c)
;; ;; ;;           (
;; ;; ;;            (outerk
;; ;; ;;             (lambda () #t))))
;; ;; ;;         (lambda () (raise #f)))
;; ;; ;;           )))))

;; ;; ;; (display
;; ;; ;;  ((call/cc
;; ;; ;;    (lambda (outerk)
;; ;; ;;      (lambda ()
;; ;; ;;        (with-exception-handler
;; ;; ;;         (lambda (c)
;; ;; ;;           (
;; ;; ;;            (outerk
;; ;; ;;             (lambda () #t))))
;; ;; ;;         (lambda () (raise #f)))
;; ;; ;;           )))))

;; ;; ;; (display
;; ;; ;; (call/cc
;; ;; ;;  (lambda (cont)
;; ;; ;;    (with-exception-handler
;; ;; ;;      (lambda (exception)
;; ;; ;;        (cont 3))
;; ;; ;;      (lambda () (raise #f))))))

;; ;; (display
;; ;; (call/cc
;; ;;  (lambda (cont)
;; ;;    (with-exception-handler
;; ;;      (lambda (exception)
;; ;;        (cont 3))
;; ;;      (lambda () (raise #f))))))

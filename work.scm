(fold-right + 0 '(1 2 3) '(4 5 6))
(exit)
(define (%cars+cdrs+ lists cars-final)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    (receive (list other-lists) (car+cdr lists)
	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
		  (receive (a d) (car+cdr list)
		    (receive (cars cdrs) (recur other-lists)
		      (values (cons a cars) (cons d cdrs))))))
	    (values (list cars-final) '()))))))


(define (fold2 kons knil lis1 . lists)
  (check-arg procedure? kons fold)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
	(receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
	  (if (null? cars+ans) ans ; Done.
	      (lp cdrs (apply kons cars+ans)))))

      (let lp ((lis lis1) (ans knil))			; Fast path
	(if (null-list? lis) ans
	    (lp (cdr lis) (kons (car lis) ans))))))

(display  (fold2 + 0 '(1 2 3) '(4 5 6)))

(receive a (call/cc (lambda (c) (c 1 2 3))) (display a))
(exit)


(exit)
(display (fold-left (lambda (max-len s) (max max-len (string-length s))) 0 '("longest" "long" "longer")))



(exit)
 (fold-left (lambda (count x) (if (odd? x) (+ count 1) count)) 0 '(3 1 4 1 5 9 2 6 5 3))

(display  (fold-left + 0 '(1 2 3) '(4 5 6)))
(exit)
(define (partition proc lst)
  (let loop ([lst lst]
             [lst1 '()]
             [lst2 '()])
    (if (null? lst)
        (values (reverse lst1) (reverse lst2))
        (let ([var (car lst)])
          (if (proc var)
              (loop (cdr lst) (cons var lst1) lst2)
              (loop (cdr lst) lst1 (cons var lst2)))))))

(receive (x y) (partition even? '(3 1 4 1 5 9 2 6))
  (display x)
  (display y))

(exit)

  (define (exists-1 proc lst1)
    (if (null? lst1)
        #f
        (let loop ([lst lst1])
          (cond
           [(not (pair? lst))
            (assertion-violation 'exists "proper list required" (list lst1))]
           [(proc (car lst))
            => (lambda (ret)
                 ret)]
           [else
            (if (null? (cdr lst))
                #f
                (loop (cdr lst)))]))))

  (define (exists-n proc list-n)
    (define (map-car l)
      (let loop ([l l])
        (cond
         [(null? l)
          '()]
         [(pair? (car l))
          (cons (caar l) (loop (cdr l)))]
         [else
          (assertion-violation 'exists "the lists all should have the same length" (list list-n))])))
    (if (null*? list-n)
        #f
        (let loop ([head (map-car list-n)]
                   [rest (map cdr list-n)])
          (if (null? (car rest))
              (apply proc head)
              (or (apply proc head)
                   (loop (map-car rest) (map cdr rest)))))))

(print (exists-1 even? '(3 1 4 1 5 9)) )
               ; ⇒ #t
(print (exists-1 even? '(3 1 1 5 9)));         ⇒ #f
;(print ((exists-1 even? '(3 1 1 5 9 . 2)) ))
               ; ⇒  &assertion exception
(print (exists-1 (lambda (n) (and (even? n) n)) '(2 1 4 14)))

(print (exists-n < '((1 2 4) (2 3 4))))
(print (exists-n > '((1 2 3) (2 3 4))))
(exit)

(= 3 'a)
(exit)
(define ($for-all proc lst . lists)
  (define (for-all-1 proc lst1)
    (if (null? lst1)
        #t
        (let loop ([lst lst1])
          (cond
           [(not (pair? lst))
            (assertion-violation 'for-all "proper list required" (list lst1))]
           [(proc (car lst))
            => (lambda (ret)
                 (if (null? (cdr lst))
                     ret
                     (loop (cdr lst))))]
           [else #f]))))
  (define (for-all-n proc list-n)
    (define (map-car l)
      (let loop ([l l])
        (cond
         [(null? l)
          '()]
         [(pair? (car l))
          (cons (caar l) (loop (cdr l)))]
         [else
          (assertion-violation 'for-all "the lists all should have the same length" (list list-n))])))
    (if (null*? list-n)
        #t
        (let loop ([head (map-car list-n)]
                   [rest (map cdr list-n)])
          (if (null? (car rest))
              (apply proc head)
              (and (apply proc head)
                   (loop (map-car rest) (map cdr rest)))))))
    (if (null? lists)
        (for-all-1 proc lst)
        (for-all-n proc (cons lst lists))))

(define (null*? lst)
  (let loop ([lst lst])
    (if (null? lst)
        #t
        (and (null? (car lst))
             (loop (cdr lst))))))

(print (null*? '(() ())))

(print ($for-all even? '(3 1 4 1 5 9)))
(print ($for-all even? '(3 1 4 1 5 9 . 2)))
(print ($for-all even? '(2 4 14)))
(exit)
;(print ($for-all even? '(2 4 14 . 9)))
;; ;                ⇒  &assertion exception
(print ($for-all (lambda (n) (and (even? n) n))
          '(2 4 14)))

(print ($for-all even? '()))
(print ($for-all even? '() '()))
;; ;                ⇒ 14
;(print (for-all < '(1 2 3) '(2 3 4)))
;; ;                ⇒ #t
;; (for-all < '(1 2 4) '(2 3 4))

(exit)

(display '#(moge))
(exit)

(display '(3 #,@3))
(exit)

(delete-file "not-cgadad///")
(exit)

(define a 3)
(define a 3)
(exit)

yyy
(exit)
(define (a y . x)
  x)
(display (a))
(exit)
(define (a x)
  (set! x 1)
  (a x)
  3)
(display (guard [c (#t (display c))]
       (a 3)))
(exit)

(#/.*/ "あいうえ")
(exit)
(let1 x '(2)
  (apply set-cdr! (list x 3))
  (display x))
(exit)
(display (apply read-char (current-input-port)))

(exit)
(reverse 3)
(exit)
(display 'h (current-error-port))
(apply cdr '(2))
(exit)
(make-record-constructor-descriptor)
(number? 2 2)
(exit)
(apply (lambda (s) (display s)) '(1 2))
(exit)
(car 3)
(display (guard (con [#t 'error])
              (car 3)))
(exit)

(call/cc (lambda (cont)
               (with-exception-handler
                (lambda (c)
                  (cont c))
                (lambda () (car 3)))))
(exit)

(define (a) ((lambda ()
  (throw 3))))
(a)
(exit)
(call/cc
 (lambda (cont)
   (with-exception-handler
    (lambda (c)
      (display c)
      (cont 3))
    (lambda () (hashtable-clear! (hashtable-copy (make-eq-hashtable) #f))))))
(exit)


(display (equal? '(#(1 2 3)  . #(one two three)) '(#(1 2 3)  . #(one two three))))
(exit)
(display (test-temp 1  2 3))
(display (test-temp 4 5 6))

(exit)

;(eval (list 'quote  (cons '1 '2)) '())
;(eval '(quote  (cons '1 '2)) '())
(eval 3 '())
(display (apply (lambda (x y) (apply y '((3 2)))) `(,car ,cdr)))
(display (apply (lambda (x) (car x)) '((3 2))))

(display (test-temp 1  2 3))
(display (test-temp 4 5 6))

   (with-exception-handler
    (lambda (e)
      (display e))
    (lambda ()
      (raise-continuable "bokeeeee")))


;; (parameterize ((x y))
;;   body ...)

;; (define-macro (parameterize . sexp)
;;   (let ([var (caaar sexp)]
;;         [val (cadaar sexp)]
;;         [body (cdr sexp)]
;;         [save (gensym)]
;;         [new (gensym)])
;;     `(let ([,save #f]
;;            [,new ,val])
;;        (dynamic-wind
;;            (lambda () (set! ,save ,var) (set! ,var ,new))
;;            (lambda () ,@body)
;;            (lambda () (set! ,var ,save))))))

(let1 x (make-parameter 3)
  (parameterize ((x 4))
    (display (x)))
    (display (x)))


(receive x (values)
  (display x))

;(exit)


;; (let ([captured '()])
;;   (dynamic-wind
;;       (lambda () (print "before"))
;;       (lambda ()
;;         (if (call/cc
;;              (lambda (cont)
;;                (set! captured cont)))
;;             '()
;;             (set! captured #f)))
;;       (lambda () (print "after")))
;;   (if captured
;;       (captured #f)
;;       (print "done")))

;; (let ([captured '()])
;;   (dynamic-wind
;;       (lambda () (print "before1"))
;;       (lambda ()
;;         (print "thunk1")
;;         (if (call/cc
;;              (lambda (cont)
;;                (set! captured cont)))
;;             '()
;;             (set! captured #f)))
;;       (lambda () (print "after1")))
;;   (dynamic-wind
;;       (lambda () (print "before2"))
;;       (lambda ()
;;         (print "thunk2")
;;         (if captured
;;             (captured #f)
;;             (print "done")))
;;       (lambda () (print "after2"))))

;; (let ([captured '()])
;;   (dynamic-wind
;;       (lambda () (print "before1"))
;;       (lambda ()
;;         (print "thunk1")
;;         (dynamic-wind
;;             (lambda () (print "before1-1"))
;;             (lambda ()
;;               (print "thunk1-1")
;;               (if (call/cc
;;                    (lambda (cont)
;;                      (set! captured cont)))
;;                   '()
;;                   (set! captured #f)))
;;             (lambda () (print "after1-1"))))
;;       (lambda () (print "after1")))
;;   (dynamic-wind
;;       (lambda () (print "before2"))
;;       (lambda ()
;;         (print "thunk2")
;;         (if captured
;;             (captured #f)
;;             (print "done")))
;;       (lambda () (print "after2"))))




;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              ((call/cc
;;                                (lambda (gen)
;;                                  (outerk
;;                                   (lambda () (if #t (begin #t) (gen (lambda () (raise c))))))))))
;;                            (lambda () #f (raise #f))))))))
;;   (lambda x
;;     x))

;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              ((call/cc
;;                                (lambda (gen)
;;                                  (outerk
;;                                   (lambda () #t))))))
;;                            (lambda () #f (raise #f))))))))
;;   (lambda x
;;     x))

;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              ((call/cc
;;                                (lambda (gen)
;;                                  (outerk
;;                                   (lambda () #t))))))
;;                            (lambda () (raise #f))))))))
;;   (lambda x x))

;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              (
;;                                  (outerk
;;                                   (lambda () #t))))
;;                            (lambda () (raise #f))))))))
;;   (lambda x x))

;; (call-with-values
;;     (lambda ()
;;       ((call/cc
;;         (lambda (outerk)
;;           (lambda ()
;;             (with-exception-handler
;;              (lambda (c)
;;                (
;;                 (outerk
;;                  (lambda () #t))))
;;              (lambda () (raise #f)))
;;             )))))
;;   (lambda x x))

;; (call-with-values
;;     (lambda ()
;;       ((call/cc
;;         (lambda (outerk)
;;           (lambda ()
;;             (with-exception-handler
;;              (lambda (c)
;;                (
;;                 (outerk
;;                  (lambda () #t))))
;;              (lambda () (raise #f)))
;;             )))))
;;   (lambda x x))

;; (display
;;  ((lambda ()
;;     ((call/cc
;;       (lambda (outerk)
;;         (lambda ()
;;           (with-exception-handler
;;            (lambda (c)
;;              (
;;               (outerk
;;                (lambda () #t))))
;;            (lambda () (raise #f)))
;;           )))))))

;; (display
;;  ((call/cc
;;    (lambda (outerk)
;;      (lambda ()
;;        (with-exception-handler
;;         (lambda (c)
;;           (
;;            (outerk
;;             (lambda () #t))))
;;         (lambda () (raise #f)))
;;           )))))

;; (display
;;  ((call/cc
;;    (lambda (outerk)
;;      (lambda ()
;;        (with-exception-handler
;;         (lambda (c)
;;           (
;;            (outerk
;;             (lambda () #t))))
;;         (lambda () (raise #f)))
;;           )))))

;; (display
;;  ((call/cc
;;    (lambda (outerk)
;;      (lambda ()
;;        (with-exception-handler
;;         (lambda (c)
;;           (
;;            (outerk
;;             (lambda () #t))))
;;         (lambda () (raise #f)))
;;           )))))

;; (display
;; (call/cc
;;  (lambda (cont)
;;    (with-exception-handler
;;      (lambda (exception)
;;        (cont 3))
;;      (lambda () (raise #f))))))

(display
(call/cc
 (lambda (cont)
   (with-exception-handler
     (lambda (exception)
       (cont 3))
     (lambda () (raise #f))))))

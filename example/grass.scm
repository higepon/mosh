(use srfi-1)
(use util.match)



(define-macro (define-primitive name tag val proc)
  `(define ,name (list 'primitive ',tag ',val ,proc)))

(define-primitive out out '() (lambda (c) (unless (primitive-is? c 'char)
                                            (error "out:charcter required"))
                                      (display (primitive-val c))
                                      c))

(define-primitive succ succ '() (lambda (c) (unless (primitive-is? c 'char)
                                              (error "succ:charctor required"))
                                        (let1 v (char->integer c)
                                          (if (= v 255)
                                              (integer->char 0)
                                              (integer->char (+ v 1))))))

(define-primitive in in '() (lambda (x) (let1 c (read-char)
                                          (if (eof-object? c)
                                              x
                                              c)))) ;; todo


(define-primitive w char #\w (lambda (c) (eq? c #\w)))


(define (primitive? p)
  (match p
    [('primitive . more) #t]
    [else             #f]))

(define (primitive-val p) (third p))
(define (primitive-proc p) (fourth p))
(define (primitive-is? p tag) (eq? (second p) tag))



(define (make-app n m)
  `(app ,n ,m))

(define (make-abs n c)
  `(abs ,n ,c))


(define (grass-eval code env dump)
  (define (env-ref i)  (list-ref env (- i 1)))
  (define (nth-code n) (car (env-ref n)))
  (define (nth-env n)  (cdr (env-ref n)))
;  (format #t "===============================\ncode=~a\nenv=~a\ndump=~a\n\n\n" code env dump)
  (match code
    [(('app m n) . c)
     (let1 mth (env-ref m)
       (if (primitive? mth)
           (grass-eval c
                       `(,((primitive-proc mth) (env-ref n)) . ,env)
                       dump)
           (grass-eval (nth-code m)
                       `((,(nth-code n) . ,(nth-env n)) . ,(nth-env m))
                       `((,c . ,env) . ,dump))))]
    [(('abs 1 cc) . c)
     (grass-eval c `((,cc . ,env) . ,env) dump)]
    [(('abs n cc) . c)
     (grass-eval c `((,(list (make-abs (- n 1) cc)) . ,env) . ,env) dump)]
    [()
     (if (null? dump)
         '()
         (grass-eval (caar dump)
                     `(, (car env) . , (cdar dump))
                     (cdr dump)))]
    [else
     (error "grass-eval runtime error")]))

(define e0 `(,out ,succ ,w ,in))
(define d0 `(((,(make-app 1 1)) . ()) (() . ())))


;; wWWwwww => print w
(grass-eval (list (make-abs 1 `(,(make-app 2 4)))) e0 d0)

;; wwWWwv
;; wwwwWWWwwWwwWWWWWWwwwwWwwv
;; wWWwwwWwwwwWwwwwwwWwwwwwwwww
;; => (+ 1 1)
(grass-eval `(,(make-abs 2 (list (make-app 2 1)))
              ,(make-abs 4 (list (make-app 3 2) (make-app 1 2) (make-app 6 4) (make-app 1 2)))
              ,(make-abs 1 (list (make-app 2 3) (make-app 1 4) (make-app 1 6) (make-app 1 9))))
            e0 d0)

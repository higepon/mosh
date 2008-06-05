(use srfi-1)
(use util.match)
(define (make-char c)
  `(char ,c))

(define (char-value c)
  (second c))

(define (grass-char? c)
  (match c
    [('char . _) #t]
    [else #f]))

;; (define-macro (define-primitive name val proc)
;;   `(define ,name ,(list val proc)))

;(define-primitive out '() (lambda (c) (display c)))

(define out (lambda (c) (display c) c))
(define in (lambda (c) (display "in called") c))
(define succ (lambda (c) (display "succ called") c))
(define w (lambda (c) (display "w called") c))

(define e (list out in succ w))


(define (make-app n m)
  `(app ,n ,m))

(define (app-n x)
  (second x))

(define (app-m x)
  (third x))


(define (app? x)
  (eq? (first x) 'app))

(define (make-abs n c)
  `(abs ,n ,c))

(define (abs? x)
  (eq? (first x) 'abs))

(define (abs-n x)
  (second n))

(define (grass-eval code env dump)
  (define (env-ref i)  (list-ref env (- i 1)))
  (define (nth-code n) (car (env-ref n)))
  (define (nth-env n)  (cdr (env-ref n)))
;  (format #t "===============================\ncode=~a\nenv=~a\ndump=~a\n\n\n" code env dump)
  (match code
    [(('app m n) . c)
     (let1 mth (env-ref m)
       (if (procedure? mth)
           (grass-eval c
                       `(,(mth (env-ref n)) . ,env)
                       dump)
           (grass-eval (nth-code m)
                       `((,(nth-code n) . ,(nth-env n)) . ,(nth-env m))
                       `((,c . ,env) . ,dump))))]
    [(('abs 1 cc) . c)
     (grass-eval c `((,cc . ,env) . ,env) dump)]
    [(('abs n cc) . c)
     (grass-eval c `((,((make-abs (- n 1) cc)) . ,env) . ,env) dump)]
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

;; 

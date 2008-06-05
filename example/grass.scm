(define (string->list str)
  (let loop ((pos (- (string-length str) 1)) (l '()))
    (if (< pos 0)
        l
      (loop (- pos 1) (cons (string-ref str pos) l)))))


(define-macro (aif test-form then-form . else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-form)))

(define (make-app n m)
  `(app ,n ,m))

(define (make-abs n c)
  `(abs ,n ,c))

(define (make-primitive tag val proc)
  `(primitive ,tag ,val ,proc))

(define (primitive? p)
  (match p
    [('primitive . more) #t]
    [else                #f]))

(define (primitive-val p) (third p))
(define (primitive-proc p) (fourth p))
(define (primitive-is? p tag) (eq? (second p) tag))

(define church-true  (cons (make-abs 1 (make-app 3 2)) (cons '() '())))
(define church-false (cons (make-abs 1 '()) '()))

(define (make-char ch)
  (make-primitive 'char ch (lambda (c) (if (eq? c ch) church-true church-false))))

(define out (make-primitive 'proc '() (lambda (c) (unless (primitive-is? c 'char)
                                            (error "out:charcter required"))
                                      (display (primitive-val c))
                                      c)))

(define succ (make-primitive 'proc '() (lambda (c) (unless (primitive-is? c 'char)
                                              (error "succ:charctor required"))
                                        (let1 v (char->integer c)
                                          (if (= v 255)
                                              (integer->char 0)
                                              (integer->char (+ v 1)))))))

(define in (make-primitive 'proc '() (lambda (x) (let1 c (read-char)
                                          (if (eof-object? c)
                                              x
                                              (make-char c))))))

(define w (make-char #\w))

(define (grass-eval text env dump)
  (define (rec code env dump)
    (define (env-ref i)  (list-ref env (- i 1)))
    (define (nth-code n) (car (env-ref n)))
    (define (nth-env n)  (cdr (env-ref n)))
    (match code
      [(('app m n) . c)
       (let1 mth (env-ref m)
         (if (primitive? mth)
             (rec c
                  `(,((primitive-proc mth) (env-ref n)) . ,env)
                  dump)
             (rec (nth-code m)
                  `((,(nth-code n) . ,(nth-env n)) . ,(nth-env m))
                  `((,c . ,env) . ,dump))))]
      [(('abs 1 cc) . c)
       (rec c `((,cc . ,env) . ,env) dump)]
      [(('abs n cc) . c)
       (rec c `((,(list (make-abs (- n 1) cc)) . ,env) . ,env) dump)]
      [()
       (if (null? dump)
           '()
           (rec (caar dump)
                       `(, (car env) . , (cdar dump))
                       (cdr dump)))]
      [else
       (error "grass-eval runtime error")]))
  (rec (parse text) env dump))

(define e0 `(,out ,succ ,w ,in))
(define d0 `(((,(make-app 1 1)) . ()) (() . ())))

(define (parse text)
  (define (normalize t)
    (list->string
     (filter (lambda (p) (memq p '(#\w #\W #\v)))
             (string->list
              (regexp-replace-all #/Ｗ/ (regexp-replace-all #/ｖ/ (regexp-replace-all #/ｗ/ t "w") "v") "W")))))
  (define (parse-body t)
    (aif (#/^(W+)(w+)(.*)/ (if t t ""))
         (cons (make-app (string-length (it 1)) (string-length (it 2)))
               (parse-body (it 3)))
         '()))
  (map (lambda (x)
         (aif (#/^(w+)(.+)/ x)
              (make-abs (string-length (it 1)) (parse-body (it 2)))
              (error "syntax error")))
       (string-split (normalize text) #\v)))

(grass-eval "ｗｗＷＷｗv
             ｗｗｗｗＷＷＷｗｗＷｗｗＷＷＷＷＷＷｗｗｗｗＷｗｗv
             ｗＷＷｗｗｗＷｗｗｗｗＷｗｗｗｗｗｗＷｗｗｗｗｗｗｗｗｗ" e0 d0)


(grass-eval "ｗＷＷｗｗｗｗ" e0 d0)

(grass-eval "無限に草植えときますねｗＷＷｗｗｗｗＷＷｗｗ" e0 d0)

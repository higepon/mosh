(library (yuni text decorate sexp)
         (export 
           decorate-sexp-string-list/ecma48
           sexp-decorate)
         (import (rnrs)
                 (shorten)
                 (match)
                 (yuni text styler)
                 (yuni ui terminal color-sequence)
                 (yuni util combinators))

(define (xterm-white str) str)
(define (xterm-comment str)
  (list 'color-ecma48 6 str))
(define (xterm-paren str)
  (list 'color-ecma48 1 str))
(define (xterm-literal str)
  (list 'color-ecma48 5 str))
(define (xterm-global-std str)
  (list 'color-ecma48 6 str))
(define (xterm-global-std-syntax str)
  (list 'color-ecma48 3 str))

(define xterm-styler
  (make-styler `((white ,xterm-white)
                 (comment ,xterm-comment)
                 (string ,xterm-literal)
                 (paren ,xterm-paren)
                 (literal ,xterm-literal)
                 (global-std ,xterm-global-std)
                 (global-std-syntax ,xterm-global-std-syntax)
                 (symbol ,xterm-white)
                 (literal-symbol ,xterm-white))))

(define (sexp-decorate-phase2 l) ;; => l
  ;; split (obj) into (literal), (symbol), (literal-symbol)
  ;; (white) ;; whitespece or error
  ;; (comment)
  ;; (string)
  ;; (paren)
  ;; (obj)

  ;; state
  (define quote-next? #f)

  (define (genobj str)
    (let ((l (string-length str))
          (c (string-ref str 0)))
      (case c
        ((#\') ;; quote char
         (cond
           ((= l 1)
            (set! quote-next? #t)
            (list (list 'literal "'")))
           (else
             (list (list 'literal "'")
                   (list 'literal-symbol (substring str 1 l))))))
        ((#\#) ;; sharp notation
         (list (list 'literal str)))
        ((#\`) ;; quasiquote
         (cond
           ((= l 1)
            ;; FIXME: increase qq count here..
            (list (list 'literal "`")))
           (else
             (list (list 'literal "`")
                   (list 'literal-symbol (substring str 1 l))))))
        ((#\,) ;; unquote
         (cond
           ((= l 1)
            (list (list 'literal ",")))
           (else
             (append (list 'literal ",")
                     (genobj (substring str 1 l))))))
        ((#\@) ;; splicing (only called from "unquote")
         (cond
           ((= l 1)
            (list (list 'literal "@")))
           (else
             (append (list 'literal "@")
                     (genobj (substring str 1 l))))))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
         (list (list 'literal str)))
        (else ;; symbol
          (cond
            (quote-next?
              (set! quote-next? #f)
              (list (list 'literal-symbol str)))
            (else
              (list (list 'symbol str))))))))
  (define (proc cur e)
    (let ((name (car e)))
      (case name
        ((obj)
         (append (reverse (genobj (cadr e))) cur))
        (else (cons e cur)))))
  (map (^e (reverse (fold-left proc '() e))) l))

(define (sexp-decorate-phase1 str* region*)
  ;; split string using region*
  ;; (white) ;; whitespece or error
  ;; (comment)
  ;; (string)
  ;; (paren)
  ;; (obj)
  (define (paint! vec start end sym)
    (cond
      ((= start end)
       (vector-set! vec start sym))
      (else
        (vector-set! vec start sym)
        (paint! vec (+ 1 start) end sym))))
  (define (output str v)
    (define (itr pos start sym cur)
      (define (next)
        (cons (list sym (substring str start pos)) cur))
      (if (= pos (vector-length v))
        (reverse (next))
        (let ((q (vector-ref v pos)))
          (if (eq? q sym)
            (itr (+ pos 1) start sym cur)
            (itr (+ pos 1) pos q (next))))))
    (itr 0 0 'white '()))
  (define (proc str region)
    (let ((l (string-length str)))
      (if (= 0 l)
        '()
        (let ((v (make-vector l 'white)))
          (for-each (^e (match e ((sym x y) (paint! v x y sym))))
                    region)
          (output str v)))))
  (map (^x (filter (^e (let ((s (cadr e)))
                         (not (= (string-length s) 0))))
                   x))
       (map proc str* region*)))

(define (sexp-decorate-phase0 l) ;; => (values str* region*)
  ; split text stream using <delimiter>
  ; ( ) [ ] " ; #
  ;; (comment)
  ;; (string)
  ;; (paren)
  ;; (obj)
  (define (delimiter? c)
    (case c
      ((#\( #\) #\[ #\] #\" #\; #\#) #t)
      (else #f)))
  (define (scan str* start cur)
    (define str (car str*))
    (define end (string-length str))
    (define (end+? cur x) (>= (+ x cur) end))
    (define (end? cur) (end+? cur 0))
    (define (return next val) ;; process next line
      (if (pair? (cdr str*))
        (scan (cdr str*) next (cons (reverse val) cur))
        (values l (reverse (cons (reverse val) cur)))))
    (define (scan-whitespace cur start pos)
      (if (end? pos)
        (return 'root cur)
        (let ((c (string-ref str pos)))
          (if (char-whitespace? c)
            (scan-whitespace cur start (+ pos 1))
            (scan-root cur pos pos)))))
    (define (scan-root cur start pos) ;; start = otherwise
      (define (next)
        (if (= start pos)
          cur
          (cons (list 'obj start (- pos 1)) cur)))
      (if (end? pos)
        (return 'root (next))
        (let ((c (string-ref str pos)))
          (cond
            ((delimiter? c)
             (case c
               ((#\( #\) #\[ #\])
                (scan-root (cons (list 'paren pos pos)
                                 (next))
                           (+ pos 1)
                           (+ pos 1)))
               ((#\#)
                (if (end+? pos 1)
                  (return 'root (next)) ;; wrong
                  (let ((y (string-ref str (+ pos 1))))
                    (case y
                      ((#\\) ;; skip next char
                       (scan-root (next) pos (+ pos 2)))
                      ((#\|) ;; start block comment
                       (scan-block-comment (next) pos pos))
                      (else
                        (scan-root (next) pos (+ pos 1)))))))
               ((#\")
                (scan-string (next) pos (+ pos 1)))
               ((#\;)
                (return 'root (cons
                                (list 'comment pos (- end 1))
                                (next))))))
            ((char-whitespace? c)
             (scan-whitespace (next) pos (+ pos 1)))
            (else
              (scan-root cur start (+ pos 1)))))))
    (define (scan-block-comment cur start pos) ;; start = blockcomment
      (if (end? pos)
        (return 'block-comment cur)
        (let ((c (string-ref str pos)))
          (if (char=? #\| c)
            (if (end+? pos 1)
              (return 'root cur)
              (let ((y (string-ref str (+ pos 1))))
                (if (char=? #\# y)
                  (scan-root
                    (cons (list 'comment start (+ pos 1))
                          cur)
                    (+ pos 2)
                    (+ pos 2))
                  (scan-block-comment cur start (+ pos 1)))))
            (scan-block-comment cur start (+ pos 1))))))
    (define (scan-string cur start pos) ;; start = <NONE>
      (if (end? pos)
        (return 'root cur) ;; wrong
        (let ((c (string-ref str pos)))
          (if (char=? c #\")
            (scan-root
              (cons (list 'string start pos)
                    cur)
              (+ pos 1)
              (+ pos 1))
            (if (char=? c #\\)
              (if (end+? pos 1)
                (return 'root cur) ;; wrong
                (let ((y (string-ref str (+ pos 1))))
                  (if (char=? y #\")
                    (scan-string cur start (+ pos 2))
                    (scan-string cur start (+ pos 1)))))
              (scan-string cur start (+ pos 1)))))))
    (case start
      ((block-comment) (scan-block-comment '() 0 0))
      ((root) (scan-root '() 0 0))
      (else (assertion-violation #f "something wrong" start))))
  (scan l 'root '()))

(define sexp-decorate
  (compose 
    sexp-decorate-phase2
    sexp-decorate-phase1
    sexp-decorate-phase0))

(define (decorate-sexp-string-list/ecma48 l)
  (define (proc-line e)
    (apply string-append
           (map (^x (terminal-style->string (styler-apply xterm-styler x))) e)))
  (map proc-line (sexp-decorate l)))

)


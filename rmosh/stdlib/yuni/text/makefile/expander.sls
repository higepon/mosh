(library (yuni text makefile expander)
         (export makefile-expand)
         (import (rnrs)
                 (shorten)
                 (srfi :8)
                 (srfi :14)
                 (srfi :26)
                 (srfi :48)
                 (yuni util files)
                 (yuni text makefile reader)
                 (yuni text peg))

(define-syntax $N
  (syntax-rules ()
    ((_ char ...)
     ($none-of (list->char-set '(char ...))))))


;; PEG rules for extract-vars
(define singlechar-name ($do (c ($N #\( #\)))
                             ($return (list->string (list c)))))

(define multichar-name ($do (($c #\())
                            (name ($many ($N #\( #\))))
                            (($c #\)))
                            ($return (list->string name))))

(define varname ($or singlechar-name multichar-name))

(define variable ($do (($c #\$))
                      (name varname)
                      ($return (list name))))

(define const-string ($do (str ($many ($N #\$) 1))
                          ($return (list->string str))))

(define item ($or variable const-string))

(define items ($many item))


(define (expand-vars str env)
  ;; "hoge$(fuga)hoge" => ("hoge" ("fuga") "hoge")
  ;; FIXME: support $@, $<, ... ? These are not used in CMake generated
  ;; Makefiles though..
  (define (extract-vars str) (peg-parse-string items str))
  (define (lookup str)
    (let ((a (assoc str env)))
      (if a (cdr a) (begin (format (current-error-port) "no expand for [~a]\n" str) ""))))
  (receive (port proc) (open-string-output-port)
    (call-with-port port
                    (^p (for-each 
                          (^e (if (string? e)
                                (put-string p e)
                                (let ((n (car e)))
                                  (put-string p (lookup n)))))
                          (extract-vars str))))
    (proc)))

(define (makefile-expand root path env) ;; => obj env
  (define (include-filename p)
    (if (relative-path? p)
      (path-append root
                   p)
      p))

  ;; FIXME: detect duplicates
  (define obj (makefile-read path))
  ;; expand macro
  (define (pass1-itr acc obj env) ;; => obj env
    (if (pair? obj)
      (let ((a (car obj))
            (next (cdr obj)))
        (if (eq? 'macro (car a))
          (let ((n (cadr a))
                (d (expand-vars (caddr a) env)))
            (pass1-itr (cons
                     `(macro ,n ,d)
                     acc)
                   next
                   (cons (cons n d) env)))
          (pass1-itr (cons a acc) next env)))
      (values (reverse acc) env)))

  (define pass1 (cut pass1-itr '() <> <>))

  ;; expand include
  (define (pass2-itr acc obj env)
    (if (pair? obj)
      (let ((a (car obj))
            (next (cdr obj)))
        (define (include-it file)
          (receive (Nobj Nenv) (makefile-expand root (include-filename file) env)
            (pass2-itr (append Nobj acc) next Nenv)))
        (if (eq? 'include (car a))
          (include-it (cadr a))
          (pass2-itr (cons a acc) next env)))
      (values (reverse acc) env)))

  (define pass2 (cut pass2-itr '() <> <>))

  ;; expand recipe
  ;; recipe won't introduce any macros.
  ;; so pass3 will return 1 object.
  (define (pass3 obj env) ;; => obj
    (define (ex o) (map (cut expand-vars <> env) o))
    (define (proc x)
      (case (car x)
        ((recipe)
         (let ((tg (cadr x))
               (pre (caddr x))
               (cmd (cdddr x)))
           `(recipe ,(ex tg)
                    ,(ex pre)
                    ,@(ex cmd))))
        ((include)
         (cons 'include
               (ex (cdr x))))
        (else x)))
    (map proc obj))

  (receive (obj env) (pass1 obj env)
    (receive (obj env) (pass2 obj env)
      (values (pass3 obj env) env))))

)

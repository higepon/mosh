(library (yuni text shell reader)
         (export shell-line-read)
         (import (rnrs)
                 (shorten)
                 (srfi :14)
                 (yuni text peg))

;; shell reader

;; special chars:
;;; | pipe
;;; || dpipe
;;; & and
;;; && dand
;;; ; semicolon
;;; ( lparen
;;; ) rparen
;;; < lbracket
;;; > rbracket
;;; >> drbracket
;;; space, tab (separator) space
;;; newline (?? - we never receive this in Makefile defs..)

(define (construct l)
  ;; phase 0 syms:
  ;;  ( ) => (subshell ...)
  ;;
  ;; phase 1 syms:
  ;;  ; => (seq ...)
  ;;  && => (and ...)
  ;;  || => (or ...)
  ;;  & => (async ...)
  ;;  | => (pipe ...)
  ;;
  ;;  phase 2 syms:
  ;;  X > Y => (> ...)
  ;;  >> => (>> ...)
  ;;

  ;; FIXME: But we don't do that!! (CMake won't use them)
  l)

#|
(define dquote-escaped
  ($seq ($c #\\)
        ($or ($c #\\)
             ($c #\"))))
|#

(define dquote-escaped
  ($seq ($c #\\)
        ($c #\")))

(define dquoted-string-char
  ($or ($try dquote-escaped)
       ($none-of (list->char-set '(#\")))))

(define dquoted
  ($do (($c #\"))
       (str ($many dquoted-string-char))
       (($c #\"))
       ($return (list->string str))))

(define quote-escaped
  ($seq ($c #\\)
        ($or ($c #\\)
             ($c #\"))))

(define quoted-string-char
  ($or ($try quote-escaped)
       ($none-of (list->char-set '(#\')))))

(define quoted
  ($do (($c #\'))
       (str ($many quoted-string-char))
       (($c #\'))
       ($return (list->string str))))

(define escaped
  ($do (($c #\\))
       (c anychar)
       ($return c)))

(define-syntax $do/r
  (syntax-rules ()
    ((_ parse ret)
     ($do (parse)
          ($return ret)))))

(define-syntax $R*
  (syntax-rules ()
    ((_ (c ret) ...)
     ($or ($do/r ($c c) 'ret) ...))))

(define special-symbol
  ($or ($try ($do/r ($many ($c #\|) 2 2) 'dpipe))
       ($try ($do/r ($many ($c #\&) 2 2) 'dand))
       ($try ($do/r ($many ($c #\>) 2 2) 'drbracket))
       ($R*
         (#\| pipe)
         (#\& and)
         (#\; semicolon)
         (#\( lparen)
         (#\) rparen)
         (#\< lbracket)
         (#\> rbracket))))

(define s ($do/r ($many ($or ($c #\space) ($c #\tab)) 1) 'space))

(define special
  ($do (($optional s))
       (sym special-symbol)
       (($optional s))
       ($return sym)))

(define item ($many ($or dquoted
                         quoted
                         ;escaped
                         ($try special)
                         s
                         anychar)))

(define (fuse l)
  (define (itr acc cur l)
    (if (pair? l)
      (let ((a (car l))
            (d (cdr l)))
        (cond
          ((char? a)
           (itr (cons a acc) cur d))
          (else
            (if (pair? acc)
              (if (eq? 'space a)
                (itr '() (cons (list->string (reverse acc)) cur) d)
                (itr '() (cons a (cons (list->string (reverse acc)) cur)) d))
              (itr '() (cons a cur) d)))))
      (reverse
        (if (pair? acc)
          (cons (list->string (reverse acc)) cur)
          cur))))
  (itr '() '() l))

(define (tokenize str)
  (fuse (peg-parse-string item str)))


(define (shell-line-read str)
  ;(write (list 'shell str) (current-error-port))
  ;(newline (current-error-port))
  (construct (tokenize str)))

)

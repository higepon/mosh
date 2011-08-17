(library (yuni text makefile reader)
         (export makefile-read)
         (import (rnrs)
                 (srfi :14)
                 (srfi :8)
                 (yuni util readerwriter)
                 (yuni text peg))

(define eol ($or ($try ($seq cr lf)) cr lf))
(define ws ($skip-many ($one-of (list->char-set '(#\space #\tab)))))
(define ows ($optional ws))
(define wsl ($skip-many space))
(define owsl ($optional wsl))
(define dquote ($c #\"))

;; macro
(define macropat ($many ($none-of (list->char-set '(#\newline)))))
(define namepat ($many ($none-of (list->char-set '(#\= #\space)))))
(define macro ($do (name namepat) 
                   ows
                   (($c #\=))
                   ows
                   (content macropat)
                   eol
                   ($return `(macro ,(list->string name) 
                                    ,(list->string content)))))

;; recipe
(define command 
  ($do (content ($many ($none-of (list->char-set '(#\newline)))))
       ($return (list->string content))))

(define commandpat 
  ($do (($c #\tab))
       (content command)
       eol
       ($return content)))

(define cmd-line 
  ($do (($c #\;))
       (content command)
       ($return content)))

(define target-unquoted
  ($many ($none-of (list->char-set '(#\newline #\space #\:))) 1))

(define quoted-name
  ($many ($none-of (list->char-set '(#\")))))

(define target-quoted
  ($between dquote quoted-name dquote))

(define targetname ($do (name ($or target-quoted target-unquoted))
                        ows
                        ($return (list->string name))))

(define targetname* ($many targetname 1))

(define tgt* ($many ($none-of (list->char-set '(#\:)))))

(define prereq-unquoted
  ;; FIXME: should exclude space
  ($many ($none-of (list->char-set '(#\newline #\;))) 1))

(define prereqname ($do (name ($or target-quoted 
                                   prereq-unquoted))
                        ows
                        ($return (list->string name))))

(define prereq* ($many prereqname 1))

(define targetpat 
  ($do (target-names targetname*)
       ows
       (($c #\:))
       ows
       (prereqs ($optional prereq* '()))
       ows
       (command ($optional cmd-line '()))
       eol
       ($return (cons `(recipe ,target-names ,prereqs)
                      command))))

(define recipe ($do (target targetpat)
                    (commands ($many commandpat))
                    ($return 
                      (let ((tgt (car target))
                            (cmd (cdr target)))
                        (if (pair? cmd)
                          (append tgt (cons cmd commands))
                          (append tgt commands))))))

(define include ($do (($string "include"))
                     ws
                     (line ($many ($none-of (list->char-set '(#\newline)))))
                     (($c #\newline))
                     ($return `(include ,(list->string line)))))

;; makefile
(define makefile-item ($do owsl
                           (content ($or ($try macro)
                                         ($try recipe) 
                                         ($try include)))
                           owsl
                           ($return content)))

(define makefile ($many makefile-item))

(define (pass1 port)
  (receive (p out) (open-string-output-port)
    (define (skip-comment)
      (let ((c (peek-char port)))
        (when (not (char=? c #\newline))
          (read-char port)
          (skip-comment))))
    (define (skip-return)
      (let ((c (peek-char port)))
        (cond
          ((or (char=? c #\newline) (char=? c #\return))
           (read-char port)
           (skip-return))
          (else (put-char p #\\)))))
    (define (loop)
      (let ((c (read-char port)))
        (if (eof-object? c)
          (out)
          (cond
            ((char=? c #\#)
             (skip-comment)
             (loop))
            ((char=? c #\return)
             (loop))
            ((char=? c #\\)
             (skip-return)
             (loop))
            (else 
              (put-char p c)
              (loop))))))
    (loop)))

(define (check-empty-makefile s)
  (define len (string-length s))
  (define (itr c)
    (if (= c len)
      '()
      (let ((r (string-ref s c)))
        (if (char-whitespace? r)
          (itr (+ c 1))
          #f))))
  (itr 0))

(define-reader (makefile-read port)
               (let ((s (pass1 port)))
                 ;; check if s is empty string...
                 (or (check-empty-makefile s)
                     (peg-parse-string makefile s))))

)

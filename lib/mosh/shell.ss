; shell.ss - Shell programing utilities
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;  $Id: shell.ss 621 2008-11-09 06:22:47Z higepon $

(library (mosh shell)
  (export def-command $def-command cd -> $-> def-alias)
  (import (only (rnrs) define define-syntax syntax-case ... syntax lambda let-values list
                       syntax->datum quote with-syntax datum->syntax string->symbol close-port
                       syntax-rules let transcoded-port make-transcoder utf-8-codec if eof-object?
                       list->string reverse cons read-char map pair? car cdr begin do = length +)
          (only (srfi :98) get-environment-variable)
          (only (system) %spawn %waitpid %pipe)
          (only (mosh) format string-split set-current-directory! current-directory))

(define-syntax def-alias
  (lambda (y)
    (syntax-case y()
      [(_ name body ...)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x ()
               [_ (begin body ...)])))])))

(define-syntax def-command
  (lambda (y)
    (syntax-case y ()
      [(_ command)
       #'(define-syntax command
           (lambda (x)
             (syntax-case x ()
               [(_ args (... ...))
                #'(spawn (any->string (syntax->datum #'command)) (map any->string (syntax->datum #'(args (... ...)))))]
               [_
                #'(spawn (any->string (syntax->datum #'command)) '())]
               )))])))

;; spawn and get result as list of string
(define-syntax $def-command
    (lambda (y)
      (syntax-case y ()
        [(k command)
         (with-syntax ([$command (datum->syntax #'k (string->symbol
                                                     (format "$~a" (syntax->datum #'command))))])
         #'(define-syntax $command
             (lambda (x)
               (syntax-case x ()
                 [(_ args (... ...))
                  #'(string-split
                     (spawn->string (any->string (syntax->datum #'command)) (map any->string (syntax->datum #'(args (... ...)))))
                                  #\newline)]
                 [_ #'(string-split
                       (spawn->string (any->string (syntax->datum #'command)) '())
                                    #\newline)]
                 )))]))))

(define-syntax cd
  (lambda (x)
    (syntax-case x ()
      [(_ arg)
       #'(set-current-directory! (format "~a" (syntax->datum #'arg)))]
      [_ #'(set-current-directory! (get-environment-variable "HOME"))])))

;; pipe
;; usage: (-> ls (grep hoge))
(define-syntax ->
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       #'(pipe ((any->string (if (pair? (syntax->datum #'x)) (car (syntax->datum #'x)) (syntax->datum #'x)))
                                (map any->string (if (pair? (syntax->datum #'x)) (cdr (syntax->datum #'x)) '())))
               ((any->string (if (pair? (syntax->datum #'y)) (car (syntax->datum #'y)) (syntax->datum #'y)))
                                (map any->string (if (pair? (syntax->datum #'y)) (cdr (syntax->datum #'y)) '())))
               ((any->string (if (pair? (syntax->datum #'z)) (car (syntax->datum #'z)) (syntax->datum #'z)))
                                (map any->string (if (pair? (syntax->datum #'z)) (cdr (syntax->datum #'z)) '()))) ...)])))

;; pipe
;; get result of pipe as string
(define-syntax $->
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       #'(let-values ([(in out) (%pipe)])
           ($pipe out
                  ((any->string (if (pair? (syntax->datum #'x)) (car (syntax->datum #'x)) (syntax->datum #'x)))
                   (map any->string (if (pair? (syntax->datum #'x)) (cdr (syntax->datum #'x)) '())))
                  ((any->string (if (pair? (syntax->datum #'y)) (car (syntax->datum #'y)) (syntax->datum #'y)))
                   (map any->string (if (pair? (syntax->datum #'y)) (cdr (syntax->datum #'y)) '())))
                  ((any->string (if (pair? (syntax->datum #'z)) (car (syntax->datum #'z)) (syntax->datum #'z)))
                   (map any->string (if (pair? (syntax->datum #'z)) (cdr (syntax->datum #'z)) '()))) ...)
           (port->string (transcoded-port in (make-transcoder (utf-8-codec)))))]
      )))

(define-syntax begin0
    (syntax-rules ()
      ((_ e es ...)
       (let ((v e))
         es ...
         v))))

(define (spawn command args)
  (let-values  ([(pid cin cout cerr) (%spawn command args (list #f #f #f))])
    (%waitpid pid)
    #f))

(define (spawn->string command args)
  (let-values ([(in out) (%pipe)])
    (let-values ([(pid cin cout cerr) (%spawn command args (list #f out #f))])
      (close-port out)
      (begin0
        (port->string (transcoded-port in (make-transcoder (utf-8-codec))))
        (close-port in)
        (%waitpid pid)))))

(define (port->string p)
  (let loop ([ret '()][c (read-char p)])
    (if (eof-object? c)
        (list->string (reverse ret))
        (loop (cons c ret) (read-char p)))))

(define (any->string x)
    (format "~a" x))

;; backend of ->.
(define-syntax pipe
  (lambda (x)
    (syntax-case x ()
      [(_ (cmd1 args1) (cmd2 args2) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list #f out1 #f))
           (close-port out1)
           (pipe "internal" in1 (cmd2 args2) ...)
           (do ([i 0 (+ i 1)])
               ((= i (length '((cmd2 args2) ...))))
             (%waitpid -1)))]
      [(_ "internal" in (cmd args))
       #'(begin
           (%spawn cmd args (list in #f #f))
           (close-port in))]
      [(_ "internal" in (cmd1 args1) (cmd2 args2) (cmd3 args3) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list in out1 #f))
           (close-port out1)
           (pipe "internal" in1 (cmd2 args2) (cmd3 args3) ...))]
      )))

(define-syntax $pipe
  (lambda (x)
    (syntax-case x ()
      [(_ out (cmd1 args1) (cmd2 args2) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list #f out1 #f))
           (close-port out1)
           ($pipe "internal" out in1 (cmd2 args2) ...)
           (do ([i 0 (+ i 1)])
               ((= i (length '((cmd2 args2) ...))))
             (%waitpid -1)))]
      [(_ "internal" out in (cmd args))
       #'(begin
           (%spawn cmd args (list in out #f))
           (close-port out)
           (close-port in))]
      [(_ "internal" out in (cmd1 args1) (cmd2 args2) (cmd3 args3) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list in out1 #f))
           (close-port out1)
           ($pipe "internal" out in1 (cmd2 args2) (cmd3 args3) ...))]
      )))



)

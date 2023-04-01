; repl.ss - REPL using (mosh shell)
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

(library (mosh shell repl)
  (export repl)
  (import (only (rnrs) define display lambda let record-type-name record-accessor case
                       else newline write unless = vector-length vector-ref + record-type-field-names
                       record-rtd simple-conditions if symbol? do null? car cdr read quote
                       guard current-error-port eof-object? begin exit get-line
                       current-input-port string-append or string=? string-length
                       call-with-port open-string-input-port - string->list)
          (only (mosh) format ungensym symbol-value current-directory))

(define (conditioon-printer e port)
    (define (ref rtd i x)
      (let ([val ((record-accessor rtd i) x)])
        (if (symbol? val)
            (ungensym val)
            val)))
    (display " Condition components:\n" port)
    (for-each-with-index
     (lambda (i x)
       (let ([rtd (record-rtd x)])
         (format port "   ~d. ~a" i (record-type-name rtd))
         (let ([v (record-type-field-names rtd)])
           (case (vector-length v)
             [(0) (newline port)]
             [(1)
              (display ": " port)
              (write (ref rtd 0 x) port)
              (newline port)]
             [else
              (display ":\n" port)
              (let f ([i 0])
                (unless (= i (vector-length v))
                  (display "       " port)
                  (display (vector-ref v i) port)
                  (display ": " port)
                  (write (ref rtd i x) port)
                  (newline port)
                  (f (+ i 1))))]))))
     (simple-conditions e)))

(define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))

(define eval-r6rs (symbol-value 'eval-r6rs))

(define (repl . x)
    (define (rec)
      (format #t "mosh:~a>" (current-directory))
      (guard (e
              (#t
               (display "\n" (current-error-port))
               (conditioon-printer e (current-error-port))))
             (let loop ([line (get-line (current-input-port))]
                        [accum ""])
               (define (parentheses-ok? text)
                 (let loop ([chars (string->list text)]
                            [p0 0]
                            [p1 0])
                   (if (null? chars)
                       (= 0 p0 p1)
                       (case (car chars)
                         [(#\()
                          (loop (cdr chars) (+ p0 1) p1)]
                         [(#\))
                          (loop (cdr chars) (- p0 1) p1)]
                         [(#\[)
                          (loop (cdr chars) p0 (+ p1 1))]
                         [(#\])
                          (loop (cdr chars) p0 (- p1 1))]
                         [else
                          (loop (cdr chars) p0 p1)]))))
               (define (eval-string-print text)
                 (unless (or (string=? "\n" text) (= 0 (string-length text)))
                   (display ((symbol-value 'eval-r6rs) (call-with-port (open-string-input-port text) read)))))
               (if (eof-object? line)
                   (begin
                     (eval-string-print accum)
                     (exit))
                   (let ([current (string-append accum line)])
                     (if (parentheses-ok? current)
                         (eval-string-print current)
                         (loop (get-line (current-input-port)) current))))))
      (newline)
      (rec))
    ;; pre-define
    (eval-r6rs '(import (mosh shell)))
    (eval-r6rs '(import (mosh)))
    (eval-r6rs '(def-command ls))

    ;; usage
    ;; register all of files in current directory
    ;;   (def-commands (filter (lambda (x) (not (eq? x 'cd))) (map string->symbol $ls))))
    ;; register pwd and svn
    ;; (def-commands '(pwd svn))
    (eval-r6rs '(define (def-commands c)
                  (for-each (lambda (y) ((symbol-value 'eval-r6rs) `(def-command ,y))) c)))
    (rec))

)

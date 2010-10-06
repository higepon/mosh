(library (nrepl simple io)
	 (export nrepl-read-one nrepl-loop-step nrepl-result-writer)
	 (import (rnrs))

;-------------------------------------------------------------------------
; simple REPL (standard I/O routines)
;-------------------------------------------------------------------------

; from psyntax-mosh's repl
(define (nrepl-read-one p)
  (define (readline)
    (get-line p))
  (define (string->datum str)
    (call-with-port (open-string-input-port str) read))
  (let loop ([line (readline)]
             [accum ""])
    (define (parentheses-ok? text)
      (let loop ([chars (string->list text)]
                 [p0 0]
                 [p1 0])
        ;; FIXME: should reject ([)] or so..
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
    (if (eof-object? line)
      (values (string->datum accum) #f)
      (let ([current (string-append accum line)])
        (if (parentheses-ok? current)
          (values (string->datum current) #t) ; return current string
          (loop (readline) (string-append current " ")))))))

(define (nrepl-result-writer . results)
  (for-each (lambda (e) (write e)(newline)) results))

(define (nrepl-loop-step reader evaluator result-writer) ; the no-guard strategy (we need this for make VM call %nmosh-failproc)
  (define (nrepl-standard-loop-step lis continue?)
    (call-with-values (lambda () (evaluator lis)) result-writer)
    continue?)
  (call-with-values reader nrepl-standard-loop-step))
)

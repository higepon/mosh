(library (nrepl simple)
	 (export nrepl)
	 (import (rnrs)
		 (only (mosh) set-symbol-value!)
		 (nmosh condition-printer)
		 (nmosh minidebug)
		 (primitives ex:destructive-eval! ex:interaction-environment exit)) ;;NMOSH only..

;-------------------------------------------------------------------------
; simple REPL
;-------------------------------------------------------------------------

(define init-k 'IT_S-NOT-INIT)

(define (display-banner)
  (display "nmosh top program")(newline))

(define (display-prompt)
  (display "nmosh> "))

(define (read-one)
  (display-prompt)
  (let ((l (get-line (current-input-port))))
    (when (eof-object? l) (exit))
    (call-with-port (open-string-input-port l) read)))

(define (do-eval l)
  (ex:destructive-eval! l (ex:interaction-environment)))

(define (eval-one)
  (do-eval (read-one)))

(define (loop) ; the no-guard strategy (we need this for make VM call %nmosh-failproc)
  (display (eval-one))(newline)
  (loop))

(define (show-traces c trace)
  (load-symbols)
  (condition-printer c (current-error-port))
  (stacktrace-printer trace (current-error-port))
  (init-k))

(define (init)
  (set-symbol-value! '%nmosh-failproc show-traces))

(define (startloop)
  (call-with-current-continuation
    (lambda (k)
      (set! init-k k)
      (loop)))
  (startloop))

(define (nrepl)
  (display-banner)
  (do-eval '(import (nmosh)))
  (init)
  (startloop))
)


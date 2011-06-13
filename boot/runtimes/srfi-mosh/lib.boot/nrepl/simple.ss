(library (nrepl simple)
	 (export nrepl)
	 (import (rnrs)
                 (nrepl simple io)
		 (nmosh condition-printer)
		 (nmosh minidebug)
		 (primitives ex:destructive-eval! ex:interaction-environment exit set-symbol-value!)) ;;NMOSH only..

;-------------------------------------------------------------------------
; simple REPL (driver loop)
;-------------------------------------------------------------------------

(define init-k 'IT_S-NOT-INIT)

(define (display-banner)
  (display "nmosh top program")
  (newline))

(define (display-prompt)
  (display "nmosh> "))

(define (do-eval l)
  (ex:destructive-eval! l (ex:interaction-environment)))

(define (loop)
  (display-prompt)
  (let ((continue?
          (nrepl-loop-step
            (lambda () (nrepl-read-one (current-input-port))) ; reader
            do-eval
            nrepl-result-writer)))
    (when (not continue?)
      (exit))
    (loop)))

(define (show-traces c trace)
  (load-symbols)
  (condition-printer c (current-error-port))
  (stacktrace-printer trace (current-error-port))
  (init-k))

(define (init)
  (set-symbol-value! '%nmosh-failproc show-traces))

(define (startloop)
  (call-with-current-continuation
    (lambda (k) (set! init-k k)
        (loop)))
  (startloop))

(define (nrepl)
  (display-banner)
  (init)
  (do-eval '(import (rnrs)))
  (do-eval '(import (rnrs load)))
  (call-with-current-continuation
    (lambda (k) (set! init-k k)
      (do-eval '(import (nmosh)))))
  (startloop))
)


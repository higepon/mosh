(library (nrepl simple)
	 (export nrepl)
	 (import (nmosh)
                 (nrepl simple io)
		 (only (mosh) set-symbol-value!)
		 (nmosh condition-printer)
		 (nmosh minidebug)
		 (primitives ex:destructive-eval! ex:interaction-environment exit)) ;;NMOSH only..

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
            (^[] (nrepl-read-one (current-input-port))) ; reader
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
    (^k (set! init-k k)
        (loop)))
  (startloop))

(define (nrepl)
  (display-banner)
  (do-eval '(import (nmosh)))
  (init)
  (startloop))
)


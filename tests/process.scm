; process.scm - test for (mosh process)

(import (rnrs)
        (only (mosh) host-os format)
        (mosh process)
        (mosh test))

(test-true (procedure? getpid))

; getpid takes no arguments
(test-error violation? (getpid 42))

; assume all pids are integers
(test-true (integer? (getpid))) 

; two consecutive calls to getpid should return the same value
(let* ((x (getpid))
       (y (getpid)))
  (test-eqv x y))

;; PIDs don't take 0 or less
(test-true (positive? (getpid))) 

(test-true (procedure? call-process))

; FIXME: wrap in OS conditional.  None of these apply to Windows.
; XXX: What about the use of ascii_c_str in call-process?

(let-values (((output exit-status termination-signal)
              (call-process "printf 'test: %s' 'mosh'")))
  (test-equal "test: mosh" output)
  (test-equal 0 exit-status)
  (test-false termination-signal))

(let-values (((output exit-status termination-signal)
              (call-process "exit 27")))
  (test-equal "" output)              ; POSIX specifies "STDOUT: Not used."
  (test-eqv 27 exit-status)
  (test-false termination-signal))

(let-values (((output exit-status termination-signal)
              (call-process "kill -9 $$")))
  (test-equal "" output)              ; POSIX specifies "STDOUT: Not used."
  (test-eqv #f exit-status)           ; I believe this is the correct semantic,
                                      ; a process that was killed by a signal
                                      ; has no real exit status.
  (test-eqv 9 termination-signal))

; Regression test to make sure Unicode is handled correctly.
; Currently disabled until we define the correct way to handle this.
;; (let ((unicode-string (string-append "Part " (string #\x2163) ": Mosh")))
;;   (let-values (((output exit-status termination-signal)
;;                 (call-process
;;                  (format "printf '%s' '~a'" unicode-string))))
;;     (test-equal unicode-string output)
;;     (test-eqv 0 exit-status)
;;     (test-false termination-signal)))

(test-true (procedure? waitpid))

(unless (string=? (host-os) "cygwin") ;; Cygwin may fail below
  (let-values (((pid cin cout cerr) (spawn "true" '() (list #f #f #f))))
    (let-values (((wpid status termsig) (waitpid pid)))
      (test-eqv pid wpid)
      (test-eqv status 0)
      (test-false termsig)))

  (let-values (((pid cin cout cerr) (spawn "false" '() (list #f #f #f))))
    (let-values (((wpid status termsig) (waitpid pid)))
      (test-eqv pid wpid)
      (test-true (not (zero? status)))
      (test-false termsig)))
  (let-values (((pid cin cout cerr) (spawn "sh" '("-c" "kill -9 $$") (list #f #f #f))))
    (let-values (((wpid status termsig) (waitpid pid)))
      (test-eqv pid wpid)
      (test-false status)
      (test-eqv 9 termsig))))

(test-results)

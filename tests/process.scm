; process.scm - test for (mosh process)

(import (rnrs)
        (mosh)
        (mosh process)
        (mosh test)
        (only (mosh file) rename-file file-executable?))

(define-syntax ignore-values
  (syntax-rules ()
    ((ignore-values producer ...)
     (call-with-values (lambda () producer ...) (lambda args #f)))))

(define (pipe->string p)
  (let ((p (transcoded-port p (make-transcoder (utf-8-codec)))))
    (let loop ([ret '()][c (read-char p)])
      (if (eof-object? c)
          (list->string (reverse ret))
          (loop (cons c ret) (read-char p))))))


(define empty-blob (make-bytevector 0))
(define (open-devnull) (open-file-input-port "/dev/null"))

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

<<<<<<< HEAD
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
=======
; consider replacing the command with 'true' or something
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
    (test-eqv 9 termsig)))
>>>>>>> higepon/master

; most basic test
(let-values (((pid cin cout cerr) (spawn "true" '())))
  (ignore-values (waitpid pid))
  (test-true (integer? pid))
  (test-false cin)
  (test-false cout)
  (test-false cerr))

; passing arguments to a spawned command
(let-values (((pid cin cout cerr) (spawn "test" '("foo" "=" "bar"))))
  (ignore-values (waitpid pid))
  (test-true (integer? pid))
  (test-false cin)
  (test-false cout)
  (test-false cerr))

; echoing strings to stdout
(let ((cin1 (open-devnull)))
  (let*-values (((pipe-in cout1) (pipe))
                ((pid cin2 cout2 cerr2)
                 (spawn "printf" '("test is %s" "mosh")
                        (list cin1 cout1 #f))))
    (close-port cout1)
    (ignore-values (waitpid pid))
    (test-true (integer? pid))
    (test-eq cin1 cin2)
    (test-eq cout1 cout2)
    (test-equal "test is mosh" (pipe->string pipe-in))))

; echoing strings to stderr
(let ((cin1 (open-devnull)))
  (let*-values (((pipe-in cerr1) (pipe))
                ((pid cin2 cout2 cerr2)
                 (spawn "sh" '("-c" "printf 'test is mosh' >&2")
                        (list cin1 #f cerr1))))
    (close-port cerr1)
    (ignore-values (waitpid pid))
    (test-true (integer? pid))
    (test-eq cin1 cin2)
    (test-eq cerr1 cerr2)
    (test-equal "test is mosh" (pipe->string pipe-in))))

; This test will only work if we are run from the right place, ie the root of the
; mosh source tree.  So skip it if there's no subdirectory 'tests'.
(let ((saved-cwd (current-directory)))
  (let ((setup-worked?
         (guard (ex (#t #f))
           (set-current-directory! "tests")
           (rename-file "printf.sh" "printf")
           (file-executable? "printf"))))
    (when setup-worked?
          (let ((cin1 (open-devnull)))
            (let*-values (((pipe-in cout1) (pipe))
                          ((pid cin2 cout2 cerr2)
                           (spawn "printf" '("test is not mosh")
                                  (list cin1 cout1 #f)
                                  #f)))
              (close-port cout1)
              (let-values (((wpid status termsig) (waitpid pid)))
                (test-eqv wpid pid)
                (test-true (integer? pid))
                (test-eq cin1 cin2)
                (test-eq cout1 cout2)
                (test-eqv 32 status)
                (test-equal "test is mosh\n" (pipe->string pipe-in)))))

        ; clean up
        (rename-file "printf" "printf.sh")
        (set-current-directory! saved-cwd))))

; emptying the environment
(let ((cin1 (open-devnull)))
  (let*-values (((pipe-in cout1) (pipe))
                ((pid cin2 cout2 cerr2)
                 (spawn "env" '()
                        (list cin1 cout1 #f)
                        #t
                        '())))
    (close-port cout1)
    (ignore-values (waitpid pid))
    (test-true (integer? pid))
    (test-eq cin1 cin2)
    (test-eq cout1 cout2)
    (test-equal "" (pipe->string pipe-in))))

; modified environment for subprocess
(let ((cin1 (open-devnull)))
   (let*-values (((pipe-in cout1) (pipe))
                 ((pid cin2 cout2 cerr2)
                  (spawn "env" '()
                         (list cin1 cout1 #f)
                         #t
                         '(("HOME" . "hige") ("PATH" . "hoge")))))
    (close-port cout1)
    (ignore-values (waitpid pid))
    (test-true (integer? pid))
    (test-eq cin1 cin2)
    (test-eq cout1 cout2)
    (test-equal "HOME=hige\nPATH=hoge\n" (pipe->string pipe-in))))

(test-results)

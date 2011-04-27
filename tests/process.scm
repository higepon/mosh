; process.scm - test for (mosh process)

(import (rnrs)
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

(test-results)

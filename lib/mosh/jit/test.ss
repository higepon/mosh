(library (mosh jit test)
  (export test)
  (import (rnrs) (mosh test) (mosh)
          (match)
          (srfi :8)
          (srfi :26)
          (only (srfi private include) include/resolve))

  (include/resolve ("mosh" "jit") "jit-impl.ss")

  (define (test)
    (test-true (register? 'rdi))
    (test-false (register? 'rrr))

    (receive (found index) (find-with-index even? '(1 3 4 5))
       (test-eq 4 found)
       (test-eq 2 index))

    (test-eq #xe3 (mod-r-m 'rbx 'rsp))

    (test-eq 4 (register->number 'rsp))

    (test-equal '(#x48 #x89 #xe3) (assemble '(movq rbx rsp)))
    (test-equal '(#x48 #x89 #xeb) (assemble '(movq rbx rbp)))
    (test-equal '(#x48 #x89 #xc4) (assemble '(movq rsp rax)))
    )
)

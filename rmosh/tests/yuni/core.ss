(import (rnrs) (shorten) (srfi :64)
        (yuni core))

(test-begin "yuni core")

;; type definition
(define* testtype
  (slot0 slot1 slot2))

(test-begin "constructors")

(define obj0 (make testtype
                   (slot0 'a)
                   (slot1 ((lambda () 'b)))
                   (slot2 (let ((a 'c)) a))))

(test-eq 'a (~ obj0 'slot0))
(test-eq 'b (~ obj0 'slot1))
(test-eq 'c (~ obj0 'slot2))

(define obj1 (make testtype))

(test-end "constructors")

(test-begin "let-with syntax")

(let-with obj0 (slot0 slot1 slot2)
          (test-eq 'a slot0)
          (test-eq 'b slot1)
          (test-eq 'c slot2))

(test-end "let-with syntax")

(test-begin "define* syntax")

(define* (proc0 (x testtype)) 'bogus)
(define* (proc1 (x testtype) a) 'bogus)

(test-end "define* syntax")

(test-end "yuni core")

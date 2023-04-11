(import (rnrs)
        (srfi :27)
        (srfi :78)
        (rbtree))

(check-set-mode! 'report-failed)

(let ([rb (make-rbtree = <)])
  (do ([i 0 (+ i 1)])
      ((= i 100))
    (rbtree-set! rb i i))
  (check (check-rbtree rb) => #t))

(let ([rb (make-rbtree = <)])

  (check (rbtree-get rb 1 'not-found) => 'not-found)

  (check (rbtree-contains? rb 1) => #f)

  (check (check-rbtree rb) => #t)
  (rbtree-set! rb 1 'val1)
  (check (rbtree-contains? rb 1) => #t)
  (check (rbtree-get rb 1) => 'val1)
  (check (check-rbtree rb) => #t)

  (rbtree-set! rb 2 'val2)
  (check (rbtree-get rb 1) => 'val1)
  (check (rbtree-get rb 2) => 'val2)

  (rbtree-set! rb 5 'val5)
  (check (rbtree-get rb 1) => 'val1)
  (check (rbtree-get rb 2) => 'val2)
  (check (rbtree-get rb 5) => 'val5)
  (check (check-rbtree rb) => #t)

  (rbtree-set! rb 3 'val3)
  (check (rbtree-get rb 1) => 'val1)
  (check (rbtree-get rb 2) => 'val2)
  (check (rbtree-get rb 3) => 'val3)
  (check (rbtree-get rb 5) => 'val5)
  (check (check-rbtree rb) => #t)

  (rbtree-set! rb 4 'val4)
  (check (rbtree-get rb 1) => 'val1)
  (check (rbtree-get rb 2) => 'val2)
  (check (rbtree-get rb 3) => 'val3)
  (check (rbtree-get rb 4) => 'val4)
  (check (rbtree-get rb 5) => 'val5)
  (check (check-rbtree rb) => #t)

  (rbtree-set! rb 0 'val0)
  (check (rbtree-get rb 0) => 'val0)
  (check (rbtree-get rb 1) => 'val1)
  (check (rbtree-get rb 2) => 'val2)
  (check (rbtree-get rb 3) => 'val3)
  (check (rbtree-get rb 4) => 'val4)
  (check (rbtree-get rb 5) => 'val5)
  (check (check-rbtree rb) => #t)

  (check (rbtree-size rb) => 6)
  (check (rbtree-keys rb) => '(0 1 2 3 4 5 ))

  (rbtree-delete! rb 1)
  (check (rbtree-get rb 1 'not-found) => 'not-found)
  (check (check-rbtree rb) => #t)
  (check (rbtree-size rb) => 5)

  (rbtree-delete! rb 5)
  (check (rbtree-get rb 5 'not-found) => 'not-found)
  (check (check-rbtree rb) => #t)

  (rbtree-delete! rb 3)
  (check (rbtree-get rb 3 'not-found) => 'not-found)
  (check (check-rbtree rb) => #t)

  (rbtree-delete! rb 0)
  (check (rbtree-get rb 0 'not-found) => 'not-found)
  (check (check-rbtree rb) => #t)

  (rbtree-delete! rb 2)
  (check (rbtree-get rb 2 'not-found) => 'not-found)
  (check (check-rbtree rb) => #t)

  (rbtree-delete! rb 4)
  (check (rbtree-get rb 4 'not-found) => 'not-found)
  (check (check-rbtree rb) => #t)

)

(when (file-exists? "rbtree.dot")
  (delete-file "rbtree.dot"))

(let ([port (open-output-file "rbtree.dot")])
  (let ([rb (make-rbtree string=? string<?)])

    (check (rbtree-get rb "abc" 'not-found) => 'not-found)

    (rbtree-set! rb "abc" 'val1)
    (rbtree-set! rb "abc" 'val2)
    (check (rbtree-get rb "abc") => 'val2)
    (check (check-rbtree rb) => #t)

    (rbtree->dot rb port)

    (rbtree-set! rb "def" 'val2)
    (check (rbtree-get rb "def") => 'val2)
    (check (check-rbtree rb) => #t)

    (rbtree-delete! rb "abc")
    (check (rbtree-get rb "abc" 'not-found) => 'not-found)
    (close-port port)
    ))

#;(let ([rb (make-rbtree = <)])
  (let ([port (open-output-file "rbtree-trees.dot")])
    (do ([i 0 (+ i 1)]
         [j (random-integer 1000000) (random-integer 1000000)])
        ((= i 100000))
      (rbtree-set! rb j j))
    (check (check-rbtree rb) => #t)
    (time (rbtree-get rb 1234 1))
    (close-port port)))

(when (file-exists? "rbtree.dot")
  (delete-file "rbtree.dot"))

(check-report)

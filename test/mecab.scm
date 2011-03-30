(import (rnrs)
        (mosh test)
        (mosh control)
        (mosh ffi)
        (mecab))

(let1 m (mecab-new2 "")
  (test-false (pointer-null? m))
  (let* ([text (string->utf8 "僕はお腹がすいた")]
         [len (bytevector-length text)])
    (write (pointer->string (mecab-sparse-tostr2 m src len))))
)

  ;;      [src (string->utf8 "ぼくひげぽん。")]
  ;;      [len (bytevector-length src)])
  ;; (write (pointer->string (mecab-sparse-tostr2 m src len)))
  ;; (let loop ([node (mecab-sparse-tonode2 m src len)])
  ;;   (cond
  ;;    [(pointer-null? node) '()]
  ;;    [else
  ;;     (write (mecab-node-length node))
  ;;     (write (mecab-node-surface node))
  ;;     (newline)
  ;;     (loop (mecab-node-next node))
  ;;     ])
  ;;   )
  ;; (mecab-destroy m))


(test-results)


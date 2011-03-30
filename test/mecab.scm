(import (rnrs)
        (mosh test)
        (mosh control)
        (mecab))

(let1 m (mecab-new2 "")
  (test-false (pointer-null? m))
)

(test-results)


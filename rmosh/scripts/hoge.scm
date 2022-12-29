(import (rnrs))
(import (match))

(define l '('HOGE _))

(match '(HOGE 3)
  [('HOGE 3) (display 'match)])
(import (rnrs) (mosh regexp))
(define (escape text)
  (fold-right (lambda (x y) (regexp-replace-all (car x) y (cdr x)))
        text
        '((#/</ . "&lt;")
          (#/>/ . "&gt;")
          (#/\"/ . "&quot;")
          (#/[^\\]'/ . "\'")
          (#/&/ . "&amp;")
          )))

(display (escape "taro&hanako<hr>"))

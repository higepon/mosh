(import (rnrs)
        (mosh control)
        (srfi :8)
        (mosh ffi))

(display (find-shared-libray #/^libmysqlclient/))

(display (receive a (begin0 (values 3 4) 5) a))
        

(let1 a 3
  (display a))

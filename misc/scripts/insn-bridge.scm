#!/usr/bin/env gosh
(use srfi-1)
(use util.match)

(define (main args)
  (with-input-from-port (current-input-port)
    (lambda ()
      (let loop ([obj (read)])
        (cond
         [(eof-object? obj) '()]
         [else
          (match obj
            [(name bsec busec asec ausec)
             (print name)]
            [else '()])
          (loop (read))])))))

 (library (party)
 ;; Total exports:
 ;; make, push, push!, make-party, pop!
  (export (rename (balloon:make make)
                  (balloon:push push))
          push!
          make-party
          (rename (party-pop! pop!)))
  (import (rnrs)
          (only (stack) make push! pop!) ; not empty!
          (prefix (balloons) balloon:))
  ;; Creates a party as a stack of balloons,
  ;; starting with two balloons
  (define (make-party)
    (let ([s (make)]) ; from stack
      (push! s (balloon:make 10 10))
      (push! s (balloon:make 12 9))
      s))
  (define (party-pop! p)
    (balloon:pop (pop! p))))

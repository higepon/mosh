(import (rnrs) (party))
(define p (make-party))
(pop! p) ; displays "Boom! 108"
(push! p (push (make 5 5) 1))
(pop! p)


(hashtable-clear! (hashtable-copy (make-eq-hashtable) #f))

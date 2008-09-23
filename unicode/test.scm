(load "downcase-inc.scm")

(define upcase-hashtable #f)
(define downcase-hashtable #f)

(define (alist->eq-hash-table alist)
  (let ([hashtable (make-eq-hashtable)])
    (for-each (lambda (x) (hashtable-set! hashtable (car x) (cdr x)))
                alist)
    hashtable))

(define (ralist->eq-hash-table alist)
  (let ([hashtable (make-eq-hashtable)])
    (for-each (lambda (x) (hashtable-set! hashtable (cdr x) (car x)))
                alist)
    hashtable))

(define (char-upcase char)
  (unless upcase-hashtable
    (set! upcase-hashtable (ralist->eq-hash-table downcase-alist)))
  (cond
   [(hashtable-ref upcase-hashtable (char->integer char)) => integer->char]
   [else char]))

(define (char-downcase char)
  (unless downcase-hashtable
    (set! downcase-hashtable (alist->eq-hash-table downcase-alist)))
  (cond
   [(hashtable-ref downcase-hashtable (char->integer char)) => integer->char]
   [else char]))


(display (char-upcase #\a))
(display #\x0109)
(display (char-upcase #\x0109))
(display (char-downcase #\Z))

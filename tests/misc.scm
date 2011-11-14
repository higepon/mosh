(import (rnrs)
        (mosh)
        (srfi :8)
        (monapi)
        (shorten)
        (mosh test))

;; Issue 201.
(test-error assertion-violation? (assert #f))
(test-equal #t (assert #t))
(test-equal "test string" (assert "test string"))

;; Issue 195.
(with-syntax ((a 1))
 (define a 1)
 (write 2))

(define (read-string str)
  (call-with-port (open-string-input-port str) read))

(define (obj->fasl obj)
  (receive (port bv-proc) (open-bytevector-output-port)
    (fasl-write obj port)
    (bv-proc)))

(define (fasl->obj bv)
  (let ([port (open-bytevector-input-port bv)])
    (fasl-read port)))

(define (hurtme string)
 (values)
 (let-values (((len)           (string-length string))
              ((port getter)   (open-string-output-port)))
   #f))

(define-test automatically-called?
  (test-true #t))

(test-false (hurtme "ciao"))

(test-equal "ABC\x0;ABC" (fasl->obj (obj->fasl (utf8->string #vu8(65 66 67 0 65 66 67)))))


(test-error assertion-violation? (vector-ref (vector) -1))
(test-error assertion-violation? (apply vector-ref (list (vector) -1)))
(test-error assertion-violation? (vector-set! (vector) -1 0))
(test-error assertion-violation? (apply vector-set! (list (vector) -1 0)))
(test-error assertion-violation? (vector-ref (vector) 'a))
(test-error assertion-violation? (apply vector-ref (list (vector) 'a)))

(test-error lexical-violation? (read-string "#(]"))
(test-error lexical-violation? (read-string "#vu8(]"))
(test-error lexical-violation? (read-string "(]"))
(test-error lexical-violation? (read-string "[)"))
(test-error lexical-violation? (read-string "#|#|"))

(test-equal "" (read-string "\"\\\n \""))
(test-equal "" (read-string "\"\\ \r \""))
(test-equal ""  (read-string "\"\\\t\r\t\""))
(test-equal 3  (read-string "3|4"))

(test-equal 1
  (let ()
    (define (f a)
      (define b a)
      b)
    (f 1)))
(test-equal 1
  (let ()
    (define (f a)
      (letrec* ((b a))
               b))
    (f 1)))

(test-error assertion-violation? (make-vector 1.0))

(test-error assertion-violation? (assoc 'a '(x)))
(test-error assertion-violation? (assoc 'a '#f))
(test-error assertion-violation? (assoc 'a '((x . y) y (a . v))))
(test-equal '(a . v) (assoc 'a '((x . y) (a . v) y)))
(test-error assertion-violation? (assoc 0 '(1)))

(test-eq #f (string->number ""))
(test-eq 1  (expt -1 (/ 4 2)))

(test-error assertion-violation? (char=? #\x))

;; Issue 213
(test-error assertion-violation? (apply + 1))

(let ([port (open-string-input-port "\"hige\"hage")])
  (test-equal "hige" (read port))
  (test-equal 'hage (read port)))

(test-equal "   : OK" (message->string #xfffff72a))
(test-equal "   :STR" (message->string #xffff29c6))
(test-equal "   :INT" (message->string #xfffe86ce))
(test-equal "   :TMR" (message->string #xffff3646))
(test-equal "   :RDY" (message->string #xffff11e2))
(test-equal "   :WDY" (message->string #xffff61e2))
(test-equal "   :TXT" (message->string #xffff3bce))
(test-equal "   :ADD" (message->string #xfffe018e))
(test-equal "   :RMV" (message->string #xffff1656))
(test-equal "   : NM" (message->string #xfffff6b2))
(test-equal "   :WHR" (message->string #xffff63c6))
(test-equal "   :STP" (message->string #xffff29be))
(test-equal "   :UPD" (message->string #xffff478e))
(test-equal "ABC DEF" (message->string #x443214))
(test-equal "ABC D1F" (message->string #x443d14))
(test-false (finite? +nan.0))
(test-false (flfinite? +nan.0))

(test-false (eqv? 4.0 4))

;; Section 11.3 of R6RS
(let ([only-once #t]
      [v0 (vector 1 2 3 4 5 6)]
      [cl '()]
      [old-v1 #f])
  (let ([v1 (vector-map
             (^e
              (call/cc
               (^c
                (set! cl (cons c cl))
                (* e e))))
             v0)])
    (when only-once
      (set! only-once #f)
      (set! old-v1 v1)
      ((car (reverse cl)) 'x))
    (test-equal '#(1 2 3 4 5 6) v0)
    (test-equal '#(1 4 9 16 25 36) old-v1)
    (test-equal '#(x 4 9 16 25 36) v1)))

;; Issue 224: segfault with nested hashtables
(let ()
(define-record-type mystruct
 (fields id
         state))

(define (add-a-struct! ht)
 (let* ((sid 53)
        (this (make-mystruct sid
                             (make-hashtable equal-hash equal?))))
   (hashtable-set! ht sid this)
   (let ((state (mystruct-state this)))
     (hashtable-ref state "something" #t)   ; this line makes SEGV likely
     #f)))

(let ((ht1 (make-eqv-hashtable)))
   (add-a-struct! ht1)
   (test-equal '#(53) (hashtable-keys ht1))
))

;; open-bytevector-output-port
(receive (port proc) (open-bytevector-output-port)
  (let*  ((bogus0 (put-bytevector port #vu8(1 2 3 4)))
          (ans0 (proc))
          (bogus1 (put-bytevector port #vu8(5)))
          (bogus2 (put-bytevector port #vu8(6)))
          (ans1 (proc)))
    (test-equal #vu8(1 2 3 4) ans0)
    (test-equal #vu8(5 6) ans1)
    ))

(test-results)


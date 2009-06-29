 (import (rnrs)
        (mosh)
        (rnrs mutable-pairs)
;        (mosh queue)
        (mosh test)
        (mosh concurrent))

<<<<<<< .mine
                  (f (- n 1) (* r n)))))
(display (fact 100000))
=======

;; ;; crash test
;; (let ([pid (spawn-link (lambda () (car 3)) '((rnrs) (mosh concurrent)))])
;;   (join! pid))
;; (receive
;;     [('exit x) (raise x)])
;; (display "not here\n")


;; (exit)

;; todo
;; spawn unquote
;; mosh concurrent auto-import
(let ([pid (spawn
             (lambda ()
               (test-eqv 'hello
                         (receive
                          [('apple)
                           (test-false #t)]
                          [('greeting what)
                           what]))
               (test-equal '(some hello)
                           (receive
                            [x x]))
               (test-equal 'good
                           (receive
                            ['good 'good]))
               (test-eqv 'hello2
                         (receive
                          [('greeting what)
                           what]))
               (test-eqv 'hello3
                         (receive
                          [('greeting what)
                           what]))
               (test-equal '(a . pen)
                           (let-values ([(x y) (receive
                                                [('this 'is x y) (values x y)])])
                             (cons x y)))
               ;; timeout
               (time (test-eqv 'time-out
                         (receive
                          [('greeting what) what]
                          [after 1
                                 'time-out])))
               (display "after")

               (time (register 'sub (self)))
               ;; doesn't work yet
              (time (receive
                 [('register from name)
                  (! from `(ok ,name))]))

;; ああ。mail-box lock してないところあるね。
              (time (receive
                 [('register from name)
                  (! from `(ok ,name))]))

               (test-results)
;               (process-exit 'normal)
;               (error 'hoge "hage")
               )
             '((rnrs) (mosh concurrent) (mosh) (mosh test)))])

(link pid)

(time (! pid '(some hello)))
(time (! pid '(greeting hello)))
(time (! pid '(greeting hello2)))
(time (! pid '(greeting hello3)))
(time (! pid 'good))
(time (! pid '(this is a pen)))

(time (! pid `(register ,(self) "higepon")))
(time (receive
    [('ok name) (test-equal "higepon" name)]))

(time (time (! 'sub `(register ,(self) "higepon"))))
(time (receive
    [('ok name) (test-equal "higepon" name)]))

(time (receive
    [('exit why) (test-equal 'normal why)]))

(time (let ([pid2 (spawn-link (lambda () (error 'unknown "hogehoge2")) '((rnrs) (mosh concurrent)))])
  (receive
      [('exit why) (test-true (error? why))
       #;(raise why)])))



(test-results)


(join! pid))
>>>>>>> .r1834

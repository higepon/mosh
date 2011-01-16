(library (yuni ui terminal color-sequence)
         (export
           terminal-style->string)
         (import (rnrs)
                 (yuni text styler))

(define (csi num cmd) ;; 7-bit csi command
  (string-append
    "\x1b;[" (number->string num) cmd))

(define (sgr num)
  (csi num "m"))

(define (b obj)
  (string-append
    (sgr 1)
    obj
    (sgr 0)))

(define (ul obj)
  (string-append
    (sgr 4)
    obj
    (sgr 0)))

(define (b+ul obj)
  (string-append
    (sgr 4)
    (sgr 1)
    obj
    (sgr 0)))

(define (color-escape-ecma num)
  (sgr (+ 30 num)))

(define (bgcolor-escape-ecma num)
  (sgr (+ 40 num)))


(define (color-ecma48 col obj)
  (if (eq? col 'default)
    (string-append (color-escape-ecma 9)
                   obj)
    (string-append (color-escape-ecma col)
                   obj
                   (color-escape-ecma 9))))

(define (bgcolor-ecma48 col obj)
  (if (eq? col 'default)
    (string-append (bgcolor-escape-ecma 9)
                   obj)
    (string-append (bgcolor-escape-ecma col)
                   obj
                   (bgcolor-escape-ecma 9))))

(define (terminal-style->string obj)
  (styler-apply escape-sequence-styler obj))

(define escape-sequence-styler 
  (make-styler `((color-ecma48 ,color-ecma48)
                 (bgcolor-ecma48 ,bgcolor-ecma48)
                 (b+ul ,b+ul)
                 (b ,b)
                 (ul ,ul))))

)

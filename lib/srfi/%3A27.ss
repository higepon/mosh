;;; porting srfi-27 reference implementation to ypsilon
;;; -- y.fujita.lwp
;;; And ported to Mosh
;;; -- Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
(library (srfi :27)
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals)
  (import (srfi :27 mrg32k3a)))

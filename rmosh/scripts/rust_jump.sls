;; Convert VM instruction offset for Rust.
;;   Instructions used in C++ VM are flat such as #(TEST 5 CONSTANT 3 PUSH).
;;   But in Rust VM they are grouped #(Test(5), Constant(3), Push).
;;   For that reason we have to adjust offset.
;;   Note that offsets can be negative for some instructions.

(import (scheme base))
(import (scheme write))
(import (match))
(import (only (mosh) format))
(import (only (mosh control) let1))
(import (mosh test))
(import (only (srfi :1) take))
(import (only (srfi :13) string-join))

(define (adjust-offset insn*)
  (match insn*
    [('LOCAL_JMP offset . more)
      
      (rust-offset (take more (- offset 1)))]))

(define (rust-offset insn*)
  (define (count-insn* insn*)
    (match insn*
      [() 0]
      [('CONSTANT _ . more)
        (+ 1 (count-insn* more))]))
  ;; Count # of instructions in between jump source and destination.
  ;; Then +1 to get destination offset.
  (+ 1 (count-insn* insn*)))

;; Jump destination is HALT.
(test-equal 2 (adjust-offset '(LOCAL_JMP 3 CONSTANT #t HALT NOP)))


(test-results)
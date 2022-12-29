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

;; Instruction with 1 argument.
(define (arg1-insn? insn)
  (or (jump1-insn? insn)
      (memq insn '(CONSTANT REFER_LOCAL RETURN))))

;; Jump instuction with 1 argument.
(define (jump1-insn? insn)
  (memq insn '(LOCAL_JMP TEST BRANCH_NOT_NUMBER_EQUAL)))

(define (adjust-offset insn*)
  (match insn*
    [((? jump1-insn? _) offset . more)
      (rust-offset (take more (- offset 1)))]
    [else
      (error (format "adjust-offset: no matching pattern ~a" (and (pair? insn*) (car insn*))))]))

(define (rust-offset insn*)
  (define (count-insn* insn*)
    (match insn*
      [() 0]
      [((? arg1-insn? _) _arg1 . more)
        (+ 1 (count-insn* more))]
      [else
        (error (format "rust-offset: no matching pattern ~a" (and (pair? insn*) (car insn*))))]))
  ;; Count # of instructions in between jump source and destination.
  ;; Then +1 to get destination offset.
  (+ 1 (count-insn* insn*)))

;; Jump destination is HALT.
(test-equal 2 (adjust-offset '(LOCAL_JMP 3 CONSTANT #t HALT NOP)))

;; Jump destination is CONSTANT #t
(test-equal 3 (adjust-offset '(TEST 5 CONSTANT #f LOCAL_JMP 3 CONSTANT #t HALT NOP)))

;; Jump destination s REFER_LOCAL 0.
(test-equal 3 (adjust-offset '(BRANCH_NOT_NUMBER_EQUAL 5 REFER_LOCAL 0 RETURN 1 REFER_LOCAL 0 PUSH CONSTANT 1)))

(test-results)
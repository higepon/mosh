;; Convert VM instruction offset for Rust.
;;   Instructions used in C++ VM are flat such as #(TEST 5 CONSTANT 3 PUSH).
;;   But in Rust VM they are grouped #(Test(5), Constant(3), Push).
;;   For that reason we have to adjust offset.
;;   Note that offsets can be negative for some instructions.
(define-library (rust_jump)
  (export adjust-offset arg2-insn? jump1-insn? jump3-insn? run-tests)
  (import (scheme base))
  (import (scheme write))
  (import (scheme case-lambda))
  (import (match))
  (import (only (mosh) format))
  (import (only (mosh control) let1))
  (import (mosh test))
  (import (only (srfi :1) take drop))
  (import (only (srfi :13) string-join))

  ;; Instruction with no argument.
  (define (arg0-insn? insn)
    (memq insn '(APPEND2 CAAR CADR CAR CAR_PUSH CDAR CDDR
                 CDR CDR_PUSH CONS EQ EQUAL EQV
                 INDIRECT NOT NULL_P
                 NUMBER_ADD NUMBER_ADD_PUSH
                 NUMBER_DIV NUMBER_EQUAL NUMBER_GE NUMBER_GT
                 NUMBER_LT NUMBER_MUL NUMBER_SUB NUMBER_SUB_PUSH
                 PAIR_P
                 PUSH READ_CHAR SET_CAR SET_CDR
                 SIMPLE_STRUCT_REF SYMBOL_P UNDEF
                 VECTOR_LENGTH VECTOR_P VECTOR_REF VECTOR_SET)))

  ;; Instruction with 1 argument.
  (define (arg1-insn? insn)
    (or (jump1-insn? insn)
        (memq insn '(ASSIGN_FREE ASSIGN_GLOBAL ASSIGN_LOCAL
                     BOX CALL CONSTANT CONSTANT_PUSH
                     DISPLAY ENTER LEAVE LET_FRAME
                     LOCAL_CALL MAKE_CONTINUATION PUSH_CONSTANT
                     PUSH_ENTER REFER_FREE REFER_FREE_PUSH
                     REFER_GLOBAL REFER_GLOBAL_PUSH
                     REFER_LOCAL REFER_LOCAL_PUSH RETURN VALUES))))

  ;; Jump instuction with 1 argument.
  (define (jump1-insn? insn)
    (memq insn '(BRANCH_NOT_EQ BRANCH_NOT_EQUAL BRANCH_NOT_EQV BRANCH_NOT_GE
                 BRANCH_NOT_GT BRANCH_NOT_LE BRANCH_NOT_LT BRANCH_NOT_NULL
                 BRANCH_NOT_NUMBER_EQUAL FRAME LOCAL_JMP NOT_TEST
                 PUSH_FRAME TEST)))

  ;; Instruction with 2 arguments.
  (define (arg2-insn? insn)
    (or (jump2-insn? insn)
        (memq insn '(LOCAL_TAIL_CALL RECEIVE REFER_FREE_CALL REFER_GLOBAL_CALL
                     REFER_LOCAL_CALL REFER_LOCAL_PUSH_CONSTANT TAIL_CALL)))) 

  ;; Jump instuction with 2 arguments.
  (define (jump2-insn? insn)
    (memq insn '(REFER_LOCAL_BRANCH_NOT_LT REFER_LOCAL_BRANCH_NOT_NULL)))

  ;; Instruction with 3 arguments.
  (define (arg3-insn? insn)
    (or (jump3-insn? insn)
        (memq insn '(SHIFTJ))))

  ;; Jump instuction with 3 arguments.
  (define (jump3-insn? insn)
    (memq insn '(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE)))    

  ;; Instruction with 6 arguments.
  (define (arg6-insn? insn)
    (memq insn '(CLOSURE)))    

  (define adjust-offset
    (case-lambda
      [(insn*)
        (adjust-offset insn* 0)]
      [(insn* start)
        (match (drop insn* start)
          ;; Jump forward.
          [((? jump1-insn? _) (? positive? offset) . more)
            (let1 new-insn* (take more (- offset 1))
              (+ (count-insn* new-insn*) 1))]
          [((? jump2-insn? _) _arg1 (? positive? offset) . more)
            (let1 new-insn* (take more (- offset 1))
              (+ (count-insn* new-insn*) 1))]   
          [((? jump3-insn? _) _arg1 _arg2 (? positive? offset) . more)
            (let1 new-insn* (take more (- offset 1))
              (+ (count-insn* new-insn*) 1))]                        
          ;; Jump backward.
          [((? jump1-insn? _) (? negative? offset) . more)
            ;; new-insn*
            ;; [...] [destination] ... [jump] [...] => [destination] ...
            (let1 new-insn* (take (drop insn* (+ start offset 1)) (- (abs offset) -1))
              (* -1 (- (count-insn* new-insn*) 1)))]            
          [any
            (error (format "adjust-offset: no matching pattern ~a" (take any 3)))])]))

  ;; Count # of instructions in the sequence.
  (define (count-insn* insn*)
    (match insn*
      [() 0]
      [((? arg0-insn? _) . more)
        (+ 1 (count-insn* more))]      
      [((? arg1-insn? _) _arg1 . more)
        (+ 1 (count-insn* more))]
      [((? arg2-insn? _) _arg1 _arg2 . more)
        (+ 1 (count-insn* more))]       
      [((? arg3-insn? _) _arg1 _arg2 _arg3 . more)
        (+ 1 (count-insn* more))]    
      [((? arg6-insn? _) _arg1 _arg2 _arg3 _arg4 _arg5 _arg6 . more)
        (+ 1 (count-insn* more))]                
      [any
        (error (format "count-insn*: no matching pattern ~a" (and (pair? any) (car any))))]))          

  (define (run-tests)
    ;; Jump destination is HALT.
    (test-equal 2 (adjust-offset '(LOCAL_JMP 3 CONSTANT #t HALT NOP)))

    ;; Jump destination is HALT.
    (test-equal 2 (adjust-offset '(CONSTANT 3 LOCAL_JMP 3 CONSTANT #t HALT NOP) 2))

    ;; Jump destination is CONSTANT #t
    (test-equal 3 (adjust-offset '(TEST 5 CONSTANT #f LOCAL_JMP 3 CONSTANT #t HALT NOP)))

    ;; Skip destination is PUSH.
    (test-equal 3 (adjust-offset '(FRAME 6 REFER_LOCAL_PUSH 0 REFER_FREE_CALL 4 1 PUSH REFER_LOCAL 1 BRANCH_NOT_GT 112)))

    ;; Jump desitnation is REFER_LOCAL_PUSH_CONSTANT.
    (test-equal 3 (adjust-offset '(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE 0 0 5 REFER_LOCAL 1 LOCAL_JMP 21 REFER_LOCAL_PUSH_CONSTANT 0 1 NUMBER_SUB_PUSH FRAME 8 REFER_FREE_PUSH 4 REFER_LOCAL_PUSH 1 REFER)))

    ;; Jumpt destination is (REFER_LOCAL_BRANCH_NOT_NULL 1 5)
    (test-equal 3 (adjust-offset '(REFER_LOCAL_BRANCH_NOT_NULL 0 5 REFER_LOCAL 1 RETURN 2 REFER_LOCAL_BRANCH_NOT_NULL 1 5 REFER_LOCAL 0)))

    ;; Jump destination is REFER_LOCAL 0.
    (test-equal 3 (adjust-offset '(BRANCH_NOT_NUMBER_EQUAL 5 REFER_LOCAL 0 RETURN 1 REFER_LOCAL 0 PUSH CONSTANT 1)))

    ;; Jump source is (LOCAL_JMP -20) and the destination is (REFER_LOCAL_BRANCH_NOT_NULL 0 5).
    (test-equal -9 (adjust-offset
                    '(ENTER 1 REFER_LOCAL_BRANCH_NOT_NULL 0 5 CONSTANT #t LOCAL_JMP 15 REFER_LOCAL 0 CAR BRANCH_NOT_NULL 10 REFER_LOCAL 0 CDR_PUSH SHIFTJ 1 1 0 LOCAL_JMP -20 LEAVE 1 RETURN 1)
                    21))

    ;; Jump source is (LOCAL_JMP -21) and the destination is (REFER_LOCAL_PUSH 0).
    (test-equal -10 (adjust-offset '(PUSH_ENTER 2 REFER_LOCAL_PUSH 0 REFER_LOCAL 1 BRANCH_NOT_EQ 5 REFER_LOCAL 0 LOCAL_JMP 13 REFER_LOCAL 0 CDR_PUSH REFER_LOCAL 1 CDR_PUSH SHIFTJ 2 2 0 LOCAL_JMP -21 LEAVE 2 LEAVE 2) 22))

    (test-results))
)  
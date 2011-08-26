;; stubs for compiler. should be generated from vm.scm/compiler.scm
(library (nmosh bootstrap stubs)
         (export 
           ;; for client
           init-compiler
           encode-compiler-output
           fetch-instruction-scm
           fetch-free-vars-scm

           ;; for compiler
           code-builder-put-extra1!
           code-builder-put-extra2!
           code-builder-put-extra3!
           code-builder-put-extra4!
           code-builder-put-extra5!
           code-builder-put-insn-arg0!
           code-builder-put-insn-arg1!
           code-builder-put-insn-arg2!
           code-builder-append!
           code-builder-emit
           make-code-builder
           push!
           
           ;; for patched compiler
           get-free-vars

           ;; for vm
           find-with-index
           errorf
           )
         (import (rnrs)
                 (rnrs mutable-pairs)
                 (only (srfi :1) first)
                 (srfi :8)
                 (srfi :48)
                 (yuni util files)
                 (yuni util binding-constructs)
                 (match))

(define (get-free-vars)
  free-vars)

;; copied/modified from vm.scm
(define (fetch-instruction-scm file)
  (with-input-from-file file
    (lambda ()
      (let loop ([obj (read)])
        (cond
         [(eof-object? obj) '()]
         [else
          (match obj
            [('define-insn name n)
             (cons (cons name n) (loop (read)))])])))))


;; copied from vm.scm
(define (insn-sym->insn-num insn-table syms)
  ;; For (PUSH 3) in #(CONSTANT (PUSH 3) ...).
  ;; We convert all instructions as *compiler-insn*.
  (define (insn-sym->insn-num-direct syms)
    (let loop ([syms syms])
      (cond [(null? syms)
             '()]
            [else
             (receive (i val) (find-with-index (lambda (insn) (eq? (first insn) (car syms))) insn-table)
               (cons (if i
                         `(*compiler-insn* ,i)
                         (car syms))
                     (loop (cdr syms))))])))
  (let loop ([index 0]
             [next-insn-index 0]
             [syms syms])
    (cond [(null? syms)
           '()]
          ;; special case. compiler has list which has instruction like '(0 UNDEF).
          ;; so we convert it into '(0 (*compiler-insn* n))
          ;; N.B. we ignore dotted pair.
          [(list? (car syms))
           (cons (insn-sym->insn-num-direct (car syms))
                 (loop (+ index 1)
                       next-insn-index
                       (cdr syms)))]
          [else
           (receive (i val) (find-with-index (lambda (insn) (eq? (first insn) (car syms))) insn-table)
             (cons (if i
                       (if (= index next-insn-index)
                           `(*insn* ,i)
                           `(*compiler-insn* ,i))
                       (car syms))
                   (loop (+ index 1) (if (= index next-insn-index)
                                         (if val (+ next-insn-index (cdr val) 1) (errorf "instruction.scm offset wrong on ~a" (car syms)))
                                         next-insn-index) (cdr syms))))])))

;; copied/modified from compiler.scm

  (define (make-array)
    (list 'array (make-vector 2) 0))

  (define (array? obj)
    (eq? 'array (car obj)))

  (define (array-length array)
    (caddr array))

  (define (vector-copy dst src length)
    (do ((i 0 (+ i 1))) ((>= i length) #f)
      (vector-set! dst i (vector-ref src i))))

  ;; use set-car! instead
  (define (set-array-length! array length)
    (set-car! (cddr array) length)
    (when (>= length (vector-length (array-data array)))
      (let1 next-data (make-vector (* length 2))
        (vector-copy next-data (array-data array) length)
        (set-car! (cdr array) next-data))))

  (define (array-data array)
    (cadr array))

  (define (array-push! array obj)
    (let* ([data (array-data array)]
           [length (array-length array)])
      (vector-set! data length obj)
      (set-array-length! array (+ length 1))))

  (define (array->list array)
    (let ([data (array-data array)]
          [length (array-length array)])
      (let loop ([i 0]
                 [ret '()])
        (if (>= i length)
            (reverse ret)
            (loop (+ i 1) (cons (vector-ref data i) ret))))))

  ;; moved to freeproc.cpp start
  ;; N.B. these procedures are still required by vm.scm
  (define (make-code-builder)
    (make-array))

  (define (code-builder-put-extra1! cb x)
    (array-push! cb x))

  (define (code-builder-put-extra2! cb a b)
    (array-push! cb a)
    (array-push! cb b)
    )


  (define (code-builder-put-extra3! cb a b c)
    (array-push! cb a)
    (array-push! cb b)
    (array-push! cb c)
    )

  (define (code-builder-put-extra4! cb a b c d)
    (array-push! cb a)
    (array-push! cb b)
    (array-push! cb c)
    (array-push! cb d)
    )

  (define (code-builder-put-extra5! cb a b c d e)
    (array-push! cb a)
    (array-push! cb b)
    (array-push! cb c)
    (array-push! cb d)
    (array-push! cb e)
    )

  (define (code-builder-append! cb1 cb2)
    (let loop ([e (array->list cb2)])
      (cond
       [(null? e)
        '()]
       [else
        (code-builder-put-extra1! cb1 (car e))
        (loop (cdr e))])))

  (define (code-builder-emit cb)
    (array->list cb))

  (define code-builder-put-insn-arg2! code-builder-put-extra3!)
  (define code-builder-put-insn-arg1! code-builder-put-extra2!)
  (define code-builder-put-insn-arg0! code-builder-put-extra1!)
  ;; moved to freeproc.cpp end



;----- utils
(define insn-table '())
(define free-vars '())

(define (fetch-free-vars-scm file)
  (map (lambda (e) (if (pair? e) (car e) e)) (cdar (file->sexp-list file))))

(define (init-compiler/file free-vars-file instruction-file)
  (set! free-vars (fetch-free-vars-scm free-vars-file))
  (set! insn-table (fetch-instruction-scm instruction-file)))

(define (init-compiler fv inst)
  (set! free-vars fv)
  (set! insn-table inst))

(define (encode-compiler-output x)
  (insn-sym->insn-num insn-table (vector->list x)))

(define (errorf fmt . obj)
  (let ((str (apply format (cons fmt obj))))
    (assertion-violation 'bootstrap-stubs str)))

(define (find-with-index proc l)
  (define (itr idx l)
    (if (pair? l)
      (let ((p (proc (car l))))
        (if p
          (values idx (car l))
          (itr (+ idx 1) (cdr l))))
      (values #f #f)))
  (itr 0 l))

(define (push! . x)
  (assertion-violation 'push! "unimplemented" x))

)

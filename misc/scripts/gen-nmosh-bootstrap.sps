(import (rnrs)
        (mosh pp)
        (srfi :8)
        (yuni core)
        (yuni util library-writer)
        (yuni util files))

(define src (file->sexp-list "boot/compiler.scm"))

(define (do-cond-expand frm)
  (define (expand x)
    (if (pair? x)
      (if (and (pair? (car x)) (eq? 'mosh (caar x)))
        (cdar x)
        (expand (cdr x)))
      '()))
  (define (target? x)
    (and (list? x) (eq? 'cond-expand (car x))))
  (if (pair? frm)
    (let ((x (car frm)))
      (if (target? x)
        (append (expand x) 
                (do-cond-expand (cdr frm)))
        (cons x (do-cond-expand (cdr frm)))))
    frm))

(define (filter-source x)
  (define (target? x)
    (and (list? x) (case (car x) ((include) #t) (else #f))))
  (if (pair? x)
    (if (target? (car x))
      (filter-source (cdr x))
      (cons (car x) (filter-source (cdr x))))
    '()))

(define (split-source frm)
  (define (target? x)
    (and (list? x) (eq? 'define-macro (car x))))
  (partition target? frm))

(let ((code (filter-source (do-cond-expand src))))
  (define (export-name x)
    (and (list? x) (eq? 'define (car x))
         (let ((name (cadr x)))
           (if (pair? name) (car name) name))))
  (define (export-all x)
    (if (pair? x)
      (let ((n (export-name (car x))))
        (if n
          (cons n (export-all (cdr x)))
          (export-all (cdr x))))
      '()))
  (define (conv x)
    (if (and (list? x) (eq? 'define-macro (car x)))
      (cons 'define (cdr x))
      x))
  (receive (macro proc) (split-source code)
    (write-library "lib/nmosh/boot/compiler.sls"
                   (make library-spec
                         (name '(nmosh boot compiler))
                         (export (remq 'top-level-macros (export-all proc)))
                         (import '((except (rnrs) 
                                           command-line
                                           do)
                                   (rnrs mutable-pairs)
                                   (yuni util binding-constructs)
                                   (nmosh boot compiler-macro)
                                   (match)
                                   (srfi :8)
                                   (primitives 
                                     *free-vars-decl*
                                     code-builder-append!
                                     code-builder-put-insn-arg1!
                                     code-builder-put-extra1! 
                                     code-builder-put-extra2! 
                                     pass3/find-sets
                                     code-builder-emit
                                     code-builder-put-insn-arg0!
                                     code-builder-put-insn-arg2!
                                     code-builder-put-extra3!
                                     code-builder-put-extra4!
                                     code-builder-put-extra5!
                                     make-code-builder
                                     pass1/find-symbol-in-lvars
                                     hashtable-for-each
                                     *command-line-args*
                                     annotated-cons
                                     fold
                                     split-at
                                     print
                                     gensym
                                     ungensym
                                     format
                                     foldr2
                                     append!
                                     push!
                                     source-info
                                     set-source-info!
                                     vm/apply
                                     pass4/fixup-labels)))
                         (code proc)
                         (comment '("nothing"))))

    (let ((macro-code (map conv macro)))
      (write-library "lib/nmosh/boot/compiler-macro.sls"
                     (make library-spec
                           (name '(nmosh boot compiler-macro))
                           (export (export-all macro-code))
                           (import '((for (except (rnrs) do) run expand)
                                     (for (match) expand run)
                                     (for (primitives gensym) expand)
                                     (nmosh util define-macro)
                                     (for (yuni util binding-constructs) expand)
                                     (for (nmosh util syntax-error) expand)
                                     ))
                           (code macro)
                           (comment '("nothing")))))))


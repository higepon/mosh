(library (yuni text makefile translator)
         (export makefile-translate)
         (import (rnrs)
                 (shorten)
                 (srfi :26)
                 (yuni core)
                 (yuni text makefile recipe)
                 (yuni text makefile expander))

(define (make-recipe* l)
  (define (make-recipe x)
    (define (make-one target dep cmd)
      (make recipe
        (phony? #f) (target target)
        (dep* dep) (cmd* cmd)))
    (let ((target (cadr x))
          (dep (caddr x))
          (cmd (cdddr x)))
      (map (cut make-one <> dep cmd) target)))
  (define (pass1-itr acc cur) ;; => recipe*
    (if (pair? cur)
      (let ((a (car cur))
            (next (cdr cur)))
        (if (eq? (car a) 'recipe)
          (let ((r (make-recipe a)))
            (pass1-itr (append r acc) next))
          (pass1-itr acc next)))
      (reverse acc)))
  (define (pass2 rec*)
    (define (mark-phony-target! l)
      (define (mark name)
        (for-each (^e (let-with e (target)
                        (when (string=? target name)
                          (touch! e (phony? #t)))))
                  rec*))
      (for-each mark l))
    (define* (pass2-proc cur (recipe))
      (let-with recipe (target dep*)
        (if (string=? ".PHONY" target)
          (begin (mark-phony-target! dep*)
                 cur)
          (cons recipe cur))))
    (reverse (fold-left pass2-proc '() rec*)))
  (pass2 (pass1-itr '() l)))

(define (makefile-translate mf-root file env) ;; => recipe*
  ;; FIXME: process include
  ;; FIXME: ...
  (define root (makefile-expand mf-root file env))
  (define recipe* (make-recipe* (filter (^e (eq? 'recipe (car e))) root)))
  recipe*)

)
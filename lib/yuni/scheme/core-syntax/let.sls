(library (yuni scheme core-syntax let)
  (export letrec*)
  (import (for (yuni scheme core-syntax backend)        expand run)
          (for (yuni scheme core-syntax with-syntax)       expand))
  
  (define-syntax letrec*
    (lambda (x)
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (syntax (let ()
                   (define i v) ...
                   (let () e1 e2 ...)))))))
  
  ) ; let

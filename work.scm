(import (system)
        (rnrs)
        (srfi :8))

(define-syntax def-command
    (lambda (y)
      (syntax-case y ()
        [(_ command)
         #'(define-syntax command
             (lambda (x)
               (syntax-case x ()
                 [(_ args (... ...))
                  #'(spawn (symbol->string (syntax->datum #'command)) (map symbol->string (syntax->datum #'(args (... ...)))))
                  ]
                 [_
                  #'(spawn (symbol->string (syntax->datum #'command)) '())
                  ]
                 )))])))

(def-command ls)
ls

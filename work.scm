(import (rnrs))

(lambda (name label)
  (begin (if (not (symbol? name))
             (error 'make-top-rib '"BUG: not a symbol" name) (void))
         (ai2ea4@extend-rib/nc! ai46ce@rib (make-simple-struct 'stx '5 (list name ai2e70@top-mark* '() '())) label)))


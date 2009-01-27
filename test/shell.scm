(import (rnrs)
        (mosh test)
        (system)
        (mosh)
        (mosh shell)
        (mosh shell repl))

(def-command ls)
($def-command ls)

(test* ls #f)
(test* (ls -la) #f)
(test* (for-all (lambda (s) (string? s)) $ls) #t)
(test* (begin (cd) (current-directory)) (get-environment-variable "HOME"))
(test* (begin (-> ls (grep main) (grep cpp)) #f) #f)
(test* (string? ($-> ls (grep main) (grep cpp))) #t)

(def-alias ls-x2 ls ls)
(test* ls-x2 #f)

(test-end)

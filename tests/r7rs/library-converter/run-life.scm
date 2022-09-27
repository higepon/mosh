;; R7RS small 5.6.2. Library example.
(import (scheme base)
        (only (example life) life)
        (rename (prefix (example grid) grid-) (grid-make make-grid)))

;; Initialize a grid with a glider.
(define grid (make-grid 24 24))
(grid-set! grid 1 1 #true)
(grid-set! grid 2 2 #true)
(grid-set! grid 3 0 #true)
(grid-set! grid 3 1 #true)
(grid-set! grid 3 2 #true)

;; Run for 80 iterations.
(life grid 80)
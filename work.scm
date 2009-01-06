;; (import (rnrs)
;;         (mosh pp))

;; (pretty-print '(define (object->limited-string obj limit)
;;   (let ((result '()) (left limit))
;;     (generic-write obj #f #f
;;       (lambda (str)
;;         (let ((len (string-length str)))
;;           (if (> len left)
;;             (begin
;;               (set! result (cons (substring str 0 left) result))
;;               (set! left 0)
;;               #f)
;;             (begin
;;               (set! result (cons str result))
;;               (set! left (- left len))
;;               #t)))))
;;     (reverse-string-append result))))

;; ;; (let1 pid (%fork)
;; ;;   (if (zero? pid)
;; ;;       (%exec "hige" '("-la"))
;; ;;       (begin
;; ;;         (%waitpid pid)
;; ;;         (print 'done))))

;; (display #;#;#;1 2 3 4) (newline)

(import (rnrs))

(define-syntax and-let*
 (lambda (stx)
   (define (get-id c)
     (syntax-case c () [(var expr) #'var] [_ #f]))
   (syntax-case stx ()
     [(_ (clause* ...) body* ...)
      (for-all identifier? (filter values (map get-id #'(clause* ...))))
      #'(and-let*-core #t (clause* ...) body* ...)])))

(define-syntax and-let*-core
 (lambda (stx)
   (syntax-case stx ()
     [(kw _ ([var expr] clause* ...) body* ...)
      #'(let ([var expr])
          (if var
              (kw var (clause* ...) body* ...)
            #f))]
     [(kw _ ([expr] clause* ...) body* ...)
      #'(let ([t expr])
          (if t
              (kw t (clause* ...) body* ...)
            #f))]
     [(kw _ (id clause* ...) body* ...)
      (or (identifier? #'id)
          (syntax-violation #f "invalid clause" stx #'id))
      #'(if id
            (kw id (clause* ...) body* ...)
          #f)]
     [(kw last () body* ...)
      (if (positive? (length #'(body* ...)))
          #'(begin body* ...)
        #'last)])))

(display
(let ((x #f))
 (and-let* (x)
   (+ x 1))))

;(begin (set! G58@dummy (begin ((lambda (G59@x) (if G59@x (+ G59@x '1) '#f)) '#f) (void))) (void))

;((lambda (G59@x) (if G59@x (+ G59@x '1) '#f)) '#f)


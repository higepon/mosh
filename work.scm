;; (import (rnrs)
;;    (core)
;; #;       (mosh))

;; (define-syntax time
;;   (syntax-rules ()
;;     ((_ expr)
;;      (destructuring-bind (real-start user-start sys-start) (time-usage)
;;        (let ((result (apply (lambda () expr) '())))
;;          (destructuring-bind (real-end user-end sys-end) (time-usage)
;;            (format #t
;;                    "~%;;~10,6f real ~11,6f user ~11,6f sys~%~!"
;;                    (- real-end real-start)
;;                    (- user-end user-start)
;;                    (- sys-end sys-start)))
;;          result)))))

(define a '())
(time
(let loop ([i 0])
  (cond
   [(= i 100000) '()]
   [else
    (call/cc (lambda (c) (set! a 3)c))
    (loop (+ i 1))])))

;; (define dynamic-wind2
;;   (lambda (in body out)
;;     (in)
;;     (current-dynamic-winders (cons (cons in out) (current-dynamic-winders)))
;;     (call-with-values
;;         body
;;         (lambda ans
;;           (current-dynamic-winders (cdr (current-dynamic-winders)))
;;           (out)
;;           (apply values ans)))))

;; (define perform-dynamic-wind
;;   (lambda (new)
;;   (define common-tail
;;     (lambda (x y)
;;       (let ((lx (length x)) (ly (length y)))
;;         ;; (do ((x (if (> lx ly) (list-tail x (- lx ly)) x) (cdr x))
;; ;;              (y (if (> ly lx) (list-tail y (- ly lx)) y) (cdr y)))
;; ;;             ((eq? x y) x))
;;         (letrec ([loop (lambda (x y)
;;                          (if (eq? x y)
;;                              x
;;                              (loop (cdr x) (cdr y))))])
;;           (loop (if (> lx ly)
;;                     (list-tail x (- lx ly))
;;                     x)
;;                 (if (> ly lx)
;;                     (list-tail y (- ly lx))
;;                     y))))))
;;   (display "perform-dynamic-wind\n")
;;     (let ((tail (common-tail new (current-dynamic-winders))))
;;       (let loop ((rec (current-dynamic-winders)))
;;         (cond ((not (eq? rec tail))
;;                (current-dynamic-winders (cdr rec))
;;                ((cdar rec))
;;                (loop (cdr rec)))))
;;       (let loop ((rec new))
;;         (cond ((not (eq? rec tail))
;;                (loop (cdr rec))
;;                ((caar rec))
;;                (current-dynamic-winders rec)))))))

;; (define k '())
;; (dynamic-wind
;;  (lambda () (display "before\n"))
;;  (lambda () (display (call/cc (lambda (c) (set! k c) c))))
;;  (lambda () (display "after\n")))

;; (k 'hello)
;; (display
;; (letrec ((paths '())
;;          (c #f)
;;          (add (lambda (s) (set! paths (cons s paths)))))
;;   (dynamic-wind
;;    (lambda () (add 'connect))
;;    (lambda ()
;;      (add (call/cc (lambda (c0) (set! c c0) 'talk1))))
;;    (lambda () (add 'disconnect)))
;;   (display (length paths))
;;   (if (< (length paths) 4)
;;       (c 'talk2)
;;       (reverse paths))))

;; (define q '())

;; (display (call/cc (lambda (k) (set! q k))))
;; (q 1 2 3)
;; (define cont '())
;; (call/cc (lambda (k)
;;            (call/cc (lambda (c) (set! cont c)))
;;            ))
;; (cont 1)

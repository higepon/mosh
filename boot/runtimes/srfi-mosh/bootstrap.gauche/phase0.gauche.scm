
(define (read-all/port p)
  (let ((r (read p)))
    (if (eof-object? r)
      '()
      (cons r (read-all/port p)))))

(define (read-all fn)
  (call-with-input-file fn read-all/port))

(define expander-src (read-all "expander.scm"))

(load "./bootstrap.common/alexpander.scm")

(let* ((expander (expand-program expander-src)))
  (call-with-output-file "bootstrap0.gexp"
			 (lambda (p)
			   (for-each 
			     (lambda (e)
			       (write e p)
			       (newline p))
			     expander))))


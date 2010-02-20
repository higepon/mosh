
(define (read-all/port p)
  (let ((r (read p)))
    (if (eof-object? r)
      '()
      (cons r (read-all/port p)))))

(define (read-all fn)
  (call-with-input-file fn read-all/port))

(define expander-src (apply append (map read-all '("compat-mosh-run.scm"
						   "runtime.scm"
						   "mosh-utils5.scm"
						   "expander.scm"))))

(load "./bootstrap.common/alexpander.scm")

(when (file-exists? "bootstrap0.exp")
  (delete-file "bootstrap0.exp"))

(let* ((expander (expand-program expander-src)))
  (call-with-output-file "bootstrap0.exp"
			 (lambda (p)
			   (for-each 
			     (lambda (e)
			       (write e p)
			       (newline p))
			     expander))))


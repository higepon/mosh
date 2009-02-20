<<<<<<< .mine
;;; --- definition begin ---
=======
(import (rnrs))
>>>>>>> .r1225

<<<<<<< .mine
(import (mosh ffi) (srfi :48) (rnrs))
=======
(define (fib n)
  (if (<= n 2) 1
      (+ (fib (- n 1)) (fib (- n 2)))))
>>>>>>> .r1225

<<<<<<< .mine
(define libc (open-shared-library "libc.so.6")) ;; linux
(define sizeof:long 4) ;; 4 for 32bit (should be 8 for 64bit but not tested)
(define timeval (make-bytevector 1024))
(define rusage (make-bytevector 1024))

(define gettimeofday
  (c-function libc int gettimeofday char* int))

(define getrusage
  (c-function libc int getrusage int char*))

(define (current-second)
  (gettimeofday timeval 0)
  (let ((sec (bytevector-u32-native-ref timeval 0))
        (usec (bytevector-u32-native-ref timeval sizeof:long)))
    (+ sec (/ usec 1000000.0))))

(define (current-usage)
  (getrusage 0 rusage) ;; 0: RUSAGE_SELF
  (let ((utime-sec (bytevector-u32-native-ref rusage 0))
        (utime-usec (bytevector-u32-native-ref rusage sizeof:long))
        (stime-sec (bytevector-u32-native-ref rusage (+ sizeof:long sizeof:long)))
        (stime-usec (bytevector-u32-native-ref rusage (+ sizeof:long sizeof:long sizeof:long))))
    (list (+ utime-sec (/ utime-usec 1000000.0))
          (+ stime-sec (/ stime-usec 1000000.0)))))

(define-syntax time
  (syntax-rules ()
    ((_ expr)
     (let ((real-start (current-second)))
       (apply (lambda () expr) '())
       (let ((real-end (current-second)))
         (let ((usage-start (current-usage)))
           (let ((result (apply (lambda () expr) '())))
             (let ((usage-end (current-usage)))
               (format #t "~%;;~10,6f real ~11,6f user ~11,6f sys~%"
                       (- real-end real-start)
                       (- (car usage-end) (car usage-start))
                       (- (cadr usage-end) (cadr usage-start)))
               result))))))))

(begin (current-second) (current-usage) (if #f #f)) ;; to preload bininds

;;; --- definition end ---

;; sample

(define (fib n)
  (if (<= n 2) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(time (fib 31))
=======
(fib 31)

>>>>>>> .r1225

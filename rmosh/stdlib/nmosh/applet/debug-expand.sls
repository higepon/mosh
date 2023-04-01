(library (nmosh applet debug-expand)
         (export debug-expand)
         (import 
           (primitives ex:expand-sequence/debug)
           (mosh pp)
           (yuni util files)
           (shorten)
           (rnrs))

(define args (command-line))
(define file
  (and (< 1 (length (command-line)))
       (cadr (command-line))))

#|
(define (preproc code)
  (if (and (pair? code) (= (length code) 1))
    (if (and (pair? (car code)) (eq? (caar code) 'begin))
      (cdar code)
      (begin
        (display ";; malfomed output ? (not started with begin)\n")
        code))
    (begin
      (display ";; malformed output? (length is not 1 but ")
      (display (and (pair? code) (length code)))
      (display ")\n")
      code)))
|#


(define (dump filename)
  (let ((ex (ex:expand-sequence/debug filename (file->sexp-list filename) #f)))
    (for-each (^e (pp e)) (car ex))))

(define (usage)
  (display "usage: debug-expand [FILE]\n" (current-error-port)))

(define (debug-expand)
  (if (string? file)
    (if (file-exists? file)
      (dump file)
      (begin (display "file not found!\n" (current-error-port))
             (usage)
             (exit -1)))
    (begin
      (usage)
      (exit 0))))

)


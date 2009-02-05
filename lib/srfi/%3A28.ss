;;; Ported to Mosh http://srfi.schemers.org/srfi-28/srfi-28.html
;;; -- Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
(library (srfi :28)
  (export format)
  (import (rnrs))

(define format
  (lambda (format-string . objects)
    (let-values (([buffer get-output-string](open-string-output-port)))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond ((null? format-list) (get-output-string))
              ((char=? (car format-list) #\~)
               (if (null? (cdr format-list))
                   (error 'format "Incomplete escape sequence")
                   (case (cadr format-list)
                     ((#\a)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (display (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                 ((#\s)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (write (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\%)
                      (newline buffer)
                      (loop (cddr format-list) objects))
                     ((#\~)
                      (display #\~ buffer)
                      (loop (cddr format-list) objects))
                     (else
                      (error 'format "Unrecognized escape sequence")))))
              (else (display (car format-list) buffer)
                    (loop (cdr format-list) objects)))))))

)

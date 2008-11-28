#!/usr/bin/env gosh
(use gauche.process)
(use srfi-1)
(use util.match)
(use gauche.parseopt)

(define (ntimes-map proc n)
  (let loop ([i 0]
             [ret '()])
    (if (>= i n)
        ret
        (loop (+ i 1) (append ret (list (proc)))))))

(define (get-process-error-line lst)
  (let* ((process (apply run-process `(,@lst :error :pipe :output "/dev/null")))
         (line (read-line (process-error process) #t)))
    (process-wait process)
    line))


(define (exec-with-time command)
  (string->number ((#/([\d\.]+)\s+total/ (get-process-error-line `("zsh" "-c" ,(string-append "time ./mosh " command)))) 1)))

(sys-system "uname -a")
(sys-system "svn info")

(for-each
 (lambda (file)
   (format #t "~25a:  ~d\n" file (apply min (ntimes-map (lambda () (exec-with-time file)) 3))))
 '("./bench/clos.scm"
   "./bench/empty.scm"
   "./bench/load-library.scm"))

;; Linux sewashi 2.6.22-15-generic #1 SMP Tue Oct 21 23:47:12 GMT 2008 i686 GNU/Linux
;; Path: .
;; URL: https://mosh-scheme.googlecode.com/svn/trunk
;; Repository Root: https://mosh-scheme.googlecode.com/svn
;; Repository UUID: 3dc39933-274d-0410-bf6f-2fa8959f7829
;; Revision: 740
;; Node Kind: directory
;; Schedule: normal
;; Last Changed Author: kokosabu
;; Last Changed Rev: 740
;; Last Changed Date: 2008-11-27 22:27:43 +0900 (Thu, 27 Nov 2008)
;; ./bench/clos.scm         :  0.61
;; ./bench/empty.scm        :  0.067
;; ./bench/load-library.scm :  0.109

;; Linux sewashi 2.6.22-15-generic #1 SMP Tue Oct 21 23:47:12 GMT 2008 i686 GNU/Linux
;; Path: .  
;; URL: https://mosh-scheme.googlecode.com/svn/trunk
;; Repository Root: https://mosh-scheme.googlecode.com/svn
;; Repository UUID: 3dc39933-274d-0410-bf6f-2fa8959f7829
;; Revision: 746
;; Node Kind: directory
;; Schedule: normal
;; Last Changed Author: higepon
;; Last Changed Rev: 746
;; Last Changed Date: 2008-11-28 17:10:32 +0900 (Fri, 28 Nov 2008)

;; ./bench/clos.scm         :  0.558
;; ./bench/empty.scm        :  0.065
;; ./bench/load-library.scm :  0.105


;; Darwin dorami.local 9.5.0 Darwin Kernel Version 9.5.0: Wed Sep  3 11:29:43 PDT 2008; root:xnu-1228.7.58~1/RELEASE_I386 i386
;; Path: .
;; URL: https://mosh-scheme.googlecode.com/svn/trunk
;; Repository Root: https://mosh-scheme.googlecode.com/svn
;; Repository UUID: 3dc39933-274d-0410-bf6f-2fa8959f7829
;; Revision: 741
;; Node Kind: directory
;; Schedule: normal
;; Last Changed Author: higepon
;; Last Changed Rev: 741
;; Last Changed Date: 2008-11-27 23:49:22 +0900 (Thu, 27 Nov 2008)

;; ./bench/clos.scm         :  0.668
;; ./bench/empty.scm        :  0.071
;; ./bench/load-library.scm :  0.117

;; Peephole optimization

;; Darwin dorami.local 9.5.0 Darwin Kernel Version 9.5.0: Wed Sep  3 11:29:43 PDT 2008; root:xnu-1228.7.58~1/RELEASE_I386 i386
;; Path: .
;; URL: https://mosh-scheme.googlecode.com/svn/trunk
;; Repository Root: https://mosh-scheme.googlecode.com/svn
;; Repository UUID: 3dc39933-274d-0410-bf6f-2fa8959f7829
;; Revision: 742
;; Node Kind: directory
;; Schedule: normal
;; Last Changed Author: higepon
;; Last Changed Rev: 742
;; Last Changed Date: 2008-11-28 15:33:31 +0900 (Fri, 28 Nov 2008)

;; ./bench/clos.scm         :  0.669
;; ./bench/empty.scm        :  0.071
;; ./bench/load-library.scm :  0.117

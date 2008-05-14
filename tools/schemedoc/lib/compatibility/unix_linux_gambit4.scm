;; Gambit-C 4 specific definitions to be loaded for compatibility when
;; using LAML.
;;
;; Copyright (c) 1999  Kurt Normark.
;; Copyright (c) 2005  Thomas Hafner.
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

(define directory-exists? #f)
(define sort-list #f)
(define directory-exists? #f)
(define make-directory-in-directory #f)
(define directory-list #f)
(define eval-cur-env #f)
(define laml-canonical-command-line #f)
(define fake-startup-parameters #f)

(define original-current-time
  (with-exception-handler
   (lambda (e) current-time)
   (lambda () original-current-time)))

(define original-command-line
  (with-exception-handler
   (lambda (e) command-line)
   (lambda () original-command-line)))

(let ()
  (include "sorting/sort.scm")

  (set! current-time
        (lambda ()
          (inexact->exact
           (floor
            (time->seconds
             (original-current-time))))))

  (set! sort-list sort:sort)

  (set! directory-exists?
        (lambda (ldir-path)
          (file-exists? (path-expand "." ldir-path))))

  (set! make-directory-in-directory
        (lambda (in-directory-path new-dir)
          (create-directory (path-expand new-dir in-directory-path))))

  (set! directory-list
        (lambda (path)
          (directory-files `(path: ,path ignore-hidden: dot-and-dot-dot))))

  (set! eval-cur-env eval)

  (set! laml-canonical-command-line
        (lambda ()
          (let* ((params (cdr (command-line)))
                 (argv (list->vector params)))
            (if (>= (vector-length argv) 2)
                (list 'laml 
                      (file-name-proper (vector-ref argv 0)) (vector-ref argv 1) 
                      (if (>= (vector-length argv) 3)
                          (vector-ref argv 2)
                          '()))
                #f))))

  (set! fake-startup-parameters
        (lambda (source-file startup-dir . optional-parameter-list)
          (let ((arg0 (car (original-command-line)))
                (program-parameters (optional-parameter 1 optional-parameter-list '())))
            (let ((args `(,arg0 ,source-file ,startup-dir ,program-parameters)))
              (set! command-line 
                    (lambda ()
                      args)))))))
  
(print "\n\n")
(define-macro (define-alist-extractor ?name ?name/cps ?tag ?default)
  `(begin
     (define-macro (,?name/cps . args)
       (match args
         ((() ?k . ?rands)
          ,(cons '?k (cons ?default '?rands)))
;          `(?k ,?default . ?rands))
         ((((',?tag ?val) . ?rest) ?k . ?rands)
          `(?k ?val . ?rands))
         ((((?another-tag ?val) . ?rest) ?k . ?rands)
          ,(cons ?name/cps (cons '?rest (cons '?k '?rands))))))
;          `(,?name/cps ?rest ?k . ?rands))))))
     (define-macro (,?name . args)
       (match args
         ((())
          ',?default)
         ((((',?tag ?val) . ?rest))
          ?val)
         ((((?another-tag ?val) . ?rest))
          (list ',?name ?rest))))))


(write (macroexpand '(define-alist-extractor extract-parent extract-parent/cps parent no-record-type)))
(print "\n\n")

;(define-macro (extract-parent . args) (match args ((()) 'no-record-type) (((('parent ?val) . ?rest)) ?val) ((((?another-tag ?val) . ?rest)) '(extract-parent ?rest))))
;
;(define-macro (extract-parent . args) (match args ((()) 'no-record-type) (((('parent ?val) . ?rest)) ?val) ((((?another-tag ?val) . ?rest)) (list 'extract-parent ?rest))))

(write (extract-parent ((hoge hige) (parent 'moge))))

;;      (define-syntax ?name
;;        (syntax-rules (?tag)
;;          ((?name ())
;;           ?default)
;;          ((?name ((?tag ?val) . ?rest))
;;           ?val)
;;          ((?name ((?another-tag ?val) . ?rest))
;;           (?name ?rest))))))

; Validation code for SXML-tree-trans.scm
;
; IMPORT
; SXML-tree-trans.scm and all of its imports
; Pretty-printer of trees (named pp)
; ,open sxml-tree-trans ppretty-prints coutputs assertions
;
; $Id: vSXML-tree-trans.scm,v 1.2 2004/07/07 16:02:31 sperber Exp $

(cout nl nl "Testing SXML-tree-trans.scm" nl)

(cout nl "Testing replace-range" nl)

(let* ((tree
	'(root
	     (n1 (n11) "s12" (n13))
	   "s2"
	   (n2 (n21) "s22")
	   (n3 
	    (n31 (n311))
	    "s32"
	    (n33 (n331) "s332" (n333))
	    "s34")))
       (test
	(lambda (pred-begin pred-end expected)
	  (let ((computed
		 (car (replace-range pred-begin pred-end (list tree)))))
	    (assert (equal? computed expected)))))
       )
  (cout "The tree under experimentation" nl)
  (pp tree)
  (newline)

  ; Remove one node, "s2"
  (test
   (lambda (node)
     (and (equal? node "s2") '()))
   (lambda (node) (list node))
   '(root (n1 (n11) "s12" (n13))
      (n2 (n21) "s22")
      (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34")))

  ; Replace one node, "s2" with "s2-new"
  (test 
   (lambda (node)
     (and (equal? node "s2") '("s2-new")))
   (lambda (node) (list node))
   '(root (n1 (n11) "s12" (n13))
      "s2-new"
      (n2 (n21) "s22")
      (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34")))

  ; Replace one node, "s2" with "s2-new" and its brother (n-new "s")
  (test 
   (lambda (node)
     (and (equal? node "s2") '("s2-new" (n-new "s"))))
   (lambda (node) (list node))
   '(root (n1 (n11) "s12" (n13))
      "s2-new" (n-new "s")
      (n2 (n21) "s22")
      (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34")))

  ; Remove everything from "s2" onward
  (test 
   (lambda (node)
     (and (equal? node "s2") '()))
   (lambda (node) #f)
   '(root (n1 (n11) "s12" (n13))))
   
   ; Remove everything from "n1" onward
  (test 
   (lambda (node)
     (and (pair? node) (eq? 'n1 (car node)) '()))
   (lambda (node) #f)
   '(root))

  ; Replace from n1 through n33
  (test 
   (lambda (node)
     (and (pair? node)
	  (eq? 'n1 (car node))
	  (list node '(n1* "s12*"))))
   (lambda (node)
     (and (pair? node)
	  (eq? 'n33 (car node))
	  (list node)))
   '(root
	(n1 (n11) "s12" (n13))
      (n1* "s12*")
      (n3 
       (n33 (n331) "s332" (n333))
       "s34")))
  )

;(cout nl nl "All tests passed" nl)

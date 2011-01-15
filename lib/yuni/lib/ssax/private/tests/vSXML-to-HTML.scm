; Validation code for SXML-to-HTML.scm
;
; IMPORT
; SXML-to-HTML.scm and all of its imports
; ,open sxml-to-html sxml-tree-trans coutputs assertions with-output-to-string srfi-23
;
; $Id: vSXML-to-HTML.scm,v 1.3 2004/07/07 16:02:31 sperber Exp $

; equal-strs? LIST-OF-PRINTABLES STRING
; Check to make sure that the result of writing out LIST-OF-PRINTABLES
; is the same as STRING
; LIST-OF-PRINTABLES can include strings, characters and numbers

(define (equal-strs?! strs expected-str)
   (let ((output-str
	  (with-output-to-string
	   (lambda ()
	     (for-each display strs)))))
     (assert (equal? output-str expected-str))))

(cout nl nl "Testing SXML-to-HTML.scm" nl nl)

(letrec ((gen (lambda (test-val)
		(with-output-to-string
		  (lambda ()
		    (SXML->HTML
		     `(p "par1" "par2" 
			 ,(and test-val (list "par3" "par4")))))))
	      ))
  (write (gen #t))
  (newline)
  (equal-strs?! '(#\newline "<p>par1par2par3par4</p>") (gen #t))
  (equal-strs?! '(#\newline "<p>par1par2</p>") (gen #f))
  )


(letrec ((gen (lambda (exp)
		(with-output-to-string
		  (lambda ()
		    (SXML->HTML exp))))))
  (equal-strs?! '(#\newline "<p>&amp;</p>") (gen '(p "&")))
  ;(write (gen '(p (^ (ALIGN "center")) "bad chars:" "<>&\"")))
  (equal-strs?! '(#\newline
		   "<p align=\"center\">bad chars:&lt;&gt;&amp;&quot;</p>")
		  (gen '(p (^ (align "center")) "bad chars:" "<>&\"")))
  (equal-strs?! '(#\newline
		   "<p align=\"center\" atr=\"&lt;value&gt;\">bad chars:"
		   #\newline "<em>&lt;&gt;&amp;&quot;</em></p>")
		(gen '(p (^ (align "center") (atr "<value>"))
			 "bad chars:" (em "<>&\""))))
  (equal-strs?! '(#\newline
		   "<p align=\"center\" atr=\"&quot;text&quot;\">"
		   #\newline "<br>"
		   #\newline "<ul compact>"
		   #\newline "<li>item 1</li></ul></p>")
		(gen '(p (^ (align "center") (atr "\"text\"")) (br)
			 (ul (^ (compact)) (li "item " 1)))))
  (equal-strs?!  '(#\newline "<p>"
		   #\newline "<br>"
		   #\newline "<ul compact>"
		   #\newline "<li>item 1</li></ul></p>")
		 (gen '(p (^) (br) (ul (^ (compact)) (li "item " 1)))))
  (equal-strs?! '("Content-type: text/html" #\newline #\newline
		  "<HTML><HEAD><TITLE>my title</TITLE></HEAD>"
		  #\newline "<body bgcolor=\"#ffffff\">"
		  #\newline "<p>par1</p></body></HTML>")
		(gen 
		 '(html:begin "my title" 
			      (body (^ (bgcolor "#ffffff")) (p "par1")))))
  )

(let ()
  (define (print-slide n max-count)
    (SXML->HTML
     `((h2 "Slide number:" ,n)     ; Note n is used in its native form
       ,(and (positive? n)
	     `(a (^ (href "base-url&slide=" ,(- n 1))) "prev"))
       ,(and (< (+ n 1) max-count)
	     `(a (^ (href "base-url&slide=" ,(+ n 1))) "next"))
       (p "the text of the slide"))))
  (equal-strs?! '(#\newline "<h2>Slide number:0</h2>" 
		  #\newline "<p>the text of the slide</p>")
		(with-output-to-string (lambda () (print-slide 0 1))))
  (equal-strs?! '(#\newline "<h2>Slide number:0</h2>"
		  #\newline "<a href=\"base-url&amp;slide=1\">next</a>"
		  #\newline "<p>the text of the slide</p>")
		(with-output-to-string (lambda () (print-slide 0 3))))
  (equal-strs?! '(#\newline "<h2>Slide number:1</h2>"
		  #\newline "<a href=\"base-url&amp;slide=0\">prev</a>"
		  #\newline "<a href=\"base-url&amp;slide=2\">next</a>"
		  #\newline "<p>the text of the slide</p>")
		(with-output-to-string (lambda () (print-slide 1 3))))
  (equal-strs?! '(#\newline "<h2>Slide number:2</h2>"
		  #\newline "<a href=\"base-url&amp;slide=1\">prev</a>"
		  #\newline "<p>the text of the slide</p>")
		(with-output-to-string (lambda () (print-slide 2 3))))
  )

(SXML->HTML
 `(ul
   ,@(map (lambda (filename-title)
	    `(li (a (^ (href ,(car filename-title))))
		 ,(cdr filename-title)))
	  '(("slides/slide0001.gif" . "Introduction")
	    ("slides/slide0010.gif" . "Summary")))
   )
 )

; Testing *PREORDER* and *MACRO* rules

(let ()
  (define (custom-sxml->html tree)
    (with-output-to-string (lambda () 
     (SRV:send-reply
      (pre-post-order tree
                ; Universal transformation rules. Work for every HTML,
                ; present and future
	`((^
	    ((*DEFAULT*       ; local override for attributes
	       . ,(lambda (attr-key . value) (enattr attr-key value))))
	    . ,(lambda (trigger . value) (cons '^ value)))
	   (*DEFAULT* . ,(lambda (tag . elems) (entag tag elems)))
	   (*TEXT* . ,(lambda (trigger str) 
			(if (string? str) (string->goodHTML str) str)))
	  (link
	   *MACRO*
	    . ,(lambda (tag url body)
		 `(a (^ (href ,url)) ,body)))
	   (vspace		; (vspace flag)
	     *PREORDER*			; where flag is a symbol: small, large
	     . ,(lambda (tag flag)
		  (case flag
		    ((large) (list "<p><br>&nbsp;</p>"))
		    ((small) (list "<br>&nbsp;<br>"))
		    (else (error "wrong flag:" flag))))))
	)))))
  (equal-strs?! '(#\newline "<p>text" 
		  #\newline "<a href=\"url\">&lt;body&gt;</a>text1</p>")
    (custom-sxml->html '(p "text" (link "url" "<body>") "text1")))

  (equal-strs?! '(#\newline "<p>text<br>&nbsp;<br>text1</p>")
    (custom-sxml->html '(p "text" (vspace small) "text1")))

  (equal-strs?! '(#\newline "<p>text<p><br>&nbsp;</p>text1</p>")
    (custom-sxml->html '(p "text" (vspace large) "text1")))
  )

;(cout nl nl "All tests passed" nl)

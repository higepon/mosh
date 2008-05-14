;=>man/schemedoc-extractor.laml

; The LAML SchemeDoc comment extracting and comment parsing tool.

; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2004  Kurt Normark.
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;;; .title Reference Manual of the SchemeDoc Extraction Tool
;;;; This is the LAML SchemeDoc tool (a library) which extracts interface documentation of a Scheme source program.
;;;; It is not likely that you wish to use the functions in this library directly. 
;;;; The SchemeDoc extraction facilities are normally used via <a href = "../../../styles/xml-in-laml/schemedoc-2/man/schemedoc.html"> LAML SchemeDoc</a>.</b>
;;;; See also the <a href="http://www.cs.auc.dk/~normark/schemedoc/">SchemeDoc home page</a>. <p>
;;;; 
;;;; Interface documentation is found in two, three or four semicolon Lisp comments.
;;;; This is called <em>multi semicolon comment style</em>.
;;;; From version 25 of LAML we also support <em>documentation mark commenting style</em>, in which 
;;;; interface comments are marked with a distinguished character, per default '!'. 
;;;; A number of internal tags in the comments make it possible to express a variety
;;;; of interface properties in a structured way. <p>
;;;;
;;;; The main procedure of the tool is extract-documentation-from-scheme-file. Two additional functions
;;;; used for internal purposes are also documented. <p>
;;;; 
;;;; If you read this via the documentation and manual facility, this text is extracted
;;;; from the Scheme source file and presented as a LAML manual. As such, Schemedoc has simply
;;;; been used on its own source file.<p>
;;;; 
;;;; We recommend that you consult <a href="../../../tutorial/schemedoc/schemedoc.html">"SchemeDoc and Manual pages"</a> for a tutorial 
;;;; introduction to SchemeDoc.<p>


; Warning: If schemdoc.scm is loaded after manual.scm, and if manual facility is used
; thereafter, problems occur. There is a conflict, yet unknown, between manual.scm and schemedoc.scm

;;; SchemeDoc parameters.
;;; This section contains a few SchemeDoc constants, which affect SchemeDoc's processing
;;; of a Scheme source file.
;;; .section-id parameters

;; The Scheme commening style which determines the identification of documentation comments.
;; Either multi-semicolon or documentation-mark (symbols).
(define scheme-documentation-commenting-style 'multi-semicolon) ; multi-semicolon or documentation-mark

;; A boolean variable which tells whether to delete the temporary (by this tool) generated source
;; file, in which comments are syntactical Lisp forms. The name of the source file is determined by
;; the variable temp-output-file. By default, the value is #t.
(define delete-comment-file? #t)

;; The character which designates the start of an internal tag i SchemeDoc (on the start of a line).
;; The preset value is the character '.'
(define internal-tagging-char #\.)

; The character which is used to separate internal tagging portions of documentation comment.
; Entirely internal to the SchemeDoc extractor.
; It is important to use a character, which is not used in other contexts.
(define end-of-tagging-line (as-char 7))

;; The escape character, which can be used to 'protect' an initial dot in a Scheme comment.
;; More concrete, we use the escaping character '$' for escaping of initial '.' in comment text.
;; Other characters can also be escaped, such as '$' itself.
;; The preset value is the character '$'.
(define escape-character #\$)

;; The character that can be used in the final position of a documentation comment line to indicate that the comment line is continued at the next (similar) comment line. 
(define line-continuation-char #\\)

;; The documentation comment character, as used for the documentation mark commenting style of SchemeDoc.
(define documentation-comment-character #\!)

; Reset the state of SchemeDoc
(define (reset-schemedoc-extractor!)
  (set! scheme-documentation-commenting-style 'multi-semicolon) ; multi-semicolon or documentation-mark
  (set! delete-comment-file? #t)
  (set! state-list '())
  (set! debugging-lexical-to-syntactical-comments #f)
  (set! temp-output-file "schemesource.tmp")
  (set! start-state 'outside-comment)
  (set! NOT-RELEVANT 0)
  (set! previous-level-2-comment "")

  (set! extracted-manual-abstract "")
  (set! extracted-manual-title "")
  (set! extracted-manual-author "")
  (set! extracted-manual-affiliation "")
  (set! extracted-laml-resource "")
  (set! extracted-css-prestylesheet "")
  (set! extracted-css-stylesheet "" )
  (set! extracted-css-stylesheet-copying "")
  (set! extracted-source-destination-delta "" )
  (set! extracted-scheme-source-linking "" )
)


;;; The main SchemeDoc function.
;;; The main SchemeDoc function is <kbd>extract-documentation-from-scheme-file<kbd>.
;;; .section-id main-procedure

;; This function is the main function of the SchemeDoc extractor. As such, this function IS the LAML SchemeDoc extraction tool.
;; The function takes as input a name of a scheme file (full path, file extension inclusive).
;; The function returns a list structure suitable for SchemeDoc to display.
;; The list is delivered in reverse order compared with the ordering of the extracted information from the source file.
;; Thus, you should reverse the result of this function before you pass it to the function make-manual of SchemeDoc.
;; In addition, this procedure assigns values to a number of global variables.
;; See <a href= "#extracted-variables">below</a> for details about these variables.
;; .parameter scheme-file The full path to the Scheme source file (including initial path and source file extension).
;; .returns A list of association lists (in reverse order compared to the content of the source file).
;; .internal-references "assigned variables" "extracted-manual-abstract" "extracted-manual-title" "extracted-manual-author" "extracted-manual-affiliation"
(define (extract-documentation-from-scheme-file scheme-file)
 (let ((temp-path (string-append (laml-temp-file-path) temp-output-file)))
  (lexical-to-syntactical-comments! scheme-file temp-path)
  ; hereby the file in temp-path is defined
  (let ((res (extract-relevant-documentation temp-path)))
      (if delete-comment-file? (delete-file temp-path))
    res)))





; ---------------------------------------------------------------------------------------------------
; PART ONE:
; State machine operated traversal of input file, which turns lexical lisp comment to syntactical (comment ...) Lisp forms.
; Main function: (lexical-to-syntactical-comments! scheme-file)


;;; The first internal function. 
;;; The function in this section makes lexical Lisp style comments to syntactical comments.
;;; We document it because it may be useful in its own right.
;;; .section-id first-internal

(define state-list '()) ; for debugging purposes
(define debugging-lexical-to-syntactical-comments #f)


;; Given the source file, scheme-input-file, make another file scheme-output-file,
;; in which the lexical comments are turned into syntactic comments in terms of comment forms.
;; The first parameter of a comment form is the number of semicolons in the lexical comment.
;; The second parameter is the comment text itself. Several consequtive lines of (equally leveled
;; comments) are organized in one comment form.
(define (lexical-to-syntactical-comments! scheme-input-file scheme-output-file)
 (if (file-exists? scheme-output-file) (delete-file scheme-output-file))
 (let* ((ip (open-input-file scheme-input-file))
        (op (open-output-file scheme-output-file)))
     (cond ((eq? scheme-documentation-commenting-style 'multi-semicolon)
               (multi-lexical-to-syntactical-comments-given-ports ip op))
           ((eq? scheme-documentation-commenting-style 'documentation-mark)
               (mark-lexical-to-syntactical-comments-given-ports ip op))
           (else (laml-error "lexical-to-syntactical-comments!: Unknown scheme documentation commenting style:" 
                             scheme-documentation-commenting-style)))
     (close-input-port ip)
     (close-output-port op)))


;; The name of the file where the function lexical-to-syntactical-comments! places it's output
(define temp-output-file "schemesource.tmp")

(define start-state 'outside-comment)

(define NOT-RELEVANT 0)

; Read and return the next character from inport.
; While the character is #\return, read on
(define (read-char-drop-return inport)
  (let ((ch (read-char inport)))
    (if (eqv? ch #\return)
        (read-char-drop-return inport)
        ch)))

(define (eof-port? port) (eof-object? (peek-char port)))

(define (write-string-to-port-scheme-doc str port)
  (write-string-to-port-scheme-doc-1 str port 0 (string-length str)))

(define (write-string-to-port-scheme-doc-1 str port i str-length)
  (if (< i str-length)
      (begin 
        (write-char (string-ref str i) port)
        (write-string-to-port-scheme-doc-1 str port (+ i 1) str-length))))

; ---------------------------------------------------------------------------------------------------------------
; Multi-semicolon documentation comment style.

(define (multi-lexical-to-syntactical-comments-given-ports inport outport)
  (set! state-list '())
  (multi-lexical-to-syntactical-comments-given-ports-1 start-state 0 0 "" inport outport))

(define (multi-lexical-to-syntactical-comments-given-ports-1 current-state semi-no new-semi-no remember-string inport outport)
  (if (not (eof-object? (peek-char inport)))
      (let* ((inch (as-string (read-char-drop-return inport)))
             (trans-res (multi-lexical-to-syntactical-comments-transition current-state semi-no new-semi-no inch remember-string inport))
             (next-state (car trans-res))
             (next-semi-no (cadr trans-res))
             (next-new-semi-no (caddr trans-res))
             (toput (as-string (cadddr trans-res)))
             (next-remember-string (fifth trans-res))
            )
       (if debugging-lexical-to-syntactical-comments
            (set! state-list (cons trans-res state-list)))
       (write-string-to-port-scheme-doc toput outport)  
       (multi-lexical-to-syntactical-comments-given-ports-1 next-state next-semi-no next-new-semi-no next-remember-string inport outport)
  )))
  

; STATES 
; We maintain  semi-no, which is the number of semicolons
; and also semi-no-new, which is the number of semicolons on the next comment line

; outside-comment                        Within a portion of a file which is not a comment (and not within a string)
; outside-comment-within-string          As outside-comment, but within a string (in which ; characters do not signify comments)
; outside-comment-within-special-char    State for reading special characters, such as #\;
; outside-comment-within-special-char-1  Ditto
; semicolon                              A semicolon has just been read
; within-comment                         Inside a 'semi-no' semicolon comment. semi-no not used for this style.
; within-comment-backslash               Inside a comment where an escaping backslash is encountered.
; leaving-comment                        Inside a 'semi-no' semicolon comment, but CR seen. Stay as long as we see white space.
;                                        The comment may be continued on next line after blank space.
; entering-comment                       We come from one comment line and is now entering another. 
;                                        We don't know yet how many semicolons in the new
; left-comment                           We just left the comment block.


(define (multi-lexical-to-syntactical-comments-transition in-state semi-no semi-no-new inch remember-string inport)
 (let ()
   (cond 
         ((and (symbol? in-state) (eq? in-state 'outside-comment))
            (cond ((equal? inch ";")                          (list  'semicolon 1 NOT-RELEVANT COMMENT-FORM-START ""))
                  ((equal? inch GATE-STRING)                  (list  'outside-comment-within-special-char 0 NOT-RELEVANT inch ""))
                  ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string 0 NOT-RELEVANT inch ""))
                  (else                                       (list  'outside-comment 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-string))
            (cond ((equal? inch QUOTE-STRING)                 (list  'outside-comment 0 NOT-RELEVANT inch ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'outside-comment-within-string-backslash 0 NOT-RELEVANT inch "")) ; new line
                  (else                                       (list  'outside-comment-within-string 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-string-backslash))   ; New state
            (cond ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string 0 NOT-RELEVANT inch ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'outside-comment-within-string 0 NOT-RELEVANT inch ""))
                  (else                                       (list  'outside-comment-within-string 0 NOT-RELEVANT inch ""))  ; actually problematic in relation to R5RS - but SchemeDoc does not care...
         ))

         ((and (symbol? in-state) (eq? in-state 'semicolon))
            (cond ((equal? inch ";")                          (list  'semicolon (+ 1 semi-no) NOT-RELEVANT EMPTY-STRING ""))
                  ((blank-or-tab? inch)                       (list  'within-comment semi-no NOT-RELEVANT
                                                                      (string-append 
                                                                       (as-string semi-no) SPACE-STRING QUOTE-STRING)
                                                                      ""))   ; eath space or tab
                  ((equal? inch CR-STRING)                    (list  'leaving-comment semi-no NOT-RELEVANT 
                                                                (string-append (as-string semi-no) SPACE-STRING QUOTE-STRING)
                                                                "")) 
                  ((equal? inch RETURN-STRING)                (list  'semicolon semi-no NOT-RELEVANT EMPTY-STRING ""))   ; ignoring control M
                  ((equal? inch BACKSLASH-STRING)             (list  'within-comment-backslash semi-no NOT-RELEVANT 
                                                                                  (string-append 
                                                                                   (as-string semi-no) SPACE-STRING QUOTE-STRING) ""))
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment semi-no NOT-RELEVANT 
                                                                                 (string-append (as-string semi-no) SPACE-STRING QUOTE-STRING 
                                                                                                ESCAPED-QUOTE-STRING) ""))  ; more here
                  (else                                       (list  'within-comment semi-no NOT-RELEVANT 
                                                                      (string-append 
                                                                       (as-string semi-no) SPACE-STRING QUOTE-STRING inch) ""))))

         ((and (symbol? in-state) (eq? in-state 'within-comment))
            (cond ((eof-port? inport)                         (list  'left-comment NOT-RELEVANT  NOT-RELEVANT 
                                                                      (string-append  remember-string inch COMMENT-FORM-END CR-STRING) ""))
                  ((equal? inch CR-STRING)                    (list  'leaving-comment semi-no NOT-RELEVANT remember-string inch))
                  ((equal? inch RETURN-STRING)                (list  'leaving-comment semi-no NOT-RELEVANT remember-string ""))   ; ignoring control M
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment semi-no NOT-RELEVANT
                                                                      (string-append remember-string ESCAPED-QUOTE-STRING) ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'within-comment-backslash semi-no NOT-RELEVANT remember-string ""))
                  (else                                       (list  'within-comment semi-no NOT-RELEVANT
                                                                      (string-append remember-string inch) ""))))

         ((and (symbol? in-state) (eq? in-state 'within-comment-backslash))
            (cond ((eof-port? inport)                         (list  'left-comment NOT-RELEVANT  NOT-RELEVANT 
                                                                              (string-append  inch COMMENT-FORM-END CR-STRING) ""))
                  ((equal? inch CR-STRING)                    (list  'leaving-comment semi-no NOT-RELEVANT DOUBLE-BACKSLASH inch))
                  ((equal? inch RETURN-STRING)                (list  'leaving-comment semi-no NOT-RELEVANT EMPTY-STRING ""))   ; ignoring control M
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment semi-no NOT-RELEVANT ESCAPED-QUOTE-STRING ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'within-comment semi-no NOT-RELEVANT DOUBLE-BACKSLASH ""))
                  (else                                       (list  'within-comment semi-no NOT-RELEVANT
                                                                      (string-append DOUBLE-BACKSLASH inch) ""))))

         ((and (symbol? in-state) (eq? in-state 'leaving-comment))
            (cond ((eof-port? inport)                         (list  'left-comment NOT-RELEVANT  NOT-RELEVANT 
                                                                              (string-append  inch COMMENT-FORM-END CR-STRING) ""))
                  ((equal? inch CR-STRING)                    (list  'left-comment 0 NOT-RELEVANT 
                                                                      (string-append COMMENT-FORM-END remember-string inch) ""))
                  ((equal? inch RETURN-STRING)                (list  'leaving-comment semi-no NOT-RELEVANT
                                                                     EMPTY-STRING ""))   ; ignoring control M
                  ((blank-space? inch)                        (list  'leaving-comment semi-no NOT-RELEVANT "" 
                                                                                  (string-append remember-string inch)))
                  ((equal? inch ";")                          (list  'entering-comment semi-no 1 CR-STRING ""))
                                                               ; before Aug 18, 2004: (list 'entering-comment semi-no 1 remember-string "")
                  ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string 0 0 
                                                                     (string-append COMMENT-FORM-END remember-string inch) "")) ; 30.12.99
                  (else                                       (list  'left-comment 0 0 
                                                                                 (string-append COMMENT-FORM-END remember-string inch) "")
         )))

         ((and (symbol? in-state) (eq? in-state 'entering-comment))
            (cond ((equal? inch ";")                          (list  'entering-comment semi-no (+ 1 semi-no-new) EMPTY-STRING ""))
                  ((equal? inch CR-STRING)                    (list  'leaving-comment semi-no NOT-RELEVANT CR-STRING ""))
                  ((blank-or-tab? inch)                       (list  'within-comment semi-no-new NOT-RELEVANT
                                                                             (if (= semi-no semi-no-new) 
                                                                                 EMPTY-STRING
                                                                                 (string-append 
                                                                                   COMMENT-FORM-END inch COMMENT-FORM-START
                                                                                    (string-append 
                                                                                     (as-string semi-no-new) SPACE-STRING QUOTE-STRING)))
                                                                              ""
                                                                           ))
                  ((equal? inch QUOTE-STRING)                  (list  'within-comment semi-no-new NOT-RELEVANT
                                                                             (if (= semi-no semi-no-new) 
                                                                                 EMPTY-STRING
                                                                                 (string-append 
                                                                                  COMMENT-FORM-END COMMENT-FORM-START
                                                                                   (string-append 
                                                                                     (as-string semi-no-new) SPACE-STRING QUOTE-STRING)))
                                                                              ESCAPED-QUOTE-STRING
                                                                           ))
                  (else                                       (list  'within-comment semi-no-new NOT-RELEVANT
                                                                             (if (= semi-no semi-no-new) 
                                                                                 EMPTY-STRING
                                                                                 (string-append 
                                                                                  COMMENT-FORM-END COMMENT-FORM-START
                                                                                   (string-append 
                                                                                     (as-string semi-no-new) SPACE-STRING QUOTE-STRING)))
                                                                              inch ; was ""
                                                                           ))))

         ((and (symbol? in-state) (eq? in-state 'left-comment))
            (cond ((equal? inch ";")                          (list  'semicolon 1 NOT-RELEVANT COMMENT-FORM-START ""))
                  ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string 0 NOT-RELEVANT inch "")); 30.12.99
                  (else                                       (list  'outside-comment 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-special-char))
            (cond ((equal? inch BACKSLASH-STRING)             (list  'outside-comment-within-special-char-1 0 NOT-RELEVANT inch ""))
                  (else                                       (list  'outside-comment 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-special-char-1))  ; gate and backslash read: now reading last char
                                                               (list  'outside-comment 0 NOT-RELEVANT inch ""))



         (else                                               (error (string-append 
                                                                       "multi-lexical-to-syntactical-comments-transition error " 
                                                                                                (as-string in-state))))

  )))

; ---------------------------------------------------------------------------------------------------------------
; Documentation mark comment style.

(define (mark-lexical-to-syntactical-comments-given-ports inport outport)
  (set! state-list '())
  (mark-lexical-to-syntactical-comments-given-ports-1 start-state 0 0 0 "" inport outport))

(define (mark-lexical-to-syntactical-comments-given-ports-1 current-state docchar-no semi-no new-semi-no remember-string inport outport)
  (if (not (eof-object? (peek-char inport)))
      (let* ((inch (as-string (read-char-drop-return inport)))
             (trans-res (mark-lexical-to-syntactical-comments-transition current-state docchar-no semi-no new-semi-no inch
                         remember-string inport))
            )
       (if (not (= 6 (length trans-res))) (laml-error "State machine problems:" (length trans-res) ":" trans-res))
       (let* ((next-state (first trans-res))
              (next-docchar-no (second trans-res))
              (next-semi-no (third trans-res))
              (next-new-semi-no (fourth trans-res))
              (toput (as-string (fifth trans-res)))
              (next-remember-string (sixth trans-res))
             )
       (if debugging-lexical-to-syntactical-comments
            (set! state-list (cons trans-res state-list)))
       (write-string-to-port-scheme-doc toput outport)  
       ; (put-in-sink-text-string toput)
       (mark-lexical-to-syntactical-comments-given-ports-1 next-state next-docchar-no next-semi-no next-new-semi-no
                                                      next-remember-string inport outport)
  ))))

; STATE DESCRIPTIONS
; We maintain the number of documentation comment characters met, docchar-no.
; In addition, we semi-no, which is the number of semicolons
; and also semi-no-new, which is the number of semicolons on the next comment line.
; semi-no and semi-no-new are not used for the Documentation mark comment style, however. 


; outside-comment                        Within a portion of a file which is not a comment (and not within a string)
; outside-comment-within-string          As outside-comment, but within a string (in which ; characters do not signify comments)
; outside-comment-within-special-char    State for reading special characters, such as #\;
; outside-comment-within-special-char-1: Ditto
; semicolon                              A semicolon has just been read
; semicolon-spaces                       We are reading spaces after a semicolon. Allows, for instance:  ;;    !!
; docchar                                We have seen one or more dochars just after semicolons. Counting them in do docchar-no.
; within-comment                         Inside a comment.
; within-comment-backslash               Inside a comment where an escaping backslash is encountered.
; leaving-comment                        Inside a comment, but CR seen. Stay as long as we see white space.
;                                        The comment may be continued on next line after blank space.
; entering-comment                       We come from one comment line and is now entering another. 
; left-comment                           We just left the comment block.


(define (mark-lexical-to-syntactical-comments-transition in-state docchar-no semi-no semi-no-new inch remember-string inport)
 (let ()
   (cond 
         ((and (symbol? in-state) (eq? in-state 'outside-comment))
            (cond ((equal? inch ";")                          (list  'semicolon 0 1 NOT-RELEVANT COMMENT-FORM-START ""))
                  ((equal? inch GATE-STRING)                  (list  'outside-comment-within-special-char 0 0 NOT-RELEVANT inch ""))
                  ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string 0 0 NOT-RELEVANT inch ""))
                  (else                                       (list  'outside-comment 0 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-string))
            (cond ((equal? inch QUOTE-STRING)                 (list  'outside-comment 0 0 NOT-RELEVANT inch ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'outside-comment-within-string-backslash 0 0 NOT-RELEVANT inch "")) ; new line
                  (else                                       (list  'outside-comment-within-string 0 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-string-backslash))   ; New state
            (cond ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string 0 0 NOT-RELEVANT inch ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'outside-comment-within-string 0 0 NOT-RELEVANT inch ""))
                  (else                                       (list  'outside-comment-within-string 0 0 NOT-RELEVANT inch ""))  ; actually problematic in relation to R5RS - but SchemeDoc does not care...
         ))

         ((and (symbol? in-state) (eq? in-state 'semicolon))
            (cond ((equal? inch ";")                          (list  'semicolon docchar-no (+ 1 semi-no) NOT-RELEVANT EMPTY-STRING ""))
                  ((equal? inch DOC-CHAR-STRING)              (list  'docchar (+ 1 docchar-no) (+ 1 semi-no)
                                                                      NOT-RELEVANT EMPTY-STRING "")) 
                  ((blank-or-tab? inch)                       (list  'semicolon-spaces docchar-no semi-no NOT-RELEVANT 
                                                                     "" ""))
                  ((blank-or-tab? inch)                       (list  'within-comment docchar-no semi-no NOT-RELEVANT 
                                                                     (string-append 
                                                                       (as-string (+ 1 docchar-no)) SPACE-STRING
                                                                       QUOTE-STRING) "")) ; eat blank or tab
                  ((equal? inch CR-STRING)                    (list  'leaving-comment docchar-no semi-no NOT-RELEVANT 
                                                                (string-append (as-string (+ 1 docchar-no)) SPACE-STRING QUOTE-STRING) 
                                                                ""))      
                  ((equal? inch RETURN-STRING)                (list  'semicolon docchar-no semi-no NOT-RELEVANT EMPTY-STRING ""))  
                                                                                                                  ; ignoring control M
                  ((equal? inch BACKSLASH-STRING)             (list  'within-comment-backslash docchar-no  semi-no NOT-RELEVANT 
                                                                      (string-append 
                                                                        (as-string (+ 1 docchar-no)) SPACE-STRING QUOTE-STRING) ""))
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment docchar-no  semi-no NOT-RELEVANT 
                                                                      (string-append (as-string (+ 1 docchar-no)) SPACE-STRING QUOTE-STRING 
                                                                                                ESCAPED-QUOTE-STRING)
                                                                      ""))
                  (else                                       (list  'within-comment docchar-no semi-no NOT-RELEVANT 
                                                                      (string-append 
                                                                       (as-string (+ 1 docchar-no)) SPACE-STRING QUOTE-STRING inch) ""))))

          ((and (symbol? in-state) (eq? in-state 'semicolon-spaces))
            (cond ((blank-or-tab? inch)                       (list  'semicolon-spaces docchar-no semi-no 
                                                                      NOT-RELEVANT  "" 
                                                                      (string-append remember-string inch))) 
                  ((equal? inch DOC-CHAR-STRING)              (list  'docchar (+ 1 docchar-no) (+ 1 semi-no)
                                                                      NOT-RELEVANT EMPTY-STRING ""))
                  ((equal? inch CR-STRING)                    (list  'leaving-comment docchar-no semi-no NOT-RELEVANT 
                                                                (string-append (as-string (+ 1 docchar-no)) 
                                                                               SPACE-STRING QUOTE-STRING remember-string ) 
                                                                ""))      
                  ((equal? inch RETURN-STRING)                (list  'semicolon docchar-no semi-no NOT-RELEVANT EMPTY-STRING ""))  
                                                                                                                  ; ignoring control M
                  ((equal? inch BACKSLASH-STRING)             (list  'within-comment-backslash docchar-no  semi-no NOT-RELEVANT 
                                                                      (string-append 
                                                                        (as-string (+ 1 docchar-no)) SPACE-STRING 
                                                                                   QUOTE-STRING remember-string ) ""))
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment docchar-no  semi-no NOT-RELEVANT 
                                                                      (string-append (as-string (+ 1 docchar-no)) SPACE-STRING
                                                                                                QUOTE-STRING remember-string 
                                                                                                ESCAPED-QUOTE-STRING)
                                                                      ""))
                  (else                                       (list  'within-comment docchar-no semi-no NOT-RELEVANT 
                                                                      (string-append 
                                                                       (as-string (+ 1 docchar-no)) SPACE-STRING 
                                                                                  QUOTE-STRING remember-string inch) ""))
         ))
                  

         ((and (symbol? in-state) (eq? in-state 'docchar))   ; we have seen the distinguishing doc char, normally '!'
            (cond ((equal? inch DOC-CHAR-STRING)              (list  'docchar (+ 1 docchar-no) semi-no NOT-RELEVANT
                                                                     EMPTY-STRING ""))
                  ((blank-or-tab? inch)                       (list  'within-comment docchar-no semi-no NOT-RELEVANT
                                                                      (string-append 
                                                                        (as-string
                                                                          (+ 1 docchar-no)) SPACE-STRING QUOTE-STRING) ""))  ; eats space
                  ((equal? inch CR-STRING)                    (list  'leaving-comment docchar-no semi-no NOT-RELEVANT
                                                                      (string-append 
                                                                        (as-string
                                                                          (+ 1 docchar-no)) SPACE-STRING QUOTE-STRING inch) ""))
                  (else                                       (list  'within-comment docchar-no semi-no NOT-RELEVANT 
                                                                      (string-append 
                                                                       (as-string (+ 1 docchar-no)) SPACE-STRING QUOTE-STRING inch) ""))))


         ((and (symbol? in-state) (eq? in-state 'within-comment))
            (cond ((eof-port? inport)                         (list  'left-comment docchar-no NOT-RELEVANT  NOT-RELEVANT 
                                                                      (string-append  remember-string inch COMMENT-FORM-END CR-STRING) ""))
                  ((equal? inch CR-STRING)                    (list  'leaving-comment docchar-no semi-no NOT-RELEVANT
                                                                      remember-string inch))
                  ((equal? inch RETURN-STRING)                (list  'leaving-comment docchar-no semi-no NOT-RELEVANT
                                                                      remember-string ""))   ; ignoring control M
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment docchar-no  semi-no NOT-RELEVANT
                                                                      (string-append remember-string ESCAPED-QUOTE-STRING) ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'within-comment-backslash docchar-no semi-no NOT-RELEVANT
                                                                      remember-string ""))
                  (else                                       (list  'within-comment docchar-no semi-no NOT-RELEVANT
                                                                      (string-append remember-string inch) ""))
         ))

         ((and (symbol? in-state) (eq? in-state 'within-comment-backslash))
            (cond ((eof-port? inport)                         (list  'left-comment docchar-no NOT-RELEVANT  NOT-RELEVANT 
                                                                      (string-append  inch COMMENT-FORM-END CR-STRING) ""))
                  ((equal? inch CR-STRING)                    (list  'leaving-comment docchar-no semi-no NOT-RELEVANT
                                                                      DOUBLE-BACKSLASH inch))
                  ((equal? inch RETURN-STRING)                (list  'leaving-comment docchar-no  semi-no NOT-RELEVANT
                                                                      EMPTY-STRING ""))   ; ignoring control M
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment docchar-no  semi-no NOT-RELEVANT
                                                                      ESCAPED-QUOTE-STRING ""))
                  ((equal? inch BACKSLASH-STRING)             (list  'within-comment docchar-no  semi-no NOT-RELEVANT
                                                                      DOUBLE-BACKSLASH ""))
                  (else                                       (list  'within-comment docchar-no  semi-no NOT-RELEVANT
                                                                      (string-append DOUBLE-BACKSLASH inch) ""))
         ))

         ((and (symbol? in-state) (eq? in-state 'leaving-comment))
            (cond ((eof-port? inport)                         (list  'left-comment docchar-no NOT-RELEVANT  NOT-RELEVANT 
                                                                      (string-append  inch COMMENT-FORM-END CR-STRING) ""))
                  ((equal? inch CR-STRING)                    (list  'left-comment 0 0 NOT-RELEVANT 
                                                                      (string-append COMMENT-FORM-END inch) ""))
                  ((equal? inch RETURN-STRING)                (list  'leaving-comment docchar-no  semi-no NOT-RELEVANT
                                                                      EMPTY-STRING ""))  ; ignoring control M
                  ((blank-space? inch)                        (list  'leaving-comment docchar-no semi-no NOT-RELEVANT "" 
                                                                     (string-append remember-string inch)))
                  ((equal? inch ";")                          (list  'entering-comment docchar-no  semi-no 1 CR-STRING ""))
                  ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string docchar-no  0 0 
                                                                     (string-append COMMENT-FORM-END remember-string inch) ""))
                  (else                                       (list  'left-comment docchar-no 0 0 
                                                                     (string-append COMMENT-FORM-END remember-string inch) ""))
         ))

         ((and (symbol? in-state) (eq? in-state 'entering-comment))
            (cond ((equal? inch ";")                          (list  'entering-comment docchar-no semi-no (+ 1 semi-no-new)
                                                                      EMPTY-STRING ""))
                  ((equal? inch CR-STRING)                    (list  'leaving-comment docchar-no semi-no NOT-RELEVANT CR-STRING ""))
                  ((blank-or-tab? inch)                       (list  'within-comment docchar-no semi-no-new NOT-RELEVANT
                                                                             (if (= semi-no semi-no-new) 
                                                                                 EMPTY-STRING
                                                                                 EMPTY-STRING)
                                                                              ""
                                                                           ))
                  ((equal? inch QUOTE-STRING)                 (list  'within-comment docchar-no  semi-no-new NOT-RELEVANT
                                                                             (if (= semi-no semi-no-new) 
                                                                                 EMPTY-STRING
                                                                                 EMPTY-STRING)
                                                                              ESCAPED-QUOTE-STRING 
                                                                           ))
                  (else                                       (list  'within-comment docchar-no  semi-no-new NOT-RELEVANT
                                                                             (if (= semi-no semi-no-new) 
                                                                                 EMPTY-STRING
                                                                                 EMPTY-STRING)
                                                                              inch ; was ""
                                                                           ))
         ))

         ((and (symbol? in-state) (eq? in-state 'left-comment))
            (cond ((equal? inch ";")                          (list  'semicolon 0 1 NOT-RELEVANT COMMENT-FORM-START ""))
                  ((equal? inch QUOTE-STRING)                 (list  'outside-comment-within-string 0 0 NOT-RELEVANT inch "")); 30.12.99
                  (else                                       (list  'outside-comment 0 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-special-char))
            (cond ((equal? inch BACKSLASH-STRING)             (list  'outside-comment-within-special-char-1 0 0 NOT-RELEVANT inch ""))
                  (else                                       (list  'outside-comment 0 0 NOT-RELEVANT inch ""))))

         ((and (symbol? in-state) (eq? in-state 'outside-comment-within-special-char-1))  ; gate and backslash read: now reading last char
                                                               (list  'outside-comment 0 0 NOT-RELEVANT inch ""))


         (else                                               (error (string-append 
                                                                      "mark-lexical-to-syntactical-comments-transition error " 
                                                                                                (as-string in-state))))

  )))

; ---------------------------------------------------------------------------------------------------------------


(define (blank-space? ch-string)
  (or (equal? ch-string CR-STRING) (equal? ch-string TAB-STRING) (equal? ch-string SPACE-STRING)))

(define (blank-or-tab? ch-string)
  (or (equal? ch-string TAB-STRING) (equal? ch-string SPACE-STRING)))
    

; ---------------------------------------------------------------------------------------------------
; Special characters, represented as strings:

(define SPACE-STRING " ")
(define TAB-STRING (as-string #\tab))
(define QUOTE-STRING "\"")
(define ESCAPED-QUOTE-STRING (string-append (as-string #\\) "\""))
(define EMPTY-STRING "")
(define DOC-CHAR-STRING (as-string documentation-comment-character))
; (define CR-STRING "
; ")
(define CR-STRING (as-string (integer->char 10)))
(define RETURN-STRING (as-string (integer->char 13)))
(define GATE-STRING "#")
(define BACKSLASH-STRING (as-string #\\))
(define DOUBLE-BACKSLASH (string-append (as-string #\\) (as-string #\\)))

(define SYNTACTICAL-COMMENT-SYMBOL 'comment!!!)    ; A symbol which is unlikely to be used for other purposes
(define COMMENT-FORM-START (string-append "(" (as-string SYNTACTICAL-COMMENT-SYMBOL) " "))
(define COMMENT-FORM-END (string-append QUOTE-STRING ")"))

; ---------------------------------------------------------------------------------------------------
; PART TWO:
; THE SYNTACTIC ANALYSIS OF THE TEMPORARY FILE WITH COMMENT FORMS:
; Main function: (extract-relevant-documentation file-name)

; State variable for the extraction of documentation:

;;; The second internal function. 
;;; The function in this section extracts relevant level n, n > 2, comments
;;; for documentation purposes
;;; .section-id second-internal


(define previous-level-2-comment "") ; always a string

; ---------------------------------------------------------------------------------------------------

; Return the contribution to the manual in terms of a list of manual entries (a list of list structure).
; return #f in case there is no contribution.
; This function reads exactly one form.
; In case a level 2 comment is read (at top level) it is stored in the global variabel previous-level-2-comment. In this case, this function returns #f.
(define (extract-next-documentation port)
  (if (not (eof-object? (peek-char port)))
      (let ((form-1 (read port)))
        (cond 
          ((comment-level? 4 form-1) 
                     (append-manual-abstract (contents-of-comment form-1)) 
                     (set! previous-level-2-comment "")
                     #f)

          ((comment-level? 3 form-1) 
                     (set! previous-level-2-comment "")
                     (list (make-manual-section-entry (contents-of-comment form-1))))
          
          ((comment-level? 2 form-1) 
                     (set! previous-level-2-comment (contents-of-comment form-1))
                     #f)

          ((define-form-scheme-doc? form-1) 
                     (let* ((signature (signature-of-define-form form-1))
                            (item3 (caddr form-1))  ; possible internal comment
                            (internal-level2-comment (if (comment-level? 2 item3) (contents-of-comment item3) #f))
                            (is-there-documentation? (or internal-level2-comment (> (string-length previous-level-2-comment) 0)))
                            (extraction 
                                 (if is-there-documentation?
                                     (make-manual-page-entry signature previous-level-2-comment internal-level2-comment)
                                     #f))
                            (sub-extractions (if is-there-documentation? (extract-sub-documentation form-1 2) '()))
                           )
                       (set! previous-level-2-comment "")
                       (if is-there-documentation?
                           (append (reverse sub-extractions) (list extraction))
                           #f)
                     ))

          (else       (set! previous-level-2-comment "")
                      #f)))
      #f))



(define (append-manual-abstract abstract)
 (let* ((parsed-abstract (extract-internal-tagging abstract))
        (the-abstract    (defaulted-get 'description parsed-abstract "-"))
        (the-title       (defaulted-get 'title parsed-abstract #f))
        (the-author      (defaulted-get 'author parsed-abstract #f))
        (the-affiliation (defaulted-get 'affiliation parsed-abstract #f))

        (laml-resource-from-abstract (defaulted-get 'laml-resource parsed-abstract (list #f)))
        (css-prestylesheet-from-abstract (defaulted-get 'css-prestylesheet parsed-abstract (list "compact")))
        (css-stylesheet-from-abstract (defaulted-get 'css-stylesheet parsed-abstract (list "original")))
        (css-stylesheet-copying-from-abstract (defaulted-get 'css-stylesheet-copying parsed-abstract (list "true")))
        (keep-syntactical-comment-file-from-abstract (defaulted-get 'keep-syntactical-comment-file parsed-abstract (list "false")))
        (source-destination-delta-from-abstract (defaulted-get 'source-destination-delta parsed-abstract (list "")))
        (scheme-source-linking-from-abstract (defaulted-get 'scheme-source-linking parsed-abstract (list "false")))
       )
  (set! extracted-manual-abstract (string-append extracted-manual-abstract " " (unescape-text (car the-abstract) escape-character)))
  (if the-title (set! extracted-manual-title (car the-title)))
  (if the-author (set! extracted-manual-author (car the-author)))
  (if the-affiliation (set! extracted-manual-affiliation (car the-affiliation)))

  (set! extracted-laml-resource (car laml-resource-from-abstract))
  (set! extracted-css-prestylesheet (car css-prestylesheet-from-abstract))
  (set! extracted-css-stylesheet (car css-stylesheet-from-abstract) )
  (set! extracted-css-stylesheet-copying (car css-stylesheet-copying-from-abstract))
  (set! extracted-source-destination-delta (car source-destination-delta-from-abstract) )
  (set! extracted-scheme-source-linking (car scheme-source-linking-from-abstract))
 )
)



; return whether form is a comment form at level n:
(define (comment-level? n form)
  (and (list? form) (> (length form) 1) (eq? (car form) SYNTACTICAL-COMMENT-SYMBOL) (number? (cadr form)) (= (cadr form) n)))

; return whether form is a define form:
(define (define-form-scheme-doc? form)
  (and (list? form) (> (length form) 1) (or (eq? (car form) 'define)  (eq? (car form) 'define-syntax) (eq? (car form) 'define-doc))))

; Return the signature (a list) of define-form.
; Define form is supposed to be a proper list which satisfies the predicate define-form-scheme-doc?
(define (signature-of-define-form define-form)
  (cond ((and (eq? (first define-form) 'define) (pair? (second define-form))) (cadr define-form))
        ((and (eq? (first define-form) 'define) (symbol? (second define-form)))
           (if (lambda-form? (third define-form))
               (cons (second define-form) (second (third define-form)))
               (second define-form)))
        ((eq? (first define-form) 'define-syntax)
           (second define-form))
        ((eq? (first define-form) 'define-doc)
           (second define-form))
        (else (laml-error "signature-of-define-form: Unknown composition of define-form:" define-form))))

(define (lambda-form? x)
  (and (pair? x) (eq? (car x) 'lambda)))


;; Extract a manual style list of documentation from file-name and return it.
;; It is assumed that file-name contains syntactical (comment ...) comments, as produced by
;; the procedure lexical-to-syntactical-comments!
;; .internal-references "relevant procedure" "lexical-to-syntactical-comments!" 
(define (extract-relevant-documentation file-name)
 (let* ((ip (open-input-file file-name))
        (res (extract-relevant-documentation-from-port ip)))
     (close-input-port ip)
     res))

(define (extract-relevant-documentation-from-port port)
  (extract-relevant-documentation-from-port-iter port '()))

; Returns the accumulated and manual formated doc-list when the temporary source file has been read
(define (extract-relevant-documentation-from-port-iter port doc-list)
  (if (not (eof-object? (peek-char port)))
      (let* ((extract (extract-next-documentation port))  ; Each top-level form/comment-form is visited here
            )
	(if extract
	    (extract-relevant-documentation-from-port-iter port (append extract doc-list))
	    (extract-relevant-documentation-from-port-iter port doc-list))
      )
        
      doc-list))

(define (make-manual-page-entry signature pre-comment inside-comment)
 (let ((unescape-description
         (lambda (x)
           (let ((kind (car x))
                 (info (cadr x)))
             (if (eq? kind 'description)
                 (list kind (unescape-text info escape-character))
                 x)))))
  ; pre-condition: signature and at least one of the comments are defined
  (let* ((kind "manual-page")
         (title (if (pair? signature) (as-string (car signature)) (as-string signature)))
         (form signature)
         (description (string-append
                        (if pre-comment  pre-comment "") CR-STRING
                        (if inside-comment  inside-comment "")))
         (parsed-description (extract-internal-tagging description))
         (unescaped-parsed-description (map unescape-description parsed-description))  ; unescape only the description element
        )

   (remove-duplicates-by-predicate
    (append
      (list (list 'kind kind))
      unescaped-parsed-description
      (list 
        (list 'title title)
        (list 'form form)
      )
    )
    (lambda (x y)
      (and (eq? (car x) (car y))
           (not (memq (car x) allowed-duplicates-elements))))
   )
   

   ; in manual.scm: remove unknown tags.
   ; bundle parameter in parameters. Similar with references. 
   ; additional parsing of parameter etc.
  )
 )
)


(define (make-manual-section-entry comment)
  (let* ((kind "manual-section")
         (parsed-comment  (extract-internal-tagging comment))
         (the-description-list (defaulted-get 'description parsed-comment "-"))
         (the-description (car the-description-list))
         (unescaped-description (unescape-text the-description escape-character))
         (section-title   (first-sentence-in-string unescaped-description))
         (section-body    (but-first-sentence-of-string unescaped-description))
         (section-id      (defaulted-get 'section-id parsed-comment #f))
        )
   (append
    (list 
     (list 'kind kind)
     (list 'section-title section-title)
     (list 'section-body section-body))
    (if section-id
        (list 
          (list 'section-id (car section-id)))
        '()))
  )
)


(define (contents-of-comment c)
  (if (= 3 (length c))
      (caddr c)
      "??"))

; -----------------------------------------------------------------------------------------------
; Extraction of nested manual contributions. Main function for these purposes: extract-sub-documentation.

; Does cell point to a syntactical section comment?
(define (start-of-section-comment? cell)
 (letrec ((is-syntactical-section-comment? 
             (lambda (x) (and (pair? x) (eq? (car x) SYNTACTICAL-COMMENT-SYMBOL)
                              (pair? (cdr x)) (= (cadr x) 3)))))
  (and (pair? (car cell)) (is-syntactical-section-comment? (car cell)))))

; Does cell point to a syntactical definition comment?
(define (start-of-definition-comment? cell)
 (letrec ((is-syntactical-definition-comment? 
             (lambda (x) (and (pair? x) (eq? (car x) SYNTACTICAL-COMMENT-SYMBOL)
                              (pair? (cdr x)) (= (cadr x) 2)))))
  (and (pair? (car cell)) (is-syntactical-definition-comment? (car cell)))))

; Return the list of all cons cells reachable from cell which satisfy either pred1 or pred2.
; If a cell c is accepted by pred1/pred2 the cells of (next1/next2 cell) are also examined for matches.
; The function next1/next2 is a predicate that checks if next1/next2 can meaningfully be applied.
; next1/2 could be cdr, with next1/next2? as pair?
(define (dual-traverse-cons-cells-with-next cell pred1 next1 next1? pred2 next2 next2?)
  (cond ((not (pair? cell)) '())
        ((and (next1? cell) (pred1 cell)) (cons cell (dual-traverse-cons-cells-with-next (next1 cell) 
                                                                                         pred1 next1 next1? pred2 next2 next2?)))
        ((and (next2? cell) (pred2 cell)) (cons cell (dual-traverse-cons-cells-with-next (next2 cell) 
                                                                                         pred1 next1 next1? pred2 next2 next2?)))
        ((pred1 cell) (list cell))
        ((pred2 cell) (list cell))
        ((and (pair? (car cell)) (pair? (cdr cell)))
            (append (dual-traverse-cons-cells-with-next (car cell) pred1 next1 next1? pred2 next2 next2?)
                    (dual-traverse-cons-cells-with-next (cdr cell) pred1 next1 next1? pred2 next2 next2?)))
        ((pair? (car cell))
            (dual-traverse-cons-cells-with-next (car cell) pred1 next1 next1? pred2 next2 next2?))
        ((pair? (cdr cell))
            (dual-traverse-cons-cells-with-next (cdr cell) pred1 next1 next1? pred2 next2 next2?))
        (else '())))

; Extract documentation from form. Return a list of manual-page and manual-section entries.
(define (extract-sub-documentation form level)
  (let* ((documentation-sequences 
            (dual-traverse-cons-cells-with-next 
                form 
                start-of-section-comment? cdr pair?
                start-of-definition-comment? cddr (lambda (x) (and (pair? x) (and (pair? (cdr x)))))) )
         (documentation-sequences-1                                     ; Cut sequences to length 2
             (map (lambda (ds) 
                   (let ((ds-lgt (length ds)))
                    (cond ((>= ds-lgt 2) (list (first ds) (second ds)))
                          ((= ds-lgt 1) (list (first ds) #f))           ; (first ds) is trailing comment
                          (else (list #f #f)))))                        ; should not happen
                  documentation-sequences))
         (documentation-sequences-2                               ; only sequences of relevant comment and define form 
             (filter 
               (lambda (s)
                  (or (comment-level? 3 (first s))
                      (and (comment-level? 2 (first s)) (define-form-scheme-doc? (second s)))))
               documentation-sequences-1))
         (result (flatten 
                  (map (lambda (doc-seq-2)
                        (if (comment-level? 2 (first doc-seq-2)) 
                            (let ((define-form (second doc-seq-2)))
                              (cons (make-sub-entry doc-seq-2 level) (extract-sub-documentation define-form (+ level 1)))) 
                            (list (make-sub-entry doc-seq-2 level))))
                  documentation-sequences-2)))
        )
     (filter (lambda (x) (not (eq? x #f))) result)))


; Make a manual page or section augumented with a level field. If not possible, return #f
(define (make-sub-entry sub-entry level)
 (let ((com-form (first sub-entry))
       (define-form (second sub-entry))
       (level-item (list 'level level)))
   (cond ((comment-level? 3 com-form)
            (cons level-item (make-manual-section-entry (contents-of-comment com-form))))
         ((comment-level? 2 com-form)
            (let* ((previous-level-2-comment (contents-of-comment com-form))
                   (signature (signature-of-define-form define-form))
                   (item3 (caddr define-form))
                   (internal-level2-comment (if (comment-level? 2 item3) (contents-of-comment item3) #f)))
              (if (or internal-level2-comment (> (string-length previous-level-2-comment) 0))
                  (cons level-item (make-manual-page-entry signature previous-level-2-comment internal-level2-comment))
                  #f)))
         (else #f))))


; ---------------------------------------------------------------------------------------------------------------
;;; Internal tagging support in schemedoc comments.
;;; .section-id internal-tagging

; This version eliminated August 11, 2004. See generalized version below.
;(define (extract-internal-tagging comment-string)
; (let* ((lgt (string-length comment-string))
;        (res-pos (first-internal-tag-position comment-string lgt 0))
;       )
;  (group-internal-tags
;   (if res-pos
;       (cons
;         (list 'description (substring comment-string 0 (- res-pos 1)))
;         (extract-internal-tagging-1 comment-string lgt (+ 1 res-pos)))
;       (list (list 'description comment-string))))))

;; Parse the comment string, in order to extract internal tagging.
;; If no internal tagging is found return ((description "comment-string")).
;; If at least one internal tag is found return an a-list of the form
;; ((tag-1-symbol va1-1-string) (tag-2-symbol va1-2-string) ...).
;; The string parts in lines, which are not initiated with the designated internal tagging char, are returned as (description "...").
(define (extract-internal-tagging comment-string)
 (let* ((divided-comment-string (split-comment-string comment-string))
        (description-part (car divided-comment-string))
        (internal-tagging-part (cdr divided-comment-string))
        (first-dot-position (find-in-string internal-tagging-part #\.))
       )
  (group-internal-tags
   (if (and (not (empty-string? internal-tagging-part)) first-dot-position)
       (cons
         (list 'description description-part)
         (extract-internal-tagging-1 internal-tagging-part (string-length internal-tagging-part) (+ 1 first-dot-position)))
       (list (list 'description description-part))))))


; Split commment-string in two parts: the descript part and the internal tagging part.
; The internal tagging part is the concatenation of lines that are initiated with the distinguised internal-tatting-char.
; The description part is the concatenation of the remaining lines.
; Returns a cons pair of the description string and the internal tagging string.
; (define (split-comment-string comment-string)
;  (let ((tagging-line?   ; is line dot initiated?
;         (lambda (line) 
;           (if (empty-string? line)
;               #f
;               (let ((line-lgt (string-length line))
;                     (something-pos (skip-chars-in-string line white-space-char-list 0)))
;                 (if (= something-pos line-lgt)
;                     #f
;                     (eqv? (string-ref line something-pos) internal-tagging-char)))))))
;    (let* ((comment-string-1 (filter-string (lambda (ch) (eqv? ch #\return)) comment-string))
;           (comment-string-lines (string-to-list comment-string-1 (list #\newline)))
;           (tagging-lines (filter tagging-line? comment-string-lines))
;           (description-lines (filter (negate tagging-line?) comment-string-lines)))
;      (cons 
;       (list-to-string description-lines (as-string #\newline))
;       (list-to-string tagging-lines (as-string #\newline))))))

; Split commment-string in two parts: the descript part and the internal tagging part.
; The internal tagging part is the concatenation of lines that are initiated with the distinguished internal-tagging-char
; and ended by another distinguished character end-of-tagging-line. The knowledge of end-of-tagging-line is local to the SchemeDoc extractor.
; (Rationale: By having a distinguished end-of-tagging-line we can have continued lines that maintain the newline characters). 
; The description part is the concatenation of the remaining lines.
; Returns a cons pair of the description string and the internal tagging string.
(define (split-comment-string comment-string)
 (let ((description-string (make-string (string-length comment-string)))
       (tag-string (make-string (string-length comment-string))))
  (split-comment-string-state-machine 'start
                                      comment-string (string-length comment-string) 0  
                                      description-string 0
                                      tag-string 0)))

; State machine that split a comment string into a description part and tagging part.
; str is traversed with index i.
; ds is filled with contributions to the description part. Next char pos is j.
; ts is filled with tag contributions. Next char pos is k.
; state: start, description, tag, ...
(define (split-comment-string-state-machine  state   str str-lgt i   ds j   ts k)
  (if (= i str-lgt)                ; end of traversal - return results
      (cons (substring ds 0 j) (substring ts 0 k))
      (let ((ch (string-ref str i)))
        (cond 
         ((eq? state 'start)            ; neutral start state
          (cond ((memv ch (list #\space #\tab #\newline #\return))  ; ignore initial white space
                 (split-comment-string-state-machine  'start   str str-lgt (+ i 1)   ds j   ts k))
                ((eqv? ch internal-tagging-char)
                 (string-set! ts k ch)
                 (split-comment-string-state-machine  'tag   str str-lgt (+ i 1)   ds j   ts (+ k 1)))
                (else 
                 (string-set! ds j ch)
                 (split-comment-string-state-machine  'description   str str-lgt (+ i 1)   ds (+ j 1)   ts k))
                )
          )

         ((eq? state 'description)      ; within a description
          (cond ((eqv? ch #\newline)
                 (string-set! ds j #\space)
                 (split-comment-string-state-machine  'start   str str-lgt (+ i 1)   ds (+ j 1)   ts k))
                (else 
                 (string-set! ds j ch)
                 (split-comment-string-state-machine  'description  str str-lgt (+ i 1)   ds (+ j 1)   ts k))
                )
          )

         ((eq? state 'tag)              ; within a tag line
          (cond ((eqv? ch line-continuation-char)
                 (split-comment-string-state-machine  'tag-continue  str str-lgt (+ i 1)   ds j   ts k))
                ((eqv? ch #\newline)    ; end of this tag
                 (string-set! ts k end-of-tagging-line)
                 (split-comment-string-state-machine  'start  str str-lgt (+ i 1)   ds j   ts (+ k 1)))
                (else 
                 (string-set! ts k ch)
                 (split-comment-string-state-machine  'tag  str str-lgt (+ i 1)   ds j   ts (+ k 1)))
                )
          )

         ((eq? state 'tag-continue)    ; just seen a tag continuation char
          (cond  ((memv ch (list #\space #\tab #\return))  ; igore white space
                  (split-comment-string-state-machine  'tag-continue   str str-lgt (+ i 1)   ds j   ts k))
                 ((eqv? ch #\newline)  ; newline after tag continuation char
                  (string-set! ts k #\newline)
                  (split-comment-string-state-machine  'tag  str str-lgt (+ i 1)   ds j   ts (+ k 1)))
                 (else   ; non-trailing tag continue
                  (string-set! ts k line-continuation-char) (string-set! ts (+ k 1) ch)
                  (split-comment-string-state-machine  'tag  str str-lgt (+ i 1)   ds j   ts (+ k 2)))

                 )
          )

         (else (laml-error "split-comment-string-state-machine. Unknown state" state))))))

;; A list of internal tag names (symbol) for which we allow multiple occurences.
;; parameter, reference, and internal-reference tags are treated specially, and are
;; always allowed to be duplicated.
(define allowed-duplicates-elements (list 'example))

; group the elements in itag-a-list into parameters, attributes, cross-references, examples, and the others.
; Return an alist with parameters and cross-references clauses, in which parameter, reference, and internal-references 
; are embedded.
(define (group-internal-tags itag-a-list)
   (group-internal-tags-1 itag-a-list '() '() '() '() '()))

(define (group-internal-tags-1 itag-a-list pars attrs refs examples others)
 (cond ((null? itag-a-list) 
          (append 
             (if (not (null? pars)) (list (cons 'parameters (reverse pars))) '())
             (if (not (null? attrs)) (list (cons 'xml-in-laml-attributes (reverse attrs))) '())
             (if (not (null? examples)) (list (cons 'examples (reverse examples))) '())
             (if (not (null? refs)) (list (cons 'cross-references (reverse refs))) '())
             (reverse others)))
       (else
         (let ((itag (car itag-a-list)))
           (cond ((eq? 'parameter (car itag))
                    (group-internal-tags-1 (cdr itag-a-list) (cons (itag-parse-parameter (cadr itag)) pars) attrs refs examples others))
                 ((eq? 'attribute (car itag))
                    (group-internal-tags-1 (cdr itag-a-list) pars (cons (itag-parse-attr (cadr itag)) attrs) refs examples others))
                 ((eq? 'reference  (car itag))
                    (group-internal-tags-1 (cdr itag-a-list) pars attrs (cons (itag-parse-reference (cadr itag)) refs) examples others))
                 ((eq? 'example  (car itag))
                    (group-internal-tags-1 (cdr itag-a-list) pars attrs refs (cons (itag-parse-example (cadr itag)) examples) others))
                 ((eq? 'internal-references  (car itag))
                    (group-internal-tags-1 (cdr itag-a-list) pars attrs (cons (itag-parse-internal-references (cadr itag)) refs) examples others))
                 (else (group-internal-tags-1 (cdr itag-a-list) pars attrs refs examples (cons itag others)))))
       )))


; Return a parsed parameter clause, in which parameter name and explanation are sorted out.
; Trivial, because there is actually no additional destructuring.
(define (itag-parse-example example-desc-string)
    (list 'example example-desc-string))

; Return a parsed parameter clause, in which parameter name and explanation are sorted out.
(define (itag-parse-parameter par-desc-string)
  (let* ((lgt (string-length par-desc-string))
         (start-pos (skip-chars-in-string par-desc-string white-space-char-list 0))
         (space-after-name (find-in-string par-desc-string #\space start-pos))
         (tab-after-name (find-in-string par-desc-string #\tab start-pos))
         (pos (cond ((and space-after-name tab-after-name) (min space-after-name tab-after-name))
                    (space-after-name space-after-name)
                    (tab-after-name tab-after-name)
                    (else lgt)))
         (start-1-pos (skip-chars-in-string par-desc-string white-space-char-list pos))
        )
    (list 'parameter (substring par-desc-string start-pos pos) (substring par-desc-string start-1-pos lgt))))

; Return a parsed attribute clause, in which parameter name and explanation are sorted out.
(define (itag-parse-attr attr-desc-string)
  (let* ((lgt (string-length attr-desc-string))
         (start-pos (skip-chars-in-string attr-desc-string white-space-char-list 0))
         (space-after-name-1 (find-in-string attr-desc-string #\space start-pos))
         (tab-after-name-1 (find-in-string attr-desc-string #\tab start-pos))
         (pos-1 (cond ((and space-after-name-1 tab-after-name-1) (min space-after-name-1 tab-after-name-1))
                    (space-after-name-1 space-after-name-1)
                    (tab-after-name-1 tab-after-name-1)
                    (else lgt)))
         (start-pos-1 (skip-chars-in-string attr-desc-string white-space-char-list pos-1))
         (space-after-name-2 (find-in-string attr-desc-string #\space start-pos-1))
         (tab-after-name-2 (find-in-string attr-desc-string #\tab start-pos-1))
         (pos-2 (cond ((and space-after-name-2 tab-after-name-2) (min space-after-name-2 tab-after-name-2))
                    (space-after-name-2 space-after-name-2)
                    (tab-after-name-2 tab-after-name-2)
                    (else lgt)))
         (start-1-pos (skip-chars-in-string attr-desc-string white-space-char-list pos-2))
        )
    (let ((attr-name (substring attr-desc-string start-pos pos-1))
          (attr-status (downcase-string (substring attr-desc-string start-pos-1 pos-2)))
          (attr-descr (substring attr-desc-string start-1-pos lgt)))
      (if (not (or (equal? attr-status "implied") (equal? attr-status "required")))
          (display-warning "Attribute" (string-append attr-name ":") "Use either required or implied in front of the attribute description." ))

      (list 'xml-in-laml-attribute attr-name attr-status attr-descr))))

; Return a parsed reference clause in which the three quoted constituents are sorted out.
(define (itag-parse-reference ref-desc-string)
  (let ((pair1 (find-delimited-string-in-string ref-desc-string #\" #\" 0)))
   (if pair1
       (let ((pair2 (find-delimited-string-in-string ref-desc-string #\" #\" (+ 1 (cdr pair1)))))
         (if pair2
             (let ((pair3 (find-delimited-string-in-string ref-desc-string #\" #\" (+ 1 (cdr pair2)))))
               (if pair3
                   (list 'reference 
                         (substring ref-desc-string (+ (car pair1) 1)  (cdr pair1))
                         (substring ref-desc-string (+ (car pair2) 1)  (cdr pair2))
                         (substring ref-desc-string (+ (car pair3) 1)  (cdr pair3)))
                   (list 'reference 
                         (substring ref-desc-string (+ (car pair1) 1)  (cdr pair1))
                         (substring ref-desc-string (+ (car pair2) 1)  (cdr pair2))
                         "???")))
             (list 'reference 
                    (substring ref-desc-string (+ (car pair1) 1) (cdr pair1))
                    "???"
                    "???")))
       (list 'reference "???" "???" "???"))))

; Return a list of at least length 2 in which the three quoted constituents are sorted out.
(define (itag-parse-internal-references int-ref-string)
 (let* ((res (itag-parse-internal-references-1 int-ref-string 0))
        (lgt (length res)))
   (cond ((null? res) (list 'internal-references "???" "???"))
         ((= 1 lgt)   (list 'internal-references (car res) "???"))
         (else        (cons 'internal-references res)))))

(define (itag-parse-internal-references-1 int-ref-string pos)
 (let ((pair (find-delimited-string-in-string int-ref-string #\" #\" pos)))
   (if pair
       (cons (substring int-ref-string (+ (car pair) 1) (cdr pair))
             (itag-parse-internal-references-1 int-ref-string (+ 1 (cdr pair))))
       '())))


; Return the indexes (cons pair) of the first substring of str delimited by
; ch1 and ch2 (in that order). (car result) designates ch1, and (cdr result) designates ch2.
; Return #f if not found. Start the search at start-pos
(define (find-delimited-string-in-string str ch1 ch2 start-pos)
 (let ((ch1-pos (find-in-string str ch1 start-pos)))
  (if ch1-pos
      (let ((ch2-pos (find-in-string str ch2 (+ 1 ch1-pos))))
        (if ch2-pos
            (cons ch1-pos ch2-pos)
            #f))
      #f)))
 
; Given position i in a string with tags, return the next position of an internal tag.
; Returns #f if no such position can be found.
(define (first-internal-tag-position str lgt i)
  (let* ((cr-pos (find-in-string str end-of-tagging-line i)))  ; earlier end-of-tagging-line
   (if cr-pos
       (let ((after-blank-pos (skip-chars-in-string str (cons end-of-tagging-line white-space-char-list) cr-pos)))         
         (cond ((not (number? after-blank-pos)) 
                   (error (string-append "first-internal-tag-position: Not number: " (as-string after-blank-pos))))
               ((and (< after-blank-pos lgt) (eqv? internal-tagging-char (string-ref str after-blank-pos))) after-blank-pos)
               ((and (< after-blank-pos lgt) (not (eqv? internal-tagging-char (string-ref str after-blank-pos))))
                   (first-internal-tag-position str lgt after-blank-pos))
               ((>= after-blank-pos lgt) #f)))
       #f)))

; Given comment-string with internal tags, which is line-oriented, lines separated by the distinguished character end-of-tagging-line.
; Parse comment-string to an a-list.
; Return an a-list of the form ((tag . "val") ...).
; An internal tag starts at tag-start-pos. tag-start-pos is an integer number, pointing at the first proper char in a tag (following internal-tagging-char).
(define (extract-internal-tagging-1 comment-string lgt tag-start-pos)
 (if (not tag-start-pos) (error "extract-internal-tagging-1 error"))
 (let* ((space-pos (find-in-string comment-string #\space tag-start-pos))  ; white space after tag name begins here
        (space-end-pos (skip-chars-in-string comment-string (list #\space #\tab) space-pos))       ; white space after tag name ends here
        (end-of-tag-pos (find-in-string comment-string end-of-tagging-line tag-start-pos))  
        (next-tag-pos (if end-of-tag-pos (first-internal-tag-position comment-string lgt end-of-tag-pos) #f))
       )
   (cond ((and space-pos space-end-pos end-of-tag-pos (< space-pos end-of-tag-pos))
               (cons
                 (list (as-symbol (substring comment-string tag-start-pos space-pos)) (substring comment-string space-end-pos end-of-tag-pos))
                 (if next-tag-pos (extract-internal-tagging-1 comment-string lgt (+ 1 next-tag-pos)) '())))
         ((and space-pos space-end-pos end-of-tag-pos (>= space-pos end-of-tag-pos))
                 (display-message (string-append "Dropping malformed internal tag: " (substring comment-string tag-start-pos end-of-tag-pos)))
                 (if next-tag-pos (extract-internal-tagging-1 comment-string lgt (+ 1 next-tag-pos)) '()))
         ((and space-pos space-end-pos) 
               (cons
                 (list (as-symbol (substring comment-string tag-start-pos space-pos)) (substring comment-string space-end-pos lgt))
                 '()))
         (end-of-tag-pos 
                 (display-message "Dropping malformed internal tag")
                 (if next-tag-pos (extract-internal-tagging-1 comment-string lgt (+ 1 next-tag-pos)) '()))
         (else '()))))

; Return the character position in comment-string that ends the tag started at tag-start-pos.
; Takes trailing occurrences of  line-continuation-char  into account.
; (define (find-end-of-tag-pos comment-string tag-start-pos)
;   (let ((eol-pos (find-in-string comment-string #\newline tag-start-pos))
;         (line-continuation-char-pos (find-in-string comment-string line-continuation-char tag-start-pos)))
;    (cond ((and line-continuation-char-pos eol-pos (= line-continuation-char-pos (- eol-pos 1)))  ; line to be continued
;              (find-end-of-tag-pos comment-string (+ eol-pos 1)))
;          (eol-pos eol-pos)
;          (else #f))))

(define (well-formed? a-list)
  (if (list? a-list)
      (cond ((null? a-list) #t)
            ((and (list? (car a-list)) (= 2 (length (car a-list))) (symbol? (car (car a-list)))  (string? (cadr (car a-list))))
                #t)
            (else #f))
      #f)) 


;;; Variables holding extracted information.
;;; The variables in this section are assigned to the abstract, title, author and affiliation,
;;; as extracted by extract-documentation-from-scheme-file from the introductory comment of the Scheme source file.
;;; Additional variables may be assigned as well, see below. This tool does not make any use of the extracted values.
;;; They are provided for other tools via the global variables documented below.
;;; .section-id extracted-variables

;; Variable holding the extracted manual abstract.
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-manual-abstract "")

;; Variable holding the extracted manual title.
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-manual-title "")

;; Variable holding the extracted manual author.
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-manual-author "")

;; Variable holding the extracted manual affiliation.
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-manual-affiliation "")

;; Variable holding the extracted value of laml-resource.
;; The extracted value is only used if the similar manual-front-matters attribute is not provided. 
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-laml-resource "")

;; Variable holding the extracted value of css-prestylesheet.
;; The extracted value is only used if the similar manual-front-matters attribute is not provided. 
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-css-prestylesheet "")

;; Variable holding the extracted value of css-stylesheet.
;; The extracted value is only used if the similar manual-front-matters attribute is not provided. 
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-css-stylesheet "" )

;; Variable holding the extracted value of css-stylesheet-copying.
;; The extracted value is only used if the similar manual-front-matters attribute is not provided. 
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-css-stylesheet-copying "")

;; Variable holding the extracted value of keep-syntactical-comment-file. A string.
;; The extracted value is only used if the similar manual-front-matters attribute is not provided. 
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
; (define extracted-keep-syntactical-comment-file "" )

;; Variable holding the extracted value of source-destination-delta.
;; The extracted value is only used if the similar manual-front-matters attribute is not provided. 
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-source-destination-delta "" )

;; Variable holding the extracted value of scheme-source-linking.
;; The extracted value is only used if the similar manual-front-matters attribute is not provided. 
;; .internal-references "extracting procedure" "extract-documentation-from-scheme-file" 
(define extracted-scheme-source-linking "" )

(cond (cs-net?  (load "/user/normark/scheme/styles.scm"))
      (else     (load "c:/users/kurt/scheme/styles.scm")))

(load "notes.scm")
(style "lecture-notes")

(set-lecture-name "")
(set-lecture-number 1)

(set-previous-lecture "")
; (set-next-lecture "")

(begin-notes)

(lecture-intro "TITLE"
  (list (copyright-owner "Kurt Nørmark") "Afdeling for Datalogi" "Aalborg Universitet")

  "INTRO"
  )

(end-notes)
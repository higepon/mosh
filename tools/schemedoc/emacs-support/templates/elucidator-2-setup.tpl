(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/elucidator-2/elucidator")

(elucidator-front-matters

 ; OVERALL attributes
 'author-mode "false"
 'processing-mode "verbose"                 ; verbose, silent
 'table-of-contents "detailed"              ; detailed, shallow
 'shallow-table-of-contents-columns "3"
 'detailed-table-of-contents-columns "2"
 'source-marker-presentation "image"        ; image, text, colored-text
 'source-marker-char "@"
 'browser-pixel-width "1100"
 'control-frame-pixel-height "120"

 ; INDEX attributes 
 'cross-reference-index "aggregated"        ; per-letter, aggregated, none
 'defined-name-index "aggregated"           ; per-letter, aggregated, none

 ; PROGRAM attributes
 'initial-program-frame "blank"             ; blank, first-source-file
 'large-font-source-file  "true"
 'small-font-source-file  "true"
 'default-source-file-font-size  "small"    ; small or large
 'program-menu "separate-frame"             ; inline-table, separate-frame, or none


 (color-scheme
   (color-entry 'group "doc" (predefined-color "documentation-background-color")) 
   (color-entry 'group "index" (predefined-color "documentation-background-color"))  
   (color-entry 'group "core" (predefined-color "program-background-color-1"))
   (color-entry 'group "others" (predefined-color "program-background-color-2"))
 )
    
 (source-files
   (program-source 'key "SOURCE-KEY" 'file-path "SOURCE-PATH"
                   'group "core" 'process "true")
   
;   (manual-source 'file-path "" 'url "")

 )
)


(begin-documentation)

(documentation-intro
    (doc-title "")
    (doc-author "")
    (doc-affiliation "")
    (doc-email "")
    (doc-abstract 
      (p "")
    )
)

(documentation-section 
   'id ""
   (section-title "")
   (section-body 
      (p "")
   )
)

(documentation-entry
    'id ""
    (entry-title "")
    (entry-body 
      (p "")
    )
)

(end-documentation)

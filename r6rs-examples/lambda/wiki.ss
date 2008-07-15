 (library (lambda wiki)
  (export wiki-main)
  (import (rnrs)
          (rnrs mutable-pairs)
          (only (mosh) read-line)
          (only (mosh list) acons assoc-ref)
          (only (mosh regexp) rxmatch)
          (only (srfi-1) first second third)
          (prefix (cgi) cgi:)
          )

(define (init)
  (display "wiki-init"))

;; Configuration
(define wiki-data-dir "/home/taro/vm/monar/wikidata/")
(define wiki-top-url  "http://lambdawiki:8001/wiki")

;;; reader for unread-line
(define (make-reader port)
  (cons port '()))

(define (reader-port r)
  (car r))

(define (reader-buffer-empty? r)
  (null? (cdr r)))

(define (pop-reader-buffer! r)
  (let ([ret (car (cdr r))]
        [rest (cdr (cdr r))])
    (set-cdr! r rest)
    ret))

(define (unread-line-reader! r line)
  (if (eof-object? line)
      '()
      (set-cdr! r (cons line (cdr r)))))

(define (read-line-reader r)
  (if (reader-buffer-empty? r)
      (read-line (reader-port r))
      (pop-reader-buffer! r)))

;;; Wiki plugin
(define *plugins* '())

(define (define-plugin name inline-proc url-proc)
  (list name inline-proc url-proc))

(define (plugin-inline-proc plugin)
  (second plugin))

(define (plugin-name plugin)
  (first plugin))

(define (plugin-url-proc plugin)
  (third plugin))

(define (register-plugin plugin)
  (set! *plugins* (acons (plugin-name plugin) plugin *plugins*)))

(define (get-plugin name)
  (assoc-ref *plugins* name))

;;; Wiki parser

(define (wiki-parse r)
  (define (parse-list match reader)
    (define (iter ulp reader nest-level)
      (let loop ([l      (read-line-reader reader)]
                 [li-lst '()])
        (cond [(eof-object? l)
               (list (if ulp 'ul 'ol) nest-level (reverse li-lst))]
              [else
               (let* ([reg     (if ulp #/^(-+)/ #/^(\++)/)]
                      [o-reg   (if ulp #/^(\++)/ #/^(\-+)/)]
                      [match   (reg l)]
                      [o-match (o-reg l)]
                      [level (if match
                                 (string-length (match))
                                 (if (and o-match (> (string-length (o-match)) nest-level))
                                     (string-length (o-match))
                                     0))])
                 (cond [(zero? level) ;; next is not list syntax
                        (unread-line-reader! reader l)
                        (list (if ulp 'ul 'ol) nest-level (reverse li-lst))]
                       [(= level nest-level) ;; same nest-level list
                        (loop (read-line-reader reader) (cons (make-li (wiki-parse-inline (match 'after))) li-lst))]
                       [(< level nest-level) ;; nest-level is shallower, so close.
                        (unread-line-reader! reader l)
                        (list (if ulp 'ul 'ol) nest-level (reverse li-lst))]
                       [else ;; nest-level is deeper, so include.
                        (unread-line-reader! reader l)
                        (let ([deeper-lst (parse-list (if match match o-match) reader)])
                          (loop (read-line-reader reader) (cons (add-li-body (car li-lst) deeper-lst) (cdr li-lst)))
                          )]))])))
    (iter (#/-+/ (match)) reader (string-length (match))))
  (define (parse-pre reader)
    (let loop ([line (read-line-reader reader)]
               [pre-lines '()])
      (cond [(eof-object? line)
             (list 'pre pre-lines)]
            [(rxmatch #/^ / line)
             (loop (read-line-reader reader)
                   (append pre-lines (list (cgi-escape line))))]
            [else
             (unread-line-reader! reader line)
             (list 'pre pre-lines)])))
  (let loop ([parsed '()]
             [line   (read-line-reader r)]
             [text   '()])
    (cond [(eof-object? line)
           (add-to-list parsed (list 'p text))]
          [else
           (let1 line (cgi-escape line)
             (cond [(#/^(-+)/ line) => (lambda (match) ;; list syntax
                                         (unread-line-reader! r line)
                                         (loop (add-to-list (add-to-list parsed (list 'p text))
                                                            (parse-list match r))
                                               (read-line-reader r)
                                               '()))]
                   [(#/^(\++)/ line) => (lambda (match) ;; list syntax
                                          (unread-line-reader! r line)
                                          (loop (add-to-list (add-to-list parsed (list 'p text))
                                                             (parse-list match r))
                                                (read-line-reader r)
                                                '()))]
                   [(#/^(\*+)/ line) => (lambda (match) ;; h1/h2/h3 syntax
                                          (loop (add-to-list (add-to-list parsed (list 'p text))
                                                             (make-head match))
                                                (read-line-reader r)
                                                '()))]
                   [(rxmatch #/^ / line) => (lambda (match) ;; pre syntax
                                      (unread-line-reader! r line)
                                      (loop (add-to-list (add-to-list parsed (list 'p text))
                                                         (parse-pre r))
                                            (read-line-reader r)
                                            '()))]
                   [(#/^#([^(^)^\s]+)(?:\(([^\)]+)\))?/ line) => (lambda (match) ;; plugin syntax
                                                                   (let1 plugin
                                                                       (if (match 2)
                                                                           (list 'plugin (match 1) (string-split (match 2) #\,))
                                                                           (list 'plugin (match 1)))
                                                                     (loop (add-to-list (add-to-list parsed (list 'p text))
                                                                                        plugin)
                                                                           (read-line-reader r)
                                                                           '())))]
                   [(#/^\r$/ line)
                    (loop (add-to-list parsed (list 'p text))
                          (read-line-reader r)
                          '())]
                   [else
                    (loop parsed (read-line-reader r) (append text (list (wiki-parse-inline line))))])
             )])))

(define (wiki-parse-inline content)
  (cond [(#/\[\[([^>^\]]+)>(https?:\/\/[^\]^\s]+)\]\]/ content) => make-alias-link] ;; [[alias>http://example.com]]
        [(#/\[\[([^\]]+)\]\]/ content) => make-wiki-name] ;; [[wiki-name]]
        [(#/https?:\/\/[^\s]+/ content) => make-link] ;; http://example.com
        [(#/&new\{([^\}]+)\}\;/ content) => make-amp]
        [else (if (equal? "" content) "" (make-inline content))]))

(define (make-li body)
  (list 'li (list body)))

(define (add-li-body li body)
  (list 'li (append (cadr li) (list body))))

(define (make-head match)
  (list 'head
        (string-length (match))
        (wiki-parse-inline (match 'after))))

(define (make-wiki-name m)
  (make-inline
        (wiki-parse-inline (m 'before))
        (list 'wiki-name
              (m 1))
        (wiki-parse-inline (m 'after))))

(define make-inline
  (lambda args
    (list 'inline (remp (lambda (s)
                            (and (string? s) (= (string-length s) 0))) args))))

(define (make-alias-link m)
  (make-inline
   (wiki-parse-inline (m 'before))
   (list 'link (m 1) (m 2))
   (wiki-parse-inline (m 'after))))

(define (make-amp match)
  (make-inline
   (wiki-parse-inline (match 'before))
   (list 'new (match 1))
   (wiki-parse-inline (match 'after))))

(define (make-link m)
  (make-inline
   (wiki-parse-inline (m 'before))
   (list 'link (m) (m))
   (wiki-parse-inline (m 'after))))

(define (wiki->html get-parameter page-name wiki)
  (define (iter wiki)
    (if (string? wiki)
        (format #t "~a" wiki)
        (case (first wiki)
          [(p)
           (unless (null? (second wiki))
             (display "<p>")
             (for-each iter (second wiki))
             (print "</p>"))]
          [(head)
           (format #t "<h~d>" (second wiki))
           (iter (third wiki))
           (format #t "</h~d>\n" (second wiki))]
          [(ul)
           (format #t "\n<ul class='list~d'>" (second wiki))
           (for-each iter (third wiki))
           (print "</ul>")]
          [(ol)
           (format #t "<ol class='list~d'>" (second wiki))
           (for-each iter (third wiki))
           (print "</ol>")]
          [(li)
           (print "<li>")
           (for-each iter (second wiki))
           (print "</li>\n")]
          [(inline)
           (for-each iter (second wiki))]
          [(wiki-name)
           (let1 page-name (second wiki)
             (cond [(page-exist? page-name)
                    (print-a (format "/wiki/~a" (cgi-encode page-name)) page-name)]
                   [else
                    (display "<span class=\"no-exist\">")
                    (print page-name)
                    (print-a (format "/wiki/~a" (cgi-encode page-name)) "?")
                    (print "</span>")]))]
          [(link)
           (format #t "<a href='~a'>~a</a>" (second wiki) (third wiki))]
          [(new)
           (format #t "<strong>~a</strong>" (second wiki))]
          [(plugin)
           (let1 plugin (get-plugin (second wiki))
             (cond [plugin
                    ((plugin-inline-proc plugin) get-parameter page-name (cddr wiki))]
                   [else
                    (format #t "~a plugin not found" (second wiki))
                    ]))]
          [(pre)
           (print "<pre>")
           (for-each display (second wiki))
           (print "</pre>")
           ]
          [else
           (cond [(string? (first wiki))
                  (format #t "<p>~a</p>" (first wiki))]
                 [else
                  (format #t "unknown element wiki [[~a]]" (car wiki))])])))
  (for-each iter wiki))

(define (page-name->path page-name)
  (string-append wiki-data-dir (cgi-encode page-name) ".dat"))

(define (wiki-enum-pages)
  (map cgi-decode
       (map (lambda (f) ((#/\.dat$/ f) 'before))
            (filter (lambda (f) (#/\.dat$/ f))
                    (sys-readdir wiki-data-dir)))))

(define (print-a uri text)
  (format #t "<a href='~a'>~a</a>" uri text))

(define (read-raw-page page-name)
  (file->string (page-name->path page-name)))

(define (print-edit-form page-name)
  (format #t "<h1>Edit ~a</h1>" (cgi-escape page-name))
  (format #t "<form method='POST' action='/wiki/~a/post'>\n  <textarea cols=50 rows=20 name='body'>~a</textarea>\n<input class='submit' type='submit' value='post'>\n  <input type='hidden' name='cmd' value='post'>\n  <input type='hidden' name='page' value='~a'>\n</form>" (cgi-encode page-name) (read-raw-page page-name) page-name))

(define (print-page get-parameter page-name)
  (let1 path (page-name->path page-name)
    (if (file-exists? path)
        (call-with-input-file path
          (lambda (p)
            (let ([r (make-reader p)])
              (wiki->html get-parameter page-name (wiki-parse r)))))
        (begin (print "page does'nt exist")
        (print-edit-form page-name)))))

(define (print-header . args)
  (define (top-menu url label)
    (format #t "<li><a href='~a'>~a</a></li>\n" url label))
  (print "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">
<html lang=\"ja\">
<head>
<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=utf-8\">")
  (format #t "<title>~a - Lambda Wiki</title>" (if (null? args) "Lambda Wiki" (cgi-escape (car args))))
  (print "<link REL=\"stylesheet\" href=\"/wiki.css\" TYPE=\"TEXT/CSS\">
<meta http-equiv=\"content-style-type\" content=\"text/css\">
</head>
<body>
<div id=\"header\">
  <h1>L<span class=\"first\">a</span>mbd<span class=\"second\">a</span> Wiki</h1>
  <div id=\"topmenu\">
    <ul>")
  (top-menu "/wiki" "Top")
  (top-menu "/wiki/page/create" "Create")
  (unless (null? args)
    (top-menu (format "/wiki/~a/edit" (car args)) "Edit")
    (top-menu (format "/wiki/~a" (car args)) "Reload"))
  (print "<li id=\"menu_left\"><a href=\"/wiki/page/list\">List</a></li>")
  (print "</ul>
  </div><!-- topmenu -->
</div>
<div id=\"navigation\" style=\"clear:both;\">
<div id=\"mynavi\">
<ul>
</ul>
</div>
</div>

<div id=\"contents\">
<div id=\"mymain\">

<div class=\"contents\">"))

(define (print-footer)
  (print "</div>
</div>
<div id=\"footer\"><p>powered by <a href=\"http://code.google.com/p/monar/\">Monar</a> Scheme</p></div>
</body>
</html>"))

(define (write-page page-name content)
  (call-with-output-file (page-name->path page-name)
    (lambda (port)
      (display content port))))

(define (print-new-page-form page-name)
  (format #t "<form method='GET' action='/wiki/page/create'>" page-name)
  (print "<input type='text' name='page' value=''><input class='submit' type='submit' value='next'></form>"))

(define (list-page . reg)
  (let1 pages (if (null? reg) (wiki-enum-pages) (filter (first reg) (wiki-enum-pages)))
    (print "<h1>List</h1>")
    (print "<ul>")
    (for-each (lambda (f)
                (print "<li>")
                (print-a (format "/wiki/~a" (cgi-encode f)) f)
                (print "</li>"))
              pages)
    (print "</ul>")))

(register-plugin (define-plugin "ls2"
                   (lambda (get-parameter page-name . args)
                     (let1 reg (string->regexp (if (null? args) (string-append "^" page-name "/") (first args)))
                       (list-page reg)))
                   (lambda (get-parameter page-name))))

(register-plugin (define-plugin "comment"
              (lambda (get-parameter page-name . args)
                (let1 comment-page (format "comment/~a" page-name)
                  (print-a (format "/wiki/~a" (cgi-encode comment-page))
                           "See comment page")
                  (print
                   (string-append
                    "<br><br>"
                    "<form method='POST' action='/wiki/" page-name "/plugin'>\n"
                    "<input type='hidden' name='cmd' value='plugin'>\n"
                    "<input type='hidden' name='plugin' value='comment'>\n"
                    "<input type='hidden' name='page' value='" page-name "'>\n"
                    "Name: <input name='name' size='15' type='text'> <input name='msg' size='70' type='text'>"
                    "<input value='Post Comment' type='submit' class=\"submit\">"
                    "</form>\n"
                    ))
                  (call-with-string-input-port (cgi-escape (read-raw-page comment-page))
;                  (call-with-string-input-port (read-raw-page comment-page)
                    (lambda (port)
                      (wiki->html get-parameter page-name (wiki-parse (make-reader port)))))))
              (lambda (get-parameter page-name)
                (let ([name (get-parameter "name")]
                      [msg  (get-parameter "msg")]
                      [comment-page (string-append "comment/" page-name)])
                  (write-page comment-page
                              (format
                               "-~a -- [[~a]]\n~a" (cgi-decode msg) (cgi-decode name) (read-raw-page comment-page)))
                  (print (cgi-moved-header (format "~a/~a" wiki-top-url (cgi-encode page-name))))))))

(register-plugin (define-plugin "topicpath"
              (lambda (get-parameter page-name . args)
                (let loop ([parent ""]
                           [paths (string-split page-name #\/)])
                  (if (null? paths)
                      '()
                      (cond [(equal? parent "")
                             (print-a "/wiki" "Top")
                             (print " / ")
                             (print-a (format "/wiki/~a" (cgi-encode (car paths))) (car paths))
                             (loop (car paths) (cdr paths))]
                            [else
                             (print " / ")
                             (print-a (format "/wiki/~a" (cgi-encode (format "~a/~a" parent (car paths)))) (car paths))
                             (loop (format "~a/~a" parent (car paths)) (cdr paths))]))))

              (lambda (get-parameter page-name)
                )))

;; todo cache with closure
(define (page-exist? page-name)
  (find (lambda (s) (equal? page-name s)) (wiki-enum-pages)))

(define (p2 str)
  (cgi-header)
  (print str))

(define (add-to-list lst a)
  (append lst (list a)))

(define (wiki-main)
  (define (get-page-cmd)
    (let1 path-info (sys-getenv "PATH_INFO")
      (cond
       [path-info
        (aif (#/\/([^\/]+)\/(edit|page|list|post|plugin|create)/ path-info)
              (values (cgi-decode (it 1)) (it 2))
              (aif (#/\/(.+)/ path-info)
                   (values (cgi-decode (it 1)) "show")
                   (values "TopPage" "show")))]
       [else
         (values "TopPage" "show")])))
  (receive (get-parameter get-request-method) (cgi-init)
    (receive (page-name cmd) (get-page-cmd)
      (cond
       [(equal? "post" cmd)
         (when (eq? 'POST (get-request-method))
           (write-to-file (page-name->path page-name)
                          (cgi-decode (get-parameter "body")))
           (cgi-moved-header (format "~a/~a" wiki-top-url (cgi-encode page-name))))]
       [(equal? "plugin" cmd)
        (let1 plugin (get-plugin (get-parameter "plugin"))
          (cond [plugin
                 ((plugin-url-proc plugin) get-parameter page-name)]
                [else
                 (cgi-header)
                 (print (string-append (get-parameter "plugin") " plugin not found"))
                 ]))]
       [else
        (cgi-header)
        (print-header page-name)
        (case cmd
          [("edit") (print-edit-form page-name)]
          [("create")
           (aif (get-parameter "page")
                (print-edit-form it)
                (print-new-page-form page-name))]
          [("list") (list-page)]
          [else
           (print-page get-parameter page-name)])
        (print-footer)]))))

;; (define (main args)
;;   (wiki-main))

;; (main (command-line))






)

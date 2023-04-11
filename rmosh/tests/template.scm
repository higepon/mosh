(import (rnrs)
        (mosh test)
        (shorten)
        (mosh file)
        (template))

(define temp-file "template-temp")

(define (test-equal-template expected template vars)
  (when (file-exists? temp-file)
    (delete-file temp-file))
  (with-output-to-file temp-file
    (^()
     (eval-template template vars)))
  (test-equal expected (file->string temp-file))
  (delete-file temp-file))

(define big "<%
  (define (for-each-with-index proc lst)
    (let ([len (length lst)])
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst)))))
%>
<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"/sameage.css\">
<title>SameAge - 同い年のための掲示板</title>
</head>
<body>
<div id=\"fb-root\"></div>
<script src=\"http://connect.facebook.net/en_US/all.js\"></script>
<script>
  FB.init({
    appId  : '120686781337412',
    status : true, // check login status
    cookie : true, // enable cookies to allow the server to access the session
    xfbml  : true  // parse XFBML
  });
</script>
<center>
<table border=0 cellpadding=0 cellspacing=0 width=\"85%\" bgcolor=#f6f6ef>
<%include _header.html %>
  <tr style=\"height:10px\"></tr>
  <tr>
    <td><h3 style=\"padding-left: 5px\">同い年のための掲示板 &nbsp;<script src=\"http://connect.facebook.net/en_US/all.js#xfbml=1\"></script><fb:like href=\"http://sameage.monaos.org/\" layout=\"button_count\" show_faces=\"true\" width=\"100\"></fb:like></h3></td>
  </tr>
  <tr>
    <td style=\"padding-left:10px\">
<% (cond [user %>
  <a href=\"/bbs/<%= (ref user aetas) %>/\">自分の同い年掲示板に書き込む</a>
<% ][else %>
  書き込みするには<a href=\"<%= (config 'oauth-uri) %>\"><img style=\"padding-right:3px\" src=\"/facebook.png\">ログイン</a>が必要です
<% ]) %>
  </td>
  </tr>
  <tr>
    <td colspan=2>
      <table border=0 width=100%>
        <tr>
          <td>
            <table border=0 cellpadding=0 cellspacing=0>
              <% (for-each (lambda (aetas) %>
              <tr>
                <td style=\"padding-top:10px\" colspan=3>
                  <h2><span class=\"title\" style=\"padding-left:10px\"><a href=\"/bbs/<%= (h (car aetas)) %>/\"><%= (h (car aetas)) %>/4 - <%= (number->string (+ 1 (string->number (car aetas)))) %>/3 生まれの掲示板</a></span></h2>
                </td>
              </tr>
              <% (for-each-with-index (lambda (i bbs) %>
              <tr>
                <td colspan=2 align=right valign=top class=\"title\" style=\"padding-left:20px\"><%= (number->string i) %>.</td>
                <td class=\"title\"><a href=\"/bbs/<%= (h (car aetas)) %>/<%= (ref bbs id) %>/\"><%= (ref bbs title) %></a><span class=\"comhead\"> (<%= (ref bbs count) %>) </span><% (when (ref bbs friend_wrote?) %>お友達が書き込みしています<% ) %>
                </td>
              </tr>
              <% ) (cdr aetas)) %>
              <% ) aetas*) %>
            </table>
          </td>
          <td align=right valign=top width=230 px style=\"padding:left:50px; padding-right: 20px;\">
    <% (for-each (lambda (user) %>
    <a href=\"<%= (ref user link) %>\"><img style=\"padding 1px; border:1px solid #CCC;\" width=\"50px\" height=\"50px\" src=\"http://graph.facebook.com/<%= (h (ref user id)) %>/picture?type=small\"></a>
    <% ) user*) %>
          </td>
        </tr>
       </table>
    </td>
  </tr>
<%include _footer.html %>
</table></center></body></html>"
)

(define big2 "<%
  (define (for-each-with-index proc lst)
    (let ([len (length lst)])
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst)))))
%>
<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"/sameage.css\">
<table border=0 cellpadding=0 cellspacing=0 width=\"85%\" bgcolor=#f6f6ef>
<%include _header.html %>3"
)
(test-equal-template "hige" "<%= a %>" '((a . "hige")))
(test-equal-template "hige" "<% (display a) %>" '((a . "hige")))
(test-equal-template "<html></html>" "<html></html>" '((a . "hige")))
(test-equal-template "<html>hoge</html>" "<html><%= a %></html>" '((a . "hoge")))
(test-equal-template "<html>\nhoge</html>" "<html>\n<%= a %></html>" '((a . "hoge")))
(test-equal-template "<li>hige</li><li>hage</li>" "<% (for-each (lambda (x) %><li><%= x %></li><% ) b) %>" '((b . '("hige" "hage"))))
(test-equal-template "" "" '())
(test-equal-template "\n3" "<%\n  (define a 3)%>\n3" '())
(test-equal-template "34" "<% (display 3) %><% (display 4) %>" '())
(test-equal-template "3%4" "<% (display 3) %>%<% (display 4) %>" '())
(test-equal-template "34" "<%= a %><%= b %>" '((a . "3") (b . "4")))
(test-equal-template "3%4" "<%= a %>%<%= b %>" '((a . "3") (b . "4")))
(test-equal-template "3%4" "<%=unsafe a %>%<%=unsafe b %>" '((a . "3") (b . "4")))
(test-equal-template "%4" "<%# hoge %>%<%=unsafe b %>" '((a . "3") (b . "4")))
(test-equal 3 (ref '((a . 3)) a))
(test-equal-template "" "<% \n %>" '())
;; (test-equal-template "" big '((a . "3") (b . "4")))
;; (test-equal-template "" big2 '((a . "3") (b . "4")))

;; todo: works on only nmosh
;(test-equal-template "#t" "<%= (hashtable? a) %>" `((a . ,(make-eq-hashtable))))
(test-results)

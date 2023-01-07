;; This reads free-vars.scm and generate Rust skelton code for them.

(import (scheme base) (scheme file) (scheme read) (scheme write) (scheme process-context) (scheme case-lambda
)
        (match) (mosh control) (only (srfi :13) string-delete) (only (mosh) format regexp-replace-all rxmatch) (only (rnrs) string-titlecase))

(define func-format "fn ~a(args: &[Object]) -> Object {
    let name: &str = \"~a\";
    panic!(\"{} not implemented\", name);
}
")

(define (file->sexp* file)
  (call-with-input-file file
    (lambda (p)
      (let loop ([sexp (read p)]
                 [sexp* '()])
        (cond
         [(eof-object? sexp) (reverse sexp*)]
         [else
          (loop (read p) (cons sexp sexp*))])))))

(define (convert-name name)
  (cond
    [(string=? name "+") "number_add"]
    [(string=? name "-") "nuber_sub"]
    [(string=? name "*") "number_mul"]
    [(string=? name "/") "number_div"]
    [(string=? name ">") "number_gt"]
    [(string=? name ">=") "number_ge"]
    [(string=? name "<") "number_lt"]
    [(string=? name "<=") "number_le"]
    [(string=? name "=") "number_eq"]
    [(#/(.*)\+$/ name) => (lambda (m) (convert-name (format "~aadd" (m 1))))]                      
    [(#/(.*)\-$/ name) => (lambda (m) (convert-name (format "~asub" (m 1))))]                      
    [(#/(.*)\*$/ name) => (lambda (m) (convert-name (format "~amul" (m 1))))]                      
    [(#/(.*)\/$/ name) => (lambda (m) (convert-name (format "~adiv" (m 1))))]                      
    [(#/(.*)\>$/ name) => (lambda (m) (convert-name (format "~agt" (m 1))))]                      
    [(#/(.*)\>=$/ name) => (lambda (m) (convert-name (format "~age" (m 1))))]                      
    [(#/(.*)\<$/ name) => (lambda (m) (convert-name (format "~alt" (m 1))))]                      
    [(#/(.*)\<=$/ name) => (lambda (m)(convert-name (format "~ale" (m 1))))]                      
    [(#/(.*)\=$/ name) => (lambda (m) (convert-name (format "~aequal" (m 1))))]                      
    ;; hoge! => hoge
    [(#/(.*)!$/ name) => (lambda (m) (format "~a_destructive" (convert-name (m 1))))]
    ;; hoge* => hoge_asterisk
    [(#/(.*)\*$/ name) => (lambda (m)
                            (convert-name (format "~a_asterisk" (m 1))))]
    [(#/(.*)\+$/ name) => (lambda (m)
                            (convert-name (format "~a_plus" (m 1))))]                         
    ;; hoge? => is_hoge
    [(#/(.*)\?$/ name) => (lambda (m)
                            (convert-name (format "is_~a" (m 1))))]
    ;; hoge->hage => hoge_to_hige
    [(#/\->/ name)
      (convert-name (regexp-replace-all #/\->/ name "_to_"))]
    ;; hoge-hage => hoge_hige
    [(#/[\-]/ name)
      (convert-name (regexp-replace-all #/\-/ name "_"))]
    ;; hoge/hage => hoge_hige
    [(#/[\/]/ name)
      (convert-name (regexp-replace-all #/\// name "_"))]  
    ;; %hage => hage
    [(#/[%\$]/ name)
      (convert-name (regexp-replace-all #/[%\$]/ name ""))]                  
    [else name]))

(define (gen-header vars)
  (display "pub fn default_free_vars(gc: &mut Gc) -> Vec<Object> {\n")
  (display "  vec![\n")
  (for-each
    (lambda (name)
      (format #t "    gc.new_procedure(~a, \"~a\"),\n" (convert-name (symbol->string name)) name))
    vars)
  (display "  ]\n"))

(define (gen-proc var)
  (match var
    [(name _) (gen-proc name)]
    [(? symbol? name) (gen-proc (symbol->string name))]
    [name 
      (format #t func-format (convert-name name) name)]))

(define (main args)
  (let* ([sexp* (file->sexp* (cadr args))]
         [vars (car (cdr (car (cdr (cdr (car sexp*))))))])
         (write vars (current-error-port))
      (gen-header vars)
      (for-each gen-proc vars)))

(main (command-line))
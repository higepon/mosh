(import 
  (rnrs)
  (srfi :78 lightweight-testing)
  (srfi :176 version-flag))

(check-set-mode! 'report-failed)

(check (list? (version-alist)) => #t)
(check (string? (cadr (assq 'version (version-alist)))) => #t)

(check-report)

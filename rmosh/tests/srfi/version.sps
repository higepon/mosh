(import 
  (rnrs)
  (mosh pp)
  (srfi :78 lightweight-testing)
  (srfi :176 version))

(check-set-mode! 'report-failed)

(pp (version-alist))
(check (list? (version-alist)) => #t)
(check (string? (cadr (assq 'version (version-alist)))) => #t)

(check-report)

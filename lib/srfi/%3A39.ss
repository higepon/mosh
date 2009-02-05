;;; http://srfi.schemers.org/srfi-39/srfi-39.html
;;; -- Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
(library (srfi :39)
  (export make-parameter parameterize)
  (import (only (system) make-parameter parameterize)))

;; (import (rnrs)
;;         (mosh) ;; format
;;         ;; (srfi :19)
;;         )


(display (call-process "ls"))
;; ;; 月の最後の日を調べる
;; (define (the-last-day-in-the-month date)
;;   (let ((m (date-month date)))
;;     (date-day
;;       (time-tai->date
;;         (subtract-duration (date->time-tai
;;           (if (= m 12)
;;             (make-date 0 0 0 0 1 1 (+ (date-year date) 1) (date-zone-offset date))
;;             (make-date 0 0 0 0 1 (+ m 1) (date-year date) (date-zone-offset date))))
;;           (make-time time-duration 0 1))))))

;; (format #t "今月の最後の日 ~a日\n" (the-last-day-in-the-month (current-date)))

;; ;; 日付の曜日を日本語で出力する
;; (let ((d (current-date)))
;;   (format #t "~a曜日\n" (string-ref "日月火水木金土" (date-week-day d))))


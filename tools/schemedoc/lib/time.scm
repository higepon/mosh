;=>man/time.sdoc

; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark.
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


;;;; This is the date and time library, on which many LAML facilities rely.
;;;; Time is represented as an integer, which represents the number of seconds since January 1, 1970.
;;;; This library contains functions which translate from seconds to year, month, day, hour, and second. 
;;;; Also the reverse translation is provided for. Besides this, the library provides functions for calculating the weekday and
;;;; the week number.
;;;; Weekday and month names can be returned in either Danish og English, depending on the variable language-preference.
;;;; The language-preference binds at a late time, not loadning time.
;;;; Be aware that you need to modify the variable time-zone-info when summer time (daylight saving time) is started and ended.
;;;; .title Reference Manual of the Time Library

; main functions: time-decode, second-count and weekday.
;
; second-count and time-decode are inverse functions. Example:
; (time-decode (second-count 1998 2 17 10 10 40)) = (1998 2 17 10 10 40)

; Given a number of seconds from January 1 in a base year, say 1900,
; calculate year, month, day, hour, minute and second. The result is
; returned as a list (which may not be the most efficient data structure...).
; Does not take leap seconds into account.

; Example (time-decode 1234567890) = (1939 2 14 0 31 30)

; take base-year as second count 0:
(define base-year 1970)

;;; Time zone and Daylight Saving Time correction.

;; Defines the time-zone. We also use this variable to adjust for daylight saving time.
;; This is, however, kind of a hack.
;; How many hours in front relative to Greenwich, England.
;; The US east coast: 5. California 8. Denmark -1.
;; If the encoded time is already corrected on your system, time-zone-info must be 0.

(define time-zone-info -2)  ; DK Summer time: -2.  DK Winter time: -1.


;;; Basic time functions.
;;; According to the conventions used in LAML, the function current-time is assumed to return the current time,
;;; represented as the number of seconds elapsed since January 1, 1970, 00:00:00.
;;; In this section there are functions that encode and decode a number of seconds to and from
;;; a conventional time representation (in terms of year, month, day, hour, minutes, second).

;; Given an integer n, which is the number of second since january 1, 1970,
;; return a list:  (year month day minutes seconds).
;; The opposite function is called second-count
(define (time-decode n)
  (let* ((year-seconds (years-and-seconds (encoded-time-zone-correction n)))
         (year (car year-seconds))
         (days-hours-minutes-seconds 
             (how-many-days-hours-minutes-seconds (cadr year-seconds)))
         (hours (second days-hours-minutes-seconds))
         (minutes (third days-hours-minutes-seconds))
         (seconds (fourth days-hours-minutes-seconds))
         (day-month (day-and-month (first days-hours-minutes-seconds) year))
         (day (first day-month))
         (month (second day-month)))
     (list year month day hours minutes seconds)))

;; Given date and time in terms of year y, month mo, day d, hour h, minutes mi, and seconds s, caluculate the second count.
;; The second-count function compensates for time zone.
;; The opposite function is called date-time.
(define (second-count y mo d h mi s)
  ; return the second count given year y, month m, day d, hours h, minutes m and seconds s
 (+ (* time-zone-info seconds-in-an-hour)
   (+
      s
      (* 60 mi )
      (* seconds-in-an-hour h)
      (* seconds-in-a-day (- (day-number d mo y) 1))
      (year-contribution y))))

;; An alias of second-count - encode year, month, day, hour, minutes, and seconds to an integer.
;; .form (time-encode y mo d h mi s)
(define time-encode second-count)


;;; Time selectors.
;;; The selectors in this section work on decoded time lists. It means that
;;; the selectors just return a given element from the decoded list representation of time. 

;; Return the year of a decoded time list. An integer greater or equal to 1970.
;; .form (year-of-time decoded-time-list)
(define year-of-time (make-selector-function 1 "year-of-time"))

;; Return the month of a decoded time list. An integer in the interval 1..12
;; .form (month-of-time decoded-time-list)
(define month-of-time (make-selector-function 2 "month-of-time"))

;; Return the days of a decoded time list. An integer in the interval 1..31
;; .form (day-of-time decoded-time-list)
(define day-of-time  (make-selector-function 3 "day-of-time"))

;; Return the hour of a decoded time list. An integer in the interval 0..23
;; .form (hour-of-time decoded-time-list)
(define hour-of-time (make-selector-function 4 "hour-of-time"))

;; Return the minutes of a decoded time list. An integer in the interval 0..59.
;; .form (minute-of-time decoded-time-list)
(define minute-of-time (make-selector-function 5 "minute-of-time"))

;; Return the seconds of a decoded time list. An integer in the interval 0..59.
;; .form (seconds-of-time decoded-time-list)
(define second-of-time (make-selector-function 6 "second-of-time"))


; ---------------------------------------------------------------------------------------------------------------
;;; Week days.
;;; The functions in this section deal with week days. Both English and Danish week day names are supported
;;; via use of the variable language-preference in laml-fundamental.scm.

; Assume that january the first in the base-year is a thursday.
; In other words, the first weekday in the base year must be (car weekdays):

; to maintain backward compatility:
(define weekdays (vector  "torsdag" "fredag" "lørdag" "søndag" "mandag" "tirsdag" "onsdag"))

; weekday name vectors:
(define weekdays-danish (vector  "torsdag" "fredag" "lørdag" "søndag" "mandag" "tirsdag" "onsdag"))
(define weekdays-english (vector  "Thursday" "Friday" "Saturday" "Sunday" "Monday" "Tuesday" "Wednesday"))

(define brief-weekdays (vector  "To" "Fr" "Lø" "Sø" "Ma" "Ti" "On"))

(define brief-weekdays-danish (vector  "To" "Fr" "Lø" "Sø" "Ma" "Ti" "On"))
(define brief-weekdays-english (vector  "Th" "Fr" "Sa" "Su" "Mo" "Tu"  "We"))


;; Given an integer, second-count, return the weekday of the time second-count.
;; Returns a string.
(define (weekday second-count)
  (let* ((day-number (quotient (encoded-time-zone-correction second-count) seconds-in-a-day))
         (weekday (modulo day-number 7)))
    (vector-ref  (weekday-list language-preference) weekday)))

;; Given an integer, second-count, return the weekday as a brief string, of the time second-count.
;; Returns a string.
(define (brief-weekday second-count)
  (let* ((day-number (quotient (encoded-time-zone-correction second-count) seconds-in-a-day))
         (weekday (modulo day-number 7)))
    (vector-ref (brief-weekday-list language-preference) weekday)))

(define (weekday-list language-preference)
  (cond ((eq? language-preference 'danish) weekdays-danish)
        ((eq? language-preference 'english) weekdays-english)
        (else (error "time library: language preference problems"))))

(define (brief-weekday-list language-preference)
  (cond ((eq? language-preference 'danish) brief-weekdays-danish)
        ((eq? language-preference 'english) brief-weekdays-english)
        (else (error "time library: language preference problems"))))


; ---------------------------------------------------------------------------------------------------------------
;;; Week number and week day number.
;;; The functions in this section deal with week numbering and, more simple, week day numbering.

; Rule: Week number one is that week in the year which contains the first thursday

;; Given a number sc, return the week number in which sc i located.
;; Weeknumbers are treated by means of Danish weeknumber rules
(define (danish-week-number sc)

  (let* ((td (time-decode sc))
         (the-day-number (day-number (caddr td) (cadr td) (car td)))
         (jan1 (second-count (car td) 1 1 0 0 0))
         (jan1-wd (weekday-number jan1)) ; monday is weekday 1
         (wn (quotient (+ the-day-number (week-number-offset jan1-wd)) 7))
        )
    (cond ((= 0 wn) (danish-week-number (second-count (- (car td) 1) 12 31 0 0 0))) ; the same the week number of last day in previous year
          ((and (= wn 53) (<= (weekday-number sc) 3)) 1)  ; not OK yet. We may choose to live with week 53 and some errors here
          (else wn))
))

(define (week-number-offset jan-1-day-number)
  (cond ((= jan-1-day-number 1) 6)  ;monday
        ((= jan-1-day-number 2) 7)  ;tuesday
        ((= jan-1-day-number 3) 8)  ; ...
        ((= jan-1-day-number 4) 9)
        ((= jan-1-day-number 5) 3)
        ((= jan-1-day-number 6) 4)
        ((= jan-1-day-number 7) 5)))

;; return the weekday-number of second-count (an integer). Monday is day number 1 in the week, sunday is day number 7.
(define (weekday-number second-count)
  (let* ((day-number (quotient (encoded-time-zone-correction second-count) seconds-in-a-day))
         (weekday-number-thurday-0 (modulo day-number 7)))  ; 0 = thursday
    (cond ((= weekday-number-thurday-0 0) 4)
          ((= weekday-number-thurday-0 1) 5)
          ((= weekday-number-thurday-0 2) 6)
          ((= weekday-number-thurday-0 3) 7)
          ((= weekday-number-thurday-0 4) 1)
          ((= weekday-number-thurday-0 5) 2)
          ((= weekday-number-thurday-0 6) 3))))



; ---------------------------------------------------------------------------------------------------------------
;;; Time pretty printing.
;;; The functions in this section return pretty printed string representation of time.

;; Given an integer, second-cound, return a list of two strings: (date time).
;; This is useful in cases you need to print the data or time as a string.
;; Takes the variable language preference into account.
(define (date-time second-count)
 (let ((time-list (time-decode second-count)))
   (let* ((year (first time-list))
          (month (second time-list))
          (day (third time-list))
          (hours (fourth time-list))
          (minutes (fifth time-list))
          (seconds (sixth time-list)))
  (cond ((eq? language-preference 'danish) (list 
					     (string-append (number->string day) ". " (get-month-name month) " "
							    (number->string year))
					     (string-append (zero-pad-string (number->string  hours)) ":"
							    (zero-pad-string (number->string  minutes)) ":"
							    (zero-pad-string (number->string  seconds)))))
        ((eq? language-preference 'english) (list 
					     (string-append (get-month-name month) " " (number->string day) ", "
							    (number->string year))
					     (string-append (zero-pad-string (number->string  hours)) ":"
							    (zero-pad-string (number->string  minutes)) ":"
							    (zero-pad-string (number->string  seconds)))))
        (else (error "date-time: language preference problems")))
    )))

;; Return a single string that pretty prints the time represented by second count.
;; Takes the variable language preference into account.
(define (date-time-one-string second-count)
  (let ((dt (date-time second-count)))
    (string-append (car dt) ", " (cadr dt))))

;; Return a string that describes the current time as generation time.
;; Takes the variable language-preference into account.
;; .reference "similar function in xhtml1.0-convenience.scm" "when-modified" "xhtml10-convenience.html#when-modified"
(define (when-generated)
 (let* ((ct (current-time))
        (dt (date-time ct))
        (day-of-week (weekday ct))
        (date (car dt))
        (time (cadr dt))
        (init-text (cond ((eq? language-preference 'danish) "Genereret: ")
                         ((eq? language-preference 'english) "Generated: ")
                         (else (error "when-generated: language preference problems"))))
       ) 
  (string-append init-text day-of-week ", " date)))
;  (string-append init-text day-of-week ", " date ", " time)))


; ---------------------------------------------------------------------------------------------------------------
;;; Time interval functions.

;; Return the number of years, months, weeks, days, hours, minutes, and seconds from second-count.
;; In this function, months are uniformly counted as 30 days, and a year is counted as 365 days.
;; Due to this, a year is not exactly counted as 12 months, and therefore slightly unexpected results may occur.
;; A list of seven integers is returned.
(define (time-interval second-count)
  (let* ((years (quotient second-count seconds-in-a-normal-year))
         (rest-1 (modulo second-count seconds-in-a-normal-year))
         (months (quotient rest-1 seconds-in-a-normal-month))
         (rest-2 (modulo rest-1 seconds-in-a-normal-month))
         (weeks (quotient rest-2 seconds-in-a-week))
         (rest-3 (modulo rest-2  seconds-in-a-week))
         (days (quotient rest-3 seconds-in-a-day))
         (rest-4 (modulo rest-3 seconds-in-a-day))
         (hours (quotient rest-4 seconds-in-an-hour))
         (rest-5 (modulo rest-4 seconds-in-an-hour))
         (minutes (quotient rest-5 60))
         (seconds (modulo rest-5 60)))
   (list years months weeks days hours minutes seconds)))

;; Return a string which presens the number of years, months, weeks, days, hours, minutes, and seconds
;; of second-count
(define (present-time-interval second-count)
  (let* ((ti (time-interval second-count))
         (y (first ti))
         (mo (second ti))
         (w (third ti))
         (d (fourth ti))
         (h (fifth ti))
         (mi (sixth ti))
         (s (seventh ti)))
    (string-append
      (if (= y 0) "" (string-append (as-string y) " " "years "))
      (if (= mo 0) "" (string-append (as-string mo) " " "months "))
      (if (= w 0) "" (string-append (as-string w) " " "weeks "))
      (if (= d 0) "" (string-append (as-string d) " " "days "))
      (if (= h 0) "" (string-append (as-string h) " " "hours "))
      (if (= mi 0) "" (string-append (as-string mi) " " "minutes "))
      (if (= s 0) "" (string-append (as-string s) " " "seconds "))
    )))



; ---------------------------------------------------------------------------------------------------------------
;;; Conventional string representation of time. 
;;; This section contains a number of functions that deal with dates and time as strings in the formats such as "ddmmyyyy" and "hhmm".
;;; The seconds are not part of the string representation.

;; Transform date and time strings to a second count (a large integer number of seconds elapsed since january 1, 1970).
;; The date parameter is a string of the form "ddmmyyyy" (eight ciffers).
;; The time parameter is of the form "hhmm" (four ciffers).
;; Assumes as a prefix that date and time represent a legal point in time. Use date-ok? and time-ok? to assure this.
;; Return #f if date is blank (or if both date and time is blank).
(define (transform-year-month-day-hour-minutes-strings date time)
  (cond ((blank-string? date) #f)
        ((and (blank-string? time) (numeric-string? date)) 
          (second-count (four-ciffer-number date 2) (two-ciffer-number date 2) (two-ciffer-number date 1) 0 0 0))
        ((and (numeric-string? date) (numeric-string? time))
          (second-count (four-ciffer-number date 2) (two-ciffer-number date 2) (two-ciffer-number date 1)
                        (two-ciffer-number time 1) (two-ciffer-number time 2) 0))
        (else (error "transform-year-month-day-hour-minutes-string: date or time string is illegal"))))

;; Return the date, in the format "ddmmyyyy" of second-count,
;; which represents the time in seconds elapsed since January 1, 1970.
(define (date-string second-count)
 (let* ((decoding (time-decode second-count))
        (y-string (as-string (first decoding)))
        (m-string (as-string (second decoding)))
        (d-string (as-string (third decoding)))
        (m-string-1 (if (< (string-length m-string) 2) (string-append "0" m-string) m-string))
        (d-string-1 (if (< (string-length d-string) 2) (string-append "0" d-string) d-string)))
   (string-append d-string-1 m-string-1 y-string)))

;; Return the date, in the format "ddmmyyyy" of second-count,
;; which represents the time in seconds elapsed since January 1, 1970.
(define (time-string second-count)
 (let* ((decoding (time-decode second-count))
        (h-string (as-string (fourth decoding)))
        (m-string (as-string (fifth decoding)))
        (h-string-1 (if (< (string-length h-string) 2) (string-append "0" h-string) h-string))
        (m-string-1 (if (< (string-length m-string) 2) (string-append "0" m-string) m-string)))
   (string-append h-string-1 m-string-1)))

; Extract a two ciffer number n of str. Outputs an integer between 0 and 99.
; Assume as a precondtion that string is long enough and purely numeric (integer).
; Example: (two-ciffer-number "123465" 1) = 12.  (two-ciffer-number "123465" 3) = 65.
(define (two-ciffer-number str n)
  (let* ((pos (- (* n 2) 2))
         (c1 (- (as-number (string-ref str pos)) 48)) ; ciffer value 1
         (c2 (- (as-number (string-ref str (+ pos 1))) 48)) ; ciffer value 2
       )
    (+ (* c1 10) c2)))

; Extract a four ciffer number n of str. Outputs an integer between 0 and 9999.
; As two-ciffer-number, but now with four ciffers
(define (four-ciffer-number str n)
  (let* ((pos (- (* n 4) 4))
         (c1 (- (as-number (string-ref str pos)) 48)) ; ciffer value 1
         (c2 (- (as-number (string-ref str (+ pos 1))) 48)) ; ciffer value 2
         (c3 (- (as-number (string-ref str (+ pos 2))) 48)) ; ciffer value 3
         (c4 (- (as-number (string-ref str (+ pos 3))) 48)) ; ciffer value 4
       )
    (+ (* c1 1000) (* c2 100) (* c3 10) c4)))

;; Return whether x is a legal date string (of the form "ddmmyyyy").
;; Exact determination, including february and leap years.
;; Uses days-in-month from the time library.
(define (date-ok? x)
  (cond ((blank-string? x) #t)
        ((and (numeric-string? x) (= 8 (string-length x)))
          (let ((d (two-ciffer-number x 1))
		(m (two-ciffer-number x 2))
		(y (four-ciffer-number x 2)))
	    (and (>= m 1) (<= m 12)
		 (>= d 1) (<= d (days-in-month m y)))))
        (else #f)))

;; Return whether x is a legal time string (of the form "hhmm")
(define (time-ok? x)
  (cond ((blank-string? x) #t)
        ((and (numeric-string? x) (= 4 (string-length x)))
          (let ((h (two-ciffer-number x 1))
		(m (two-ciffer-number x 2)))
	    (and (>= h 0) (<= h 23)
		 (>= m 0) (<= m 59))))
        (else #f)))


;; Decode a string of the form "h:m" or "h.m" to a list of the form (h m).
;; Does also accept the juxtaposition "hm" in which case m is taken as the last two ciffers.
;; Given for instance "12:35", "12.35" or "1235" return (12 35).
(define (hours-minutes-decode-string hour-minute-string)
  (let ((div-pos-colon (find-in-string hour-minute-string #\:))
        (div-pos-point (find-in-string hour-minute-string #\.)))
    (cond (div-pos-colon
            (let ((res-1 (as-number (substring hour-minute-string 0 div-pos-colon)))
		  (res-2 (as-number (substring hour-minute-string (+ div-pos-colon 1) (string-length hour-minute-string)))))
	      (list (if res-1 res-1 0)
		    (if res-2 res-2 0))))
          (div-pos-point
            (let ((res-1 (as-number (substring hour-minute-string 0 div-pos-point)))
		  (res-2 (as-number (substring hour-minute-string (+ div-pos-point 1) (string-length hour-minute-string)))))
	      (list (if res-1 res-1 0)
		    (if res-2 res-2 0))))
          ((numeric-string? hour-minute-string)
            (let ((hour-minute-number (as-number hour-minute-string)))
              (list (quotient hour-minute-number 100) (remainder hour-minute-number 100))))
          (else (laml-error "hours-minutes-decode-string: Cannot decode string" hour-minute-string)))))



;; Decode a string of the form "y-m-d" to a list of the form (y m d). A proper list of three integers.
;; Given for instance "2005-9-12", return the list (2005 9 12) representing September 12, 2005.
(define (year-month-day-decode-string year-month-day-string)
  (let* ((div-pos-1 (find-in-string year-month-day-string #\-))
         (div-pos-2 (find-in-string year-month-day-string #\- (+ div-pos-1 1))))
    (list (as-number (substring year-month-day-string 0 div-pos-1))
          (as-number (substring year-month-day-string (+ 1 div-pos-1) div-pos-2)) 
          (as-number (substring year-month-day-string (+ div-pos-2 1) (string-length year-month-day-string))))))


; ---------------------------------------------------------------------------------------------------------------
;;; Underlying time related constants and functions.
;;; In this section there is a number of auxiliary time functions.
;;; In addition, we document a number of time related constants.

;; The number of seconds in a non-leap year.
(define seconds-in-a-normal-year 31536000)

;; The number of seconds in a leap year.
(define seconds-in-a-leap-year 31622400)

;; The number of seconds in a 30-day month.
(define seconds-in-a-normal-month 2592000)

;; The number of seconds in a week.
(define seconds-in-a-week 604800)

;; The number of seconds in a day.
(define seconds-in-a-day 86400)

;; The number of seconds in an hour.
(define seconds-in-an-hour 3600)

(define month-length-normal-year
  (vector 31 28 31 30 31 30 31 31 30 31 30 31))


; a month name vector, to maintain backward compatibility:
(define month-name (vector "Januar" "Februar" "Marts" "April" "Maj" "Juni" "Juli" "August" "September"
                                                     "Oktober" "November" "December"))


; month name vectors:
(define month-name-danish (vector "Januar" "Februar" "Marts" "April" "Maj" "Juni" "Juli" "August" "September"
                                                     "Oktober" "November" "December"))

(define month-name-english (vector "January" "February" "March" "April" "May" "June" "July" "August" "September"
                                                     "October" "November" "December"))

;; Return the month name of month-number. January is number one.
;; The result depends on the free variable language-preference.
;; .reference "applied variable" "language-preference" "../../man/laml.html#language-preference"
(define (get-month-name month-number)
 (vector-ref
  (cond ((eq? language-preference 'danish) month-name-danish)
        ((eq? language-preference 'english) month-name-english)
        (else (error "time library: language preference problems")))
  (- month-number 1)
  ))

;; Return whether y is a leap year.
(define (leap-year y)
  (cond ((= (modulo y 400) 0) #t)
        ((= (modulo y 100) 0) #f)
        ((= (modulo y 4) 0) #t)
        (else #f)))

(define (years-and-seconds n)
  (cycle-years 0 base-year n))

(define (cycle-years n year u)
; The second count January 1, 00:00 in year is n. Go to next year if u
; is not in year. Return if u is in year.
; In this case return (list year u)
  (let ((year-length (if (leap-year year)
                          seconds-in-a-leap-year
                          seconds-in-a-normal-year)))
    (if (< u year-length) 
        (list year u)
        (cycle-years (+ n year-length) (+ 1 year) (- u year-length)))))

(define (day-and-month day-count year)
; Day-count is a number of days in a year. Return the list (day-in-month month-number)
  (day-and-month-help 0 1 year (+ 1 day-count)) )

; about (+ 1 day-count):  One day into the year is january 2, NOT january 1.


(define (day-and-month-help n m y c)
; We have counted n days at the beginning of the first day in month m in year y
  (if (<= c (days-in-month m y)) 
      (list c m)
      (day-and-month-help (+ n (days-in-month m y)) (+ m 1)
                          y (- c (days-in-month m y)))))

;; Return the number of days in month and year
(define (days-in-month month year)
  (if (= month 2)
      (if (leap-year year) 29 28)
      (vector-ref month-length-normal-year (- month 1))))
  

(define (how-many-days-hours-minutes-seconds n)
; Return the number days, hours, minutes and seconds in second count n.
; n is less than the number of seconds in a year
  (let* ((days (quotient n seconds-in-a-day))
         (n-rest-1 (modulo n seconds-in-a-day))
         (hours (quotient n-rest-1 seconds-in-an-hour))
         (n-rest-2 (modulo n-rest-1 seconds-in-an-hour))
         (minutes (quotient n-rest-2 60))
         (seconds (modulo n-rest-2 60)))
    (list days hours minutes seconds)))

                
(define (encoded-time-zone-correction n)
  (+ n (- (* seconds-in-an-hour time-zone-info))))


(define (zero-pad-string str)
 (if (= 1 (string-length str)) (string-append "0" str) str))





(define (day-number d m y)
 ; Return the day number of day d in month m in the year y. January 1 is day one.
 (day-count 0 1 d m y))

(define (day-count dc mc d m y)
  ; the tail recursive counter function for day-number
  (if (= mc m)
      (+ dc d)
      (day-count (+ dc (days-in-month mc y)) (+ mc 1) d m y)))

(define (year-contribution y)
  ; return the number of seconds from (and including) the base year until
  ; but not including the year y
  (year-counter 0 base-year y))

(define (year-counter sc yc y)
  (if (= yc y)
      sc
      (year-counter
          (+ sc (if (leap-year yc) seconds-in-a-leap-year seconds-in-a-normal-year))
          (+ yc 1)
          y)))


;; Return a list of (hours minutues seconds) given an integer second-count.
(define (hours-minutes-seconds-decode second-count)
  (let* ((hours (quotient second-count seconds-in-an-hour))
         (rest (remainder  second-count seconds-in-an-hour))
         (minutes (quotient rest 60))
         (seconds (remainder rest 60)))
    (list hours minutes seconds)))


;;; Other time related functions.

;; Given time-list, which is Emacs' representation of Universal time, return a single (large) integer that represents universal time.
;; .parameter time-list A list of three 16 bit integers (high low microsec) which represents a point in time after 1969. 
;; .returns high * 2**16 + low
(define (emacs-lisp-time-to-second-count time-list)
  (let ((high (car time-list))
        (low (cadr time-list)))
    (+ (* high (expt 2 16)) low)))

;; Given a second count, which represents a point in time as a single large integer, return a list of three 16 bit numbers that corresponds to Emacs's representation.
;; .parameter second-count An integer - the number of seconds elapsed since Jan 1, 1970.
;; .returns A list of three 16 bit integers (high low 0), for which high * 2**16 + low = second-count.
(define (second-count-to-emacs-lisp-time second-count)
  (let* ((two-pow-16 (expt 2 16))
         (low (remainder second-count two-pow-16))
         (high (quotient second-count two-pow-16)))
    (list high low 0)))

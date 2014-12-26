;; Idea for this algorithm comes from blog post where 
;; one can memoize rules which can be used to calculate given date 
;; to weekday in ones memory.
;; 
;; ref: https://litemind.com/how-to-become-a-human-calendar/
;;

(defpackage :org.d4gg4d.day-of-the-week
  (:use :common-lisp))

(defvar *month-to-code*
  '(1 4 4 0 2 5 0 3 6 1 4 6))

(defvar *code-to-day*
  '("Saturday" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"))

(defun resolve-year-code (year)
  (flet ((century-code (century)
		       (- 6 (* 2 (mod century 4))))
	 (last-two-digits (year)
			  (rem year 100)))
	(let* ((century-value (century-code (floor year 100)))
	       (last-digits (last-two-digits year))
	       (digits-division-value (floor last-digits 4)))
	  (mod (+ century-value last-digits digits-division-value) 7))))

(defun resolve-leap-year-code (year month)
  (if (and (zerop (mod year 4)) (> 3 month)) -1 0))

(defun resolve-month-code (month)
  (nth (1- month) *month-to-code*))

(defun resolve-day-code (year-code leap-year-code month-code day)
  (mod (+ year-code leap-year-code month-code day) 7))

(defun day-of-the-week (year month day)
  "returns the day of the week for given date by human calculatable rules"
  (let* ((year-code (resolve-year-code year))
	 (month-code (resolve-month-code month))
	 (leap-year-code (resolve-leap-year-code year month))
	 (day-code (resolve-day-code year-code leap-year-code month-code day))) ;
    (nth day-code *code-to-day*)))

(day-of-the-week 2014 12 24)
;; => "Wednesday"

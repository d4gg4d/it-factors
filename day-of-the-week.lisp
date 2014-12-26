;; Idea for this algorithm comes from blog post where 
;; one can memoize rules which can be used to calculate given date 
;; to weekday in ones memory.
;; 
;; ref: https://litemind.com/how-to-become-a-human-calendar/
;;

(defpackage :org.d4gg4d.day-of-the-week
  (:use :common-lisp))

(defun partial (f &rest args)
  "currying function"
  (lambda (&rest more-args)
    (apply f (append args more-args))))

(defun -put-entry-to (hash-table key value)
  "set entry set to hash table"
  (setf (gethash key hash-table) value))

(defun put-entry-to (hash-table key-value)
  (-put-entry-to hash-table (car key-value) (cadr key-value)))

(defun last-two-digits (year)
  (nth-value 1 (truncate year 100)))

(defvar month-to-code)
(setq month-to-code (make-hash-table))
(mapcar (partial 'put-entry-to month-to-code) '((1 1)
						(2 4)
						(3 4)
						(4 0)
						(5 2)
						(6 5)
						(7 0)
						(8 3)
						(9 6)
						(10 1)
						(11 4)
						(12 6)))

(defvar code-to-day)
(setq code-to-day (make-hash-table))
(mapcar (partial 'put-entry-to code-to-day) '((0 "Saturday")
					      (1 "Sunday")
					      (2 "Monday")
					      (3 "Tuesday")
					      (4 "Wednesday")
					      (5 "Thursday")
					      (6 "Friday")))

(defun century-code (century)
  (- 6 (* 2 (mod century 4))))

(defun resolve-year-code (year)
  (let* ((century-value (century-code (floor year 100)))
	 (last-digits (last-two-digits year))
	 (digits-division-value (floor last-digits 4)))
    (mod (+ century-value last-digits digits-division-value) 7)))

(defun resolve-leap-year-code (year month)
  (if (and (zerop (mod year 4)) (> 3 month)) -1 0))

(defun fetch-month-code (month)
  (gethash month month-to-code))

(defun resolve-day-code (year-code leap-year-code month-code day)
  (mod (+ year-code leap-year-code month-code day) 7))

(defun day-of-the-week (year month day)
  "returns the day of the week for given date by human calculatable rules"
  (let* ((year-code (resolve-year-code year))
	 (month-code (fetch-month-code month))
	 (leap-year-code (resolve-leap-year-code year month))
	 (day-code (resolve-day-code year-code leap-year-code month-code day)))
    (gethash day-code code-to-day)))

(day-of-the-week 2014 12 24)
;; => "Wednesday"

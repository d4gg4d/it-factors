;;
;; Tail recursion version of classic factorial task (in common lisp)
;; 
;; "Given a number n, return list of numbers which product is number
;; n and if list is only number n then n is a prime."
;;
(defun factors-of(n)
  (labels 
      ((factors (n i list)
	       (cond ((= n 1) list)
		     ((= 0 (mod n i)) (factors (/ n i) i (cons i list)))
		     (t (factors n (1+ i) list)))))
    (factors n 2 '())))

;; simple unit test cases, but first utility function
(defun test@func=>in==out(test-function input output)
  (and (equal (eval (cons test-function (car input))) (car output)) 
       (if (not (cdr input)) 
	   t
	   (test@func=>in==out test-function (cdr input) (cdr output)))))

;; now test for factorial function
(test@func=>in==out 'factors-of '((2) (107) (8) (9) (286)) '((2) (107) (2 2 2) (3 3) (13 11 2)))

(factors-of 2)
(factors-of 107)
(factors-of 8)
(factors-of 9)
(factors-of 286)

(factors-of 9234567635731) 
;;=> (701554937 13163)

(defpackage :org.d4gg4d.it-factors
  (:use :common-lisp))

(defun fibonacci-value(index array)
  "calculating coefficients for fibonacci triangle"
  (let ((i (car index)) (j (cadr index)))
    (setf (aref array i j) (+ (aref array (1- i) (1- j)) (aref array (1- i) (- j 2))))))

(defun sum-of-col (array nrow)
  "summing over given array row"
  (apply '+ (mapcar (lambda (el) (aref array (car el) (cadr el))) 
		    (loop :for i from 0 :to (1- (array-dimension array 0)) :collect (list i nrow)))))

(defun fibonacci (n)
  "Calculating nth fibonacci with linear programming algorithm"
  (if (zerop n)
      0
      (let* ((n-dim (1+ n)) (coefs (make-array `(,n-dim ,n-dim))))
	(setf (aref coefs 1 1) 1)
	(mapcar (lambda (el) (fibonacci-value el coefs))
		(loop :for i :from 2 :to n-dim
		      :appending (loop :for j :from i :to (min (1- n-dim) (* i 2)) :collect (list i j))))
	(sum-of-col coefs (1- n-dim)))))

;; first 20 fibonacci...
(mapcar 'fibonacci (loop :for i :from 0 :to 20 :collect i))
;; => (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)

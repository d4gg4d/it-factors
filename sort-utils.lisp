;; practicing to do quicksort in common-lisp
;; - simple version
;; - how would you improve efficiency?

(defun quicksort (list)
  (if (null (cdr list))
      list
      (let ((less '()) (greater '()) (pivot (car list)))
	(mapc #'(lambda (x) (if (<= x pivot) (push x greater) (push x less))) (cdr list))
	(append (quicksort less) (cons pivot (quicksort greater))))))


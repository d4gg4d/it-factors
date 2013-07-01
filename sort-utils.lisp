;; practicing to do quicksort in common-lisp
;; - simple version
;; - how would you improve efficiency?

(defun quicksort (list)
  (if (null (cdr list))
      list
      (let ((less '()) (greater '()) (pivot (car list)))
	(mapc #'(lambda (x) (if (> x pivot) (push x greater) (push x less))) (cdr list))
	(append (quicksort less) (cons pivot (quicksort greater))))))

(quicksort '(1 3 10 5435 2 94 7 4 1))
;; (:ok "=> (1 1 2 3 4 7 10 94 5435)")
